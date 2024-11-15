;; *cur-bytecode* - хранит текущий исполняемый байт-код.
(defvar *cur-bytecode* nil)
;; *cur-inst* - хранит текущую инструкцию для выполнения макросом inner-vm-exec-inst.
(defvar *cur-inst* nil)


;; Регистры:

;; ACC - хранит результат последней операции.
(defvar *acc* nil)
;; GLOBALS-MEM - хранит значения глобальных переменных.
(defvar *globals-mem* nil)
;; PC - хранит адрес текущей выполняемой инструкции.
(defvar *pc* 0)
;; *stack-max* - размер стека.
(defvar *stack-max* 2048)
;; STACK - стек общего назначения.
(defvar *stack* (make-array *stack-max*))
;; *stack-head* - указатель на вершину стека.
(defvar *stack-head* 0)

;; Инструкции:

;; LDA - поместить S-выражение expr в регистр ACC.
(defmacro lda (expr)
  `(setq *acc* ',expr)
  `(setq *pc* (+ *pc* 2)))

;; JMP - безусловный относительный переход на адрес addr.
(defun jmp (addr)
  (setq *pc* (+ *pc* addr)))

;; JNT - если ACC != t, то относительный переход на адрес addr.
(defun jnt (addr)
  (if *acc*
      (setq *pc* (+ *pc* 2))
      (jmp addr)))

;; GLOBAL-SET - присвоить глобальной переменной с индексом i значение регистра ACC.
(defun global-set (i)
  (seta *globals-mem* i *acc*)
  (setq *pc* (+ *pc* 2)))

;; GLOBAL-GET - присвоить регистру ACC значение глобальной переменной с индексом i.
(defun global-get (i)
  (setq *acc* (aref *globals-mem* i))
  (setq *pc* (+ *pc* 2)))

;; PUSH - добавляет значение регистра ACC в стэк.
(defun push ()
  (stack-push *acc*)
  (setq *pc* (+ *pc* 1)))

(defun stack-push (val)
  (seta *stack* *stack-head* val)
  (setq *stack-head* (++ *stack-head*))
  (when (>= *stack-head* *stack-max*)
    (error "Stack overflow")))

;; POP - загружает верхний элемент стека в регистр ACC, при этом удаляет этот элемент из стека.
(defun pop ()
  (setq *acc* (stack-pop))
  (setq *pc* (+ *pc* 1)))

(defun stack-pop ()
  (setq *stack-head* (- *stack-head* 1))
  (when (< *stack-head* 0)
    (error "Stack underflow"))
  (aref *stack* *stack-head*))

;; LOCAL-GET - загружает в ACC значение i-го элемента стэка, начиная с верхушки.
(defun local-get (i)
  (let ((idx (- (- *stack-head* i) 1)))
    (when (< idx 0)
      (error "Stack underflow"))
    (setq *acc* (aref *stack* idx)
          *pc* (+ *pc* 2))))

;; LOCAL-SET i - присваивает элементу стека с индексом i, начиная с верхушки, значение регистра ACC.
(defun local-set (i)
  (let ((idx (- (- *stack-head* i) 1)))
    (when (< idx 0)
      (error "Stack underflow"))
    (seta *stack* idx *acc*)
    (setq *pc* (+ *pc* 2))))

;; DROP n - удаляет n верхних элементов из стека.
(defun drop (n)
  (setq *stack-head* (- *stack-head* n))
  (when (< *stack-head* 0)
      (error "Stack underflow"))
  (setq *pc* (+ *pc* 2)))

;; CALL addr - добавляет адрес следующей инструкции в стэк и производит переход на относительный адрес addr.
(defun call (addr)
  (stack-push (+ *pc* 2))
  (jmp addr))

;; RET - производит переход на адрес из верхушки стэка, при этом удаляет этот адрес из стэка.
(defun ret ()
  (jmp (- (stack-pop) *pc*)))


;; Выполняет программу program.
;; bytecode - массив инструкций (пар опкодов и операндов).
;; Возвращает значение регистра ACC.
(defun vm-run (bytecode)
  (setq *cur-bytecode* bytecode
        *acc* nil
        *pc* 0
        *stack-head* 0)
  (when (> *globals-count* 0)
    (setq *globals-mem*
          (make-array *globals-count*)))
  (let ((bytecode-len (array-size bytecode)))
    (while (< *pc* bytecode-len)
      (case (aref bytecode *pc*)
        (0 (vm-exec-inst 'lda 1))
        (1 (vm-exec-inst 'jmp 1))
        (2 (vm-exec-inst 'jnt 1))
        (3 (vm-exec-inst 'global-set 1))
        (4 (vm-exec-inst 'global-get 1))
        (5 (vm-exec-inst 'push 0))
        (6 (vm-exec-inst 'pop 0))
        (7 (vm-exec-inst 'drop 1))
        (8 (vm-exec-inst 'local-get 1))
        (9 (vm-exec-inst 'local-set 1))
        (10 (vm-exec-inst 'call 1))
        (11 (vm-exec-inst 'ret 0))
        (otherwise (error (concat
                           "Unknown opcode: "
                           (inttostr (aref bytecode *pc*)))))))
    *acc*))

;; Выполняет инструкцию inst с args-len операндами.
(defun vm-exec-inst (inst args-len)
  (let ((inst-args nil))
    (for i 1 (+ args-len 1)
         (setq inst-args
               (append inst-args
                       `(,(aref *cur-bytecode*
                                (+ *pc* i))))))
    (setq *cur-inst*
          (append `(,inst) inst-args))
    (inner-vm-exec-inst)))

(defmacro inner-vm-exec-inst ()
  *cur-inst*)
