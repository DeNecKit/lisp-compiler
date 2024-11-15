;; *cur-inst* - хранит текущую инструкцию для выполнения макросом vm-exec-inst.
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
  `(setq *pc* (++ *pc*)))

;; JMP - безусловный относительный переход на адрес addr.
(defun jmp (addr)
  (setq *pc* (+ *pc* addr)))

;; JNT - если ACC != t, то относительный переход на адрес addr.
(defun jnt (addr)
  (if *acc*
      (setq *pc* (++ *pc*))
      (jmp addr)))

;; GLOBAL-SET - присвоить глобальной переменной с индексом i значение регистра ACC.
(defun global-set (i)
  (seta *globals-mem* i *acc*)
  (setq *pc* (++ *pc*)))

;; GLOBAL-GET - присвоить регистру ACC значение глобальной переменной с индексом i.
(defun global-get (i)
  (setq *acc* (aref *globals-mem* i))
  (setq *pc* (++ *pc*)))

;; PUSH - добавляет значение регистра ACC в стэк.
(defun push ()
  (stack-push *acc*)
  (setq *pc* (++ *pc*)))

(defun stack-push (val)
  (seta *stack* *stack-head* val)
  (setq *stack-head* (++ *stack-head*))
  (when (>= *stack-head* *stack-max*)
    (error "Stack overflow")))

;; LOCAL-GET - загружает в ACC значение i-го элемента стэка, начиная с верхушки.
(defun local-get (i)
  (let ((idx (- (- *stack-head* i) 1)))
    (when (< idx 0)
      (error "Stack underflow"))
    (setq *acc* (aref *stack* idx)
          *pc* (++ *pc*))))

;; POP - загружает верхний элемент стека в регистр ACC, при этом удаляет этот элемент из стека.
(defun pop ()
  (setq *acc* (stack-pop))
  (setq *pc* (++ *pc*)))

(defun stack-pop ()
  (setq *stack-head* (- *stack-head* 1))
  (when (< *stack-head* 0)
    (error "Stack underflow"))
  (aref *stack* *stack-head*))

;; LOCAL-SET i - присваивает элементу стека с индексом i, начиная с верхушки, значение регистра ACC.
(defun local-set (i)
  (let ((idx (- (- *stack-head* i) 1)))
    (when (< idx 0)
      (error "Stack underflow"))
    (seta *stack* idx *acc*)
    (setq *pc* (++ *pc*))))

;; DROP n - удаляет n верхних элементов из стека.
(defun drop (n)
  (setq *stack-head* (- *stack-head* n))
  (when (< *stack-head* 0)
      (error "Stack underflow"))
  (setq *pc* (++ *pc*)))

;; CALL addr - добавляет адрес следующей инструкции в стэк и производит переход на относительный адрес addr.
(defun call (addr)
  (stack-push (+ *pc* 1))
  (jmp addr))

;; RET - производит переход на адрес из верхушки стэка, при этом удаляет этот адрес из стэка.
(defun ret ()
  (jmp (- (stack-pop) *pc*)))


;; Выполняет программу program.
;; bytecode - массив инструкций (пар опкодов и операндов).
;; Возвращает значение регистра ACC.
(defun vm-run (bytecode)
  (setq *acc* nil
        *pc* 0
        *stack-head* 0)
  (when (> *globals-count* 0)
    (setq *globals-mem*
          (make-array *globals-count*)))
  (let ((bytecode-len (array-size bytecode)))
    (while (< *pc* bytecode-len)
      (setq *cur-inst* (aref bytecode *pc*))
      (vm-exec-inst))
    *acc*))

(defmacro vm-exec-inst ()
  `,*cur-inst*)
