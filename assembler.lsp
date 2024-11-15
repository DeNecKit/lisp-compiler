;; *inst-table* - список инструкций для генерации байт-кода.
(defvar *inst-table*
  '(lda
    jmp jnt
    global-set global-get
    push pop drop local-get local-set
    call ret))
;; *inst-jmp-table* - список номеров инструкций перехода.
(defvar *inst-jmp-table* '(1 2 10))

;; Превращает список инструкций с метками в байт-код.
;; program - список инструкций.
(defun assemble (program)
  (let ((bytecode nil))
    (while (not (null program))
      (let ((cur-inst (car program)))
        (if (symbolp cur-inst)
            (setq bytecode
                  (append bytecode `(,cur-inst)))
            (let ((opcode (car cur-inst))
                  (inst-i nil))
              (foldl '(lambda (i inst)
                       (when (eq opcode inst)
                         (setq inst-i i))
                       (++ i))
                     0 *inst-table*)
              (when (null inst-i)
                (error (concat
                        "Unknown instruction: "
                        (symbol-name opcode))))
              (setq bytecode
                    (append bytecode
                            `((,inst-i ,@(cdar program))))))))
      (setq program (cdr program)))
    (asm-calc-labels bytecode)))

;; Заменяет метки на относительные адреса.
;; program - байт-код; список.
(defun asm-calc-labels (program)
  (let ((lbls nil)
        (pc 0)
        (bytecode nil))
    (app '(lambda (inst)
           (if (symbolp inst)
               (setq lbls
                     (append lbls
                             (list (cons inst pc))))
               (setq pc (+ pc (list-length inst)))))
         program)
    (setq pc 0)
    (app '(lambda (inst)
           (unless (symbolp inst)
             (let ((new-inst inst)
                   (opcode (car inst)))
               (when (contains *inst-jmp-table* opcode)
                 (let ((lbl (cadr inst))
                       (addr 0))
                   (app '(lambda (lbl-pair)
                          (when (eq (car lbl-pair) lbl)
                            (setq addr (cdr lbl-pair))))
                        lbls)
                   (when (null addr)
                     (error "Unreachable"))
                   (setq new-inst `(,opcode ,(- addr pc)))))
               (setq bytecode (append bytecode `(,@new-inst))
                     pc (+ pc (list-length inst))))))
         program)
    (let* ((bytecode-len (foldl '(lambda (len _)
                                  (++ len))
                                0 bytecode))
           (bytecode-arr (make-array bytecode-len)))
      (foldl '(lambda (i inst)
               (seta bytecode-arr i inst)
               (++ i))
             0 bytecode)
      bytecode-arr)))

(defun contains (lst elem)
  (if (null lst)
      nil
      (if (eq (car lst) elem)
          t
          (contains (cdr lst) elem))))
