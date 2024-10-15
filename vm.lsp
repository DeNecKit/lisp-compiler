;; Выполняемая в данный момент программа.
(defvar *cur-program* nil)

;; Регистры:

;; ACC - хранит результат последней операции.
(defvar *acc* nil)
;; GLOBALS-MEM - хранит значения глобальных переменных.
(defvar *globals-mem* nil)


;; Инструкции:

;; LDA - поместить S-выражение expr в регистр ACC.
(defmacro lda (expr)
  `(setq *acc* ',expr))

;; JMP - безусловный переход на метку label.
(defmacro jmp (label)
  `(go ,label))

;; JNT - если ACC != t, то переход на метку label.
(defmacro jnt (label)
  `(unless *acc* (jmp ,label)))

;; GLOBAL-SET - присвоить глобальной переменной с индексом i значение регистра ACC
(defun global-set (i)
  (seta *globals-mem* i *acc*))

;; GLOBAL-GET - присвоить регистру ACC значение глобальной переменной с индексом i
(defun global-get (i)
  (setq *acc* (aref *globals-mem* i)))


;; Выполняет программу program.
;; program - список инструкций (пар опкодов и операндов).
;; Возвращает значение регистра ACC.
(defun vm-run (program)
  (setq *cur-program* program)
  (setq *acc* nil)
  (when (> *globals-count* 0)
    (setq *globals-mem*
          (make-array *globals-count*)))
  (inner-vm-run)
  *acc*)

(defmacro inner-vm-run ()
  `(tagbody ,@*cur-program*))
