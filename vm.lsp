;; Выполняемая в данный момент программа.
(defvar *cur-program* nil)

;; Регистры:

;; ACC - хранит результат последней операции.
(defvar *acc* nil)


;; Инструкции:

;; LDA - поместить S-выражение expr в регистр ACC.
(defun lda (expr)
  (setq *acc* expr))

;; JMP - безусловный переход на метку label.
(defmacro jmp (label)
  `(go ,label))

;; JNT - если ACC != t, то переход на метку label.
(defmacro jnt (label)
  `(unless *acc* (jmp ,label)))


;; Выполняет программу program.
;; program - список инструкций (пар опкодов и операндов).
;; Возвращает значение регистра ACC.
(defun vm-run (program)
  (setq *cur-program* program)
  (inner-vm-run)
  *acc*)

(defmacro inner-vm-run ()
  `(tagbody ,@*cur-program*))
