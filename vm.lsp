; Архитектура:
; ACC - регистр результата последней операции

; Список инструкций:
; lda X - поместить X в регистр ACC

(defvar *acc* nil)


(defun inst-lda (val)
    ; Помещает val в ACC
  (setq *acc* val))

(defun vm-run (program)
  ; program - список инструкций (пар опкодов и операндов)
  ; Выполняет поочерёдно каждую инструкцию
  ; Возвращает значение регистра ACC  
  (if (null program)
      *acc*
      (let ((inst (car program)))
        (unless (null inst)
          (let ((opcode (car inst))
                (ops (cdr inst)))
            (funcall opcode (car ops))))
        (vm-run (cdr program)))))


(vm-run nil)
(vm-run '((inst-lda 10)))
(vm-run `((inst-lda (a b c ,nil 1 2))))
(vm-run `(,nil))
(vm-run `((inst-lda 10) ,nil))
(vm-run `((inst-lda ,nil)))
