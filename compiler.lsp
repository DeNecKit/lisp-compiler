;; *program* хранит накопленный результат компиляции функцией compile.
(defvar *program* nil)


;; Создаёт список инструкций для вычисления S-выражения expr на виртуальной машине с помощью функции vm-run.
;; expr - S-выражение
(defun compile (expr)
  (setq *program* nil)
  (inner-compile expr)
  (when (null *program*)
    (setq *program* `((lda ,nil))))
  *program*)

(defun inner-compile (expr)
  (if (atom expr)
      (emit `(lda ,expr))
	  (case (car expr)
        ('progn (compile-progn (cdr expr)))
        ('if (compile-if (cdr expr))))))

;; Компилирует блок progn.
;; lst - список S-выражений внутри блока progn.
(defun compile-progn (lst)
  (unless (null lst)
	(inner-compile (car lst))
	(compile-progn (cdr lst))))

;; Вычисляет if-выражение.
;; if-body - тело if-выражения (условие, ветка по "Да" и по "Нет").
(defun compile-if (if-body)
  (let ((if-cond (car if-body))
        (if-true (cadr if-body))
        (if-false (caddr if-body))
        (label-false (gensym))
        (label-after (gensym)))
    (inner-compile if-cond)
    (emit `(jnt ,label-false))
    (inner-compile if-true)
    (emit `(jmp ,label-after))
    (emit label-false)
    (inner-compile if-false)
    (emit label-after)))

;; Добавляет инструкцию к текущей накопленной программе *program*.
(defun emit (val)
  (setq *program* (append *program* `(,val))))
