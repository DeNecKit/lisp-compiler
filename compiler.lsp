(defvar *program* nil)


(defun emit (val)
  (setq *program* (append *program* val)))

(defun compile-progn (lst)
  (unless (null lst)
	(inner-compile (car lst))
	(compile-progn (cdr lst))))

(defun inner-compile (expr)
  (if (atom expr)
      (emit `((lda ,expr)))
	  (case (car expr)
        ('progn (compile-progn (cdr expr))))))

(defun compile (expr)
  (setq *program* nil)
  (inner-compile expr)
  (when (null *program*)
    (setq *program* `((lda ,nil))))
  *program*)
