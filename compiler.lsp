(defun compile-func (func-body)
  (unless (null func-body)
    (let ((func (car func-body))
          (args (cdr func-body))
          (res nil))
      (cond
        ((eq func 'progn)
         (while (not (null args))
           (setq res (inner-compile `(,(car args)) res))
           (setq args (cdr args))))
        (t (list-add res nil)))
      res)))

(defun inner-compile (expr-list program)
  (if (null expr-list)
      program
      (let ((expr (car expr-list)))
        (list-add program
                  (if (atom expr)
                      `((inst-lda ,expr))
                      (compile-func expr)))
        (inner-compile (cdr expr-list) program))))

(defun compile (expr-list)
    ; expr-list - список s-выражений
    ; Возвращает набор инструкций для vm.lsp
  (inner-compile expr-list nil))


(setq p (compile '(1 2 3)))
(vm-run p)
(setq p (compile '(a (progn 1 2) (progn (progn)) (progn))))
(vm-run p)
(setq p (compile `(1 2 3 ,nil (a) (b) (c))))
(vm-run p)
(setq p (compile '(a b (progn 1 2))))
(vm-run p)
(compile '(a a a a a a a a a a a a a))
