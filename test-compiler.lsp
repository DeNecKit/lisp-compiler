;; Тест компиляции и выполнения скомпилированной программы.
(defun test (expr-list expected-res)
  (let ((program (compile expr-list)))
    (print '-----------------)
    (print expr-list)
    (print program)
    (print (assert (vm-run program) expected-res))))


(test '(progn 1 2 3) 3)
(test '(progn (progn 2 (progn 11) 4) (progn 3)) 3)
(test '(progn) nil)
(test '(progn (progn (progn) (progn 1)) 2) 2)
(test '(if t 1 2) 1)
(test '(if 5 1 2) 2)
(test `(if ,nil 1 2) 2)
(test '(if t (if t 1 2) 3) 1)
(test `(if t (if ,nil 1 2) 3) 2)
(test `(if ,nil (if ,nil 1 2) 3) 3)
