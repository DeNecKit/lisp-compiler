(defun test (expr-list expected-res)
  (let ((program (compile expr-list)))
    (print '-----------------)
    (print expr-list)
    (print program)
    (assert (vm-run program) expected-res)))


(test '(progn 1 2 3) 3)
(test '(progn (progn 2 (progn 11) 4) (progn 3)) 3)
(test '(progn) nil)
(test '(progn (progn (progn) (progn 1)) 2) 2)
