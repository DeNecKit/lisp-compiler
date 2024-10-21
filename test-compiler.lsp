;; *test-failed* - флаг - провалился ли хотя бы один тест.
(defvar *test-failed* nil)


;; Тест компиляции и выполнения скомпилированной программы.
(defun test (expr-list expected-res)
  (unless *test-failed*
    (let ((program (compile expr-list))
          (target nil))
      (if *comp-err*
          (setq target *comp-err-msg*)
          (setq target (vm-run program)))
      (let ((res (assert target expected-res)))
        (when (eq (car res) 'fail)
          (print expr-list)
          (print program)
          (print res)
          (setq *test-failed* t))))))

;; Проверка, все ли тесты успешны
(defun check-tests ()
  (when (null *test-failed*)
    (print "All tests OK")))


(test '(progn 1 2 3) 3)
(test '(progn (progn 2 (progn 11) 4) (progn 3)) 3)
(test '(progn) nil)
(test '(progn (progn (progn) (progn 1)) 2) 2)

(test '(if) "[Compilation error] if: no params")
(test '(if t) "[Compilation error] if: not enough params")
(test '(if t 1) "[Compilation error] if: not enough params")
(test '(if t 1 2 3) "[Compilation error] if: too many params")
(test '(if t 1 2) 1)
(test '(if 5 1 2) 2)
(test `(if ,nil 1 2) 2)
(test '(if t (if t 1 2) 3) 1)
(test `(if t (if ,nil 1 2) 3) 2)
(test `(if ,nil (if ,nil 1 2) 3) 3)

(test 'a "[Compilation error] No such global variable: A")
(test '(setq a 5) 5)
(test '(setq) "[Compilation error] setq: no params")
(test '(setq a) "[Compilation error] setq: no expression to set")
(test '(setq 1 1) "[Compilation error] setq: variable name is not a symbol")
(test '(setq (a) 1) "[Compilation error] setq: variable name is not a symbol")
(test '(setq t 1) "[Compilation error] setq: variable name is constant: T")
(test `(setq ,nil 1) "[Compilation error] setq: variable name is not a symbol")
(test '(progn (setq a 5) a) 5)
(test '(progn (setq a 10) (setq b a) b) 10)
(test '(progn (setq a (progn)) a) nil)

(check-tests)
