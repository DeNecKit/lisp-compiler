;; *test-compile-failed* - флаг - провалился ли хотя бы один тест компилятора.
(defvar *test-compile-failed* nil)


;; Тест компиляции программы.
(defun test-compile (expr expected-res)
  (unless *test-compile-failed*
    (let ((program (compile expr)))
      (print "Expression")
      (print expr)
      (print "Compiler")
      (print program)
      (print "Generator")
      (dolist (ins (generate program))
        (print ins))
      (let ((res (assert program expected-res)))
        (print res)
        (when (equal (car res) 'fail)
          (setq *test-compile-failed* t))))))

(test-compile '(progn 1 2 3) '(seq (const 1) (seq (const 2) (const 3))))
(test-compile '(progn) '(const ()))
(test-compile '(if 1 (progn 1 3) (progn 2))
	      '(ALTER (CONST 1) (SEQ (CONST 1) (CONST 3)) (CONST 2)))
(test-compile '(if t t nil)
	      '(ALTER (GLOBAL-REF 0) (GLOBAL-REF 0) (GLOBAL-REF 1)))
(test-compile '(progn (setq a 1 b 2) a b)
	      '(SEQ (SEQ (GLOBAL-SET 2 (CONST 1)) (GLOBAL-SET 3 (CONST 2))) (SEQ (GLOBAL-REF 2) (GLOBAL-REF 3))))

(test-compile '(setq a (+ (* 1 2) (* 2 3)))
	      '(GLOBAL-SET 2 (PRIM + ((PRIM * ((CONST 1) (CONST 2))) (PRIM * ((CONST 2) (CONST 3)))))))

(test-compile '(defun test (x) (setq a 2) (setq x a) x)
	      '(LABEL TEST (FIX-CLOSURE 1 (SEQ (SEQ (GLOBAL-SET 2 (CONST 2)) (SEQ (LOCAL-SET 0 (GLOBAL-REF 2)) (LOCAL-REF 0))) (RETURN)))))

(test-compile '(progn (defun test (x) x x) (test 10))
	      '(SEQ (LABEL TEST (FIX-CLOSURE 1 (SEQ (SEQ (LOCAL-REF 0) (LOCAL-REF 0)) (RETURN)))) (REG-CALL TEST 0 ((CONST 10)))))

(test-compile '(progn (defun test (x y) (progn x y)) (test 10 (if t 3 4)))
	      '(SEQ (LABEL TEST (FIX-CLOSURE 2 (SEQ (SEQ (LOCAL-REF 0) (LOCAL-REF 1)) (RETURN)))) (REG-CALL TEST 0 ((CONST 10) (ALTER (GLOBAL-REF 0) (CONST 3) (CONST 4))))))

(test-compile '((lambda (x) ((lambda (y) (cons x y)) 1)) 2)
	      '(SEQ (LABEL G445 (FIX-CLOSURE 1 (SEQ (SEQ (LABEL G446 (FIX-CLOSURE 1 (SEQ (PRIM CONS ((DEEP-REF 1 0) (LOCAL-REF 0))) (RETURN)))) (REG-CALL G446 1 ((CONST 1)))) (RETURN)))) (REG-CALL G445 0 ((CONST 2)))))

(test-compile '(progn (defun fac (x) (if (equal x 1) 1 (* x (fac (- x 1))))) (fac 4))
	      '(SEQ (LABEL FAC (FIX-CLOSURE 1 (SEQ (ALTER (PRIM EQUAL ((LOCAL-REF 0) (CONST 1))) (CONST 1) (PRIM * ((LOCAL-REF 0) (REG-CALL FAC 0 ((PRIM - ((LOCAL-REF 0) (CONST 1)))))))) (RETURN)))) (REG-CALL FAC 0 ((CONST 4)))))
