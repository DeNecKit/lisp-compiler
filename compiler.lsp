;; *program* - хранит накопленный результат компиляции функцией compile.
(defvar *program* nil)
;; *globals* - список имён глобальных переменных.
(defvar *globals* nil)
;; *globals-count* - число глобальных переменных в списке *globals*.
(defvar *globals-count* 0)
;; *comp-err* - флаг, произошла ли ошибка при последней компиляции.
(defvar *comp-err* nil)
;; *comp-err-msg* - сообщение о последней ошибки компиляции.
(defvar *comp-err-msg* nil)


;; Создаёт список инструкций для вычисления S-выражения expr на виртуальной машине с помощью функции vm-run.
;; expr - S-выражение
(defun compile (expr)
  (setq *program* nil)
  (setq *globals* nil)
  (setq *globals-count* 0)
  (setq *comp-err* nil)
  (setq *comp-err-msg* nil)
  (inner-compile expr)
  (if *comp-err*
      (progn
        (setq *comp-err-msg* (concat "Compilation error: " *comp-err-msg*))
        ;; (print *comp-err-msg*)
        nil)
      (progn
        (when (null *program*)
          (setq *program* `((lda ,nil))))
        *program*)))

(defun inner-compile (expr)
  (unless *comp-err*
    (if (atom expr)
        (if (and (symbolp expr)
                 (not (eq expr t)))
            (let ((var (find-global-var expr)))
              (if (null var)
                  (comp-err (concat "No such global variable: " (symbol-name expr)))
                  (emit `(global-get ,var))))
            (emit `(lda ,expr)))
	    (case (car expr)
          ('progn (compile-progn (cdr expr)))
          ('if (compile-if (cdr expr)))
          ('setq (compile-setq (cdr expr)))
          (otherwise (comp-err (concat "Unknown func: " (symbol-name (car expr)))))))))

;; Компилирует блок progn.
;; lst - список S-выражений внутри блока progn.
(defun compile-progn (lst)
  (unless (null lst)
	(inner-compile (car lst))
	(compile-progn (cdr lst))))

;; Компилирует if-выражение.
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

;; Компилирует setq-выражение.
;; setq-body - пара из символа и выражения, которое необходимо установить этому символу.
(defun compile-setq (setq-body)
  (if (null setq-body)
      (comp-err "setq: no params")
      (if (or
           (atom setq-body)
           (null (cdr setq-body)))
          (comp-err "setq: no expression to set")
          (let* ((setq-sym (car setq-body))
                 (setq-expr (cadr setq-body))
                 (setq-i (find-global-var setq-sym)))
            (if (symbolp setq-sym)
                (if (or (eq setq-sym t))
                    (comp-err (concat "setq: variable name is constant: " (symbol-name setq-sym)))
                    (progn
                      (inner-compile setq-expr)
                      (when (null setq-i)
                        (setq *globals* (append *globals* `(,setq-sym)))
                        (setq setq-i *globals-count*)
                        (setq *globals-count* (++ *globals-count*)))
                      (emit `(global-set ,setq-i))))
                (comp-err "setq: variable name is not a symbol"))))))

;; Добавляет инструкцию к текущей накопленной программе *program*.
(defun emit (val)
  (setq *program* (append *program* `(,val))))

;; Производит поиск переменной в глобальном окружении по символу.
;; Возвращает индекс в массиве глобального окружения, если символ найден, иначе nil.
(defun find-global-var (var)
  (let ((globals *globals*)
        (i 0))
    (while (not
            (or
             (null globals)
             (eq (car globals) var)))
      (setq globals (cdr globals))
      (setq i (++ i)))
    (if (null globals) nil i)))

;; Устанавливает флаг ошибки компиляции и сохраняет сообщение об ошибке.
(defun comp-err (msg)
  (setq *comp-err* t)
  (setq *comp-err-msg* msg))
