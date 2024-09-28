(defvar *acc* nil)
(defvar *cur-program* nil)


(defun lda (val)
  (setq *acc* val))

(defun jmp (label)
  (unless (null label)
    (let ((res *cur-program*))
      (while (not (eq (car res) label))
        (setq res (cdr res)))
      res)))

(defun jnt (label)
  (when (not (eq *acc* t))
    (jmp label)))

(defun inner-vm-run (program)
  (if (null program)
      *acc*
      (let ((inst (car program)))
        (unless (atom inst)
          (let ((opcode (car inst))
                (op (cadr inst)))
            (case opcode
              ('jmp (setq program (jmp op)))
              ('jnt
               (let ((jnt-res (jnt op)))
                 (unless (null jnt-res)
                   (setq program jnt-res))))
              (otherwise (funcall opcode op)))))
        (inner-vm-run (cdr program)))))

(defun vm-run (program)
  (setq *cur-program* program)
  (inner-vm-run program))
