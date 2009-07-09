;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Utilities dealing with atomicity wrt concurrency in various lisps

(in-package #:fare-utils)


(defmacro with-atomic-file-creation ((s filename &optional tmpname) &body body)
  "create some file contents and atomically commit them to file when they are
complete"
  `(evaluating-once (,filename ,tmpname)
     (unless ,tmpname (setq ,tmpname (make-tmpname-for ,filename)))
     (with-open-file (,s ,tmpname :direction :io
			     :if-does-not-exist :create
			     :if-exists :error)
       (unwind-protect
	   (prog1
	       ,@body
	     (rename-file ,tmpname ,filename))
	 (ignore-errors (delete-file ,tmpname) nil)))))
#| There is still a small race condition, since there is a small moment just
after the file is opened that we don't have protection against async signals;
but at worst it will leak a dangling temporary file, which we cannot avoid
anyway: we may get killed at any time by a more powerful signal or a segfault,
and no unwind-protect can help us at this point; the absence of ultimate
protection against internal or external errors is precisely the reason why
we're doing non-atomic file operations to a temporary file to be blessed
atomically, anyway.

Under Linux, rename-file is atomic, except if you're using an old
non-journalled filesystem and the kernel crashes just in the middle
of the rename, in which case who knows what is left after the fsck.
|#



#| ;;; broken...
(defmacro with-interrupts-disabled (&body body)
  #+sbcl (sb-sys:without-interrupts ,@body)
  #-(or sbcl)
  (progn ,@body))

(defmacro atomic-setq (var val)
  `(with-interrupts-disabled ()
     (setq ,var ,val)))

(defmacro unwind-protect-against-interrupts (body protection)
  #-(or)
  `(unwind-protect ,body ,protection))

(defun normalize-let-binding (x)
  (cond
    ((and (consp x) (consp (cdr x)) (null (cddr x)))
     x)
    ((symbolp x) (list x nil))
    (error "malformed let binding ~A" x)))

(defmacro protected-let* (bindings body protection)
  (let ((bindings (mapcar 'normalize-let-binding bindings)))
    `(let ,(mapcar 'car bindings)
      (unwind-protect
       (progn (atomic-setq ,@(reduce 'append bindings)) ,body)
       ,protection))))
|#
