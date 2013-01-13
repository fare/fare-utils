;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("package" "base/utils")))

(in-package :fare-utils)

(def*parameter *standard-readtable* (copy-readtable nil))
(def*parameter *safe-package* :cl)

(def*fun safe-read (&optional s (eof-error-p t) eof-value)
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*read-default-float-format* 'single-float)
          (*readtable* *standard-readtable*)
          (*package* (find-package *safe-package*)))
      (read-preserving-whitespace s eof-error-p eof-value))))

(def*fun safe-write (x &rest r)
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*read-default-float-format* 'single-float)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*print-circle* t)
          (*package* (find-package *safe-package*)))
      (apply #'write x r))))

(defun call-with-user-output-file (f fun)
  (if (equal f "-")
    (funcall fun *standard-output*)
    (with-open-file (o f :direction :output :if-exists :supersede)
      (funcall fun o))))

(def*macro with-user-output-file ((s f) &body body)
  `(call-with-user-output-file ,f #'(lambda (,s) ,@body)))
