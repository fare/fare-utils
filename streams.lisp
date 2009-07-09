;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(in-package :fare-utils)

(def*generic call-with-output (x thunk)
  (:method ((x null) thunk)
           (with-output-to-string (s) (funcall thunk s)))
  (:method ((x (eql t)) thunk)
           (funcall thunk *standard-output*) nil)
  (:method ((x stream) thunk)
           (funcall thunk x) nil)
  (:method ((x string) thunk)
           (assert (fill-pointer x))
           (with-output-to-string (s x) (funcall thunk s)))
  (:method (x thunk)
           (declare (ignore thunk))
           (error "not a valid stream designator ~S" x)))

(def*macro with-output ((x &optional (value x)) &body body)
  `(call-with-output ,value #'(lambda (,x) ,@body)))

;;; Probably something similar for with call-with-input...

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
          (*print-readably* t)
          (*print-pretty* nil)
          (*print-circle* t)
          (*package* (find-package *safe-package*)))
      (apply #'write x r))))
