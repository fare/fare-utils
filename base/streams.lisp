;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("package" "base/utils")))

(in-package :fare-utils)

(def*generic call-with-output-stream (x fun)
  (:documentation
   "Calls FUN with an actual stream argument, behaving like FORMAT with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error.")
  (:method ((x null) fun)
    (with-output-to-string (s) (funcall fun s)))
  (:method ((x (eql t)) fun)
    (funcall fun *standard-output*) nil)
  #-genera
  (:method ((x stream) fun)
    (funcall fun x) nil)
  (:method ((x string) fun)
    (assert (fill-pointer x))
    (with-output-to-string (s x) (funcall fun s)))
  (:method (x fun)
    (declare (ignorable fun))
    (cond
      #+genera
      ((typep x 'stream) (funcall fun x) nil)
      (t (error "not a valid stream designator ~S" x)))))

(def*macro with-output-stream ((x &optional (value x)) &body body)
  `(call-with-output-stream ,value #'(lambda (,x) ,@body)))

(def*generic call-with-input-stream (x fun)
  (:documentation
   "Calls FUN with an actual stream argument, coercing behaving like READ with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error.")
  (:method ((x null) fun)
    (funcall fun *terminal-io*))
  (:method ((x (eql t)) fun)
    (funcall fun *standard-input*) nil)
  #-genera
  (:method ((x stream) fun)
    (funcall fun x) nil)
  (:method ((x string) fun)
    (with-input-from-string (s x) (funcall fun s)))
  (:method (x fun)
    (declare (ignorable fun))
    (cond
      #+genera
      ((typep x 'stream) (funcall fun x) nil)
      (t (error "not a valid stream designator ~S" x)))))

(def*macro with-input-stream ((x &optional (value x)) &body body)
  `(call-with-input-stream ,value #'(lambda (,x) ,@body)))

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
