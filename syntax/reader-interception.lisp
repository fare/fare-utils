(in-package :cl-user)

(defpackage :reader-interception
  (:use :cl)
  (:export
   #:prepare-reader-interception
   #:with-reader-interception
   #:*reader*))

(in-package :reader-interception)

(defvar *saved-readtable* (copy-readtable))
(defvar *reader* 'standard-read)
(defvar *interception-readtable* (copy-readtable nil))
(defvar *intercepted-characters* (make-hash-table :test 'equal))

(defun initialize-interception ()
  (setf *interception-readtable* (copy-readtable nil))
  (setf *intercepted-characters* (make-hash-table :test 'equal)))

(defun prepare-character (x)
  (unless (gethash x *intercepted-characters*)
    (setf (gethash x *intercepted-characters*) t)
    (set-macro-character x 'intercept-char-reader nil
                         *interception-readtable*)))

(defun intercept-char-reader (stream char)
  (unread-char char stream)
  (prog1 (let ((*readtable* *saved-readtable*))
           (funcall *reader* stream))
    (prepare-reader-interception stream)))

(defun prepare-reader-interception (hint &key (external-format :default))
  (etypecase hint
    (null
     nil)
    ((eql t)
     (prepare-all-ascii))
    (character
     (prepare-character hint))
    (string
     (when (plusp (length hint))
       (prepare-character (aref hint 0))))
    (pathname
     (with-open-file (s hint :direction :input
                        :if-does-not-exist :error
                        :element-type 'character
                        :external-format external-format)
       (prepare-reader-interception s)))                      
    (stream
     (let ((next (read-char hint nil nil)))
       (when next
         (unread-char next)
         (prepare-character next)))))
  (values))

(defun prepare-all-ascii ()
  (loop :for c :from 0 :below 128 :do
    (prepare-character (code-char c))))

(defun call-with-reader-interception (thunk &optional hint (reader *reader*))
  (prepare-reader-interception hint)
  (let ((*reader* reader)
        (*saved-readtable* *readtable*)
        (*readtable* *interception-readtable*))
    (funcall thunk)))

(defmacro with-reader-interception
    ((&optional hint (reader '*reader*)) &body body)
  `(call-with-reader-interception (lambda () ,@body) ,hint ,reader))
