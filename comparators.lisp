;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;; Comparison and ordered structures

(in-package :fare-utils)

(defun-inline nonzerop (x)
  (not (zerop x)))

(defun-inline number-comparator (x1 x2)
  (cond
    ((< x1 x2) -1)
    ((> x1 x2) 1)
    (t 0)))

(defun-inline comparator< (comparator x1 x2)
  (< (funcall comparator x1 x2) 0))
(defun-inline comparator= (comparator x1 x2)
  (= (funcall comparator x1 x2) 0))
(defun-inline comparator> (comparator x1 x2)
  (> (funcall comparator x1 x2) 0))

(defun-inline char-comparator (x1 x2)
  (number-comparator (char-code x1) (char-code x2)))

(defun keyed-comparator (key comparator)
  #'(lambda (x1 x2)
      (funcall comparator (funcall key x1) (funcall key x2))))

(defun pair-comparator (fa fb)
  #'(lambda (x1 x2)
      (let ((ac (funcall fa (car x1) (car x2))))
        (if (not (zerop ac))
            ac
            (funcall fb (cdr x1) (cdr x2))))))

(defun vector-comparator (ec)
  #'(lambda (x1 x2)
      (loop for i from 0
            for e1 across x1
            for e2 across x2
            for cr = (funcall ec e1 e2) do
            (cond
              ((< cr 0) (return -1))
              ((> cr 0) (return 1)))
            finally
            (let ((lc (number-comparator (length x1) (length x2))))
              (cond
                ((< lc 0) (return -2))
                ((> lc 0) (return 2))
                (t (return 0)))))))

(let ((f (vector-comparator #'char-comparator)))
  (defun string-comparator (x1 x2)
    (funcall f x1 x2)))

(defun comparator-from-lessp (lessp)
  #'(lambda (x1 x2)
      (cond
        ((funcall lessp x1 x2) -1)
        ((funcall lessp x2 x1) 1)
        (t 0))))


;;; simple algorithm using a comparator
(defun sorted-list-differences (list1 list2 &key (comparator #'number-comparator))
  (labels
      ((rec (list1 list2 only1 common only2)
         (cond
           ((and (null list1) (null list2))
            (values (nreverse only1) (nreverse common) (nreverse only2)))
           ((null list1)
            (values (nreverse only1) (nreverse common) (nreconc only2 list2)))
           ((null list2)
            (values (nreconc only1 list1) (nreverse common) (nreverse only2)))
           (t
            (let ((r (funcall comparator (car list1) (car list2))))
              (cond
                ((= r 0)
                 (rec (cdr list1) (cdr list2) only1 (cons (car list1) common) only2))
                ((< r 0)
                 (rec (cdr list1) list2 (cons (car list1) only1) common only2))
                (t ;(> r 0)
                 (rec list1 (cdr list2) only1 common (cons (car list2) only2)))))))))
    (rec list1 list2 nil nil nil)))


;;; mixin for classes relying on an ordering
(defclass order-mixin ()
  ((comparator :initarg :comparator
               :accessor order-comparator)))

(defmethod shared-initialize :after ((container order-mixin) slot-names
                                     &key comparator lessp &allow-other-keys)
  (declare (ignorable slot-names))
  (when (and lessp (not comparator))
    (setf (order-comparator container)
          (comparator-from-lessp lessp))))

(defgeneric order< (order item1 item2)
  (:method ((order order-mixin) item1 item2)
           (comparator< (order-comparator order) item1 item2)))
(defgeneric order= (order item1 item2)
  (:method ((order order-mixin) item1 item2)
           (comparator= (order-comparator order) item1 item2)))
(defgeneric order> (order item1 item2)
  (:method ((order order-mixin) item1 item2)
           (comparator< (order-comparator order) item1 item2)))

