;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Order

#+xcvb (module (:depends-on ("eq")))

(in-package :fare-utils)

(defpackage :order
  (:use :eq)
  (:export
   #:<order> #:<numeric> #:<string> #:<char>
   #:<lessp> #:<compare> #:<key> #:<parameter>
   #:< #:<= #:> #:>= #:= #:compare))

(defclass order:<order> (eq:<eq>) ())
(defgeneric order:< (i x y))
(defgeneric order:<= (i x y))
(defgeneric order:> (i x y))
(defgeneric order:>= (i x y))
(defgeneric order:compare (i x y))

(defclass <order-from-lessp> (order:<order>) ())
(defmethod order:<= ((i <order-from-lessp>) x y)
  (not (order:< i y x)))
(defmethod order:> ((i <order-from-lessp>) x y)
  (order:< i y x))
(defmethod order:>= ((i <order-from-lessp>) x y)
  (not (order:< i x y)))
(defmethod order:= ((i <order-from-lessp>) x y)
  (not (or (order:< i x y) (order:< i y x))))
(defmethod order:compare ((i <order-from-lessp>) x y)
  (cond
    ((order:< i x y) -1)
    ((order:> i x y) 1)
    (t 0)))

(defclass <order-from-compare> (order:<order>) ())
(defmethod order:< ((i <order-from-compare>) x y)
  (ecase (order:compare i x y)
    ((-1) t)
    ((0 1) nil)))
(defmethod order:<= ((i <order-from-compare>) x y)
  (ecase (order:compare i x y)
    ((-1 0) t)
    (1 nil)))
(defmethod order:> ((i <order-from-compare>) x y)
  (ecase (order:compare i x y)
    ((-1 0) nil)
    ((1) t)))
(defmethod order:>= ((i <order-from-compare>) x y)
  (ecase (order:compare i x y)
    ((-1) nil)
    ((0 1) t)))
(defmethod order:= ((i <order-from-compare>) x y)
  (ecase (order:compare i x y)
    ((-1 1) nil)
    ((0) t)))

(defclass order:<lessp> (<order-from-lessp>)
  ((lessp :initarg :lessp :reader lessp-function)))
(defun order:<lessp> (lessp)
  (memo:memoized 'make-instance 'order:<lessp> :lessp lessp))
(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for name :in names :collect
                    `(defmethod ,name ((i order:<lessp>) x y)
                       (,(conc-symbol :call name) (lessp-function i)
                              (funcall (key-function i) x)
                              (funcall (key-function i) y)))))))
  (delegate order:< order:<= order:> order:>= order:=))

(defun call< (lessp x y)
  (funcall lessp x y))
(defmethod call<= (lessp x y)
  (not (funcall lessp y x)))
(defmethod call> (lessp x y)
  (funcall lessp y x))
(defmethod call>= (lessp x y)
  (not (funcall lessp x y)))
(defmethod call= (lessp x y)
  (not (or (funcall lessp x y) (funcall lessp y x))))
(defmethod call-compare (lessp x y)
  (cond
    ((funcall lessp i x y) -1)
    ((funcall lessp i y x) 1)
    (t 0)))

(macrolet ((builtin (name prefix)
             `(progn
                (defclass ,name (order:<order>) ())
                ,@(loop :for n :in '(< <= > >=) :collect
                    `(defmethod ,(conc-symbol-in :order n) ((i ,name) x y)
                       (,(conc-symbol prefix n) x y)))
                (defmethod order:compare ((i ,name) x y)
                  (cond
                    ((,(conc-symbol prefix '<) x y) -1)
                    ((,(conc-symbol prefix '>) x y) 1)
                    (t 0)))
                (defparameter ,name (make-instance ',name)))))
  ;;(builtin function call)
  (builtin order:<numeric> "")
  (builtin order:<char> char)
  (builtin order:<string> string))

(defclass order:<key> ()
  ((order-key :initarg :key :reader key-function)
   (order-key-interface :initarg :order :reader order-interface)))
(defun order:<key> (&key key order)
  (memo:memoized 'make-instance 'order:<key> :key key :order order))
(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for name :in names :collect
                    `(defmethod ,name ((i order:<key>) x y)
                       (,name (order-interface i)
                              (funcall (key-function i) x)
                              (funcall (key-function i) y)))))))
  (delegate order:< order:<= order:> order:>= order:= order:compare))

(defclass order:<parameter> ()
  ((order-interface :initarg :order :reader order-interface)))
(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for name :in names :collect
                    `(defmethod ,name ((i order:<parameter>) x y)
                       (,name (order-interface i) x y))))))
  (delegate order:< order:<= order:> order:>= order:= order:compare))


;;; simple algorithm using order
(defun sorted-list-differences (list1 list2 &key (order order:<numeric>))
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
            (let ((r (compare order (car list1) (car list2))))
              (cond
                ((= r 0)
                 (rec (cdr list1) (cdr list2) only1 (cons (car list1) common) only2))
                ((< r 0)
                 (rec (cdr list1) list2 (cons (car list1) only1) common only2))
                (t ;(> r 0)
                 (rec list1 (cdr list2) only1 common (cons (car list2) only2)))))))))
    (rec list1 list2 nil nil nil)))
