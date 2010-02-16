;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Order

#+xcvb (module (:depends-on ("eq")))

(in-package :fare-utils)

(defpackage :order
  (:use :eq)
  (:export
   #:<order> #:<numeric> #:<string>
   #:<order-lessp> #:<order-compare>
   #:< #:<= #:> #:>= #:=
   #:compare))

(defclass order:<order> (eq:<eq>) ())
(defgeneric order:< (i x y))
(defgeneric order:<= (i x y))
(defgeneric order:> (i x y))
(defgeneric order:>= (i x y))
(defgeneric order:compare (i x y))

(defclass order:<order-lessp> (order:<order>) ())
(defmethod order:<= ((i order:<order-lessp>) x y)
  (not (order:< i y x)))
(defmethod order:> ((i order:<order-lessp>) x y)
  (order:< i y x))
(defmethod order:>= ((i order:<order-lessp>) x y)
  (not (order:< i x y)))
(defmethod order:= ((i order:<order-lessp>) x y)
  (not (or (order:< i x y) (order:< i y x))))
(defmethod order:compare ((i order:<order-lessp>) x y)
  (cond
    ((order:< i x y) -1)
    ((order:> i x y) 1)
    (t 0)))

(defclass order:<order-compare> (order:<order>) ())
(defmethod order:< ((i order:<order-compare>) x y)
  (ecase (order:compare i x y)
    ((-1) t)
    ((0 1) nil)))
(defmethod order:<= ((i order:<order-compare>) x y)
  (ecase (order:compare i x y)
    ((-1 0) t)
    (1 nil)))
(defmethod order:> ((i order:<order-compare>) x y)
  (ecase (order:compare i x y)
    ((-1 0) nil)
    ((1) t)))
(defmethod order:>= ((i order:<order-compare>) x y)
  (ecase (order:compare i x y)
    ((-1) nil)
    ((0 1) t)))
(defmethod order:= ((i order:<order-compare>) x y)
  (ecase (order:compare i x y)
    ((-1 1) nil)
    ((0) t)))

(defclass order:<numeric> (order:<order-lessp>) ())
(defmethod order:< ((i order:<numeric>) x y)
  (< x y))
(defmethod order:<= ((i order:<numeric>) x y)
  (<= x y))
(defmethod order:> ((i order:<numeric>) x y)
  (> x y))
(defmethod order:>= ((i order:<numeric>) x y)
  (>= x y))
(defmethod order:= ((i order:<numeric>) x y)
  (= x y))

(defclass order:<string> (order:<order-lessp>) ())
(defmethod order:< ((i order:<string>) x y)
  (string< x y))
(defmethod order:<= ((i order:<string>) x y)
  (string<= x y))
(defmethod order:> ((i order:<string>) x y)
  (string> x y))
(defmethod order:>= ((i order:<string>) x y)
  (string>= x y))
(defmethod order:= ((i order:<string>) x y)
  (string= x y))
