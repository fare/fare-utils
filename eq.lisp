;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Equality

#+xcvb (module (:depends-on ("basic-utils" "memoization")))

(in-package :fare-utils)

(defpackage :eq
  (:use)
  (:export
   #:<eq> #:<eq-simple> #:<eq-slot>
   #:<equal>
   #:= #:test-function
   #:<hashable>
   #:hash
   ))

(defclass eq:<eq> () ())
(defparameter eq:<eq> (memo:memoized 'make-instance 'eq:<eq>))
(defgeneric eq:= (i x y))
(defgeneric eq:test-function (i)
  (:documentation "test function for <eq> interface"))

(defmethod eq:= ((i eq:<eq>) x y)
  (eql x y))
(defmethod eq:test-function ((i eq:<eq>))
  #'eql)

(defclass eq:<eq-simple> (eq:<eq>) ())
(defmethod eq:test-function ((i eq:<eq-simple>))
  (lambda (x y) (eq:= i x y)))

(defclass eq:<eq-slot> (eq:<eq>)
  ((test :initform #'eql :initarg :test :reader eq:test-function)))
(defmethod eq:= ((i eq:<eq-slot>) x y)
  (funcall (eq:test-function i) x y))

(defclass eq:<hashable> (eq:<eq>) ())
(defgeneric eq:hash (i x))
(defmethod eq:hash ((i eq:<hashable>) x)
  (sxhash x))

(defclass eq:<equal> (eq:<hashable>) ())
(defparameter eq:<equal> (memo:memoized 'make-instance 'eq:<equal>))
(defmethod eq:= ((i eq:<equal>) x y)
  (equal x y))
(defmethod eq:test-function ((i eq:<equal>))
  #'equal)



