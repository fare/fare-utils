;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Equality

#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :fare-utils)

(defpackage :eq
  (:use)
  (:export
   #:<eq> #:<eq-simple> #:<eq-slot> #:<eql>
   #:= #:test-function))

(defclass eq:<eq> () ())
(defgeneric eq:= (i x y))
(defgeneric eq:test-function (i)
  (:documentation "test function for <eq> interface"))

(defclass eq:<eq-simple> (eq:<eq>) ())
(defmethod eq:test-function ((i eq:<eq-simple>))
  (lambda (x y) (eq:= i x y)))

(defclass eq:<eq-slot> (eq:<eq>)
  ((test :initform #'eql :initarg :test :reader eq:test-function)))
(defmethod eq:= ((i eq:<eq-slot>) x y)
  (funcall (eq:test-function i) x y))

(defclass eq:<eql> (eq:<eq>) ())
(defmethod eq:= ((i eq:<eql>) x y)
  (eql x y))
(defmethod eq:test-function ((i eq:<eql>))
  #'eql)
