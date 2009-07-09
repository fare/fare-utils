;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(in-package :fare-utils)

(def*fun absolute-pathname-p (p)
  "Return true iff pathname P is an absolute pathname"
  (let ((d (pathname-directory p)))
    (and (consp d) (eq (car d) :absolute))))

