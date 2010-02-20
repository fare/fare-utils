;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module (:depends-on ("basic-utils" "memoization")))

(in-package :cl)

(defpackage :pf
  (:nicknames #:pure-functional)
  (:use)
  (:export

   ;;; General purpose gfs
   #:check-invariant

   ;;; Functional Maps and Containers: classes
   #:<map> #:<alist>
   #:<binary-tree> #:<avl-tree>
   #:<integer-map> #:<im>
   #:<hash-table> #:<equal-hash-table> #:<ht> #:<equal-ht>
   #:<fast-append-integer-map> #:<faim>

   ;;; Functional Maps and Containers: Generic Functions
   #:empty
   #:empty-p
   #:lookup
   #:insert
   #:remove
   #:first-key-value
   #:decons
   #:fold-left
   #:fold-right
   #:for-each
   #:append
   #:divide
   #:count
   #:append/list
   #:divide/list
   #:update
   #:merge
   #:convert))

(defgeneric pf:check-invariant (interface role object)
  (:documentation "Check whether an OBJECT fulfills the invariant(s) required
to play a given ROLE with respect to the given INTERFACE.
Interface is an interface, role is a class or keyword,
object is whatever makes sense.
On success the OBJECT itself is returned. On failure an error is signalled."))
