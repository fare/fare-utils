;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module (:depends-on ("basic-utils" "memoization")))

(in-package :fare-utils)

(defpackage :pf
  (:nicknames #:pure-functional)
  (:use)
  (:export

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
