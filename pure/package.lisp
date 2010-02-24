;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module (:depends-on ("basic-utils" "memoization")))

(in-package :cl)

(defpackage :pure
  (:nicknames #:pure-functional)
  (:use :cl :fare-utils :interface :order :eq)
  (:export

   ;;; Trees
   #:<tree>
   #:node #:locate #:join
   #:left #:right #:leftmost #:rightmost

   ;;; Functional Maps and Containers: classes
   #:<map> #:<alist>
   #:<binary-tree> #:<avl-tree>
   #:<number-map> #:<nm>
   #:<hash-table> #:<equal-hash-table>
   #:<fmim>

   ;;; Functional Maps and Containers: Generic Functions
   #:empty
   #:empty-p
   #:lookup
   #:insert
   #:drop
   #:first-key-value
   #:decons
   #:fold-left
   #:fold-right
   #:for-each
   #:join
   #:divide
   #:size
   #:join/list
   #:divide/list
   #:update-key
   #:map/2
   #:convert

   #:check-invariant))
