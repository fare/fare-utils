;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module (:depends-on ("basic-utils" "memoization")))

(in-package :cl)

(defpackage :pure
  (:nicknames #:pure-functional)
  (:use :cl :fare-utils)
  (:export

   ;;; General purpose gfs
   #:check-invariant

   ;;; Trees
   #:<tree>
   #:node #:locate #:join
   #:left #:right #:leftmost #:rightmost

   ;;; Functional Maps and Containers: classes
   #:<map> #:<alist>
   #:<binary-tree> #:<avl-tree>
   #:<integer-map> #:<im>
   #:<hash-table> #:<equal-hash-table> #:<ht> #:<equal-ht>
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
   #:update
   #:map/2
   #:convert))

(in-package :pure)

(defclass <interface> ()
  ()
  (:documentation "An interface, encapsulating an algorithm"))

(defclass <type> (<interface>) ()
  (:documentation "An interface encapsulating a particular type of objects"))

(defgeneric make (<type> &key)
  (:documentation "Given a <type>, create an object conforming to the interface
based on provided initarg keywords, returning the object."))

(defgeneric update (<type> object &key)
  (:documentation "Update OBJECT by overriding some of its slots
with those specified as initarg keywords, returning a new object."))

(defgeneric check-invariant (<type> object)
  (:documentation "Check whether an OBJECT fulfills the invariant(s) required
to play a given ROLE with respect to the given INTERFACE.
Interface is an interface, role is a class or keyword,
object is whatever makes sense.
On success the OBJECT itself is returned. On failure an error is signalled."))

(defmethod check-invariant :around (type object)
  (call-next-method)
  object)
