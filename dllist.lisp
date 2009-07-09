;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; doubly-linked lists
#|
doubly-linked-mixin and doubly-linked list.

THIS IS BROKEN CODE THAT NEEDS BE WORKED ON SOME MORE

IDEAS
* a list is any contents between two list headers, so we can do things like the treadmill GC trick


IMPLEMENTED:

TODO:

|#

(in-package :fare-utils)

(defclass doubly-linked-mixin ()
  ((previous :accessor doubly-linked-previous)
   (next :accessor doubly-linked-next)))

(defmethod container-empty-p ((container doubly-linked-mixin))
  (eq container (doubly-linked-next container)))

(defmethod isolate-node! ((node doubly-linked-mixin))
  (setf (doubly-linked-next node) node
        (doubly-linked-previous node) node)
  node)

(defmethod empty-container! ((container doubly-linked-mixin))
  (isolate-node! container)
  container)

(defmethod unlink-node! ((container doubly-linked-mixin))
  (let ((previous (doubly-linked-previous container))
        (next (doubly-linked-next container)))
    (setf (doubly-linked-next previous) next
          (doubly-linked-previous next) previous)
    nil))

(defun join-dl-chains (first1 last1 first2 last2)
  (setf (doubly-linked-next last1) first2
        (doubly-linked-previous first1) last2
        (doubly-linked-next last2) first1
        (doubly-linked-previous first2) last1)
  nil)

;;;; a container based on doubly-linked objects

(defclass doubly-linked-list (doubly-linked-mixin container node-container-mixin) ;...
  ((node-class :initform 'doubly-linked-list-node)))

(defclass doubly-linked-list-node (doubly-linked-mixin node-container-mixin node-mixin) ;...
  ((node-class :initform 'doubly-linked-list)))

(defmethod empty-container! ((container doubly-linked-list))
  (slot-makunbound container 'content)
  (call-next-method))

(defmethod insert-item! ((container doubly-linked-list) item)
  ;; insert item as the next
  (with-slots (next) container
    (let ((new (make-instance 'doubly-linked-list
                 :content item)))
      (isolate-node! new)
      (join-dl-chains new new (doubly-linked-next container) container)
      new)))

(defmethod pop-item! ((container doubly-linked-list))
  (check-not-empty container)
  (let* ((next (doubly-linked-next container))
         (next2 (doubly-linked-next next)))
    (setf (doubly-linked-next container) next2
          (doubly-linked-previous next2) container)
    (node-content next)))

(defmethod delete-node! ((container doubly-linked-list) item)
  (let* ((next (doubly-linked-next container))
         (next2 (doubly-linked-next next)))
    (setf (doubly-linked-next container) next2
          (doubly-linked-previous next2) container)
    next))

(defmethod container-contents ((container doubly-linked-list))
  (loop with node = container
        for next = (doubly-linked-next node)
        until (eq next container)
        do (setf node next)
        collect (node-content node)))
