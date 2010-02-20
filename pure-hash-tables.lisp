;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("pure-trees")))

(in-package :fare-utils)

(defclass pf:<hash-table>
    (pf:<map> eq:<hashable>
     fmap-simple-empty fmap-simple-append)
  ((alist-interface :accessor alist-interface))
  (:documentation "pure hash table"))

(defmethod shared-initialize :after ((i pf:<hash-table>)
                                     slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (setf (alist-interface i) (pf:<alist> i)))

(defparameter pf:<hash-table>
  (memo:memoized 'make-instance 'pf:<hash-table>))
(defparameter pf:<ht> pf:<hash-table>)

(defclass pf:<equal-hash-table> (pf:<hash-table> eq:<equal>) ())
(defparameter pf:<equal-hash-table>
  (memo:memoized 'make-instance 'pf:<equal-hash-table>))
(defparameter pf:<equal-ht> pf:<equal-hash-table>)

(defmethod pf:check-invariant ((i pf:<hash-table>) role map)
  (pf:check-invariant pf:<im> role map)
  (pf:for-each
   pf:<im> map
   (lambda (hash bucket)
     (declare (ignore hash))
     (pf:check-invariant (alist-interface i) role bucket)))
  map)

(defmethod pf:lookup ((i pf:<hash-table>) node key)
  (if (null node) (values nil nil)
      (let ((bucket (pf:lookup pf:<im> node (eq:hash i key))))
        (if bucket
            (pf:lookup (alist-interface i) bucket key)
            (values nil nil)))))
(defmethod pf:insert ((i pf:<hash-table>) node key value)
  (let ((hash (eq:hash i key)))
    (pf:insert
     pf:<im> node hash
     (pf:insert (alist-interface i)
                  (pf:lookup pf:<im> node hash)
                  key value))))
(defmethod pf:remove ((i pf:<hash-table>) node key)
  (if (null node)
      (values nil nil nil)
      (let* ((hash (node-key node))
             (bucket (node-value node))
             (key (caar bucket))
             (value (cdar bucket))
             (rest (rest bucket)))
        (values
         (if rest
             (pf:insert pf:<im> node hash rest)
             (pf:remove pf:<im> node hash))
         key value t))))
(defmethod pf:first-key-value ((i pf:<hash-table>) map)
  (if map
      (pf:first-key-value (alist-interface i)
                            (nth 1 (pf:first-key-value pf:<im> map)))
      (values nil nil nil)))
(defmethod pf:fold-left ((i pf:<hash-table>) node f seed)
  (pf:fold-left pf:<im> node
                  (lambda (a h bucket)
                    (declare (ignore h))
                    (pf:fold-left (alist-interface i) bucket f a))
                  seed))
(defmethod pf:fold-right ((i pf:<hash-table>) node f seed)
  (pf:fold-right pf:<im> node
                   (lambda (h bucket a)
                     (declare (ignore h))
                     (pf:fold-right (alist-interface i) bucket f a))
                  seed))
(defmethod pf:for-each ((i pf:<hash-table>) map f)
  (pf:for-each
   pf:<im> map
   (lambda (hash bucket)
     (declare (ignore hash))
     (pf:for-each (alist-interface i) bucket f))))
(defmethod pf:divide ((i pf:<hash-table>) node)
  (cond
    ((null node)
     (values nil nil))
    ((and (null (left node)) (null (right node)))
     (let ((hash (node-key node))
           (bucket (node-value node)))
       (multiple-value-bind (b1 b2) (pf:divide (alist-interface i) bucket)
         (values (when b1 (pf:insert i nil hash b1))
                 (when b2 (pf:insert i nil hash b2))))))
    (t
     (pf:divide pf:<im> node))))
(defmethod pf:divide/list ((i pf:<hash-table>) node)
  (cond
    ((null node)
     (values nil nil))
    ((and (null (left node)) (null (right node)))
     (multiple-value-list (pf:divide i node)))
    (t
     (pf:divide/list pf:<im> node))))
