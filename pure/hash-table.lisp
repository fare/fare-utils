;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("pure-trees")))

(in-package :pure)

(defclass <hash-table>
    (<map>
     map-simple-join)
  ((key-interface :reader key-interface :initarg :key)
   (hashmap-interface :reader hashmap-interface :initarg :hashmap)
   (bucketmap-interface :reader bucketmap-interface :initarg :bucketmap))
  (:documentation "pure hash table"))

(defun <hash-table> (&key (key eq:<equal>)
                     (hashmap <im>)
                     (bucketmap (<alist> key)))
  (assert (typep key 'eq:<hashable>))
  (assert (typep hashmap '<map>))
  (assert (typep bucketmap '<map>))
  (memo:memoized 'make-instance '<hash-table>
                 :key key :hashmap hashmap :bucketmap bucketmap))
(defparameter <hash-table> (<hash-table>))

(defmethod check-invariant ((i <hash-table>) map)
  (check-invariant (hashmap-interface i) map)
  (for-each
   (hashmap-interface i) map
   (lambda (hash bucket)
     (declare (ignore hash))
     (check-invariant (bucketmap-interface i) bucket))))

(defmethod empty ((i <hash-table>))
  (empty (hashmap-interface i)))
(defmethod empty-p ((i <hash-table>) map)
  (empty-p (hashmap-interface i) map))
(defmethod lookup ((i <hash-table>) map key)
  (let ((bucket (lookup (hashmap-interface i) map
                        (eq:hash (key-interface i) key))))
    (lookup (bucketmap-interface i) bucket key)))
(defmethod insert ((i <hash-table>) node key value)
  (let ((hash (eq:hash (key-interface i) key)))
    (insert
     (hashmap-interface i) node hash
     (insert (bucketmap-interface i)
             (multiple-value-bind (bucket foundp)
                 (lookup (hashmap-interface i) node hash)
               (if foundp bucket (empty (bucketmap-interface i))))
             key value))))
(defmethod drop ((i <hash-table>) map key)
  (multiple-value-bind (hash bucket) ;; hashfoundp
      (first-key-value (hashmap-interface i) map)
    (multiple-value-bind (new-bucket key value) ;; foundp
        (decons (bucketmap-interface i) bucket)
      (values
       (if new-bucket
           (insert (hashmap-interface i) map hash new-bucket)
           (drop (hashmap-interface i) map hash))
       key value t))))
(defmethod first-key-value ((i <hash-table>) map)
  (multiple-value-bind (hash bucket foundp)
      (first-key-value (hashmap-interface i) map)
    (declare (ignore hash))
    (if foundp
        (first-key-value (bucketmap-interface i) bucket)
        (values nil nil nil))))
(defmethod fold-left ((i <hash-table>) node f seed)
  (fold-left (hashmap-interface i) node
             (lambda (a h bucket)
               (declare (ignore h))
               (fold-left (bucketmap-interface i) bucket f a))
             seed))
(defmethod fold-right ((i <hash-table>) node f seed)
  (fold-right (hashmap-interface i) node
              (lambda (h bucket a)
                (declare (ignore h))
                (fold-right (bucketmap-interface i) bucket f a))
              seed))
(defmethod for-each ((i <hash-table>) map f)
  (for-each
   (hashmap-interface i) map
   (lambda (hash bucket)
     (declare (ignore hash))
     (for-each (bucketmap-interface i) bucket f))))
(defmethod divide ((i <hash-table>) node)
  (cond
    ((and (null (left node)) (null (right node)))
     (let ((hash (node-key node))
           (bucket (node-value node)))
       (multiple-value-bind (b1 b2) (divide (bucketmap-interface i) bucket)
         (values (when b1 (insert i nil hash b1))
                 (when b2 (insert i nil hash b2))))))
    (t
     (divide (hashmap-interface i) node))))
(defmethod divide/list ((i <hash-table>) node)
  (let ((list (divide/list (hashmap-interface i) node)))
    (if (cdr list)
        list
        (multiple-value-list (divide i node)))))
