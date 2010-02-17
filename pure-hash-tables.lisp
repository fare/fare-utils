;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("pure-trees")))

(in-package :fare-utils)

(defclass fmap:<pure-hash-table>
    (fmap:<map> eq:<hashable>
     fmap-simple-empty)
  ((alist-interface :accessor alist-interface))
  (:documentation "pure hash table"))

(defmethod shared-initialize :after ((i fmap:<pure-hash-table>)
                                     slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (setf (alist-interface i) (fmap:<alist> i)))

(defparameter fmap:<pure-hash-table> (make-instance 'fmap:<pure-hash-table>))
(defparameter <pht> fmap:<pure-hash-table>)

(defclass fmap:<pure-equal-hash-table> (fmap:<pure-hash-table> eq:<equal>) ())
(defparameter fmap:<pure-equal-hash-table> (make-instance 'fmap:<pure-equal-hash-table>))
(defparameter <equal-pht> fmap:<pure-equal-hash-table>)

(defmethod fmap:lookup ((i fmap:<pure-hash-table>) node key)
  (if (null node) (values nil nil)
      (let ((bucket (fmap:lookup fmap:<nkfm> node (eq:hash i key))))
        (if bucket
            (fmap:lookup (alist-interface i) bucket key)
            (values nil nil)))))
(defmethod fmap:insert ((i fmap:<pure-hash-table>) node key value)
  (let ((hash (eq:hash i key)))
    (fmap:insert
     fmap:<nkfm> node hash
     (fmap:insert (alist-interface i)
                  (fmap:lookup fmap:<nkfm> node hash)
                  key value))))
(defmethod fmap:remove ((i fmap:<pure-hash-table>) node key)
  (if (null node)
      (values nil nil nil)
      (let* ((hash (node-key node))
             (bucket (node-value node))
             (key (caar bucket))
             (value (cdar bucket))
             (rest (rest bucket)))
        (values
         (if rest
             (fmap:insert fmap:<nkfm> node hash rest)
             (fmap:remove fmap:<nkfm> node hash))
         key value t))))
(defmethod fmap:first-key-value ((i fmap:<pure-hash-table>) map)
  (if map
      (fmap:first-key-value (alist-interface i)
                            (nth 1 (fmap:first-key-value fmap:<nkfm> map)))
      (values nil nil nil)))
(defmethod fmap:fold-left ((i fmap:<pure-hash-table>) node f seed)
  (fmap:fold-left fmap:<nkfm> node
                  (lambda (a h bucket)
                    (declare (ignore h))
                    (fmap:fold-left (alist-interface i) bucket f a))
                  seed))
(defmethod fmap:fold-right ((i fmap:<pure-hash-table>) node f seed)
  (fmap:fold-right fmap:<nkfm> node
                   (lambda (h bucket a)
                     (declare (ignore h))
                     (fmap:fold-right (alist-interface i) bucket f a))
                  seed))
(defmethod fmap:divide ((i fmap:<pure-hash-table>) node)
  (cond
    ((null node)
     (values nil nil))
    ((and (null (left node)) (null (right node)))
     (let ((hash (node-key node))
           (bucket (node-value node)))
       (multiple-value-bind (b1 b2) (fmap:divide (alist-interface i) bucket)
         (values (when b1 (fmap:insert i nil hash b1))
                 (when b2 (fmap:insert i nil hash b2))))))
    (t
     (fmap:divide fmap:<nkfm> node))))
(defmethod fmap:divide/list ((i fmap:<pure-hash-table>) node)
  (cond
    ((null node)
     (values nil nil))
    ((and (null (left node)) (null (right node)))
     (multiple-value-list (fmap:divide i node)))
    (t
     (fmap:divide/list fmap:<nkfm> node))))

#|
(defparameter <pht> fmap:<pure-hash-table>)

(sort (fmap:fold-left <pht> (reduce (lambda (x i) (fmap:insert <pht> x i i)) (loop :for i :below 1000 :collect i) :initial-value nil) (lambda (m k v) k (cons v m)) nil) '<)

|#
