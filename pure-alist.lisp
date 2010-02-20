;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map implementation: alists.

#+xcvb (module (:depends-on ("pure-maps")))

(in-package :fare-utils)


(defclass pf:<alist>
    (pf:<map> eq:<hashable>
     fmap-simple-empty fmap-simple-decons fmap-simple-update fmap-simple-divide/list
     fmap-simple-merge fmap-simple-append fmap-simple-append/list)
  ((eq-interface
    :initarg :eq
    :initform eq:<eq>
    :reader eq-interface)))

(defmethod pf:check-invariant ((i pf:<alist>) role map)
  (declare (ignore role))
  (loop :for ((key . val) . rest) :on map :do
    (assert (not (member key rest :key 'car :test (eq:test-function i))) ()
            "Key ~S is present twice in alist ~S" key map))
  map)

(defun pf:<alist> (&optional (eq eq:<eq>))
  (memo:memoized 'make-instance 'pf:<alist> :eq eq))

(defparameter pf:<alist> (pf:<alist>))
(defmethod eq:= ((i pf:<alist>) x y)
  (eq:= (eq-interface i) x y))
(defmethod eq:test-function ((i pf:<alist>))
  (eq:test-function (eq-interface i)))

(defmethod pf:lookup ((i pf:<alist>) map key)
  (let ((pair (assoc key map :test (eq:test-function i))))
    (if pair
        (values (cdr pair) t)
        (values nil nil))))
    
(defmethod pf:insert ((i pf:<alist>) map key value)
  (acons key value (pf:remove i map key)))
(defmethod pf:remove ((i pf:<alist>) map key)
  (multiple-value-bind (v f) (pf:lookup i map key)
    (if f
        (values (remove key map :key 'car :test (eq:test-function i)) v t)
        (values map nil nil))))
(defmethod pf:first-key-value ((i pf:<alist>) map)
  (values (caar map) (cdar map) (not (null map))))
(defmethod pf:fold-left ((i pf:<alist>) map f seed)
  (reduce (lambda (acc pair) (funcall f acc (car pair) (cdr pair)))
          map :initial-value seed))
(defmethod pf:fold-right ((i pf:<alist>) map f seed)
  (reduce (lambda (pair acc) (funcall f (car pair) (cdr pair) acc))
          map :initial-value seed :from-end t))
(defmethod pf:for-each ((i pf:<alist>) map f)
  (loop :for (key . val) :in map :do (funcall f key val))
  (values))
(defmethod pf:divide ((i pf:<alist>) map)
  (let* ((l (length map))
         (l1 (floor l 2)))
    (values (subseq map 0 l1) (nthcdr l1 map))))
(defmethod pf:count ((i pf:<alist>) map)
  (length map))

(defmethod pf:convert (i2 i1 map1)
  (pf:fold-right
   i1 map1
   (lambda (k v map2) (pf:insert i2 map2 k v))
   (pf:empty i2)))
