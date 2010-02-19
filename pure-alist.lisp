;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map implementation: alists.

#+xcvb (module (:depends-on ("functional-map")))

(in-package :fare-utils)


(defclass fmap:<alist>
    (fmap:<map> eq:<hashable>
     fmap-simple-empty fmap-simple-decons fmap-simple-update fmap-simple-divide/list
     fmap-simple-merge fmap-simple-append fmap-simple-append/list)
  ((eq-interface
    :initarg :eq
    :initform eq:<eq>
    :reader eq-interface)))

(defmethod check-invariant ((i fmap:<alist>) role map)
  (declare (ignore role))
  (loop :for ((key . val) . rest) :on map :do
    (assert (not (member key rest :key 'car :test (eq:test-function i))) ()
            "Key ~S is present twice in alist ~S" key map))
  map)

(defun fmap:<alist> (&optional (eq eq:<eq>))
  (memo:memoized 'make-instance 'fmap:<alist> :eq eq))

(defparameter fmap:<alist> (fmap:<alist>))
(defmethod eq:= ((i fmap:<alist>) x y)
  (eq:= (eq-interface i) x y))
(defmethod eq:test-function ((i fmap:<alist>))
  (eq:test-function (eq-interface i)))

(defmethod fmap:lookup ((i fmap:<alist>) map key)
  (let ((pair (assoc key map :test (eq:test-function i))))
    (if pair
        (values (cdr pair) t)
        (values nil nil))))
    
(defmethod fmap:insert ((i fmap:<alist>) map key value)
  (acons key value (fmap:remove i map key)))
(defmethod fmap:remove ((i fmap:<alist>) map key)
  (multiple-value-bind (v f) (fmap:lookup i map key)
    (if f
        (values (remove key map :key 'car :test (eq:test-function i)) v t)
        (values map nil nil))))
(defmethod fmap:first-key-value ((i fmap:<alist>) map)
  (values (caar map) (cdar map) (not (null map))))
(defmethod fmap:fold-left ((i fmap:<alist>) map f seed)
  (reduce (lambda (acc pair) (funcall f acc (car pair) (cdr pair)))
          map :initial-value seed))
(defmethod fmap:fold-right ((i fmap:<alist>) map f seed)
  (reduce (lambda (pair acc) (funcall f (car pair) (cdr pair) acc))
          map :initial-value seed :from-end t))
(defmethod fmap:for-each ((i fmap:<alist>) map f)
  (loop :for (key . val) :in map :do (funcall f key val))
  (values))
(defmethod fmap:divide ((i fmap:<alist>) map)
  (let* ((l (length map))
         (l1 (floor l 2)))
    (values (subseq map 0 l1) (nthcdr l1 map))))
(defmethod fmap:count ((i fmap:<alist>) map)
  (length map))

(defmethod fmap:convert (i2 i1 map1)
  (fmap:fold-right
   i1 map1
   (lambda (k v map2) (fmap:insert i2 map2 k v))
   (fmap:empty i2)))
