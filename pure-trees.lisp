;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("functional-map" "order")))

(in-package :fare-utils)

;;; Generic tree interface

(defpackage :tree
  (:use)
  (:export
   #:<tree> #:node #:find #:join #:leftmost #:rightmost))

(defclass tree:<tree> () ())
(defgeneric tree:node (i &key)
  (:documentation "create a new node for a <tree> interface.
Assumes that any ordering and balancing constraints are respected
in the constituents, and that any ordering constraint is respected
between them, re-balancing as needed."))
(defgeneric tree:find (i tree key path)
  (:documentation "lookup a tree for a key, return a path to the proper node."))
(defgeneric tree:join (i a b)
  (:documentation "join two trees, assuming.
Assumes that any ordering and balancing constraints are respected
in the constituents, and that any ordering constraint is respected
between them, re-balancing as needed."))
(defgeneric tree:leftmost (i a)
  (:documentation "key value and flag for leftmost node in a"))
(defgeneric tree:rightmost (i a)
  (:documentation "key value and flag for rightmost node in a"))

;;; Vanilla Binary Tree

(defclass fmap:<binary-tree>
    (tree:<tree> fmap:<map> order:<order>
     fmap-simple-empty fmap-simple-decons fmap-simple-update
     fmap-simple-merge fmap-simple-append fmap-simple-append/list
     fmap-simple-count)
  ()
  (:documentation "Keys in binary trees increase from left to right"))


(defgeneric check-invariant (i &key))

(defclass pure-binary-branch ()
  ((left
    :initarg :left
    :initform nil
    :reader left)
   (right
    :initarg :right
    :initform nil
    :reader right)))

(defclass association-pair ()
  ((key
    :initarg :key
    :initform nil
    :reader node-key)
   (value
    :initarg :value
    :initform nil
    :reader node-value)))

(defclass pure-binary-tree-node (pure-binary-branch association-pair)
  ())

(defmethod check-invariant ((i fmap:<binary-tree>) &key node)
  (etypecase node
    (null
     nil)
    (pure-binary-tree-node
     (when (left node)
       (check-invariant i :node (left node))
       (assert (order:< i (tree:rightmost i (left node)) (node-key node))))
     (when (right node)
       (check-invariant i :node (right node))
       (assert (order:< i (node-key node) (tree:leftmost i (right node))))))))


(defmethod tree:node ((i fmap:<binary-tree>) &key left right key value)
  (make-instance 'pure-binary-tree-node
                 :key key :value value :left left :right right))

(defmethod tree:find ((i fmap:<binary-tree>) node key path)
  (if (null node) (values nil nil)
      (ecase (order:compare i key (node-key node))
        (0 (values node path))
        (-1 (tree:find i (left node) key (cons 'left path)))
        (1 (tree:find i (right node) key (cons 'right path))))))
(defmethod fmap:lookup ((i fmap:<binary-tree>) node key)
  (if (null node) (values nil nil)
      (ecase (order:compare i key (node-key node))
        (0 (values (node-value node) t))
        (-1 (fmap:lookup i (left node) key))
        (1 (fmap:lookup i (right node) key)))))
(defmethod fmap:insert ((i fmap:<binary-tree>) node key value)
  (if (null node)
      (tree:node i :key key :value value)
      (ecase (order:compare i key (node-key node))
        (0 (tree:node i :key key :value value
                      :left (left node) :right (right node)))
        (-1 (tree:node i :key (node-key node) :value (node-value node)
                       :left (fmap:insert i (left node) key value) :right (right node)))
        (1 (tree:node i :key (node-key node) :value (node-value node)
                      :left (left node) :right (fmap:insert i (right node) key value))))))
(defmethod fmap:remove ((i fmap:<binary-tree>) node key)
  (if (null node)
      (values nil nil nil)
      (let ((k (node-key node))
            (v (node-value node)))
        (ecase (order:compare i key k)
          (0 (values
              (tree:join i (left node) (right node))
              v t))
          (-1 (multiple-value-bind (left value foundp) (fmap:remove i (left node) key)
                (values (tree:node i :key k :value v
                                   :left left :right (right node))
                        value foundp)))
          (1 (multiple-value-bind (right value foundp) (fmap:remove i (right node) k)
               (values (tree:node i :key k :value v
                                  :left (left node) :right right)
                       value foundp)))))))
(defmethod fmap:first-key-value ((i fmap:<binary-tree>) map)
  (if map
      (values (node-key map) (node-value map) t)
      (values nil nil nil)))
(defmethod fmap:fold-left ((i fmap:<binary-tree>) node f seed)
  (if (null node)
      seed
      (fmap:fold-left i (right node) f
                      (funcall f
                               (fmap:fold-left i (left node) f seed)
                               (node-key node) (node-value node)))))
(defmethod fmap:fold-right ((i fmap:<binary-tree>) node f seed)
  (if (null node)
      seed
      (fmap:fold-right i (left node) f
                       (funcall f
                                (node-key node) (node-value node)
                                (fmap:fold-right i (right node) f seed)))))
(defmethod fmap:divide ((i fmap:<binary-tree>) node)
  (cond
    ((null node)
     (values nil nil))
    ((null (left node))
     (values (tree:node i :key (node-key node) :value (node-value node))
             (right node)))
    (t
     (values (left node) (fmap:insert i (right node) (node-key node) (node-value node))))))
(defmethod fmap:divide/list ((i fmap:<binary-tree>) node)
  (if (null node) '()
      (let* ((rlist (cons (tree:node i :key (node-key node) :value (node-value node))
                          (if (null (right node)) '() (list (right node))))))
        (if (null (left node)) rlist (cons (left node) rlist)))))


(defmethod tree:leftmost ((i fmap:<binary-tree>) node)
  (cond
    ((null node)
     (values nil nil nil))
    ((null (left node))
     (values (node-key node) (node-value node) t))
    (t
     (tree:leftmost i (left node)))))
(defmethod tree:rightmost ((i fmap:<binary-tree>) node)
  (cond
    ((null node)
     (values nil nil nil))
    ((null (right node))
     (values (node-key node) (node-value node) t))
    (t
     (tree:rightmost i (right node)))))

(defmethod tree:join ((i fmap:<binary-tree>) a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t
     (assert (order:< i (tree:rightmost a) (tree:leftmost b)))
     (tree:node i :key (node-key a) :value (node-value a)
                :left (left a)
                :right (tree:node i :key (node-key b) :value (node-value b)
                                  :left (tree:join i (right a) (left b))
                                  :right (right b))))))


;;; pure AVL-tree

(defclass fmap:<avl-tree> (fmap:<binary-tree>) ())

(defclass pure-avl-tree-node (pure-binary-tree-node)
  ((height
    :initarg :height
    :initform 0
    :type integer
    :reader node-height)))

(defmethod node-height ((node null))
  0)

(defgeneric node-balance (node))
(defmethod node-balance ((node null))
  0)
(defmethod node-balance ((node pure-avl-tree-node))
  (- (node-height (right node))
     (node-height (left node))))

(defmethod check-invariant :before ((i fmap:<avl-tree>) &key node)
  (when node
    (assert (typep (node-height node)
                   `(integer 1 ,most-positive-fixnum)))
    (assert (= (node-height node)
               (1+ (max (node-height (left node))
                        (node-height (right node))))))
    (assert (member (node-balance node) '(-1 0 1)))))

(defmethod tree:node ((i fmap:<avl-tree>) &key left right key value)
  (flet ((mk (&key left right key value)
           (let ((lh (node-height left))
                 (rh (node-height right)))
             (assert (member (- rh lh) '(-1 0 1)))
             (make-instance 'pure-avl-tree-node
                            :key key :value value
                            :left left :right right
                            :height (1+ (max lh rh))))))
    (ecase (- (node-height right) (node-height left))
      ((-1 0 1) (mk :key key :value value :left left :right right))
      ((-2)
       (ecase (node-balance left)
         ((-1)
          ;; LL rebalance:
          ;; (LL2 KL LR1) K R1 ==> (LL2 KL (LR1 K R1))
          (mk :left (left left)
              :key (node-key left) :value (node-value left)
              :right (mk :key key :value value :left (right left) :right right)))
         ((1)
          ;; LR rebalance:
          ;; (LL1 KL (LRL21 KLR LRR21)) K R1 ==> (LL1 KL LRL21) KLR (LRR21 K R1)
          (mk :left (mk :left (left left)
                        :key (node-key left) :value (node-value left)
                        :right (left (right left)))
              :key (node-key (right left)) :value (node-value (right left))
              :right (mk :left (right (right left))
                         :key key :value value
                         :right right)))))
      ((2)
       (ecase (node-balance right)
         ((-1)
          ;; RL rebalance:
          ;; L1 K ((RLL21 KRL RLR21) KR RR1) ==> (L1 K RLL21) KRL (RLR21 KR RR1)
          (mk :left (mk :left left
                        :key key :value value
                        :right (left (left right)))
              :key (node-key (left right)) :value (node-value (left right))
              :right (mk :left (right (left right))
                         :key (node-key right) :value (node-value right)
                         :right (right right))))
         ((1)
          ;; RR rebalance:
          ;; L1 K (RL1 KR RR2) ==> (L1 K RL1) KR RR2
          (mk :left (mk :left left
                        :key key :value value
                        :right (left right))
              :key (node-key right) :value (node-value right)
              :right (right right))))))))

(defclass fmap:<number-keyed-functional-map> (fmap:<avl-tree> order:<numeric>) ())
(defparameter fmap:<number-keyed-functional-map>
  (memo:memoized 'make-instance 'fmap:<number-keyed-functional-map>))
(defparameter fmap:<nkfm> fmap:<number-keyed-functional-map>)

#|
Simple tests:
(load "/home/fare/cl/asdf/asdf.lisp")

(asdf:load-system :fare-utils)
(in-package :fare-utils)

(defparameter <nmap> fmap:<number-keyed-functional-map>)
(defparameter <alist> fmap:<alist>)

(fmap:convert <alist> <nmap>
              (fmap:convert <nmap> <alist>
                            '((1 un) (2 deux) (5 cinq) (3 trois) (4 quatre))))

(let ((m (fmap:append <nmap>
                           (fmap:convert <nmap> <alist>
                                         '((1 un) (2 deux) (5 cinq) (3 trois) (4 quatre)))
                           (fmap:convert <nmap> <alist>
                                         '((1 one) (6 six) (7 seven) (3 three) (0 zero))))))
  (check-invariant <nmap> :node m)
  (fmap:convert <alist> <nmap> m))

(let ((m (fmap:convert <nmap> <alist>
                       (loop :for i :from 1 :to 1000 :collect (cons i (format nil "~@R" i))))))
  (check-invariant <nmap> :node m)
  (format t "~&height: ~A   count: ~A~%" (node-height m) (fmap:count <nmap> m)))

(let ((m (fmap:convert <nmap> <alist>
                       (loop :for i :from 1 :to 10000 :collect (cons (random (expt 10 9)) i)))))
  (check-invariant <nmap> :node m)
  (format t "~&height: ~A   count: ~A~%" (node-height m) (fmap:count <nmap> m)))
|#
