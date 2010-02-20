;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("pure-maps" "order")))

(in-package :fare-utils)

;;; Generic tree interface

(defpackage :tree
  (:use)
  (:export
   #:<tree> #:node #:find #:join #:leftmost #:rightmost))


;;; Trees in general

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

(defclass pf:<binary-tree>
    (tree:<tree> pf:<map> order:<order>
     fmap-simple-empty fmap-simple-decons fmap-simple-update
     fmap-simple-merge fmap-simple-append fmap-simple-append/list
     fmap-simple-count)
  ()
  (:documentation "Keys in binary trees increase from left to right"))

(defclass binary-branch ()
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

(defclass binary-tree-node (binary-branch association-pair)
  ())

(defmethod check-invariant ((i pf:<binary-tree>) role node)
  (etypecase node
    (null
     nil)
    (binary-tree-node
     (when (left node)
       (check-invariant i role (left node))
       (assert (order:< i (tree:rightmost i (left node)) (node-key node))))
     (when (right node)
       (check-invariant i role (right node))
       (assert (order:< i (node-key node) (tree:leftmost i (right node)))))))
  node)


(defmethod tree:node ((i pf:<binary-tree>) &key left right key value)
  (make-instance 'binary-tree-node
                 :key key :value value :left left :right right))

(defmethod tree:find ((i pf:<binary-tree>) node key path)
  (if (null node) (values nil nil)
      (ecase (order:compare i key (node-key node))
        (0 (values node path))
        (-1 (tree:find i (left node) key (cons 'left path)))
        (1 (tree:find i (right node) key (cons 'right path))))))
(defmethod pf:lookup ((i pf:<binary-tree>) node key)
  (if (null node) (values nil nil)
      (ecase (order:compare i key (node-key node))
        (0 (values (node-value node) t))
        (-1 (pf:lookup i (left node) key))
        (1 (pf:lookup i (right node) key)))))
(defmethod pf:insert ((i pf:<binary-tree>) node key value)
  (if (null node)
      (tree:node i :key key :value value)
      (ecase (order:compare i key (node-key node))
        (0 (tree:node i :key key :value value
                      :left (left node) :right (right node)))
        (-1 (tree:node i :key (node-key node) :value (node-value node)
                       :left (pf:insert i (left node) key value) :right (right node)))
        (1 (tree:node i :key (node-key node) :value (node-value node)
                      :left (left node) :right (pf:insert i (right node) key value))))))
(defmethod pf:remove ((i pf:<binary-tree>) node key)
  (if (null node)
      (values nil nil nil)
      (let ((k (node-key node))
            (v (node-value node)))
        (ecase (order:compare i key k)
          (0 (values
              (tree:join i (left node) (right node))
              v t))
          (-1 (multiple-value-bind (left value foundp) (pf:remove i (left node) key)
                (values (tree:node i :key k :value v
                                   :left left :right (right node))
                        value foundp)))
          (1 (multiple-value-bind (right value foundp) (pf:remove i (right node) k)
               (values (tree:node i :key k :value v
                                  :left (left node) :right right)
                       value foundp)))))))
(defmethod pf:first-key-value ((i pf:<binary-tree>) map)
  (if map
      (values (node-key map) (node-value map) t)
      (values nil nil nil)))
(defmethod pf:fold-left ((i pf:<binary-tree>) node f seed)
  (if (null node)
      seed
      (pf:fold-left i (right node) f
                      (funcall f
                               (pf:fold-left i (left node) f seed)
                               (node-key node) (node-value node)))))
(defmethod pf:fold-right ((i pf:<binary-tree>) node f seed)
  (if (null node)
      seed
      (pf:fold-right i (left node) f
                       (funcall f
                                (node-key node) (node-value node)
                                (pf:fold-right i (right node) f seed)))))
(defmethod pf:for-each ((i pf:<binary-tree>) node f)
  (when node
    (pf:for-each i (left node) f)
    (funcall f (node-key node) (node-value node))
    (pf:for-each i (right node) f))
  (values))
(defmethod pf:divide ((i pf:<binary-tree>) node)
  (cond
    ((null node)
     (values nil nil))
    ((null (left node))
     (values (tree:node i :key (node-key node) :value (node-value node))
             (right node)))
    (t
     (values (left node) (pf:insert i (right node) (node-key node) (node-value node))))))
(defmethod pf:divide/list ((i pf:<binary-tree>) node)
  (if (null node) '()
      (let* ((rlist (cons (tree:node i :key (node-key node) :value (node-value node))
                          (if (null (right node)) '() (list (right node))))))
        (if (null (left node)) rlist (cons (left node) rlist)))))


(defmethod tree:leftmost ((i pf:<binary-tree>) node)
  (cond
    ((null node)
     (values nil nil nil))
    ((null (left node))
     (values (node-key node) (node-value node) t))
    (t
     (tree:leftmost i (left node)))))
(defmethod tree:rightmost ((i pf:<binary-tree>) node)
  (cond
    ((null node)
     (values nil nil nil))
    ((null (right node))
     (values (node-key node) (node-value node) t))
    (t
     (tree:rightmost i (right node)))))

(defmethod tree:join ((i pf:<binary-tree>) a b)
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

(defclass pf:<avl-tree> (pf:<binary-tree>) ())

(defclass avl-tree-node (binary-tree-node)
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
(defmethod node-balance ((node avl-tree-node))
  (- (node-height (right node))
     (node-height (left node))))

(defmethod check-invariant :before ((i pf:<avl-tree>) role node)
  (when node
    (assert (typep (node-height node)
                   `(integer 1 ,most-positive-fixnum)))
    (assert (= (node-height node)
               (1+ (max (node-height (left node))
                        (node-height (right node))))))
    (assert (member (node-balance node) '(-1 0 1)))))

#| Minimum number of nodes in a tree of height n (maximum is 2^n-1)
(memo:define-memo-function f (n)
  (cond ((zerop n) 0)
        ((= n 1) 1)
        (t (+ 1 (f (1- n)) (f (- n 2))))))
It's a variant of the fibonacci function,
and it grows exponentially like phi^n when n is big.
This ensures that even in the worst-case scenario,
a balanced tree is logarithmically shallow.

Exercise: prove that the in the above algorithms,
tree:node is always called with branches that are of comparable height...
|#

(defmethod tree:node ((i pf:<avl-tree>) &key left right key value)
  (flet ((mk (&key left right key value)
           (let ((lh (node-height left))
                 (rh (node-height right)))
             (assert (member (- rh lh) '(-1 0 1)))
             (make-instance 'avl-tree-node
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

(defclass pf:<integer-map> (pf:<avl-tree> order:<numeric>) ())
(defparameter pf:<integer-map>
  (memo:memoized 'make-instance 'pf:<integer-map>))
(defparameter pf:<im> pf:<integer-map>)
