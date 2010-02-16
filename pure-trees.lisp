;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("functional-map" "order")))

(in-package :fare-utils)

;;; Generic tree interface

(defpackage :tree
  (:use)
  (:export
   #:<tree> #:node #:find))

(defclass tree:<tree> () ())
(defgeneric tree:node (i &key)
  (:documentation "create a new node for a <tree> interface.
Assumes that any ordering and balancing constraints are respected
in the constituents, and that any ordering constraint is respected
between them, re-balancing as needed."))
(defgeneric tree:find (i tree key path)
  (:documentation "lookup a tree for a key, return a path to the proper node."))


;;; Vanilla Binary Tree

(defclass fmap:<binary-tree>
    (tree:<tree> fmap:<map> order:<order>
     fmap-simple-decons fmap-simple-update
     fmap-simple-merge fmap-simple-append/list
     fmap-simple-count)
  ()
  (:documentation "Keys in binary trees increase from left to right"))


(defgeneric check-invariant (i &key))

(defmethod check-invariant ((i fmap:<binary-tree>) &key node)
  (etypecase node
    (null
     nil)
    (pure-binary-tree-node
     (when (left node)
       (check-invariant i (left node))
       (assert (order:< i (node-key (left node)) (node-key node))))
     (when (right node)
       (check-invariant i (right node))
       (assert (order:< i (node-key node) (node-key (right node))))))))

(defclass pure-binary-tree-node ()
  ((left
    :initarg :left
    :initform nil
    :type (or null pure-binary-tree-node)
    :reader left)
   (right
    :initarg :right
    :initform nil
    :type (or null pure-binary-tree-node)
    :reader right)
   (key
    :initarg :key
    :initform nil
    :reader node-key)
   (value
    :initarg :value
    :initform nil
    :reader node-value))
  (:metaclass structure-class))

(defmethod tree:node ((i fmap:<binary-tree>) &key left right key value)
  (make-instance 'pure-binary-tree-node
                 :key key :value value :left left :right right))

(defmethod fmap:empty ((i fmap:<binary-tree>))
  '())
(defmethod fmap:empty-p ((i fmap:<binary-tree>) map)
  (null map))
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
(defmethod fmap:insert ((i fmap:<binary-tree>) map key value)
  (if (null node)
      (make-mode i :key key :value value)
      (ecase (order:compare i key (node-key node))
        (0 (tree:node i :key key :value value
                      :left (left node) :right (right node)))
        (-1 (tree:node i :key (node-key node) :value (node-value node)
                       :left (fmap:insert i (left node) key value) :right (right node)))
        (1 (tree:node i :key (node-key node) :value (node-value node)
                      :left (left node) :right (fmap:insert i (right node) key value))))))
(defmethod fmap:remove ((i fmap:<binary-tree>) map key)
  (if (null node)
      (values nil nil nil)
      (ecase (order:compare i key (node-key node))
        (0 (values
            (fmap:append i (left node) (right node))
            (node-value node) t))
        (-1 (multiple-value-bind (left v f) (fmap:remove i (left node) k)
              (values (tree:node i :key key :value value
                                 :left left :right (right node))
                      v f)))
        (1 (multiple-value-bind (right v f) (fmap:remove i (right node) k)
             (values (tree:node i :key key :value value
                                :left (left node) :right right)
                     v f))))))
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
(defmethod fmap:fold-right ((i fmap:<binary-tree>) map f seed)
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
(defmethod fmap:append ((i fmap:<binary-tree>) a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t
     (ecase (order:compare i (node-key a) (node-key b))
       (0
        (tree:node :key (node-key a) :value (node-value a)
                   :left (fmap:append i (left a) (left b))
                   :right (fmap:append i (right a) (right b))))
       (1
        (tree:node :key (node-key a) :value (node-value a)
                   :left (left a)
                   :right (tree:node :key (node-key b) :value (node-value b)
                                     :left (fmap:append i (right a) (left b))
                                     :right (right b))))
       (-1
        (tree:node :key (node-key b) :value (node-value b)
                   :left (left b)
                   :right (tree:node :key (node-key a) :value (node-value a)
                                     :left (fmap:append i (right b) (left a))
                                     :right (right a))))))))

;;; pure AVL-tree

(defclass fmap:<avl-tree> (fmap:<binary-tree>) ())

(defclass pure-avl-tree-node (pure-binary-tree-node)
  ((height
    :initarg :balance
    :initform 0
    :type integer
    :reader node-height))
  (:metaclass structure-class))

(defmethod node-height ((node null))
  0)
(defmethod node-balance ((node null))
  0)
(defmethod node-balance ((node pure-avl-tree-node))
  (- (node-height (right node))
     (node-height (left node))))

(defmethod check-invariant :after ((i fmap:<avl-tree>) &key node)
  (when node
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
