;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("pure-maps" "order")))

(in-package :pure)

;;; Trees in general

(defclass <tree> (<type>) ()
  (:documentation "abstract interface for trees"))

#|
(defclass <node> (<type>) ()
  (:documentation "abstract interface for nodes of trees"))
(defgeneric node-interface (<tree>)
  (:documentation "returns the interface for nodes of given tree interface"))
(defgeneric key-interface (<interface>)
  (:documentation "returns the interface for keys of given tree interface"))
|#

(defgeneric join-nodes (<tree> node1 node2)
  (:documentation "join two nodes.
Unlike join, which accepts arbitrary trees,
join assumes that any ordering and balancing constraints are respected
in the constituents, and that any ordering constraint is respected
between them, re-balancing as needed."))
(defgeneric leftmost (<tree> tree)
  (:documentation "key, value, foundp for leftmost node in TREE"))
(defgeneric rightmost (<tree> tree)
  (:documentation "key, value, foundp for rightmost node in TREE"))

(defgeneric locate (<tree> tree key path)
  (:documentation "lookup a tree for a key, return a path to the proper node."))

(defgeneric node (<tree> &key)
  (:documentation "make a node for a tree interface"))

;;; Vanilla Binary Tree

(defclass <binary-tree>
    (<tree> <map>
     order:<order> ;; TODO: delegate that to a key interface?
     map-simple-empty ;; handles all the null cases so we don't have to.
     map-simple-decons map-simple-update-key
     map-simple-join map-simple-map/2 map-simple-join/list
     map-simple-size)
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

(defclass box ()
  ((datum :initarg :datum :reader datum)))

(defclass binary-tree-node (binary-branch association-pair)
  ;;; Or should we have a box instead of an association-pair ???
  ;;; Or let the user just inherit from binary-branch,
  ;;; and use a node-interface with make and update?
  ())

(defmethod check-invariant ((i <binary-tree>)
                            (node binary-branch))
  (when (left node)
    (check-invariant i (left node))
    (assert (order< i (rightmost i (left node)) (node-key node))))
  (when (right node)
    (check-invariant i (right node))
    (assert (order< i (node-key node) (leftmost i (right node))))))

;;(defmethod node ((i <tree>) &rest keys &key &allow-other-keys)
;;  (apply #'make (node-interface i) keys))
(defmethod node ((i <binary-tree>) &key left right key value)
  (make-instance 'binary-tree-node
                 :key key :value value :left left :right right))

;;(defmethod compare-key ((i <map>) key1 key2)
;;  (compare (key-interface i) key1 key2))

(defmethod locate ((i <binary-tree>) node key path)
  (ecase (order:compare i key (node-key node)) ;; (compare-key i key (node-key node))
        (0 (values node path))
        (-1 (locate i (left node) key (cons 'left path)))
        (1 (locate i (right node) key (cons 'right path)))))
(defmethod lookup ((i <binary-tree>) node key)
  (if (null node)
      (values nil nil)
      (ecase (order:compare i key (node-key node)) ;; (compare-key i key (node-key node))
        (0 (values (node-value node) t))
        (-1 (lookup i (left node) key))
        (1 (lookup i (right node) key)))))
(defmethod insert ((i <binary-tree>) node key value)
  (if (null node)
      (node i :key key :value value)
      (ecase (order:compare i key (node-key node))
        (0 (node i :key key :value value ;; (update-node i node :key key :value value)
                 :left (left node) :right (right node)))
        (-1 (node i :key (node-key node) :value (node-value node)
                  :left (insert i (left node) key value) :right (right node)))
        (1 (node i :key (node-key node) :value (node-value node)
                 :left (left node) :right (insert i (right node) key value))))))
(defmethod drop ((i <binary-tree>) node key)
  (if (null node)
      (values nil nil nil)
      (let ((k (node-key node))
            (v (node-value node)))
        (ecase (order:compare i key k)
          (0 (values
              (cond
                ((null (left node)) (right node))
                ((null (right node)) (left node))
                (t
                 (multiple-value-bind (kk vv)
                     (leftmost i (right node))
                   (node i :key kk :value vv
                         :left (left node) :right (drop i (right node) kk)))))
              v t))
          (-1
           (multiple-value-bind (left value foundp) (drop i (left node) key)
                (values (node i :key k :value v
                              :left left :right (right node))
                    value foundp)))
          (1
           (multiple-value-bind (right value foundp) (drop i (right node) key)
               (values (node i :key k :value v
                             :left (left node) :right right)
                       value foundp)))))))
(defmethod first-key-value ((i <binary-tree>) map)
  (if (null map)
      (values nil nil nil)
      (values (node-key map) (node-value map) t)))
(defmethod fold-left ((i <binary-tree>) node f seed)
  (if (null node)
      seed
      (fold-left i (right node) f
                      (funcall f
                               (fold-left i (left node) f seed)
                               (node-key node) (node-value node)))))
(defmethod fold-right ((i <binary-tree>) node f seed)
  (if (null node)
      seed
      (fold-right i (left node) f
                       (funcall f
                                (node-key node) (node-value node)
                                (fold-right i (right node) f seed)))))
(defmethod for-each ((i <binary-tree>) node f)
  (when node
    (for-each i (left node) f)
    (funcall f (node-key node) (node-value node))
    (for-each i (right node) f))
  (values))
(defmethod divide ((i <binary-tree>) node)
  (cond
    ((null node)
     (values nil nil))
    ((null (left node))
     (values (node i :key (node-key node) :value (node-value node))
             (right node)))
    (t
     (values (left node) (insert i (right node) (node-key node) (node-value node))))))
(defmethod divide/list ((i <binary-tree>) node)
  (if (null node) '()
      (let* ((rlist (cons (node i :key (node-key node) :value (node-value node))
                          (if (null (right node)) '() (list (right node))))))
        (if (null (left node)) rlist (cons (left node) rlist)))))


(defmethod leftmost ((i <binary-tree>) node)
  (cond
    ((null node) nil)
    ((null (left node)) (values (node-key node) (node-value node) t))
    (t (leftmost i (left node)))))
(defmethod rightmost ((i <binary-tree>) node)
  (cond
    ((null node) nil)
    ((null (right node)) (values (node-key node) (node-value node) t))
    (t (rightmost i (right node)))))

(defmethod join-nodes ((i <binary-tree>) a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t
     (assert (order< i (rightmost i a) (leftmost i b)))
     (node i :key (node-key a) :value (node-value a)
           :left (left a)
           :right (node i :key (node-key b) :value (node-value b)
                        :left (join-nodes i (right a) (left b))
                        :right (right b))))))

;;; pure AVL-tree

(defclass <avl-tree> (<binary-tree>) ())

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

(defmethod check-invariant :before ((i <avl-tree>) (node avl-tree-node))
  (assert (typep (node-height node)
                 `(integer 1 ,most-positive-fixnum)))
  (assert (= (node-height node)
             (1+ (max (node-height (left node))
                      (node-height (right node))))))
  (assert (member (node-balance node) '(-1 0 1))))

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
node is always called with branches that are of comparable height...
|#

(defmethod node ((i <avl-tree>) &key left right key value)
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

#| For debugging only
(defmethod print-object ((x binary-tree-node) stream)
  (format stream "#<avl ~A>"
          (flatten-binary-tree x)))

(defun flatten-binary-tree (x)
  (etypecase x
    (null '())
    (binary-tree-node
     (remove-if #'null
                (list (flatten-binary-tree (left x))
                      (node-key x) (flatten-binary-tree (right x)))))))
|#

;;; Common special case: when keys are numbers
;;; TODO: rename to number-map ???
(defclass <integer-map> (<avl-tree> order:<numeric>) ())
(defparameter <integer-map>
  (memo:memoized 'make-instance '<integer-map>))
(defparameter <im> <integer-map>)
