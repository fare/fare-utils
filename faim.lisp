;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Fast Appendable Integer Maps
;;;
;;; See "Fast Mergable Integer Maps"
;;; Chris Okasaki & Andrew Gill, 1998
;;; http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
;;; Under the hood: Big Endian Patricia Trees (Tries).
;;; Except that in our API, what *they* call Merge is called Append.

#+xcvb (module (:depends-on ("functional-map" "pure-trees")))

(in-package :fare-utils)

;;; Generic tree interface

(defclass fmap:<faim>
    (tree:<tree> fmap:<map>
     fmap-simple-empty fmap-simple-decons fmap-simple-update
     fmap-simple-merge fmap-simple-append/list fmap-simple-count)
  ()
  (:documentation "Keys in binary trees increase from left to right"))

(defparameter fmap:<faim> (make-instance 'fmap:<faim>))

(defclass pure-box ()
  ((datum
    :initarg :datum
    :reader datum)))

;;; big-endian patricia trie
(defclass trie-head (pure-box)
  ((height
    :type fixnum
    :initform 0
    :initarg :height
    :reader node-height)))
(defclass trie-node () ())
(defclass trie-skip (trie-node pure-box)
  ((prefix-bits
    :type (integer 0 *)
    :initarg :prefix-bits
    :reader node-prefix-bits)
   (prefix-length
    :type fixnum
    :initarg :prefix-length
    :reader node-prefix-length)))
(defclass trie-branch (trie-node pure-binary-branch) ())
(defclass full-trie-branch (trie-branch) ())
;;; Not needed: position tells us! (defclass trie-leaf (trie-node pure-box) ())

(defmethod check-invariant ((i fmap:<faim>) role map)
  (declare (optimize (debug 3)))
  (declare (ignore role))
  (check-type map trie-head)
  (trie-check-invariant (datum map) (node-height map) 0)
  map)

(defun trie-check-invariant (trie position key)
  (declare (optimize (debug 3)))
  (check-type position (unsigned-byte))
  (check-type key (unsigned-byte))
  (assert (zerop (ldb (byte position 0) key)))
  (unless (zerop position)
    (etypecase trie
      (trie-skip
       (let ((pbits (node-prefix-bits trie))
             (plen (node-prefix-length trie)))
         (check-type pbits (unsigned-byte))
         (check-type plen (integer 1 *))
         (assert (<= (integer-length pbits) plen))
         (assert (<= plen position))
         (let ((pos (- position plen)))
           (trie-check-invariant (datum trie) pos (dpb pbits (byte plen pos) key)))))
       (trie-branch
         (let ((pos (1- position)))
           (trie-check-invariant (left trie) pos key)
           (trie-check-invariant (right trie) pos (dpb 1 (byte 1 pos) key))))))
  (values))


(defmethod fmap:lookup ((i fmap:<faim>) map key)
  (check-type map trie-head)
  (check-type key (integer 0 *))
  (if map
      (let ((len (integer-length key))
            (height (node-height map)))
        (if (< height len)
            (values nil nil)
            (trie-lookup (datum map) height key)))
      (values nil nil)))
(defun trie-lookup (trie position key)
  (cond
    ((zerop position) (values trie t))
    ((null trie) (values nil nil))
    (t
     (check-type trie trie-node)
     (assert (plusp position))
     (etypecase trie
       (trie-skip
        (let* ((pbits (node-prefix-bits trie))
               (plen (node-prefix-length trie))
               (pos (- position plen)))
          (if (= pbits (ldb (byte plen pos) key))
              (trie-lookup (datum trie) pos key)
              (values nil nil))))
       (trie-branch
        (let ((pos (1- position)))
          (trie-lookup
           (if (zerop (ldb (byte 1 pos) key))
               (left trie)
               (right trie))
           pos key)))))))

(defun make-trie-leaf (position key value)
  (if (zerop position)
      value
      (make-trie-skip position position (ldb (byte position 0) key) value)))
(defun make-trie-skip (position length bits datum)
  (cond
    ((zerop length)
     datum)
    ((and (plusp position) (null datum))
     nil)
    ((and (> position length) (typep datum 'trie-skip))
     (make-instance
      'trie-skip
      :prefix-length (+ length (node-prefix-length datum))
      :prefix-bits (dpb bits (byte length (node-prefix-length datum))
                        (node-prefix-bits datum))
      :datum (datum datum)))
    (t
     (make-instance
      'trie-skip
      :prefix-length length
      :prefix-bits bits
      :datum datum))))
(defun make-trie-branch (pos left right)
  (cond
    ((or (zerop pos)
         (and (typep left 'full-trie-branch)
              (typep right 'full-trie-branch)))
     (make-instance 'full-trie-branch :left left :right right))
    ((and left right)
     (make-instance 'trie-branch :left left :right right))
    (left
     (make-trie-skip pos 1 0 left))
    (right
     (make-trie-skip pos 1 1 right))
    (t
     nil)))
(defun make-trie-head (height trie)
  (if (and (plusp height)
           (typep trie 'trie-skip)
           (zerop (ldb (byte 1 (1- (node-prefix-length trie))) (node-prefix-bits trie))))
      (let* ((plen (integer-length (node-prefix-bits trie)))
             (datum (datum trie))
             (height (- height (- (node-prefix-length trie) plen)))
             (trie (make-trie-skip height plen (node-prefix-bits trie) datum)))
        (make-instance 'trie-head :height height :datum trie))
      (make-instance 'trie-head :height height :datum trie)))

(defmethod fmap:insert ((i fmap:<faim>) map key value)
  (check-type map (or null trie-head))
  (check-type key (integer 0 *))
  (let ((len (integer-length key)))
    (multiple-value-bind (l d)
        (if (null map)
            (values len (make-trie-skip len len key value))
            (let ((height (node-height map))
                  (trie (datum map)))
              (if (< height len)
                  (values len
                          (make-trie-branch
                           len
                           (make-trie-skip len (- len height 1) 0 trie)
                           (make-trie-leaf (1- len) key value)))
                  (values height
                          (trie-insert trie height key value)))))
      (make-trie-head l d))))
(defun trie-insert (trie position key value)
  (if (zerop position) value
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (if (= pbits (ldb (byte plen pos) key))
               (make-trie-skip position plen pbits
                               (trie-insert (datum trie) pos key value))
               (let* ((datum (datum trie))
                      (len (1- plen))
                      (pos (1- position))
                      (trie1 (make-trie-skip
                              position len (ldb (byte len 0) pbits) datum))
                      (hb (ldb (byte 1 len) pbits))
                      (new-hb (ldb (byte 1 pos) key)))
                 (if (= hb new-hb)
                     (make-trie-skip
                      position 1 hb
                      (trie-insert trie1 pos key value))
                     (let ((leaf (make-trie-leaf pos key value)))
                       (if (zerop new-hb)
                           (make-trie-branch pos leaf trie1)
                           (make-trie-branch pos trie1 leaf))))))))
        (trie-branch
         (let ((pos (1- position)))
           (if (zerop (ldb (byte 1 pos) key))
               (make-trie-branch
                pos
                (trie-insert (left trie) pos key value)
                (right trie))
               (make-trie-branch
                pos
                (left trie)
                (trie-insert (right trie) pos key value))))))))
(defmethod fmap:remove ((i fmap:<faim>) map key)
  (check-type map (or null trie-head))
  (multiple-value-bind (v f)
      (fmap:lookup i map key)
    (if f
        (values (make-trie-head
                 (node-height map)
                 (trie-remove (datum map) (node-height map) key))
                v f)
        (values map nil nil))))
(defun trie-remove (trie position key)
  ;; from our contract with fmap:remove,
  ;; we do assume the key IS in fact in the trie.
  (if (zerop position)
      (values trie nil nil)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (assert (= pbits (ldb (byte plen pos) key)))
           (make-trie-skip
            position plen pbits
            (trie-remove (datum trie) pos key))))
        (trie-branch
         (let ((pos (1- position)))
           (if (zerop (ldb (byte 1 pos) key))
               (make-trie-branch
                pos
                (trie-remove (left trie) pos key)
                (right trie))
               (make-trie-branch
                pos
                (left trie)
                (trie-remove (right trie) pos key))))))))
(defmethod fmap:first-key-value ((i fmap:<faim>) map)
  (tree:leftmost i map))
(defmethod fmap:fold-left ((i fmap:<faim>) map f seed)
  (if (null map)
      seed
      (trie-fold-left (datum map) (node-height map) 0 f seed)))
(defun trie-fold-left (trie position key f seed)
  (if (zerop position)
      (funcall f seed key trie)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-fold-left
            (datum trie) pos (dpb pbits (byte plen pos) key) f seed)))
        (trie-branch
         (let ((pos (1- position)))
           (trie-fold-left
            (right trie) pos (dpb 1 (byte 1 pos) key) f
            (trie-fold-left
             (left trie) pos key f seed)))))))
(defmethod fmap:fold-right ((i fmap:<faim>) map f seed)
  (if (null map)
      seed
      (trie-fold-right (datum map) (node-height map) 0 f seed)))
(defun trie-fold-right (trie position key f seed)
  (if (zerop position)
      (funcall f key trie seed)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-fold-right
            (datum trie) pos (dpb pbits (byte plen pos) key) f seed)))
        (trie-branch
         (let ((pos (1- position)))
           (trie-fold-right
            (left trie) pos key f
            (trie-fold-right
             (right trie) pos (dpb 1 (byte 1 pos) key) f seed)))))))
(defmethod tree:leftmost ((i fmap:<faim>) map)
  (if map
      (trie-leftmost (datum map) (node-height map) 0)
      (values nil nil nil)))
(defun trie-leftmost (trie position key)
  (if (zerop position)
      (values key trie t)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-leftmost
            (datum trie) pos (dpb pbits (byte plen pos) key))))
        (trie-branch
         (trie-leftmost (left trie) (1- position) key)))))
(defmethod tree:rightmost ((i fmap:<faim>) map)
  (if map
      (trie-rightmost (datum map) (node-height map) 0)
      (values nil nil nil)))
(defun trie-rightmost (trie position key)
  (if (zerop position)
      (values key trie t)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-rightmost
            (datum trie) pos (dpb pbits (byte plen pos) key))))
        (trie-branch
         (let ((pos (1- position)))
           (trie-rightmost (right trie) pos (dpb 1 (byte 1 pos) key)))))))

(defmethod fmap:divide ((i fmap:<faim>) node)
  (NIY))
(defmethod fmap:divide/list ((i fmap:<faim>) node)
  (NIY))
(defmethod tree:join ((i fmap:<faim>) a b)
  (NIY))

;;; The whole point of faim is that we could do a fast "append",
(defmethod fmap:append ((i fmap:<faim>) a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t
     (check-type a trie-head)
     (check-type b trie-head)
     (let* ((ha (node-height a))
            (hb (node-height b))
            (h (max ha hb)))
       (make-trie-head
        h (trie-append (make-trie-skip h (- h ha) 0 (datum a))
                       (make-trie-skip h (- h hb) 0 (datum b))
                       h))))))
(defun trie-append (a b position)
  (if (zerop position) a
      (etypecase a
        (full-trie-branch a)
        (trie-branch
         (let ((pos (1- position)))
           (etypecase b
             (trie-branch
              (make-trie-branch
               position
               (trie-append (left a) (left b) pos)
               (trie-append (right a) (right b) pos)))
             (trie-skip
              (let* ((pbits (node-prefix-bits b))
                     (plen (node-prefix-length b))
                     (bh (ldb (byte 1 (1- plen)) pbits))
                     (b1
                      (make-trie-skip
                       pos (1- plen)
                       (ldb (byte (1- plen) 0) pbits) (datum b))))
                (if (zerop bh)
                    (make-trie-branch
                     position (trie-append (left a) b1 pos) (right a))
                    (make-trie-branch
                     position (left a) (trie-append (right a) b1 pos))))))))
        (trie-skip
         (let* ((pbits (node-prefix-bits a))
                (plen (node-prefix-length a))
                (pos (1- position))
                (ah (ldb (byte 1 (1- plen)) pbits))
                (a1
                 (make-trie-skip
                  pos (1- plen)
                  (ldb (byte (1- plen) 0) pbits) (datum a))))
           (etypecase b
             (trie-branch
                (if (zerop ah)
                    (make-trie-branch
                     position (trie-append a1 (left b) pos) (right b))
                    (make-trie-branch
                     position (left b) (trie-append a1 (right b) pos))))
             (trie-skip
              (let* ((pbitsb (node-prefix-bits b))
                     (plenb (node-prefix-length b))
                     (bh (ldb (byte 1 (1- plenb)) pbitsb))
                     (b1
                      (make-trie-skip
                       pos (1- plenb)
                       (ldb (byte (1- plenb) 0) pbitsb) (datum b))))
                (if (= ah bh)
                    (make-trie-skip position 1 0 (trie-append a1 b1 pos))
                    (if (zerop ah)
                        (make-trie-branch position a1 b1)
                        (make-trie-branch position b1 a1)))))))))))
