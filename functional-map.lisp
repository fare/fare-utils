;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional mapping of keys to values

#+xcvb (module (:depends-on ("eq")))

(in-package :fare-utils)

(defpackage :functional-map
  (:nicknames :fmap)
  (:use)
  (:export
   #:<map> #:<alist>
   #:<binary-tree> #:<avl-tree>
   #:<number-keyed-functional-map> #:<nkfm>
   #:<pure-hash-table> #:<pure-equal-hash-table>
   #:<faim>
   #:empty
   #:empty-p
   #:lookup
   #:insert
   #:remove
   #:first-key-value
   #:decons
   #:fold-left
   #:fold-right
   #:append
   #:divide
   #:count
   #:append/list
   #:divide/list
   #:update
   #:merge
   #:convert))

;;; On this "Interface-Passing Style" of programming, see
;;;  http://fare.livejournal.com/155094.html

(defclass fmap:<map> (eq:<eq>) ())

(defgeneric fmap:empty (interface)
  (:documentation "Return an empty map"))
(defgeneric fmap:empty-p (interface map)
  (:documentation "Return a boolean indicating whether the map was empty"))
(defgeneric fmap:lookup (interface map key)
  (:documentation "Lookup what map associates to a key,
return two values, the associated value and
a boolean that is true iff an association was found"))
(defgeneric fmap:insert (interface map key value)
  (:documentation "Add a key-value pair to a map,
replacing any previous association for this key,
return a new map."))
(defgeneric fmap:remove (interface map key)
  (:documentation "Remove from a map the association corresponding to given key,
returning three values:
a new map without that association,
the value from the removed association,
and a boolean that is true iff an association was found."))
(defgeneric fmap:first-key-value (interface map)
  (:documentation "Return three values:
a key, a value, and a boolean indicating
whether the map was already empty."))
(defgeneric fmap:decons (interface map)
  (:documentation "Remove an association from a map,
returning four values:
a new map, a key, a value, and a boolean indicating
whether the map was already empty."))
(defgeneric fmap:fold-left (interface map f seed)
  (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons,
yielding association k_1 v_1 .. k_n v_n, and computing
(f (f ... (f (f seed k_1 v_1) k2 v_2) ... k_n-1 v_n-1) k_n v_n)"))
(defgeneric fmap:fold-right (interface map f seed)
  (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons,
yielding association k_1 v_1 .. k_n v_n, and computing
(f k_1 v_1 (f k2 v_2 (f ... (f k_n-1 v_n-1 (f k_n v_n seed))...)))"))

(defgeneric fmap:append (interface map1 map2)
  (:documentation "Merge two maps, returning a merged map.
Mappings from MAP1 override those from MAP2."))
(defgeneric fmap:divide (interface map)
  (:documentation "Divide a map in two,
returning two maps MAP1 and MAP2 that each have strictly
fewer associations than MAP unless MAP is of size one or two."))
(defgeneric fmap:count (interface map)
  (:documentation "Count the number of elements in a map"))
(defgeneric fmap:append/list (interface list)
  (:documentation "Merge a list of maps,
returning a merged map where mappings from
earlier mappings override those from latter mappings."))
(defgeneric fmap:divide/list (interface map)
  (:documentation "Divide a map in a list of several submaps and return that list,
such that merging those maps with append/list
will return a map similar to the original one,
that the returned list is empty iff the initial map is empty,
that the returned list is of length one iff the initial map is a singleton,
and that otherwise, each element of the list is non-empty."))
(defgeneric fmap:update (interface map key fun)
  (:documentation "Update the association of a map for a given key and return a new map,
calling fun with the previous associated value and T if found, with NIL and NIL otherwise,
where fun will return two values,
the new value and a boolean,
the association being removed if the boolean is NIL,
otherwise a new association being setup with the new value."))
(defgeneric fmap:merge (interface fun map1 map2)
  (:documentation "Merge two maps, returning a merged map.
For each key K present in either MAP1 or MAP2,
the function FUN is called with arguments K V1 F1 V2 F2 where
V1 and F1 are the value and found flag for MAP1, and
V2 and F2 are the value and found flag for MAP2,
and FUN returns value V and found flag F,
that correspond the lookup for K in the result."))

(defgeneric fmap:convert (interface2 interface1 map1)
  (:documentation "Convert a map from interface1 to interface2."))

#|
Instead of divide and divide/list and in the spirit of fold-left and fold-right,
we could have a
(defgeneric monoid-fold (i map m-null m-singleton m-append m-append/list))
|#

;;; Simple cases for a lot of the above functions

(defclass fmap-simple-empty () ())
(defmethod fmap:empty ((i fmap-simple-empty))
  '())
(defmethod fmap:empty-p ((i fmap-simple-empty) map)
  (null map))

(defclass fmap-simple-decons () ())
(defmethod fmap:decons ((i fmap-simple-decons) map)
  (multiple-value-bind (k v f) (fmap:first-key-value i map)
    (if f
        (values (fmap:remove i map k) k v f)
        (values map nil nil nil))))

(defclass fmap-simple-update () ())
(defmethod fmap:update ((i fmap-simple-update) map key fun)
  (multiple-value-bind (value foundp) (fmap:lookup i map key)
   (multiple-value-bind (new-value new-foundp) (funcall fun value foundp)
     (cond
       (new-foundp
        (fmap:insert i map key new-value))
       (foundp
        (fmap:remove i map key))
       (t
        map)))))

(defclass fmap-simple-append () ())
(defmethod fmap:append ((i fmap-simple-append) map1 map2)
  (fmap:fold-left i map1 (lambda (m k v) (fmap:insert i m k v)) map2))

(defclass fmap-simple-append/list () ())
(defmethod fmap:append/list ((i fmap-simple-append/list) maplist)
  (reduce #'fmap:append maplist :from-end t))

(defclass fmap-simple-divide/list () ())
(defmethod fmap:divide/list ((i fmap-simple-divide/list) map)
  (cond
    ((null map) '())
    ((null (cdr map)) (list map))
    (t (multiple-value-list (fmap:divide map)))))

(defclass fmap-simple-merge () ())
(defmethod fmap:merge ((i fmap-simple-merge) fun map1 map2)
  (labels ((merge1 (a k v1)
             (let ((mm (car a))
                   (m2 (cdr a)))
               (multiple-value-bind (v2 f2) (fmap:lookup i m2 k)
                 (multiple-value-bind (v f) (funcall fun k v1 t v2 f2)
                   (let ((nmm (if f (fmap:insert i mm k v) mm))
                         (nm2 (if f2 (fmap:remove i m2 k) m2)))
                     (cons nmm nm2))))))
           (merge2 (mm k v2)
             (multiple-value-bind (v f) (funcall fun k nil nil v2 t)
               (if f (fmap:insert i mm k v) mm))))
    (destructuring-bind (mm . m2)
        (fmap:fold-left i map1 #'merge1 (cons (fmap:empty i) map2))
      (fmap:fold-left i m2 #'merge2 mm))))

(defclass fmap-simple-count () ())
(defmethod fmap:count ((i fmap-simple-count) map)
  (fmap:fold-left i map (lambda (x k v) (declare (ignore k v)) (1+ x)) 0))
