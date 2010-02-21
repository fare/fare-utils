(in-package :fare-utils-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defparameter <im> <im>)
(defparameter <alist> <alist>)

(defun sort-alist (alist) (sort (copy-seq alist) #'< :key #'car))
(defun shuffle-list (list)
  (mapcar #'cdr
          (sort (mapcar (lambda (x) (cons (random most-positive-fixnum) x)) list)
                #'< :key #'car)))
(defun make-alist (n &optional (formatter "~D"))
  (loop :for i :from 1 :to n :collect
    (cons i (format nil formatter i))))
(defun equal-alist (x y)
  (equal (sort-alist x) (sort-alist y)))

(defparameter *alist-10-latin* (make-alist 10 "~@R"))
(defparameter *alist-100-decimal* (make-alist 100 "~D"))
(defparameter *alist-100-latin* (make-alist 100 "~@R"))
(defparameter *alist-100-english* (make-alist 100 "~R"))

(defparameter *al-1* (shuffle-list *alist-100-decimal*))
(defparameter *al-2* (remove-if-not #'evenp *alist-100-decimal* :key #'car))
(defparameter *al-3* (remove-if-not (lambda (x) (< (length x) 5)) *alist-100-latin* :key #'cdr))
(defparameter *al-5* (remove-duplicates (append *al-2* *al-3*) :key #'car :from-end t))

(defun alist-from (i map)
  (convert <alist> i map))

(defun from-alist (i map)
  (check-invariant i (convert i <alist> map)))

(defgeneric test-map (fmap-interface))

(defmethod test-map ((i <map>))
  ;;; TODO: test each and every function in the API
  (assert (null (alist-from i (empty i))))
  (assert (empty-p i (from-alist i ())))
  (assert (equal "12"
                 (lookup
                  i
                  (from-alist
                   i '((57 . "57") (10 . "10") (12 . "12")))
                  12)))
  (loop :for (k . v) :in *al-1* :with m = (from-alist i *al-1*) :do
    (assert (eq v (lookup i m k))))
  (assert (equal-alist *alist-10-latin*
                       (alist-from i (from-alist i *alist-10-latin*))))
  (assert (equal-alist *alist-10-latin*
                       (alist-from i (from-alist i *alist-10-latin*))))
  (assert (equal-alist *alist-100-decimal*
                       (alist-from i (from-alist i *al-1*))))
  (assert (equal-alist *al-5*
                       (alist-from
                        i (check-invariant
                           i (join i (from-alist i *al-2*)
                                   (from-alist i *al-3*))))))
  t)

(defmethod test-map :after ((i <integer-map>))
  (let* ((a1 (make-alist 1000 "~@R"))
         (a2 (shuffle-list a1))
         (m1 (convert i <alist> a1))
         (m2 (convert i <alist> a2)))
    (check-invariant i m1)
    (check-invariant i m2)
    (assert (= 10 (pure::node-height m1)))
    (assert (<= 10 (pure::node-height m2) 15))
    (assert (= 1000 (size i m1)))
    (assert (= 1000 (size i m2)))))

(test-map <alist>)
(test-map <im>)
(test-map <hash-table>)
(test-map <fmim>)
