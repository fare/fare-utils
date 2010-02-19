(in-package :fare-utils-test)

;;; Simple tests:
(load "/home/fare/cl/asdf/asdf.lisp")

(defparameter <nkfm> fmap:<nkfm>)
(defparameter <alist> fmap:<alist>)

(defun sort-alist (alist) (sort (copy-seq alist) #'< :key #'car))
(defun shuffle-list (list)
  (mapcar #'cdr
          (sort (mapcar (lambda (x) (cons (random most-positive-fixnum) x)) list)
                #'< :key #'car)))
(defun make-alist (n &optional (formatter "~D"))
  (loop :for i :from 1 :to n :collect
    (cons n (format nil formatter n))))
(defun equal-alist (x y)
  (equal (sort-alist x) (sort-alist y)))

(defparameter *alist-100-decimal* (make-alist 100 "~D"))
(defparameter *alist-100-latin* (make-alist 100 "~@R"))
(defparameter *alist-100-english* (make-alist 100 "~R"))

(defparameter *al-1* (shuffle-list *alist-100-decimal*))
(defparameter *al-2* (remove-if-not #'evenp *alist-100-decimal* :key #'car))
(defparameter *al-3* (remove-if-not (lambda (x) (< (length x) 5)) *alist-100-latin* :key #'cdr))
(defparameter *al-5* (remove-duplicates (append *al-2* *al-3*) :key #'car :from-end t))

(defun roundtrip (i al)
  (fmap:convert
   <alist> i
   (check-invariant
    i :map
    (fmap:convert i <alist> al))))

(defmethod test-map ((i fmap:<map>))
  (assert (equal-alist *alist-100-decimal* (roundtrip i *al-1*)))
  (assert (equal-alist *al-5* (fmap:convert <alist> i
                                            (fmap:append (fmap:convert i <alist> *al-2*)
                                                         (fmap:convert i <alist> *al-3*)))))

(defmethod test-map :after ((i fmap:<nkfm>))
  (let ((m (fmap:convert
            i <alist>
            (make-alist 1000 "~@R"))))
    (check-invariant i :node m)
    (assert (equal 16 (node-height m)))
    (assert (equal 1000 (fmap:count i m)))))
