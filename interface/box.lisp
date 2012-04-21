;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("interface/interface" "pure/package")))

(in-package :interface)


;;;; Interface

;;; A class for box objects themselves
(defclass box () ())

(defgeneric %unbox (box &key)
  (:documentation "open a box and return its contents"))

;;; An interface for boxes

;;; A box: you can make it, or get something out of it
(define-interface <box> (<interface>) ())

(defgeneric make-box (<box> generator &key)
  (:documentation "Make a box from a generator for the value inside the box"))

(defgeneric unbox (<box> box &key)
  (:documentation "Return the value inside the box"))


;;; Classy box: same, based on a class
(define-interface <classy-box> (<box> <classy>) ())

(defmethod make-box ((i <classy-box>) generator &rest keys &key &allow-other-keys)
  (apply 'instantiate i :generator generator keys))

(defmethod unbox ((i <classy-box>) box &rest keys &key &allow-other-keys)
  (declare (ignorable i))
  (apply '%unbox box keys))


;;;; Boxes that hold a value

(defclass value-box (box)
  ((value :initarg :value :reader box-value)))

(defmethod %unbox ((box value-box) &key)
  (if (slot-boundp box 'value)
    (box-value box)
    (call-next-method)))

(defclass simple-value-box (value-box)
  ((value :initarg :generator)))

(defmethod %unbox ((box simple-value-box) &key)
  (box-value box))

(define-interface <value-box> (<classy-box>)
  ((class :initform 'simple-value-box)))

;;;; Boxes that hold a computation

(defclass thunk-box (box)
  ((thunk :initarg :thunk :reader box-thunk)))

(defclass simple-thunk-box (box)
  ((thunk :initarg :generator)))

(defmethod %unbox ((box simple-thunk-box) &key)
  (funcall (box-thunk box)))

(define-interface <thunk-box> (<classy-box>)
  ((class :initform 'simple-thunk-box)))


;;;; Boxes that hold a promise

(defclass promise-box (value-box simple-thunk-box) ())

(define-interface <promise-box> (<value-box> <simple-thunk-box>)
  ((class :initform 'promise-box)))

(defmacro delay (&body body)
  `(make-instance 'promise-box :thunk #'(lambda () ,@body)))

(defun force (promise)
  (%unbox promise))


;;;; Boxes that can only be used once
(defclass one-use-box (box)
  ((usedp :type boolean :initform nil :accessor %box-usedp)))

(define-interface <one-use-box> (<classy-box>)
  ((class :initform 'one-use-box)))

(defmethod %unbox :before ((box one-use-box) &key)
  (when (%box-usedp box)
    (error "Tried to use ~A more than once" box)))

(defmethod %unbox :after ((box one-use-box) &key)
  (setf (%box-usedp box) t))

;;; Some concrete classes following that pattern.
(defclass one-use-value-box (one-use-box value-box) ())
(define-interface <one-use-value-box> (<one-use-box> <value-box>)
  ((class :initform 'one-use-value-box)))

(defclass one-use-thunk-box (one-use-box thunk-box) ())
(define-interface <one-use-thunk-box> (<one-use-box> <thunk-box>)
  ((class :initform 'one-use-thunk-box)))

(defun make-one-use-function (function &optional name)
  (let ((usedp t))
    (lambda (&rest args)
      (cond
        ((not usedp)
         (let ((fun function))
           (setf usedp t function nil)
           (apply fun args)))
        (t
         (error "Function ~@[~A ~]already called once" name))))))

(defmacro one-use-lambda (formals &body body)
  `(make-one-use-function #'(lambda ,formals ,@body)))


#|
;;;; Boxes that can only be opened with a key

(defclass lock-box ()
  ((lock :initarg :lock :reader %box-lock)))

(define-interface <lock-box> (<lock> <box>) ())

(defmethod unbox :before ((i <lock-box>) box &key key)
  (unless (unlockedp (lock-interface i) (box-lock i box) key)
    (error "Tried to access ~A with invalid key ~A" box key)))

(defmethod make-box :before ((i <lock-box>) generator &key key)
  (declare (ignorable i generator))
  (setf (slot-value box 'key) key))
|#


;;; Some boxes can be empty
(define-interface <emptyable-box> (<box>) ())

(defgeneric empty (<emptyable-box> &key)
  (:documentation "Return an empty box"))

(defgeneric empty-p (<emptyable-box> box &key)
  (:documentation "Return a boolean indicating whether the box was empty"))

;;; Some boxes can be refilled
(defclass mutable-box (box) ())
(defgeneric %set-box! (box value))

(define-interface <mutable-box> (<box>) ())
(defgeneric set-box! (<box> box value))

(defmethod set-box! ((i <classy-box>) box value)
  (declare (ignorable i))
  (%set-box! box value))


(defclass box! (mutable-box emptyable-box value-box) ())

(define-interface <box!> (<mutable-box> <classy-box> <emptyable-box>)
  ((class :initform 'box!)))

(defmethod %set-box! ((box box!) value)
  (setf (slot-value box 'value) value))

(defmethod empty-p ((i <box!>) box &key)
  (declare (ignorable i))
  (slot-boundp box 'value))

(defmethod empty ((i <box!>) &key)
  (declare (ignorable i))
  (make-instance (interface-class <box!>)) 'value)