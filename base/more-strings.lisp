;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; String utilities

#+xcvb (module (:depends-on ("package" "base/utils" "base/streams")))

(in-package :fare-utils)

(def*fun join-strings (strings &key stream separator)
  (with-output (stream)
    (loop
      :with sep = (->string separator)
      :for (string . more-strings) :on strings :do
      (write-string string stream)
      (when more-strings
        (princ sep stream)))))

(def*fun last-char (string)
  (check-type string string)
  (let ((l (length string)))
    (unless (zerop l)
      (char string (1- l)))))

(def*fun but-last-char (string)
  (check-type string string)
  (let ((l (length string)))
    (unless (zerop l)
      (subseq string 0 (1- l)))))

(def*fun first-char (string)
  (check-type string string)
  (unless (zerop (length string))
    (char string 0)))

(def*fun string-prefix-p (prefix string)
  (let* ((x (string prefix))
         (y (string string))
         (lx (length x))
         (ly (length y)))
    (and (<= lx ly) (string= x y :end2 lx))))

(def*fun string-postfix-p (string postfix)
  (let* ((x (string string))
         (y (string postfix))
         (lx (length x))
         (ly (length y)))
    (and (<= ly lx) (string= x y :start1 (- lx ly)))))

(def*fun string-enclosed-p (prefix string postfix)
  (and (string-prefix-p prefix string)
       (string-postfix-p string postfix)))
