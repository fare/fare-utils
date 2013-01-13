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

#| use ASDF:LAST-CHAR and ASDF:FIRST-CHAR
(unless (fboundp 'last-char)
(def*fun last-char (string)
  (check-type string string)
  (let ((l (length string)))
    (unless (zerop l)
      (char string (1- l))))))

(unless (fboundp 'first-char)
(def*fun first-char (string)
  (check-type string string)
  (unless (zerop (length string))
    (char string 0))))
|#

(def*fun but-last-char (string)
  (check-type string string)
  (let ((l (length string)))
    (unless (zerop l)
      (subseq string 0 (1- l)))))

(def*fun string-strip-prefix (prefix string)
  (when (string-prefix-p prefix string)
    (subseq string (1+ (length prefix)))))

(def*fun string-strip-suffix (string suffix)
  (when (string-suffix-p string suffix)
    (subseq string 0 (- (length string) (length suffix)))))
