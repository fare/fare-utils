;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Basic String Manipulation

(in-package :fare-utils)


;;; simplifying strings into base-strings
(eval-now
 (def*parameter +all-chars-base-feature+ (featurify (subtypep 'character 'base-char))))

(exporting-definitions

(defun base-char-p (c)
  (typep c 'base-char))

(defun null-string-p (x)
  (and (stringp x) (zerop (length x))))


(defun stuff->string (stuff)
  "transform some stuff into a string"
  (typecase stuff
    (character (make-string 1
                            :initial-element stuff
                            :element-type (type-of stuff)))
    (string stuff)
    (null "")
    (symbol (symbol-name stuff))
    (t (format nil "~A" stuff))))

(defun conc-string (&rest rest)
  "make a string by concatenating stuff"
  (apply #'strcat (mapcar #'stuff->string rest)))

)


#+#.fare-utils:+all-chars-base-feature+
(exporting-definitions

(defun string-all-base-char-p (s)
  (check-type s string)
  t)

(defun string-basic-p (s)
  (check-type s string)
  t)

(defun simplify-string (s)
  (check-type s string)
  s)

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

)


#-#.fare-utils:+all-chars-base-feature+
(exporting-definitions

(defun string-all-base-char-p (s)
  (check-type s string)
  (every #'base-char-p s))

(defun string-basic-p (s)
  (check-type s string)
  (or (typep s 'base-string)
      (string-all-base-char-p s)))

(defun simplify-string (s)
  (check-type s string)
  (if (and (not (typep s 'base-char))
           (string-all-base-char-p s))
      (coerce s 'simple-base-string)
      s)
  s)

(defun strcat (&rest strings)
  (let ((basicp (every #'string-basic-p strings)))
    (apply #'concatenate (if basicp 'base-string 'string) strings)))
)
