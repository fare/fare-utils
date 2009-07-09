;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(in-package :cl-user)

(asdf:defsystem :fare-utils
  :version "1.96"
  :serial nil
  :components ((:file "package")
	       (:file "basic-utils" :depends-on ("package"))
	       (:file "basic-strings" :depends-on ("basic-utils"))
	       (:file "basic-symbols" :depends-on ("basic-strings"))
	       (:file "basic-macros" :depends-on ("basic-symbols"))
	       (:file "basic-lists" :depends-on ("basic-macros"))
               (:file "basic-packages" :depends-on ("basic-lists"))
               (:file "basic-objects" :depends-on ("basic-macros"))
               (:file "comparators" :depends-on ("basic-macros"))
               (:file "containers" :depends-on ("comparators"))
               (:file "hash-tables" :depends-on ("containers"))
               (:file "binary-heaps" :depends-on ("containers"))
               (:file "binomial-heaps" :depends-on ("containers"))
               (:file "avl-trees" :depends-on ("containers"))
               (:file "fifo" :depends-on ("containers"))
               (:file "dllist" :depends-on ("containers"))
	       (:file "pathnames" :depends-on ("basic-utils"))
	       (:file "streams" :depends-on ("basic-utils"))
	       (:file "strings" :depends-on ("basic-strings" "streams"))
	       (:file "files" :depends-on ("basic-utils"))
	       (:file "sorting" :depends-on ("binary-heaps" "binomial-heaps"))
	       (:file "atomic" :depends-on ("basic-macros"))
	       (:file "msv" :depends-on ("hash-tables"))))
