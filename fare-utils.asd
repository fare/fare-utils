;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(in-package :cl-user)

(asdf:defsystem :fare-utils
  :components ((:file "package")
	       (:file "basic-utils" :depends-on ("package"))
	       (:file "basic-strings" :depends-on ("basic-utils"))
	       (:file "basic-symbols" :depends-on ("basic-strings"))
	       (:file "basic-macros" :depends-on ("basic-symbols"))
	       (:file "basic-lists" :depends-on ("basic-macros"))
               (:file "basic-packages" :depends-on ("basic-lists"))
               (:file "basic-objects" :depends-on ("basic-macros"))
	       (:file "streams" :depends-on ("basic-utils"))
	       (:file "pathnames" :depends-on ("basic-utils" "streams"))
	       (:file "strings" :depends-on ("basic-strings" "streams"))
	       (:file "files" :depends-on ("basic-utils"))
	       (:file "atomic" :depends-on ("basic-macros"))
	       (:file "msv" :depends-on ("hash-tables"))

               ;;; Independent libraries
               (:file "memoization")

               ;;; Interface-Passing Style generic libraries
               (:file "eq" :depends-on ("basic-utils" "memoization"))
               (:file "order" :depends-on ("eq" "basic-symbols"))

               ;;; IPS pure functional datastructures
               (:file "pure" :depends-on ("basic-utils" "memoization"))
               (:file "pure-maps" :depends-on ("eq" "pure"))
               (:file "pure-alist" :depends-on ("pure-maps"))
               (:file "pure-trees" :depends-on ("pure-maps" "order"))
               (:file "pure-hash-tables" :depends-on ("pure-trees" "pure-alist"))
               (:file "faim" :depends-on ("pure-maps" "pure-trees"))

               ;;; Imperative containers
               (:file "containers" :depends-on ("order" "basic-macros" "basic-utils"))
               (:file "hash-tables" :depends-on ("containers"))
               (:file "binary-heaps" :depends-on ("containers"))
               (:file "binomial-heaps" :depends-on ("containers"))
               (:file "fifo" :depends-on ("containers"))
               (:file "dllist" :depends-on ("containers"))
	       (:file "sorting" :depends-on ("binary-heaps" "binomial-heaps"))))
