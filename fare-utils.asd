;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem "fare-utils"
  :version "1.0.0.5"
  :description "Basic functions and macros, interfaces, pure and stateful datastructures"
  :long-description "fare-utils is a small collection of utilities.
It contains a lot of basic everyday functions and macros"
  :license "MIT" ;; also BSD or bugroff
  :author "Francois-Rene Rideau"
  :depends-on ((:version "asdf" "3.0"))
  :components
  ((:file "package")

   ;;; Utilities wrt Lisp
   (:module "base"
    :depends-on ("package")
    :components
    ((:file "utils")
     (:file "character-classes" :depends-on ("utils"))
     (:file "strings" :depends-on ("utils"))
     (:file "symbols" :depends-on ("strings"))
     (:file "macros" :depends-on ("symbols"))
     (:file "lists" :depends-on ("macros"))
     (:file "packages" :depends-on ("lists"))
     (:file "objects" :depends-on ("macros"))
     (:file "streams" :depends-on ("utils"))
     (:file "hash-tables" :depends-on ("macros"))
     (:file "more-strings" :depends-on ("strings" "streams"))
     (:file "parse-cl-syntax" :depends-on ("macros"))))

   ;;; Utilities wrt Lisp
   (:module "filesystem"
    :depends-on ("base")
    :components
    ((:file "pathnames")
     (:file "files")
     (:file "atomic")))

   ;;; Stateful containers
   (:module "stateful"
    :depends-on ("base")
    :components
    ((:file "package")
     (:file "container" :depends-on ("package"))
     #|;; Instead of reimplementing that here, move any new code to cl-containers.
     (:file "binary-heap" :depends-on ("container"))
     (:file "binomial-heap" :depends-on ("container"))
     |#
     (:file "dllist" :depends-on ("container"))
     #|(:file "sorting" :depends-on ("binary-heap" "binomial-heap"))|#)))
  :in-order-to ((test-op (test-op "fare-utils-test"))))
