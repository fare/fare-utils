;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem "fare-utils-test"
  :version "1.0.0.1"
  :description "Tests for fare-utils"
  :license "MIT" ;; also BSD or bugroff
  :author "Francois-Rene Rideau"
  :depends-on ("fare-utils" "hu.dwim.stefil")
  :components
  ((:file "package")
   (:file "strings" :depends-on ("package")))
  :perform (test-op (o c) (symbol-call :fare-utils-test :test-suite)))
