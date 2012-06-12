;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :fare-utils-test
  :depends-on (:fare-utils :hu.dwim.stefil)
  :components
  ((:file "package")
   (:file "strings" :depends-on ("package"))))
