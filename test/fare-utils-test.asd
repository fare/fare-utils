;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :fare-utils-test
  :depends-on (:fare-utils :hu.dwim.stefil)
  :components
  ((:file "package")
   (:file "functional-map" :depends-on ("package"))))
