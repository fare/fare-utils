#+xcvb (module ())
(defpackage :fare-utils-test
  (:use :cl :fare-utils :uiop :hu.dwim.stefil)
  (:export #:test-suite))

(in-package :fare-utils-test)

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing fare-utils"))
