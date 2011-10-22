#+xcvb (module ())
(defpackage :fare-utils-test
  (:use :fare-utils
        :interface :eq :order :pure
        :reader-interception
        :cl :hu.dwim.stefil))
