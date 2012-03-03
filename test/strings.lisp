#+xcvb (module (:depends-on ("package")))
(in-package :fare-utils-test)

(defsuite* (test-strings
            :in test-suite
            :documentation "Test string functions"))

(deftest test-strcat ()
  (is (equal (strcat "foo" "bar" "baz") "foobarbaz")))

(deftest test-join-strings ()
  (is (equal (join-strings '("/bin" "/usr/bin" "/usr/local/bin") :separator ":")
             "/bin:/usr/bin:/usr/local/bin")))
