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


(deftest test-eol-p ()
  (with-input-from-string (s (format nil "x~cy~cz~at" #\return #\newline +crlf+))
    (is (equal (n-stream-eol-p s) nil))
    (is (equal (n-stream-has-char-p #\a s) nil))
    (is (equal (n-stream-has-char-p #\x s) #\x))
    (is (equal (n-stream-eol-p s) #\return))
    (is (equal (n-stream-eol-p s) nil))
    (is (equal (n-stream-has-char-p #\y s) #\y))
    (is (equal (n-stream-eol-p s) #\newline))
    (is (equal (n-stream-eol-p s) nil))
    (is (equal (n-stream-has-char-p #\z s) #\z))
    (is (equal (n-stream-eol-p s) +crlf+))
    (is (equal (n-stream-eol-p s) nil))
    (is (equal (n-stream-has-char-p #\t s) #\t))
    (is (equal (n-stream-eol-p s) nil))
    (is (equal (n-stream-has-char-p #\a s) nil))))
