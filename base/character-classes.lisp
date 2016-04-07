(in-package :fare-utils)

(exporting-definitions

(defun ascii-char-p (x)
  (and (characterp x) (<= 0 (char-code x) 127)))

(defun ascii-uppercase-letter-p (x)
  (char<= #\A x #\Z))

(defun ascii-lowercase-letter-p (x)
  (char<= #\a x #\z))

(defun ascii-letter-p (x)
  (or (ascii-uppercase-letter-p x) (ascii-lowercase-letter-p x)))

(defun ascii-digit-p (x)
  (char<= #\0 x #\9))

(defun ascii-alphanumeric-p (x)
  (or (ascii-letter-p x) (ascii-digit-p x)))

(defun ascii-non-alphanumeric-p (x)
  (and (ascii-char-p x) (not (ascii-alphanumeric-p x))))

(defun ascii-alphanumeric-or-underscore-p (x)
  (or (ascii-alphanumeric-p x)
      (eql x #\_)))
); exporting-definitions
