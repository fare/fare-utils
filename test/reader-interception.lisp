(in-package :fare-utils-test)

(declaim (optimize (debug 3) (space 1) (speed 1)))

(defvar *standard-readtable* (copy-readtable nil))
(defun standard-read (&key
                      (stream *standard-input*)
                      (eof-error-p t) (eof-value nil)
                      (recursive-p nil))
  (let ((*readtable* *standard-readtable*))
    (with-standard-io-syntax
      (read-preserving-whitespace
       stream eof-error-p eof-value recursive-p))))

(defun oppositep (x)
  (single-arg-form-p '- x))
(defun inversep (x)
  (single-arg-form-p '/ x))
(defun spacep (x)
  (find x #.(coerce #(#\space #\tab #\return #\linefeed) 'string)))

(defvar *char-read-count* 0)

(defun read-count-char (stream &optional eof-error-p)
  (let ((char (read-char stream eof-error-p nil t)))
    (when char (incf *char-read-count*))
    char))

(defun undo-read-char (char stream)
  (when char
    (unread-char char stream)
    (decf *char-read-count*))
  nil)

(defun terminatorp (char)
  (and (position char #(nil #\) #\] #\} #\; #\return #\linefeed)) t))

(defun parse-infix-sum (stream)
  (let ((*count* 0))
    (values (parse-sum stream) *count*)))

(defun next-term-char (stream &key eof-error-p)
  (loop :for char = (read-count-char stream eof-error-p)
    :for spacep = (and char (spacep char))
    :while spacep :finally
    (return (or (and char (not spacep) char)
                (undo-read-char char stream)))))

(defun open-paren-p (char)
  (find char "([{"))

(defun paren-closer (char)
  (char ")]}" (position char "([{")))

(defun parse-sum (stream)
  (parse-sum-continuation
   stream (list (parse-product stream))))

(defun sum-results (rterms)
  (let ((terms (reverse rterms)))
    (cond
      ((null (rest terms))
       (first terms))
      ((every 'oppositep (rest terms))
       `(- ,(first terms) ,@(mapcar 'second (rest terms))))
      (t
       `(+ ,@terms)))))

(defun parse-sum-continuation (stream rterms)
  (let ((char (next-term-char stream :eof-error-p nil)))
    (cond
      ((terminatorp char)
       (undo-read-char char stream)
       (sum-results rterms))
      ((eql char #\+)
       (parse-sum-continuation
        stream (cons (parse-product stream) rterms)))
      ((eql char #\-)
       (parse-sum-continuation
        stream (cons (list '- (parse-product stream)) rterms)))
      (t
       (undo-read-char char stream)
       (error "Invalid character ~S after ~S" char (sum-results rterms))))))

(defun parse-product (stream)
  (parse-product-continuation
   stream (list (parse-terminal stream))))

(defun product-results (rfactors)
  (let ((factors (reverse rfactors)))
    (cond
      ((null (rest factors))
       (first factors))
      ((every 'inversep (rest factors))
       `(/ ,(first factors) ,@(mapcar 'second (rest factors))))
      (t
       `(* ,@factors)))))

(defun parse-product-continuation (stream rfactors)
  (let ((char (next-term-char stream :eof-error-p nil)))
    (cond
      ((or (terminatorp char) (find char "+-"))
       (undo-read-char char stream)
       (product-results rfactors))
      ((eql char #\*)
       (parse-product-continuation
        stream (cons (parse-terminal stream) rfactors)))
      ((eql char #\/)
       (parse-product-continuation
        stream (cons (list '/ (parse-terminal stream)) rfactors)))
      (t
       (undo-read-char char stream)
       (error "Invalid character ~S after ~S" char (product-results rfactors))))))

(defun parse-terminal (stream)
  (let ((char (next-term-char stream :eof-error-p t)))
    (cond
      ((open-paren-p char)
       (parse-paren char stream))
      ((find char "+-")
       (list (find-symbol (string char) :cl)
             (parse-terminal stream)))
      ((digit-char-p char)
       (undo-read-char char stream)
       (parse-decimal stream))
      ((alpha-char-p char)
       (undo-read-char char stream)
       (parse-identifier stream))
      (t
       (undo-read-char char stream)
       (error 'reader-error :stream stream)))))      
  
(defun parse-decimal (stream)
  ;; assume the first is already read then unread and a digit
  (loop :with sum = 0
    :for char = (read-count-char stream nil)
    :for value = (and char (digit-char-p char))
    :while value :do (setf sum (+ (* 10 sum) value))
    :finally
    (undo-read-char char stream)
    (return sum)))
(defun parse-identifier (stream)
  ;; assume the first is already read then unread and alphabetical
  (loop :for char = (read-count-char stream nil)
    :while (and char (alphanumericp char)) :collect char :into chars
    :finally
    (undo-read-char char stream)
    (return (intern (string-upcase (coerce chars 'string))))))
(defun parse-paren (paren stream)
  (let ((sum (parse-sum stream))
        (char (read-count-char stream t)))
    (unless (eql char (paren-closer paren))
      (error "unmatched paren ~S after ~S" paren sum))
    (list 'identity sum)))

(defun string-parse-sum (string)
  (with-input-from-string (s string)
    (parse-sum s)))

(defsuite* (test-reader-interception
            :in root-suite
            :documentation "Testing reader interception"))

(defun string-parse-sum-via-read (x)
  (with-reader-interception (x 'parse-sum)
    (with-input-from-string (s x)
      (read s))))

(defvar *infix-expressions*
  '(("42" . 42)
    ("1+(2*3)/4*(5-6*7)+-8+9" .
     (+ 1 (* (identity (* 2 3)) (/ 4) (identity (- 5 (* 6 7)))) (- 8) 9))
    ("a*a+2*a*b+b*b" .
     (+ (* a a) (* 2 a b) (* b b)))
    ("(-b+deltaroot)/2/a" .
     (/ (identity (+ (- b) deltaroot)) 2 a))
    ("b*b-4*a*c" .
     (- (* b b) (* 4 a c)))))

(defun test-parser (parser)
  (loop :for (string . sexp) :in *infix-expressions* :do
    (eval `(is (equal (,parser ,string) ',sexp)))))

(deftest test-parse-sum ()
  (test-parser 'string-parse-sum))

(deftest test-reader-interception ()
  (test-parser 'string-parse-sum-via-read))
