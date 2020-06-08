(uiop:define-package :ppg/tests/generated
  (:use :cl :rove)
  (:use :ppg/grammar/all))

(in-package :ppg/tests/generated)


(deftest test-parse-digits
  (testing "parallel parse validity"
    (with-grammar ((term (char)
			 (char (:limit term 10)))
		   (:token char (:predicate #'digit-char-p)))
      (let ((input (generate term)))
	(ok (equalp (parse input :as term
				 :worker-count 1)
		    (parse input :as term
				 :worker-count 4)))))))

(deftest test-json
  (testing "JSON paralel parse check"
    (with-grammar ((json (value))

		   (value (string)
			  (number)
			  (obj)
			  (arr)
			  ("true")
			  ("false")
			  ("null"))

		   (obj ("{" pair pairs "}")
			("{" "}"))

		   (arr ("[" value values "]")
			("[" "]"))

		   (values ("," value values))

		   (pair (string ":" value))

		   (pairs (pair pairs))

		   (:tokens tokens ((:none-or-many )))

		   (ws (#\Space)
		       (#\Tab)
		       (#\Newline)
		       (#\Return))

		   (:token token
			   (string)
			   (number)
			   ((:immediate)))

		   (string (#\" (:none-or-many string-char) #\"))

		   (esc ("\\b")
			("\\f")
			("\\n")
			("\\r")
			("\\t"))
		   
		   (string-char ((:range "\\u0000" "\\u001F"))
				((:predicate #'identity)))

		   (number (simple-num (:optional exponent)))

		   (simple-num ((:optional "-") digits))

		   (exponent ("e" simple-num)
			     ("E" simple-num))

		   (digits (digit digits)
			   (digit))

		   (digit ((:predicate #'digit-char-p))))

      (let ((input (generate term)))
	(ok (equalp (parse input :as term
				 :worker-count 1)
		    (parse input :as term
				 :worker-count 4)))))))
