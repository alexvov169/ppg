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

(defun json-language-info ()
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

                 (:tokens tokens ((:none-or-many ws token) (:none-or-many ws)))

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

    (list :lexer (lambda (worker-count)
                   (parse input :as tokens
                                :worker-count worker-count))
          :parser (lambda (worker-count)
                    (parse input :as json
                                 :worker-count worker-count))
          :generator (lambda ()
                       (generate json)))))

(defparameter +json-info+ (json-language-info)) 

(deftest test-json
  (testing "JSON paralel parse check"
    (destructuring-bind (&key lexer parser generator) +json-info+
      (let ((input (funcall generator)))
        (ok (equalp (funcall parser 1)
                    (funcall parser 4)))))))


(defun sub-c-language-info ()
  (with-grammar ((primary-expression (identifier)
				     (constant)
				     ((:many string-literal))
				     ("(" expression ")")
				     ("__builtin_va_arg" "(" type-name "," unary-expression ")"))

		 (postfix-expression (primary-expression)
				     (postfix-expression "[" expression "]")
				     (postfix-expression "(" (:optional argument-expression-list) ")")
				     (postfix-expression "." identifier)
				     (postfix-expression "->" identifier)
				     (postfix-expression "++")
				     (postfix-expression "--")
				     ("(" type-name ")" "{" initializer-list (:optional ",") "}"))

		 (argument-expression-list (argument-expression (:none-or-many "," argument-expression-list)))

		 (unary-expression (postfix-expression)
				   ("++" unary-expression)
				   ("--" unary-expression)
				   (unary-operator cast-expression)
				   ("sizeof" unary-expression)
				   ("sizeof" "(" type-name ")"))

		 (unary-operator ("&")
				 ("*")
				 ("+")
				 ("-")
				 ("~")
				 ("!"))

		 (cast-expression ("(" type-name ")" cast-expression)
				  (unary-expression))

		 (multiplicative-expression (cast-expression)
					    (cast-expression "*" multiplicative-expression)
					    (cast-expression "/" multiplicative-expression)
					    (cast-expression "%" multiplicative-expression))

		 (additive-expression (multiplicative-expression)
				      (multiplicative-expression "+" additive-expression)
				      (multiplicative-expression "-" additive-expression))

		 (shift-expression (additive-expression)
				   (additive-expression "<<" shift-expression)
				   (additive-expression ">>" shift-expression))

		 (relational-expression (shift-expression)
					(shift-expression "<" relational-expression)
					(shift-expression ">" relational-expression)
					(shift-expression "<=" relational-expression)
					(shift-expression ">=" relational-expression))

		 (equality-expression (relational-expression)
				      (relational-expression "==" equality-expression)
				      (relational-expression "!=" equality-expression))

		 (and-expression (equality-expression)
				 (equality-expression "&" and-expression))

		 (exclusive-or-expression (and-expression)
					  (and-expression "^" exclusive-or-expression))

		 (inclusive-or-expression (exclusive-or-expression)
					  (exclusive-or-expression "|" inclusive-or-expression))

		 (logical-and-expression (inclusive-or-expression)
					 (inclusive-or-expression "&&" logical-and-expression))

		 (logical-or-expression (logical-and-expression)
					(logical-and-expression "||" logical-or-expression))

		 (conditional-expression (logical-or-expression)
					 (logical-or-expression "?" expression conditional-expression))

		 (assignment-expression (conditional-expression)
					(unary-expression assignment-operator assignment-expression))

		 (assignment-operator ("=")
				      ("*=")
				      ("/=")
				      ("%=")
				      ("+=")
				      ("-=")
				      ("<<=")
				      (">>=")
				      ("&=")
				      ("^=")
				      ("|="))

		 (expression (assignment-expression)
			     (assignment-expression "," expression))

		 (constant-expression (conditional-expression))

		 (declaration (declaration-specifiers init-declaration-list ";")
			      (declaration-specifiers ";")
			      (static-asssert-declaration))

		 (declaration-specifier (storage-class-specifier)
					(type-specifier)
					(type-qualifier)
					(function-specifier)
					(alignment-specifier))

		 (storage-class-specifier ("typedef")
					  ("extern")
					  ("static")
					  ("auto")
					  ("_Tread_local")
					  ("register"))

		 (type-specifier ("void")
				 ("char")
				 ("short")
				 ("int")
				 ("long")
				 ("float")
				 ("double")
				 ("signed")
				 ("unsinged")
				 (atomic-type-specifier)
				 (struct-or-union-specifier)
				 (enum-specifier)
				 (typedef-name)
				 (type-specifier pointer))

		 (struct-or-union-specifier (struct-or-union identifier)
					    (struct-or-union (:optional identifier)
							     "{" struct-declaration-list "}"))

		 (struct-or-union ("struct")
				  ("union"))

		 (struct-declaration-list (:one-or-many struct-declaration))

		 (struct-declaration (static-asssert-declaration)
				     (specifier-qualifier-list (:optional struct-declaration-list) ";"))

		 (pointer ("*" (:optional type-qualifier-list))
			  ("*" (:optional type-qualifier-list) pointer))

		 (typedef-name (identifier))

		 (initializer (assignment-expression)
			      ("{" initializer-list (:optional ",") "}"))

		 (statement (labelled-statement)
			    (compound-statement)
			    (expression-statement)
			    (selection-statement)
			    (iteration-statement)
			    (jump-statement))
		 
		 (labelled-statement (identifier ":" )
				     ("case" constant-expression ":" statement)
				     ("default" ":" statement))

		 (compound-statement ("{" (:optional block-item-list) "}"))

		 (block-item-list (:one-or-many block-item))

		 (block-item statement
			     declaration)

		 (expression-statement ((:optional expression) ";"))

		 (selection-statement ("if" "(" expression ")" statement (:optional "else" statement))
				      ("switch" "(" expression ")" statement))
 
		 (iteration-statement ("while" "(" expression ")" statement)
				      ("do" statement "while" "(" expression ")" ";")
				      ("for" "(" for-condition ")" statement))

		 (for-condition (for-declaration
				 ";"
				 (:optional for-expression)
				 ";"
				 (:optional for-expression))
				((:optional expression) ";"
				 (:optional for-expression) ";"
				 for-expression))

		 (jump-statement ("goto" identifier ";")
				 ("continue" ";")
				 ("break" ";")
				 ("return" (:optional expression) ";"))

		 (:top translation-unit
		       ((:none-or-many external-declaration)))

		 (external-declaration (function-definition)
				       (declaration)
				       (";"))

		 (function-definition ((:optional declaration-specifiers)
				       (:one-or-many declarator)
				       compound-statement))

		 (:token assign ("="))

		 (:token equal ("=="))

		 (:token not-equal ("!="))

		 (:token dot ("."))

		 (:token ellipsis ("..."))

		 (:token arrow ("->"))

		 (:token identifier ((:predicate (lambda (x) (not (digit-char-p x))))
				     (:none-or-many (:predicate (lambda (x)
								  (or (digit-char-p x)
								      (alpha-char-p x)))))))
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

                 (:tokens tokens ((:none-or-many ws token) (:none-or-many ws)))

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

    (list :lexer (lambda (worker-count)
                   (parse input :as tokens
                                :worker-count worker-count))
          :parser (lambda (worker-count)
                    (parse input :as json
                                 :worker-count worker-count))
          :generator (lambda ()
                       (generate json)))))


(defparameter +c-info+ (sub-c-language-info))

(deftest test-c
  (testing "C subset paralel parse check"
    (destructuring-bind (&key lexer parser generator) +c-info+
      (let ((input (funcall generator)))
        (ok (equalp (funcall parser 1)
                    (funcall parser 4)))))))
