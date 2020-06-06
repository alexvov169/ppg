(uiop:define-package :ppg/main
    (:nicknames :ppg)
  (:use :cl)
  (:use :ppg/utils)
  (:import-from :alexandria
		#:alist-hash-table
		#:iota)
  (:export
   #:.top
   #:.let
   #:.is
   #:.or
   #:.and
   #:.nil
   #:.maybe
   #:.+
   #:.*
   ;; character classes
   #:.digit
   #:.alpha
   #:.alnum
   ;; error classes
   #:make-not-matched))

(in-package :ppg)


(defgeneric all (type)
  (:method ((type (eql 'character)))
    (loop :for code :upto 10000
	  :collect (code-char code))))

(defgeneric input-first (input)
  (:method ((input list))
    (first input))
  (:method ((input string))
    (char input 0)))

(defgeneric input-rest (input)
  (:method ((input string))
    (multiple-value-bind (string displacement) 
        (array-displacement input)      
      (make-array (1- (length input))
                  :displaced-to (or string input)
                  :displaced-index-offset (1+ displacement)
                  :element-type (array-element-type input)))))

(defstruct not-matched
  what where)

(defstruct lazy-if
  test then else)

(defstruct lazy-case
  table)

(defun .if (test then else)
  (make-lazy-case
   :table
   (alist-hash-table
    (loop :for char :in (all 'character)
	  :if (funcall test char)
	    :collect (cons char then)
	  :else
	    :collect (cons char else)))))

;; (defstruct lazy-result
;;   function)

;; (defun .just (fn-of-input)
;;   (make-lazy-result :function fn-of-input))

;; (defun .constant (value)
;;   (.just (lambda (input)
;; 	   (values value input))))

;; (defstruct lazy-bind
;;   value target)

;; (defstruct lazy-or
;;   predicates)

;; (defgeneric lazy-eval (expr)
;;   (:method ((expr lazy-if))
;;     (with-slots (test then else) expr
;;       (let ((then (lazy-eval then))
;; 	    (else (lazy-eval else)))
;; 	(lambda (input)
;; 	  (funcall (if (funcall test input)
;; 		       then
;; 		       else)
;; 		   input)))))
;;   (:method ((expr lazy-result))
;;     (lazy-result-function expr))
;;   (:method ((expr lazy-or))
;;     )
;;   (:method ((expr not-matched))
;;     (with-slots (what where) expr
;;       (lambda (input)
;; 	(make-not-matched :what what :where input))))
;;   ;; (:method ((expr lazy-bind))
;;   ;;   (with-slots (value target) expr
;;   ;;     (let ((then (let ((value (lazy-eval value)))
;;   ;; 			    (lambda (input1)
;;   ;; 			      (multiple-value-bind (result1 input2) (funcall value input1)
;;   ;; 				(funcall target
;;   ;; 					 input2))))))
;;   ;; 	(lazy-eval (funcall target
;;   ;; 			    )))))
;;   )

;; ;; eval-val -> eval-tar -> call_val -> (call_tar val)

;; (defun .top (expr)
;;   (lazy-eval expr))

;; (defun .not-matched (what)
;;   (make-not-matched :what what))

;; ;; (defun .bind (parser fn-of-result)
;; ;;   (with-slots (test then else) parser
;; ;;     (.if test
;; ;; 	 (.just
;; ;; 	  (let ((then (lazy-eval then)))
;; ;; 	    (lambda (input)
;; ;; 	      (multiple-value-bind (result input) (funcall then input)
;; ;; 		(funcall (funcall fn-of-result result)
;; ;; 			 input)))))
;; ;; 	 else)))

;; ;; (defmacro .let (binding &body body)
;; ;;   `(.bind ,(second binding)
;; ;; 	  (lambda (,(first binding))
;; ;; 	    ,@body)))

;; (defun .or-aux (other-else parser &rest parsers)
;;   (with-slots (test then else) parser
;;     (.if test
;; 	 then
;; 	 (let ((merged-else (cons (not-matched-what else)
;; 				  other-else)))
;; 	   (if parsers
;; 	       (apply #'.or-aux merged-else parsers)
;; 	       (.not-matched merged-else))))))

;; (defun .or (&rest parsers)
;;   ;; (apply #'.or-aux nil parser parsers)
;;   (let* ((i->parser (alist-hash-table
;; 		     (mapcar (lambda (i parser)
;; 			       (cons i ))
;; 			     (iota (length parsers))
;; 			     parsers)))
;; 	 (maybe-disjoint-case-table
;; 	   (alist-hash-table
;; 	    (mapcar (lambda (char)
;; 		      (cons char
;; 			    (remove-if-not (lambda (parser)
;; 					     (gethash char (lazy-case-table parser)))
;; 					   parsers)))
;; 		    (all 'character))))
;; 	 (non-disjoint-parsers (loop ))))
;;   )

;; (defun .add (fn parser-expr1 parser-expr2)
;;   (let ((parser1 (lazy-eval parser-expr1))
;; 	(parser2 (lazy-eval parser-expr2)))
;;     (.just (lambda (input1)
;; 	     (multiple-value-bind (result1 input2) (funcall parser1 input1)
;; 	       (multiple-value-bind (result2 input) (funcall parser2 input2)
;; 		 (values (funcall fn result1 result2)
;; 			 input)))))))

;; (defun .and-base (adder initial-value parser &rest parsers)
;;   (with-slots (test then else) parser
;;     (.if test
;; 	 (if parsers
;; 	     (.add adder then (apply #'.and-base adder initial-value parsers))
;; 	     (.add adder then (.just (lambda (input)
;; 				       (values initial-value input)))))
;; 	 else)))

;; (defun .and (parser &rest parsers)
;;   (apply #'.and-base #'cons nil parser parsers))

;; (defun .maybe (parser)
;;   (with-slots (test then) parser
;;     (.if test
;; 	 then
;; 	 (.constant nil))))

;; ;; (defun .*-base (adder initial-value parser &optional delimiter)
;; ;;   (.maybe (.+-base adder initial-value parser delimiter)))

;; ;; (defun .+-base (adder initial-value parser &optional delimiter)
;; ;;   (with-slots (test then else) parser
;; ;;     (let ((p (lazy-eval then)))
;; ;;       (.if test
;; ;; 	   (.just (lambda (input)
;; ;; 		    ))
;; ;; 	   else))))

;; ;; (defun .+ (parser &optional delimiter)
;; ;;   (.+-base #'cons nil parser delimiter))

;; ;; (defun .* (parser &optional delimiter)
;; ;;   (.maybe (.+ parser delimiter)))

;; (defun* .test-first ((predicate) (input))
;;   (funcall predicate (input-first input)))

;; (defun .first ()
;;   (.just (lambda (input)
;; 	   (values (input-first input)
;; 		   (input-rest input)))))

;; (defun .is-char (name predicate)
;;   (.if (.test-first predicate)
;;        (.first)
;;        (.not-matched name)))

;; (defun .digit ()
;;   (.is-char "digit" #'digit-char-p))

;; (defun .alpha ()
;;   (.is-char "alpha" #'alpha-char-p))

;; (defun .alnum ()
;;   (.is-char "alpha" (lambda (char)
;; 		      (or (alpha-char-p char)
;; 			  (digit-char-p char)))))

;; ;; (defun run (parser input)
;; ;;   ())
