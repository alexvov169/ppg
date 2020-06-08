(uiop:define-package :ppg/grammar/core
  (:use :cl)
  (:import-from :alexandria
		#:alist-hash-table)
  (:export #:build-bnf-bindings))

(in-package :ppg/grammar/core)


(defstruct rule
  name directives alternatives)

(defclass terminal ()
  ())

(defclass lambda-terminal (terminal)
  ((function :initarg :function
	     :reader terminal-function)))

(defclass named-terminal (terminal)
  ((name :initarg :name
	 :reader terminal-name)))

(defstruct alt-part
  limit terminal)

(defparameter +terminal-directive+
  (alist-hash-table '((:limit t))))

(defun build-alt-part (alt-part)
  (flet ((%terminal (alt-part)
	   (cond ((and (consp alt-part))
		  (case (first alt-part)
		    (:predicate `(make-instance 'lambda-terminal :function ,(second alt-part)))))
		 (t `(make-instance 'named-terminal :name ',alt-part)))))
    (cond ((consp alt-part)
	   (or (%terminal alt-part)
	       (when (gethash (first alt-part) +terminal-directive+)
		 (destructuring-bind (terminal-directive terminal &optional value) alt-part
		   (case terminal-directive
		     (:limit `(make-alt-part :limit ,value
					     :terminal ,(%terminal terminal)))
		     (otherwise (error "unhandled terminal directive ~A"
				       terminal-directive))))))))))

(defun build-alternative (alternative)
  `(list ,@(mapcar #'build-alt-part alternative)))

(defun build-bnf-bindings (bnf-bindings)
  `(,@ (mapcar (lambda (x)
		 (loop :for i :in x
		       :if (consp i)
			 :collect i :into alternatives
		       :else :if (keywordp i)
			       :collect i :into directives
		       :else :collect i :into rule-name
		       :finally (return (if (rest rule-name)
					    (error "extra symbols ~{~A~^ ~} are supplied in the rule"
						   rule-name)
					    `(,(first rule-name)
					      (make-rule
					       :name ',(first rule-name)
					       :directives ',directives
					       :alternatives (list ,@(mapcar #'build-alternative
									     alternatives))))))))
	       bnf-bindings)))
