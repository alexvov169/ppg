(uiop:define-package :ppg/main
    (:nicknames :ppg)
  (:use :cl)
  (:use :ppg/utils)
  (:import-from :alexandria
		#:alist-hash-table
		#:iota)
  (:export))

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



(defun .if (test then else)
  (alist-hash-table
   (loop :for char :in (all 'character)
	 :if (funcall test char)
	   :collect (cons char then)
	 :else
	   :collect (cons char else))))
