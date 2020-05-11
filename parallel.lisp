(uiop:define-package :ppg/parallel
  (:use :cl)
  (:use :ppg/utils)
  (:export
   #:parallel-eval))

(in-package :ppg/parallel)


;; (defgeneric parallel-eval (expr)
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
;;     (lazy-result-function expr)))
