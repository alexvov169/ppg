(uiop:define-package :ppg/tests
  (:use :cl :rove)
  (:use :ppg/core)
  (:export))

(in-package :ppg/tests)


(defparameter *dc*
  (.top (.digit)))

(defparameter *ac*
  (.top (.alpha)))

(defparameter *or*
  (.top (.or (.digit)
	     (.alpha))))

(defparameter *or.1*
  (.top (.or (.digit))))

(defparameter *maybe.dc*
  (.top (.maybe (.digit))))

(defparameter *and*
  (.top (.and (.digit) (.alpha))))

;; (defparameter *bind*
;;   (.top (.bind (.digit)
;; 	       (.just (lambda (x input)
;; 			(values (list :token x)
;; 				input))))))

;; (defparameter *number*
;;   (.top (.bind (.+ (.digit))
;; 	       (lambda (digits)
;; 		 (.just (lambda (input)
;; 			  (reduce (lambda (number digit)
;; 				    (+ (* number 10)
;; 				       digit))
;; 				  digits)))))))

;; (defparameter *number.1*
;;   (let ((value (lazy-eval (.+ (.digit))))
;; 	(target (lazy-eval (funcall (lambda (digits)
;; 				      (let ((digits )))
;; 				      (.or (.alpha)
;; 					   (.just (lambda (input)
;; 						    (reduce (lambda (number digit)
;; 							      (+ (* number 10)
;; 								 digit))
;; 							    digits)))))
;; 				    (lambda (input)
;; 				      )))))
;;     (lambda (input)
;;       )))

(defparameter *non-disjoint-or*
  (.top (.or (.and (.digit) (.alpha))
	     (.and (.alnum) (.digit)))))

(deftest dc.1 ()
  (ok (equalp (funcall *dc* "0")
	      #\0))
  (ok (equalp (funcall *dc* "a")
	      (make-not-matched :what "digit"
				:where "a")))
  (ok (equalp (funcall *ac* "a")
	      #\a))
  (ok (equalp (funcall *ac* "0")
	      (make-not-matched :what "alpha"
				:where "0")))
  (ok (equalp (funcall *or* "0")
	      #\0))
  (ok (equalp (funcall *or* "a")
	      #\a))
  (ok (equalp (funcall *or* "-")
	      (make-not-matched :what '("alpha" "digit")
				:where "-")))
  (ok (equalp (funcall *and* "0a")
	      '(#\0 #\a)))
  (ok (equalp (funcall *and* "a0")
	      (make-not-matched :what "digit"
				:where "a0"))))
