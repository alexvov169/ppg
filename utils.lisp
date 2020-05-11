(uiop:define-package :ppg/utils
  (:use :cl)
  (:export
   #:defun*))

(in-package :ppg/utils)


(defmacro defun* (name (&rest lambda-lists) &body body)
  (labels ((%lambdas (lambda-lists)
             (if lambda-lists
                 `((lambda ,(first lambda-lists)
                      ,@(%lambdas (rest lambda-lists))))
                 body)))
    `(defun ,name ,(first lambda-lists)
       ,@(%lambdas (rest lambda-lists)))))
