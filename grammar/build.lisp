(uiop:define-package :ppg/grammar/build
  (:use :cl)
  (:use :ppg/grammar/core)
  (:export #:with-grammar))

(in-package :ppg/grammar/build)


(defmacro with-grammar ((&rest bnf-bindings) &body body)
  `(let (,@(build-bnf-bindings bnf-bindings))
     ,@body))
