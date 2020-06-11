(uiop:define-package :ppg/parse/core
  (:use :cl)
  (:export #:parse))

(in-package :ppg/parse/core)


(defgeneric parse (input &key as worker-count)
  "Generic function of parse.
Returns the AST from analyzed INPUT. 
The parameter AS specifies the term that's top-level to parse.
WORKER-COUNT specifies the either a sequensed or parallel execusion policy.
If equals 1, it's the sequenced policy, otherwise if > 1, then the parallel
with WORKER-COUNT.")
