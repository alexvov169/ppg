(uiop:define-package :ppg/parse/parallel
  (:use :cl :sb-thread)
  (:use :ppg/parse/core :ppg/grammar/all)
  (:export #:create-rule-dependency-graph))

(in-package :ppg/parse/parallel)


(defun create-rule-dependency-graph (grammar top &key (rule-hash-test #'eq))
  (let ((visited-rules (make-hash-table :test rule-hash-test))
        (rdg (make-hash-table :test rule-hash-test)))
    (labels ((%visit (rule)
               (unless (gethash (rule-name rule) visited-rules)
                 (setf (gethash (rule-name rule) visited-rules)
                       rule))
               (loop :for alt :in (rule-alts rule) :do
                 (loop :for alt-part :in alt :do
                   (progn (push alt-part (gethash rule rdg))
                          (%visit alt-part))))))
      (let ((top-rule (get-rule grammar top)))
        (%visit top-rule)))
    rdg))

(defun execute-parallel-parser (parallel-parser partial-input)
  (make-thread
   (lambda ()
     (funcall parallel-parser
              partial-input))))

(defun merge-asts (merging-sets)
  (lambda (ast1 ast2)
    (labels ((%merge (ast1 ast2)
               (funcall (gethash (ast-rule ast1))
                        (let ((new-asts (loop :for ast-subtree :in (ast-subtrees ast1)
                                              :collect (%merge ast-subtree
                                                               (according-ast ast2
                                                                              :to ast-subtree))))
                              (upper-level-node (select-upper-node (ast-top ast1)
                                                                   (ast-top ast2))))
                          (build-ast-node upper-level-node
					  new-asts)))))
      (%merge ast1 ast2))))

(defun build-parallel-parser (sequenced-parser parallel-parser merging-sets worker-count)
  (let ((ast-merger (merge-asts merging-sets)))
    (lambda (input)
      (execute-partial-parser sequenced-parser input)
      (let ((threads
              (loop :for (i partial-input) := (split-input input)
                    :collect (execute-partial-parser parallel-parser partial-input))))
        (let ((first-ast-part (join-thread (first threads))))
          (loop :for thread :in (rest threads)
                :for ast1 := first-ast-part :then ast2
                :for ast2 := (funcall ast-merger ast1 ast2)
                :finally (return ast2)))))))

(defmethod parse (input &key as worker-count)
  (with-slots (sequenced-parser partial-parser merging-sets) as
    (cond ((= 1 worker-count)
           (funcall sequenced-parser input))
          (otherwise
           (let ((parser (build-parallel-parser sequenced-parser)))
             (funcall parser input))))))
