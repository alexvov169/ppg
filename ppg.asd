(defsystem :ppg
  :description "Parallel Parser Generator"
  :author "Oleksii Vovchok"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:ppg/core
	       :ppg/parallel
	       :ppg/tests))
