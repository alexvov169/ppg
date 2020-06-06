(defsystem :ppg
  :description "Parallel Parser Generator"
  :author "Oleksii Vovchok"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:ppg/main
	       :ppg/utils
	       :ppg/generate/all
	       :ppg/grammar/all
	       :ppg/parse/all
	       :ppg/tests/all))
