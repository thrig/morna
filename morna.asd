#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
  (error "You need ASDF >= 3.1 to load this system correctly."))

(asdf:defsystem :morna
  :components ((:file "morna"))
  :in-order-to ((test-op (test-op :morna/test)))
  :version "0.0.7"
  :author "Jeremy Mates <jmates@example.org>"
  :license "BSD"
  :description "simple text pattern generation routines")

(asdf:defsystem :morna/test
  :depends-on (:morna :fiveam)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :5am :run! :morna-suite))
  :description "test suite for morna")
