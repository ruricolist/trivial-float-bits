;;;; trivial-float-bits.asd

(asdf:defsystem "trivial-float-bits"
  :description "Convert floats to bits and back."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "1.0.0"
  :in-order-to ((asdf:test-op (asdf:test-op "trivial-float-bits/test")))
  :serial t
  :depends-on ("cffi")
  :components ((:file "package")
               (:file "cffi")
               (:file "trivial-float-bits")))

(asdf:defsystem "trivial-float-bits/test"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :depends-on ("fiveam" "trivial-float-bits")
  :perform (asdf:test-op (o c) (uiop:symbol-call :trivial-float-bits/test :run-tests))
  :components ((:file "test")))
