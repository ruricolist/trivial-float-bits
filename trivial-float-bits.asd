;;;; trivial-float-bits.asd

(asdf:defsystem #:trivial-float-bits
  :description "Convert floats to bits and back."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("ieee-floats")
  :components ((:file "package")
               (:file "trivial-float-bits")))
