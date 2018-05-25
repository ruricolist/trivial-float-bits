;;;; package.lisp

(defpackage #:trivial-float-bits
  (:use #:cl)
  (:export
   #:single-float-bits
   #:double-float-bits
   #:make-single-float
   #:make-double-float))
