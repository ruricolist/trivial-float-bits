(defpackage :trivial-float-bits/test
  (:use :cl :trivial-float-bits :fiveam)
  (:import-from :trivial-float-bits
    :single-float-bits/cffi
    :double-float-bits/cffi)
  (:export :run-tests))
(in-package :trivial-float-bits/test)

(def-suite trivial-float-bits)
(in-suite trivial-float-bits)

(defun run-tests ()
  (run! 'trivial-float-bits))

(test single-float-vs-cffi
  (flet ((correct? (s)
           (= (single-float-bits s)
              (single-float-bits/cffi s))))
    (is (correct? 0s0))
    (is (correct? 0s0))
    (is (correct? -0s0))
    (is (correct? -0s0))
    (is (correct? 1s0))
    (is (correct? -1s0))
    (is (correct? most-negative-single-float))
    (is (correct? least-negative-single-float))
    (is (correct? most-positive-single-float))
    (is (correct? least-positive-single-float))
    (is (correct? single-float-epsilon))
    (is (correct? single-float-negative-epsilon))))

(test single-float-round-trip
  (declare (notinline single-float-bits make-single-float))
  (flet ((round-trips? (s)
           (is (= s (make-single-float (single-float-bits s))))))
    (is-true (round-trips? 0s0))
    (is-true (round-trips? -0s0))
    (is-true (round-trips? -0s0))
    (is-true (round-trips? 1s0))
    (is-true (round-trips? -1s0))
    (is-true (round-trips? most-negative-single-float))
    (is-true (round-trips? least-negative-single-float))
    (is-true (round-trips? most-positive-single-float))
    (is-true (round-trips? least-positive-single-float))
    (is-true (round-trips? single-float-epsilon))
    (is-true (round-trips? single-float-negative-epsilon))))

(test single-float-round-trip/exhaustive
  (print "Testing all single floats...")
  (force-output)
  (is-true
   (loop for i from 0 below (expt 2 32)
         always
         (or
          ;; Ignore signaling NaNs.
          (<= #x7f800001 i #x7fbfffff)
          (<= #xff800001 i #xffbfffff)
          (= i (single-float-bits (make-single-float i)))))))

(test double-float-vs-cffi
  (declare (notinline make-double-float double-float-bits))
  (flet ((round-trips? (d)
           (multiple-value-bind (lo1 hi1) (double-float-bits d)
             (multiple-value-bind (lo2 hi2) (double-float-bits/cffi d)
               (is (= lo1 lo2))
               (is (= hi1 hi2))))))
    (is-true (round-trips? 0d0))
    (is-true (round-trips? -0d0))
    (is-true (round-trips? 1d0))
    (is-true (round-trips? -1d0))
    (is-true (round-trips? most-negative-double-float))
    (is-true (round-trips? least-negative-double-float))
    (is-true (round-trips? most-positive-double-float))
    (is-true (round-trips? least-positive-double-float))
    (is-true (round-trips? double-float-epsilon))
    (is-true (round-trips? double-float-negative-epsilon))))

(test double-float
  (declare (notinline make-double-float double-float-bits))
  (flet ((round-trips? (d)
           (multiple-value-bind (lo hi) (double-float-bits d)
             (is (= d (make-double-float lo hi))))))
    (is-true (round-trips? 0d0))
    (is-true (round-trips? -0d0))
    (is-true (round-trips? 1d0))
    (is-true (round-trips? -1d0))
    (is-true (round-trips? most-negative-double-float))
    (is-true (round-trips? least-negative-double-float))
    (is-true (round-trips? most-positive-double-float))
    (is-true (round-trips? least-positive-double-float))
    (is-true (round-trips? double-float-epsilon))
    (is-true (round-trips? double-float-negative-epsilon))))
