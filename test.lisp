(defpackage :trivial-float-bits/test
  (:use :cl :trivial-float-bits :fiveam)
  (:export :run-tests))
(in-package :trivial-float-bits/test)

(def-suite trivial-float-bits)
(in-suite trivial-float-bits)

(defun run-tests ()
  (run! 'trivial-float-bits))

(test single-float
  (declare (notinline single-float-bits make-single-float))
  (flet ((round-trips? (s)
           (is (= s (make-single-float (single-float-bits s))))))
    (is-true (round-trips? 0s0))
    (is-true (round-trips? -0s0))
    (is-true (round-trips? 1s0))
    (is-true (round-trips? -1s0))
    (is-true (round-trips? most-negative-single-float))
    (is-true (round-trips? least-negative-single-float))
    (is-true (round-trips? most-positive-single-float))
    (is-true (round-trips? least-positive-single-float))
    (is-true (round-trips? single-float-epsilon))
    (is-true (round-trips? single-float-negative-epsilon))

    (print "Testing single float range...")
    (force-output)
    (let* ((low (single-float-bits most-negative-single-float))
           (high (single-float-bits most-positive-single-float))
           (start (min low high))
           (end (max low high))
           (count 0))
      (is-true
       (loop for i from start to end
             always (= i (single-float-bits (make-single-float i)))
             do (incf count))))))

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
