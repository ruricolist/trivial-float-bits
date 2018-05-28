;;;; trivial-float-bits.lisp

(in-package #:trivial-float-bits)

(declaim (inline single-float-bits
                 double-float-bits
                 make-single-float
                 make-double-float
                 float-bits))

#+cmcucl
(declaim (ext:constant-function single-float-bits
                                double-float-bits
                                make-single-float
                                make-double-float))

(defun single-float-bits (x)
  (declare (type single-float x))
  #+abcl    (system:single-float-bits x)
  #+allegro (multiple-value-bind (high low)
                (excl:single-float-to-shorts x)
              (declare (type (unsigned-byte 16) high low))
              (logior (ash high 16) low))
  #+ccl (ccl::single-float-bits x)
  #+cmu  (kernel:single-float-bits x)
  #+sbcl (sb-kernel:single-float-bits x)
  #+lispworks (lispworks-float:single-float-bits x)
  #-(or abcl allegro ccl cmu sbcl lispworks)
  (ieee-floats:encode-float32 x))

(defun double-float-bits (x)
  (declare (type double-float x))
  #+abcl    (values (system:double-float-low-bits x)
                    (system:double-float-high-bits x))
  #+allegro (multiple-value-bind (us3 us2 us1 us0)
                (excl:double-float-to-shorts x)
              (logior (ash us1 16) us0)
              (logior (ash us3 16) us2))
  #+ccl  (multiple-value-bind (high low)
             (ccl::double-float-bits x)
           (values low high))
  #+cmu  (values (kernel:double-float-low-bits x)
                 (kernel:double-float-high-bits x))
  #+sbcl (values (sb-kernel:double-float-low-bits x)
                 (sb-kernel:double-float-high-bits x))
  #+lispworks (let ((bits (lispworks-float:double-float-bits x)))
                (values (logand #xffffffff bits)
                        (ash bits -32)))
  #-(or abcl allegro ccl cmu sbcl lispworks)
  (encode-float64 x))

(defun encode-float64 (x)
  (let ((bits (ieee-floats:encode-float64 x)))
    (values (logand #xffffffff bits)
            (ash bits -32))))

(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  #+abcl    (system:make-single-float bits)
  #+allegro (excl:shorts-to-single-float (ldb (byte 16 16) bits)
                                         (ldb (byte 16 0) bits))
  #+ccl  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+cmu  (kernel:make-single-float bits)
  #+sbcl (sb-kernel:make-single-float bits)
  #+lispworks (lispworks-float:make-single-float bits)
  #-(or abcl allegro ccl cmu sbcl lispworks)
  (ieee-floats:decode-float32 bits))

(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low)
           (type (signed-byte 32) high))
  #+abcl (system:make-double-float (logior (ash high 32) low))
  #+allegro (excl:shorts-to-double-float (ldb (byte 16 16) high)
                                         (ldb (byte 16 0) high)
                                         (ldb (byte 16 16) low)
                                         (ldb (byte 16 0) low))
  #+ccl  (ccl::double-float-from-bits (logand high #xffffffff) low)
  #+cmu  (kernel:make-double-float high low)
  #+sbcl (sb-kernel:make-double-float high low)
  #+lispworks (lispworks-float:make-double-float high low)
  #-(or abcl allegro ccl cmu sbcl lispworks)
  (decode-float64 low high))

(defun decode-float64 (low high)
  (ieee-floats:decode-float64
   (logior (ash high 32) low)))

(defun float-bits (f)
  (etypecase f
    (single-float (single-float-bits f))
    (double-float (double-float-bits f))))
