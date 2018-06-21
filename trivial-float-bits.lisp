;;;; trivial-float-bits.lisp

(in-package #:trivial-float-bits)

;;; TODO Make the behavior of make-*-float consistent between Lisps:
;;; it should always take unsigned bytes.

(declaim (inline single-float-bits
                 double-float-bits
                 make-single-float
                 make-double-float
                 float-bits))

#+cmucl
(declaim (ext:constant-function single-float-bits
                                double-float-bits
                                make-single-float
                                make-double-float))

(declaim (ftype (function (single-float)
                          (unsigned-byte 32))
                single-float-bits))
(defun single-float-bits (x)
  (declare (type single-float x))
  (logand #xFFFFFFFF
          #+abcl (system:single-float-bits x)
          #+allegro (multiple-value-bind (high low)
                        (excl:single-float-to-shorts x)
                      (declare (type (unsigned-byte 16) high low))
                      (logior (ash high 16) low))
          #+ccl (ccl::single-float-bits x)
          #+cmu (kernel:single-float-bits x)
          #+sbcl (sb-kernel:single-float-bits x)
          #+lispworks (lispworks-float:single-float-bits x)
          #- (or abcl allegro ccl cmu sbcl lispworks)
          (ieee-floats:encode-float32 x)))

(declaim (ftype (function (double-float)
                          (values (unsigned-byte 32) (unsigned-byte 32)
                                  #+sbcl &optional))))
(defun double-float-bits (x)
  (declare (type double-float x))
  (multiple-value-bind (lo hi)
      (progn                            ;for indentation
        #+abcl (values (system:double-float-low-bits x)
                       (system:double-float-high-bits x))
        #+allegro (multiple-value-bind (us3 us2 us1 us0)
                      (excl:double-float-to-shorts x)
                    (logior (ash us1 16) us0)
                    (logior (ash us3 16) us2))
        #+ccl (multiple-value-bind (high low)
                  (ccl::double-float-bits x)
                (values low high))
        #+cmu (values (kernel:double-float-low-bits x)
                      (kernel:double-float-high-bits x))
        #+sbcl (values (sb-kernel:double-float-low-bits x)
                       (sb-kernel:double-float-high-bits x))
        #+lispworks (let ((bits (lispworks-float:double-float-bits x)))
                      (values (logand #xffffffff bits)
                              (ash bits -32)))
        #- (or abcl allegro ccl cmu sbcl lispworks)
        (encode-float64 x))
    (values lo (logand #xFFFFFFFF hi))))

(defun encode-float64 (x)
  (let ((bits (ieee-floats:encode-float64 x)))
    (values (logand #xffffffff bits)
            (ash bits -32))))

(declaim (inline unsigned->signed))
(defun unsigned->signed (n)
  (if (logbitp 31 n)
      (dpb n (byte 32 0) -1)
      n))

(defun make-single-float (unsigned)
  (symbol-macrolet ((signed (unsigned->signed unsigned)))
    #+abcl (system:make-single-float signed)
    #+allegro (excl:shorts-to-single-float (ldb (byte 16 16) unsigned)
                                           (ldb (byte 16 0) unsigned))
    #+ccl (ccl::host-single-float-from-unsigned-byte-32 unsigned)
    #+cmu (kernel:make-single-float signed)
    #+sbcl (sb-kernel:make-single-float signed)
    ;; TODO I'm not sure about LispWorks!
    ;; #+lispworks (lispworks-float:make-single-float unsigned)
    #- (or abcl allegro ccl cmu sbcl lispworks)
    (ieee-floats:decode-float32 unsigned)))

(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low))
  (symbol-macrolet ((high-signed (unsigned->signed high)))
    #+abcl (system:make-double-float (logior (ash high 32) low))
    #+allegro (excl:shorts-to-double-float (ldb (byte 16 16) high)
                                           (ldb (byte 16 0) high)
                                           (ldb (byte 16 16) low)
                                           (ldb (byte 16 0) low))
    #+ccl (ccl::double-float-from-bits high low)
    #+cmu (kernel:make-double-float high-signed low)
    #+sbcl (sb-kernel:make-double-float high-signed low)
    ;; TODO I'm not sure about LispWorks!
    ;; #+lispworks (lispworks-float:make-double-float high low)
    #- (or abcl allegro ccl cmu sbcl lispworks)
    (decode-float64 low high)))

(defun decode-float64 (low high)
  (ieee-floats:decode-float64
   (logior (ash high 32) low)))

(defun float-bits (f)
  (etypecase f
    (single-float (single-float-bits f))
    (double-float (double-float-bits f))))
