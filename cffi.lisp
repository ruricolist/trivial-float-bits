;;;; cffi.lisp -- fallback methods using CFFI.

(in-package :trivial-float-bits)

(defun single-float-bits/cffi (x)
  (declare (type single-float x))
  (cffi:with-foreign-object (ptr :float)
    (setf (cffi:mem-ref ptr :float) x)
    (cffi:mem-ref ptr :uint32)))

(defun double-float-bits/cffi (x)
  (declare (type double-float x))
  (let ((bits
          (cffi:with-foreign-object (ptr :double)
            (setf (cffi:mem-ref ptr :double) x)
            (cffi:mem-ref ptr :uint64))))
    (values (ldb (byte 32 0) bits)
            (ldb (byte 64 32) bits))))

(defun make-single-float/cffi (x)
  (declare (type (unsigned-byte 32) x))
  (cffi:with-foreign-object (ptr :uint32)
    (setf (cffi:mem-ref ptr :uint32) x)
    (cffi:mem-ref ptr :float)))

(defun make-double-float/cffi (lo hi)
  (declare (type (unsigned-byte 32) lo hi))
  (let ((n (logior (ash hi 32) lo)))
    (cffi:with-foreign-object (ptr :uint64)
      (setf (cffi:mem-ref ptr :uint64) n)
      (cffi:mem-ref ptr :double))))
