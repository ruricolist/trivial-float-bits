# trivial-float-bits
### _Paul M. Rodriguez <pmr@ruricolist.com>_

You can use `trivial-float-bits` to convert between a float and its
representation as an integer using implementation-specific functions.

``` lisp
(single-float-bits 1s0)
=> 1065353216

(double-float-bits 1d0)
=> 0, 1072693248

(make-single-float 1065353216)
=> 1.0

(make-double-float 0 1072693248)
=> 1.0D0 (100.0D0%)
```

Conversion is done using low-level functions provided by the Lisp
implementation. If the Lisp implementation is unsupported, we fall
back to casting via FFI.

Note that implementations differ in whether they preserve the sign bit
as the sign of the returned integer. This library normalizes the
functions so they all take, and receive, only unsigned integers.

## License

MIT
