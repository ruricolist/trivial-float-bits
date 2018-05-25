# trivial-float-bits
### _Paul M. Rodriguez <pmr@ruricolist.com>_

You can use `trivial-float-bits` to convert between a float and its representation as an integer.

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

For efficiency, when possible, implementation-specific functions are
used to do the conversion. For unknown implementations, we fall back
to the [IEEE-Floats][] library.

The list of implementation-specific functions is taken unchanged from
the source of [cl-protobufs][].

## License

MIT

[IEEE-Floats]: https://github.com/marijnh/ieee-floats/
[cl-protobufs]: https://gitlab.common-lisp.net/qitab/cl-protobufs
