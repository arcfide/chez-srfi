;; SRFI-143 r6rs library implementation
;;
;; Implements the fixnum operators specified in SRFI-143 using a combination of
;; R6RS sepcified version and Chez Scheme provided operators where R6RS does
;; not include them.  These are in the helpers library and non-Chez Scheme
;; versions are included for supporting other R6RS implementations.
;;
;; Copyright (c) 2018 - 2020 Andrew W. Keep

(library (srfi :143 fixnums)
  (export
    fx-width fx-greatest fx-least

    fixnum? fx=? fx<? fx>? fx<=? fx>=? fxzero? fxpositive? fxnegative? fxodd?
    fxeven? fxmax fxmin

    fx+ fx- fxneg fx* fxquotient fxremainder fxabs fxsquare fxsqrt

    fx+/carry fx-/carry fx*/carry

    fxnot fxand fxior fxxor fxarithmetic-shift fxarithmetic-shift-left
    fxarithmetic-shift-right fxbit-count fxlength fxif fxbit-set? fxcopy-bit
    fxfirst-set-bit fxbit-field fxbit-field-rotate fxbit-field-reverse)
  (import (rnrs) (srfi :143 helpers))

  (define fx-width (fixnum-width))
  (define fx-greatest (greatest-fixnum))
  (define fx-least (least-fixnum))

  (define fxneg (lambda (i) (fx- i)))

  (define fxsquare (lambda (i) (fx* i i)))

  (define fxsqrt (lambda (i) (exact-integer-sqrt i)))

  (define fxfirst-set-bit (lambda (i) (fxfirst-bit-set i)))

  (define fxbit-field-rotate (lambda (i c s e) (fxrotate-bit-field i s e c)))

  (define fxbit-field-reverse (lambda (i s e) (fxreverse-bit-field i s e))))
