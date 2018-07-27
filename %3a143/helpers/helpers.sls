(library (srfi :143 fixnums helpers)
  (export fxabs fxremainder fxquotient)
  (import (rnrs) (rnrs r5rs))

  (define fxabs (lambda (i) (if (fx<? i 0) (fx- i) i)))

  (define fxremainder (lambda (n d) (remainder n d)))

  (define fxquotient (lambda (n d) (quotient n d))))
