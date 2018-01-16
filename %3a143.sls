(library (srfi :143)
  (export
    fx-width fx-greatest fx-least

    fixnum? fx=? fx<? fx>? fx<=? fx>=? fxzero? fxpositive? fxnegative? fxodd?
    fxeven? fxmax fxmin

    fx+ fx- fxneg fx* fxquotient fxremainder fxabs fxsquare fxsqrt

    fx+/carry fx-/carry fx*/carry

    fxnot fxand fxior fxxor fxarithmetic-shift fxarithmetic-shift-left
    fxarithmetic-shift-right fxbit-count fxlength fxif fxbit-set? fxcopy-bit
    fxfirst-set-bit fxbit-field fxbit-field-rotate fxbit-field-reverse)
  (import (srfi :143 fixnums)))
