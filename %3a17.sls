(library (srfi :17)
  (export getter-with-setter set!
          car cdr
          caar cadr cdar cddr
          caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
          string-ref vector-ref
          bytevector-ieee-double-native-ref bytevector-ieee-double-ref
          bytevector-ieee-single-native-ref bytevector-ieee-single-ref
          bytevector-s16-native-ref bytevector-s16-ref bytevector-s24-ref
          bytevector-s32-native-ref bytevector-s32-ref bytevector-s40-ref
          bytevector-s48-ref bytevector-s56-ref bytevector-s64-native-ref
          bytevector-s64-ref bytevector-s8-ref bytevector-sint-ref
          bytevector-u16-native-ref bytevector-u16-ref bytevector-u24-ref
          bytevector-u32-native-ref bytevector-u32-ref bytevector-u40-ref
          bytevector-u48-ref bytevector-u56-ref bytevector-u64-native-ref
          bytevector-u64-ref bytevector-u8-ref bytevector-uint-ref
          foreign-ref fxvector-ref hashtable-ref eq-hashtable-ref
          symbol-hashtable-ref list-ref)
  (import (srfi :17 generalized-set!)))
