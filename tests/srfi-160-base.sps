;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(import (only (chezscheme) include)
        (rnrs (6))
        (only (srfi :1 lists) list= take drop count take-while drop-while list-index)
        (rename (srfi :1 lists) (remove remove/?))
        (srfi :64 testing)
        (only (srfi :152 strings) string-segment)
        (only (srfi :158 generators-and-accumulators) circular-generator gfilter gmap)
        (only (srfi :194 random-data-generators)
              make-random-integer-generator
              make-random-char-generator
              make-random-u8-generator
              make-random-u16-generator
              make-random-u32-generator
              make-random-u64-generator
              make-random-s8-generator
              make-random-s16-generator
              make-random-s32-generator
              make-random-s64-generator
              make-random-real-generator
              make-random-rectangular-generator)
        (only (srfi :235 combinators) flip)
        (only (srfi :252 property-testing) list-generator-of symbol-generator)
        (srfi :160 base)
        (srfi :160 meta curried)
        (srfi :160 meta utils)
        (srfi :160 test aux))

(include "tests/srfi-160-base-properties.scm")
;;
(define-syntax run-all-base-tests
  (syntax-rules (exact)
    [(_ (exact exact?)
        *type-of*
        %min% %max% *mk-gen-relem*
        *from-args* *make* *from-list* *to-list*
 	*length* *sub* *upd!*)
     (begin
       #| Start by defining some helpers and generators |#
       (define (~l str) (format str *type-of*))
       (define (gen-range min max) (make-random-integer-generator min max))
       (define gen-min+max (and exact? (circular-generator %min% %max%)))
       (define gen-^min+max (and exact? (circular-generator (- %min% 1) (+ %max% 1))))
       (define mk-gen-rlist
         (case-lambda
          [() (list-generator-of (*mk-gen-relem*))]
          [(min-size) (gfilter (lambda (lst) (<= min-size (length lst))) (list-generator-of (*mk-gen-relem*)))]
          [(min-size max-size) (gfilter (lambda (lst) (<= min-size (length lst) max-size)) (list-generator-of (*mk-gen-relem*)))]))
       (define mk-gen-rvec
         (case-lambda
          [()
           (gmap *from-list* (list-generator-of (*mk-gen-relem*)))]
          [(min-size)
           (gfilter (lambda (vec) (<= min-size (*length* vec)))
                    (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]
          [(min-size max-size)
           (gfilter (lambda (vec) (<= min-size (*length* vec) max-size))
                    (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]))

       (test-begin (~l"~a initialisation"))
       (define (from-largs args) (apply *from-args* args))
       (define-init-property from-list-property *from-list*)
       (define-init-property from-args-property from-largs)
       (define-non-list-property from-list/non-list-property *from-list* *type-of* "list->" "")
       (define-non-elem-property from-list/bad-elem-property *from-list* *type-of* "list->" "")
       (define-non-elem-property from-args/bad-elem-property from-largs *type-of* "" "")
       (test-property (~l"list->~a") from-list-property (list (mk-gen-rlist)))
       (test-property (~l"~a") from-args-property (list (mk-gen-rlist)))
       (when exact?
          (test-property (~l"list->~a (minimum/maximum)") from-list-property (list (mk-gen-rlist) gen-min+max))
          (test-property (~l"~a (minimum/maximum)") from-args-property (list (mk-gen-rlist) gen-min+max))
          (test-property-expect-fail (~l"list->~a (1 - minimum, 1 + maximum)") from-list-property (list (mk-gen-rlist) gen-^min+max))
          (test-property-expect-fail (~l"~a (1 - minimum, 1 + maximum)") from-args-property (list (mk-gen-rlist) gen-^min+max)))
       (test-property (~l"list->~a (non-list)") from-list/non-list-property (list (symbol-generator)))
       (test-property (~l"list->~a (bad list element)") from-list/bad-elem-property (list (mk-gen-rlist 24) (gen-range 0 24) (symbol-generator)))
       (test-property (~l"~a (bad list element)") from-args/bad-elem-property (list (mk-gen-rlist 24) (gen-range 0 24) (symbol-generator)))
       (test-end)

       (test-begin (~l"~a make"))
       (define-make-property make-property *make*)
       (define-make/neg-width-property make-neg-width-property *make* *type-of* "make-" "")
       (define-make/non-width-property make-non-width-property *make* *type-of* "make-" "")
       (define-repeating-non-elem-property/1 make-non-elem-property *make* *type-of* "make-" "")
       (test-property (~l"make-~a (size + repeating element)") make-property (list (gen-range 0 65536) (*mk-gen-relem*)))
       (test-property (~l"make-~a (size, omit repeating element)") make-property (list (gen-range 0 65536)))
       (test-property (~l"make-~a (negative size)") make-neg-width-property (list (gen-range -65536 0) (*mk-gen-relem*)))
       (test-property (~l"make-~a (non-numeric size)") make-non-width-property (list (symbol-generator) (*mk-gen-relem*)))
       (test-property (~l"make-~a (invalid repeating element)") make-non-elem-property (list (gen-range 0 65536) (symbol-generator)))
       (test-end)

       (test-begin (~l"~a length"))
       (define-length-property length-property *from-list* *length*)
       (define-non-vector-property length/non-vec-property *length* *type-of* "" "-length")
       (test-property (~l"~a-length") length-property (list (list-generator-of (*mk-gen-relem*))))
       (test-property (~l"~a-length (invalid vector)") length/non-vec-property (list (list-generator-of (symbol-generator) 24)))
       (test-end)

       (test-begin (~l"~a to list"))
       (define-to-list-property           to-list-property *to-list* *length*)
       (define-non-vector-property        to-list/non-vec-property *to-list* *type-of* "" "->list")
       (define-to-list/neg-start-property to-list/neg-start-property *to-list* *type-of* "" "->list")
       (define-to-list/neg-end-property   to-list/neg-end-property *to-list* *type-of* "" "->list")
       (define-to-list/bounds-property    to-list/bounds-property *to-list* *type-of* "" "->list")
       (define-to-list/overflow-property  to-list/overflow-property *to-list* *type-of* "" "->list")

       (test-property (~l"~a->list (single vector argument)") to-list-property (list (mk-gen-rvec 24)))
       (when exact?
         #| There is some kind of bug in the property testing suite invoked in calling
            test-property here. The trace points to the first thunk in the second clause of `gmap'.
            The SRFI 64 property testing API's default runner so poor that it doesn't distinguish
            between this sort of issue and errors raised by the test. |#
         (test-property (~l"~a->list (vector and start arguments)") to-list-property (list (mk-gen-rvec 24) (gen-range 0 12)))
         (test-property (~l"~a->list (vector, start and end arguments)") to-list-property (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24))))
       (test-property (~l"~a->list (non-~a argument)") to-list/non-vec-property (list (symbol-generator)))
       (test-property (~l"~a->list (negative start)") to-list/neg-start-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
       (test-property (~l"~a->list (negative end)") to-list/neg-end-property (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0)))
       (test-property (~l"~a->list (flipped start/end)") to-list/bounds-property (list (mk-gen-rvec 48) (gen-range 24 48) (gen-range 0 24)))
       (test-property (~l"~a->list (end overflows)") to-list/overflow-property (list (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 48)))
       (test-end)

       (test-begin (~l"~a subscript"))
       (define-sub-property sub-property *sub* *length*)
       (define-non-vector-property sub/non-vec-property *sub* *type-of* "" "-ref")
       (define-sub+upd!/non-index-property sub/non-index-property *sub*  *type-of* "" "-ref")
       (define-sub+upd!/bounds-property sub/bounds-property *sub* *length* *type-of* "" "-ref")
       (define-sub+upd!/neg-index-property sub/neg-index-property *sub* *type-of* "" "-ref")
       (test-property (~l"~a-ref") sub-property (list (mk-gen-rvec 1)))
       (test-property (~l"~a-ref (non argument)") sub/non-vec-property (list (list-generator-of (*mk-gen-relem*) 24) (gen-range 0 24)))
       (test-property (~l"~a-ref (non-numeric index)") sub/non-index-property (list (mk-gen-rvec 1) (symbol-generator)))
       (test-property (~l"~a-ref (index overflows)") sub/bounds-property (list (mk-gen-rvec 1) (gen-range 0 24)))
       (test-property (~l"~a-ref (negative index)") sub/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
       (test-end)

       (test-begin (~l"~a update!"))
       (define-upd!-property upd!-property *upd!*)
       (define-upd!/non-elem-property upd!/non-elem-property *upd!* *type-of* "" "-set!")
       (define-upd!/non-elem-range-property upd!/non-elem-range-property *upd!* *type-of* "" "-set!")
       (define-non-vector-property upd!/non-vec-property *upd!* *type-of* "" "-set!")
       (define-sub+upd!/non-index-property upd!/non-index-property *upd!* *type-of* "" "-set!")
       (define-sub+upd!/bounds-property upd!/bounds-property *upd!* *length* *type-of* "" "-set!")
       (define-sub+upd!/neg-index-property upd!/neg-index-property *upd!* *type-of* "" "-set!")
       (test-property (~l"~a-set!") upd!-property (list (mk-gen-rvec 24) (gen-range 0 24) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (non ~a argument)") upd!/non-vec-property (list (mk-gen-rlist 24) (gen-range 0 24) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (non-numeric index)") upd!/non-index-property (list (mk-gen-rvec 1) (symbol-generator) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (index overflows)") upd!/bounds-property (list (mk-gen-rvec 1) (gen-range 0 24) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (negative index)") upd!/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (non-numeric element)") upd!/non-elem-property (list (mk-gen-rvec 24) (gen-range 0 24) (symbol-generator)))
       (when exact?
         (test-property (~l"~a-set! (element out of range)") upd!/non-elem-range-property (list (mk-gen-rvec 24) (gen-range 0 24) gen-^min+max)))
       (test-end))]))

#| BEGIN THE TESTING! |#

(run-all-base-tests (exact #t)
 "u8vector"
 0 255 make-random-u8-generator
 u8vector make-u8vector list->u8vector u8vector->list
 u8vector-length u8vector-ref u8vector-set!)

(run-all-base-tests (exact #t)
 "u16vector"
 0 65535 make-random-u16-generator
 u16vector make-u16vector list->u16vector u16vector->list
 u16vector-length u16vector-ref u16vector-set!)

(run-all-base-tests (exact #t)
 "u32vector"
 0 4294967295 make-random-u32-generator
 u32vector make-u32vector list->u32vector u32vector->list
 u32vector-length u32vector-ref u32vector-set!)

(run-all-base-tests (exact #t)
 "u64vector"
 0 18446744073709551615 make-random-u64-generator
 u64vector make-u64vector list->u64vector u64vector->list
 u64vector-length u64vector-ref u64vector-set!)

(run-all-base-tests (exact #t)
 "s8vector"
 -128 127 make-random-s8-generator
 s8vector make-s8vector list->s8vector s8vector->list
 s8vector-length s8vector-ref s8vector-set!)

(run-all-base-tests (exact #t)
 "s16vector"
 -32768 32767 make-random-s16-generator
 s16vector make-s16vector list->s16vector s16vector->list
 s16vector-length s16vector-ref s16vector-set!)

(run-all-base-tests (exact #t)
 "s32vector"
 -2147483648 2147483647 make-random-s32-generator
 s32vector make-s32vector list->s32vector s32vector->list
 s32vector-length s32vector-ref s32vector-set!)

(run-all-base-tests (exact #t)
 "s64vector"
 -9223372036854775808 9223372036854775807 make-random-s64-generator
 s64vector make-s64vector list->s64vector s64vector->list
 s64vector-length s64vector-ref s64vector-set!)

#| 32 and 64-bit floating point vectors |#

(run-all-base-tests (exact #f)
 "f32vector"
 #f #f (thunk (make-random-real-generator 0 1))
 f32vector make-f32vector list->f32vector f32vector->list
 f32vector-length f32vector-ref f32vector-set!)

(run-all-base-tests (exact #f)
 "f64vector"
 #f #f (thunk (make-random-real-generator 0 1))
 f64vector make-f64vector list->f64vector f64vector->list
 f64vector-length f64vector-ref f64vector-set!)

#| 64 and 128-bit complex vectors |#

(run-all-base-tests (exact #f)
 "c64vector"
 #f #f (thunk (make-random-rectangular-generator 0 256 0 256))
 c64vector make-c64vector list->c64vector c64vector->list
 c64vector-length c64vector-ref c64vector-set!)

(run-all-base-tests (exact #f)
 "c128vector"
 #f #f (thunk (make-random-rectangular-generator 0 256 0 256))
 c128vector make-c128vector list->c128vector c128vector->list
 c128vector-length c128vector-ref c128vector-set!)
