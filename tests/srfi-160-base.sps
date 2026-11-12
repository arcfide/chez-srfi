;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(import (only (chezscheme) format random)
        (rename (rnrs (6))
                (exact? r6rs:exact?)
                (inexact? r6rs:inexact?))
        (only (srfi :1 lists)
              list= take drop count take-while drop-while list-index split-at)
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
        (only (rename (srfi :252 property-testing)
                      (test-property test-property/unnamed)
                      (test-property-expect-fail test-property-expect-fail/unnamed))
              list-generator-of
              symbol-generator
              test-property/unnamed
              test-property-expect-fail/unnamed)
        (srfi :160 base))

(define-syntax (raises    stx) (syntax-violation #f "invalid use of auxilliary syntax" stx))
(define-syntax (irritants stx) (syntax-violation #f "invalid use of auxilliary syntax" stx))
(define-syntax (message   stx) (syntax-violation #f "invalid use of auxilliary syntax" stx))

(define-syntax (define-test-property stx)
  (syntax-case stx (raises irritants message)
    [(_ (bind-to . args)
	body ... body*
        (raises who)
	(irritants irr ...)
	(message msg))
     #'(define (bind-to . args)
         (guard (ex [(and (assertion-violation? ex)
			  (who-condition? ex)
			  (irritants-condition? ex)
			  (message-condition? ex)
			  (eq? (condition-who ex) (quote who))
			  msg)
		     (and (string=? (condition-message ex) msg)
			  (equal? (condition-irritants ex) (list irr ...)))]
		    [(and (assertion-violation? ex)
			  (who-condition? ex)
			  (irritants-condition? ex)
			  (eq? (condition-who ex) (quote who)))
		     (equal? (condition-irritants ex) (list irr ...))])
                body ... body*
                #f))]))

(define-syntax (define-test-property/curried stx)
  (syntax-case stx (raises irritants message)
    [(_ ((generator argU argU* ...) . argsL)
        body ... body*
        (irritants irr ...)
        (message msg))
     #'(define-syntax (generator stx)
         (syntax-case stx ()
           [(_ bind-to argU argU* ...)
            #'(define-test-property (bind-to . argsL)
                body ... body*
                (raises argU)
                (irritants irr ...)
                (message msg))]))]))

(define-syntax (test-property stx)
  (syntax-case stx ()
    [(_ name property gen-list)
     #'(test-property name property gen-list 100)]
    [(_ name property gen-list runs)
     #'(begin
	 (test-property/unnamed property gen-list runs)
	 (let* ([resp (test-result-alist (test-runner-current))]
		[kind (assq 'result-kind resp)])
	   (when (and (pair? kind) (eq? (cdr kind) 'fail))
	     (display (format "PROPERTY FAIL ~a: ~a\n" name (quote property))
		      (current-output-port)))))]))

(define-syntax (test-property-expect-fail stx)
  (syntax-case stx ()
    [(_ name property gen-list)
     #'(test-property-expect-fail name property gen-list 100)]
    [(_ name property gen-list runs)
     #'(begin
	 (test-property-expect-fail/unnamed property gen-list runs)
	 (let* ([resp (test-result-alist (test-runner-current))]
		[kind (assq 'result-kind resp)])
	   (when (and (pair? kind) (eq? (cdr kind) 'fail))
	     (display (format "PROPERTY XPASS ~a: ~a\n" name (quote property))
		      (current-output-port)))))]))

(define (gen-range min max) (make-random-integer-generator min max))

(define like-vowels
  (string->list "faeiohlmnrstvwxyzFAEIOHLMNRSTVWXYZ"))

(define (one-of str)
  (if (and (fx>? (string-length str) 0)
           (memq (string-ref str 0) like-vowels))
      "an"
      "a"))

(define (format-vector-type fmt-str type-of)
  (let ([template (format "~a ~a" (one-of type-of) type-of)])
    (format fmt-str template)))

(define-syntax run-all-base-tests
  (syntax-rules (exact)
    [(_ (exact exact?)
        *type-of*
        %min% %max% *mk-gen-relem*
        *from-args* *make* *from-list* *to-list*
 	*length* *subscript* *update!*)
     (begin
       ;;;
       (define (~l str) (format str *type-of*))
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

       (define-test-property/curried ((define-non-vector-property op) xs . args)
         (apply op xs args)
         (irritants xs) (message (format "~~a is not of type ~a" *type-of*)))
       (define-test-property/curried ((define-sub+upd!/non-index-property op) v k . rest)
         (apply op v k rest)
         (irritants k) (message "index ~a is not a non-negative fixnum"))
       (define-test-property/curried ((define-sub+upd!/bounds-property op) v k . rest)
         (let ([ind (fx+ k (*length* v))])
           (apply op v ind rest))
         (irritants (fx+ k (*length* v)) v); reflects above body
         (message "~a is not a valid index for ~a"))
       (define-test-property/curried ((define-sub+upd!/neg-index-property op) v k . rest)
         (apply op v k rest)
         (irritants k v) (message "~a is not a valid index for ~a"))
       ;;;
       (test-begin (~l"~a initialisation"))
       (define (from-largs args) (apply *from-args* args))
       (define (make-init-property from-list)
         (case-lambda
          [(lst)       (from-list lst)]
          [(lst extra) (from-list (cons extra lst))]))
       (define from-list-property (make-init-property *from-list*))
       (define from-args-property (make-init-property from-largs))
       (define-test-property (non-list-property xs)
         (*from-list* xs)
         (raises *from-list*) (irritants xs) (message "~a is not a proper list"))
       (define-test-property (non-elem-property xs k x)
         (let-values ([(pre-vec post-vec) (split-at xs k)])
           (*from-list* (append pre-vec (cons x post-vec))))
         (raises *from-list*) (irritants x) (message (format-vector-type "element ~~a cannot be contained within ~a" *type-of*)))
       (test-property (~l"list->~a") from-list-property (list (mk-gen-rlist)))
       (test-property (~l"~a") from-args-property (list (mk-gen-rlist)))
       (when exact?
          (test-property (~l"list->~a (minimum/maximum)") from-list-property (list (mk-gen-rlist) gen-min+max))
          (test-property (~l"~a (minimum/maximum)") from-args-property (list (mk-gen-rlist) gen-min+max))
          (test-property-expect-fail (~l"list->~a (1 - minimum, 1 + maximum)") from-list-property (list (mk-gen-rlist) gen-^min+max))
          (test-property-expect-fail (~l"~a (1 - minimum, 1 + maximum)") from-args-property (list (mk-gen-rlist) gen-^min+max)))
       (test-property (~l"list->~a (non-list)") non-list-property (list (symbol-generator)))
       (test-property (~l"list->~a (bad list element)") non-elem-property (list (mk-gen-rlist 24) (gen-range 0 24) (symbol-generator)))
       (test-property (~l"~a (bad list element)") non-elem-property (list (mk-gen-rlist 24) (gen-range 0 24) (symbol-generator)))
       (test-end (~l"~a initialisation"))
       ;;;
       ;;;
       (test-begin (~l"~a make"))
       (define (make-property . args) (apply *make* args))
       (define-test-property (make/neg-width-property width rep)
         (*make* width rep)
         (raises *make*) (irritants width) (message "length ~a is not a non-negative fixnum"))
       (define-test-property (make/non-width-property width rep)
         (*make* width rep)
         (raises *make*) (irritants width) (message (format-vector-type "~~a is not a valid length for ~a" *type-of*)))
       (define-test-property (make/non-elem-property pre rep)
         (*make* pre rep)
         (raises *make*) (irritants rep) (message (format-vector-type "repeating element ~~a cannot be contained within ~a" *type-of*)))
       (test-property (~l"make-~a (size + repeating element)") make-property (list (gen-range 0 65536) (*mk-gen-relem*)))
       (test-property (~l"make-~a (size, omit repeating element)") make-property (list (gen-range 0 65536)))
       (test-property (~l"make-~a (negative size)") make/neg-width-property (list (gen-range -65536 0) (*mk-gen-relem*)))
       (test-property (~l"make-~a (non-numeric size)") make/non-width-property (list (symbol-generator) (*mk-gen-relem*)))
       (test-property (~l"make-~a (invalid repeating element)") make/non-elem-property (list (gen-range 0 65536) (symbol-generator)))
       (test-end (~l"~a make"))
       ;;;
       ;;;
       (test-begin (~l"~a length"))
       (define (length-property lst) (fx=? (length lst) (*length* (*from-list* lst))))
       (define-non-vector-property length/non-vec-property *length*)
       (test-property (~l"~a-length") length-property (list (list-generator-of (*mk-gen-relem*))))
       (test-property (~l"~a-length (invalid vector)") length/non-vec-property (list (list-generator-of (symbol-generator) 24)))
       (test-end (~l"~a length"))
       ;;;
       ;;;
       (test-begin (~l"~a to list"))
       (define to-list-property
         (case-lambda
          [(vec) (to-list-property vec 0 (*length* vec))]
          [(vec start) (to-list-property vec start (*length* vec))]
          [(vec start end) (fx=? (length (*to-list* vec start end))
                                 (fx- end start))]))
       (define-non-vector-property  to-list/non-vec-property *to-list*)
       (define-test-property (to-list/neg-start-property vec start)
         (*to-list* vec start)
         (raises *to-list*) (irritants start) (message "start ~a is not a non-negative fixnum"))
       (define-test-property (to-list/neg-end-property vec start end)
         (*to-list* vec start end)
         (raises *to-list*) (irritants end) (message "end ~a is not a non-negative fixnum"))
       (define-test-property (to-list/bounds-property vec start end)
         (*to-list* vec start end)
         (raises *to-list*) (irritants end start) (message "end ~a must be greater than or equal to start ~a"))
       (define-test-property (to-list/overflow-property vec start end)
         (*to-list* vec start end)
         (raises *to-list*) (irritants end vec) (message "end ~a overflows ~a"))
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
       (test-end (~l"~a to list"))
       ;;;
       ;;;
       (test-begin (~l"~a subscript"))
       (define (sub-property v) (*subscript* v (random (*length* v))))
       (define-non-vector-property sub/non-vec-property *subscript*)
       (define-sub+upd!/non-index-property sub/non-index-property *subscript*)
       (define-sub+upd!/bounds-property sub/bounds-property *subscript*)
       (define-sub+upd!/neg-index-property sub/neg-index-property *subscript*)
       (test-property (~l"~a-ref") sub-property (list (mk-gen-rvec 1)))
       (test-property (~l"~a-ref (non argument)") sub/non-vec-property (list (list-generator-of (*mk-gen-relem*) 24) (gen-range 0 24)))
       (test-property (~l"~a-ref (non-numeric index)") sub/non-index-property (list (mk-gen-rvec 1) (symbol-generator)))
       (test-property (~l"~a-ref (index overflows)") sub/bounds-property (list (mk-gen-rvec 1) (gen-range 0 24)))
       (test-property (~l"~a-ref (negative index)") sub/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
       (test-end (~l"~a subscript"))
       ;;;
       ;;;
       (test-begin (~l"~a update!"))
       (define (upd!-property v k x) (*update!* v k x))
       (define-non-vector-property upd!/non-vec-property *update!*)
       (define-sub+upd!/non-index-property upd!/non-index-property *update!*)
       (define-sub+upd!/bounds-property upd!/bounds-property *update!*)
       (define-sub+upd!/neg-index-property upd!/neg-index-property *update!*)
       (define-test-property (upd!/non-elem-property v k x)
         (*update!* v k x)
         (raises *update!*) (irritants x) (message (format-vector-type "element ~~a cannot be contained within ~a" *type-of*)))
       (define-test-property (upd!/non-elem-range-property v k x)
         (*update!* v k x)
         (raises *update!*) (irritants x) (message (format-vector-type "element ~~a is out of range for ~a" *type-of*)))
       (test-property (~l"~a-set!") upd!-property (list (mk-gen-rvec 24) (gen-range 0 24) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (non ~a argument)") upd!/non-vec-property (list (mk-gen-rlist 24) (gen-range 0 24) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (non-numeric index)") upd!/non-index-property (list (mk-gen-rvec 1) (symbol-generator) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (index overflows)") upd!/bounds-property (list (mk-gen-rvec 1) (gen-range 0 24) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (negative index)") upd!/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0) (*mk-gen-relem*)))
       (test-property (~l"~a-set! (non-numeric element)") upd!/non-elem-property (list (mk-gen-rvec 24) (gen-range 0 24) (symbol-generator)))
       (when exact?
         (test-property (~l"~a-set! (element out of range)") upd!/non-elem-range-property (list (mk-gen-rvec 24) (gen-range 0 24) gen-^min+max)))
       (test-end (~l"~a update!"))
       )]))

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
 #f #f (lambda () (make-random-real-generator 0 1))
 f32vector make-f32vector list->f32vector f32vector->list
 f32vector-length f32vector-ref f32vector-set!)

(run-all-base-tests (exact #f)
 "f64vector"
 #f #f (lambda () (make-random-real-generator 0 1))
 f64vector make-f64vector list->f64vector f64vector->list
 f64vector-length f64vector-ref f64vector-set!)

#| 64 and 128-bit complex vectors |#

(run-all-base-tests (exact #f)
 "c64vector"
 #f #f (lambda () (make-random-rectangular-generator 0 256 0 256))
 c64vector make-c64vector list->c64vector c64vector->list
 c64vector-length c64vector-ref c64vector-set!)

(run-all-base-tests (exact #f)
 "c128vector"
 #f #f (lambda () (make-random-rectangular-generator 0 256 0 256))
 c128vector make-c128vector list->c128vector c128vector->list
 c128vector-length c128vector-ref c128vector-set!)
