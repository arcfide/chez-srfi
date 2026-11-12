;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(import (only (chezscheme) include format random)
        (rename (rnrs (6)) (exact? r6rs:exact?) (inexact? r6rs:inexact?))
        (only (rnrs r5rs (6)) exact->inexact)
        (only (rename (srfi :1 lists)
                      (remove remove/?))
              list= take drop count take-while drop-while list-index split-at iota
              remove/?)
        (srfi :64 testing)
        (only (srfi :152 strings) string-segment)
        (only (srfi :158 generators-and-accumulators) circular-generator gfilter gmap)
        (only (srfi :194 random-data-generators)
              make-random-integer-generator
              make-random-char-generator
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
        (srfi :160 base)
	(srfi :160 u8) (srfi :160 u16) (srfi :160 u32) (srfi :160 u64)
	(srfi :160 s8) (srfi :160 s16) (srfi :160 s32) (srfi :160 s64)
	(srfi :160 f32) (srfi :160 f64)
        (srfi :160 c64) (srfi :160 c128))

#| General test procedures |#

(define-syntax define-test-equiv
  (syntax-rules ()
    [(_ bind-to to-list)
     (define bind-to
       (case-lambda
        [(expect expr)
	 (test-equal expect (to-list expr))]
	[(name expect expr)
	 (test-equal name expect (to-list expr))]))]))

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

#| Utility procedures |#

(define (compose f . rest)
  (if (null? rest)
      f
      (let ([g (apply compose rest)])
	(lambda args
	  (call-with-values (lambda () (apply g args)) f)))))

(define (identity x) x)

(define (make-range A B)
  (if (or (not (fixnum? A)) (not (fixnum? B)) (fx<? B A))
      (error 'make-range "range must be from fixnums A to B where A <= B")
      (iota (fx- B A) A)))

(define (gen-range min max) (make-random-integer-generator min max))
(define pgen (circular-generator values))

(define (list-index-right pred? lst)
  (let ([res (list-index pred? (reverse lst))])
    (and res
	 (- (length lst) res 1))))

(define (x>=0.5? x) (>= x 0.5))
(define x<0.5? (compose not x>=0.5?))
(define (magn>0.5? x) (> (magnitude x) 0.5))
(define (magn<0.5? x) (< (magnitude x) 0.5))

(define (compare-lengths len comp v . vs)
  "Apply length and run a comparison like min/max, and return the result or #f"
  (let loop ([src vs] [prev (len v)])
    (if (null? src)
	prev
	(let ([res (comp prev (len (car src)))])
	  (and res
	       (loop (cdr src) res))))))

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

#| Mutators |#

(define (fake-from-args *from-args* target-exact? target-real? lst)
  (cond
   [(not target-real?)
    (apply *from-args*
           (map (lambda (x) (make-rectangular (exact->inexact x) 0.0))
                lst))]
   [(not target-exact?)
    (apply *from-args* (map inexact lst))]
   [else
    (apply *from-args* lst)]))

(define (fake-from-list target-exact? target-real? lst)
  (cond
   [(not target-real?)
    (map (lambda (x) (make-rectangular (exact->inexact x) 0.0))
         lst)]
   [(not target-exact?) (map inexact lst)]
   [else lst]))

(define-test-property/curried ((define-non-vector-property op *type-of*) xs . args)
  (apply op xs args)
  (irritants xs) (message (format "~~a is not of type ~a" *type-of*)))

(define-test-property/curried ((define-non-vector-property/1 op *type-of*) proc xs . args)
  (apply op proc xs args)
  (irritants xs) (message (format "~~a is not of type ~a" *type-of*)))

(define-test-property/curried ((define-neg-start-property op) vec start . rest)
  (apply op vec start rest)
  (irritants start) (message "start ~a is not a non-negative fixnum"))

(define-test-property/curried ((define-neg-start-property/1 op) proc vec start . rest)
  (apply op proc vec start rest)
  (irritants start) (message "start ~a is not a non-negative fixnum"))

(define-test-property/curried ((define-neg-end-property op) vec start end . rest)
  (apply op vec start end rest)
  (irritants end) (message "end ~a is not a non-negative fixnum"))

(define-test-property/curried ((define-neg-end-property/1 op) proc vec start end . rest)
  (apply op proc vec start end rest)
  (irritants end) (message "end ~a is not a non-negative fixnum"))

(define-test-property/curried ((define-bounds-property op) vec start end . rest)
  (apply op vec start end rest)
  (irritants end start)
  (message "end ~a must be greater than or equal to start ~a"))

(define-test-property/curried ((define-bounds-property/1 op) proc vec start end . rest)
  (apply op proc vec start end rest)
  (irritants end start) (message "end ~a must be greater than or equal to start ~a"))

(define-test-property/curried ((define-overflow-property op) vec start end . rest)
  (apply op vec start end rest)
  (irritants end vec) (message "end ~a overflows ~a"))

(define-test-property/curried ((define-overflow-property/1 op) proc vec start end . rest)
  (apply op proc vec start end rest)
  (irritants end vec) (message "end ~a overflows ~a"))

#| Constructors |#

(define (reverse-drop-start+end lst start end)
  (if (fx>=? (length lst) end start 0)
      (let loop
	  ([k 0] [src lst] [acc '()])
	(cond [(fx=? k end) acc]
	      [(fx<? k start) (loop (fx+ k 1) (cdr src) acc)]
	      [else
	       (loop (fx+ k 1)
		     (cdr src)
		     (cons (car src) acc))]))
      (error 'drop-start+end
	     "invalid start/end spec" start end)))

(define drop-start+end
  (compose reverse reverse-drop-start+end))

#| The idea behind the test is to have some random source vector.
Then, in a specific area start-end, fill it with the *index*. |#
(define reverse-fill-within
  (case-lambda
   [(exact? lst start end)
    (reverse-fill-within exact? lst start end identity identity)]
   [(exact? lst start end on-inner on-outer)
    (let ([max (length lst)])
      (if (fx>=? max end start 0)
	  (let loop ([k 0] [src lst] [acc '()])
	    (cond [(fx=? k max) acc]
		  [(or (fx<? k start) (fx>=? k end))
                   (let* ([tgt (on-outer (car src))]
                          [sub (if exact?
                                   tgt
                                   (exact->inexact tgt))])
		     (loop (fx+ k 1)
			   (cdr src)
			   (cons sub acc)))]
		  [else
                   (let* ([tgt (on-inner k)]
                          [sub (if exact?
                                   tgt
                                   (exact->inexact tgt))])
		     (loop (fx+ k 1)
			   (cdr src)
			   (cons sub acc)))]))
	  (error 'fill-within
		 "invalid start/end spec" start end)))]))

(define fill-within (compose reverse reverse-fill-within))

(define-test-property/curried ((define-persistent-neg-length-property op) proc size ini)
  (op proc size ini)
  (irritants size) (message "length ~a is not a non-negative fixnum"))

#| Iteration |#

(define (sqrt-nearest x . x*)
  "Get the nearest square root (of absolute value)"
  (call-with-values
      (lambda () (exact-integer-sqrt
                  (abs (apply + x x*))))
    (lambda (first _)
      first)))

(define (char-list-segment lst n)
  (let* ([wide-string (list->string lst)]
	 [segmented (string-segment wide-string n)]
	 [resegmented (map string->list segmented)])
    resegmented))

(define (list-cumulate proc id lst)
  (let loop ([src lst] [prev id] [acc '()])
    (if (null? src)
	(reverse acc)
	(let ([res (proc prev (car src))])
	  (loop (cdr src)
		res
		(cons res acc))))))

(define-test-property/curried ((define-iter-neg-index-property op) vec k . rest)
  (apply op vec k rest)
  (irritants k)
  (message "index ~a is not a non-negative fixnum"))

(define-test-property/curried ((define-iter-overflow-property op) vec k . rest)
  (apply op vec k rest)
  (irritants k vec)
  (message "index ~a overflows ~a"))

(define-test-property/curried ((define-fold-non-vector-property/1 op *type-of*) proc id v . vs)
  (apply op proc id v vs)
  (irritants v) (message (format "~~a is not of type ~a" *type-of*)))

(define-test-property/curried ((define-fold-non-vector-property/2 op *type-of*) proc id v vv . vs)
  (apply op proc id v vv vs)
  (irritants vv) (message (format "~~a is not of type ~a" *type-of*)))

(define-test-property/curried ((define-map-non-vector-property op *type-of*) pre v . vs)
  (apply op pre v vs)
  (irritants v)
  (message (format "~~a is not of type ~a" *type-of*)))

(include "test/srfi-160-mutators-tests.scm")
(include "test/srfi-160-constructors-tests.scm")
(include "test/srfi-160-iteration-tests.scm")
(include "test/srfi-160-searching-tests.scm")
