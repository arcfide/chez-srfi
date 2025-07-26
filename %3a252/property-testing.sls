;; Property-based testing extension for SRFI 64.
;; SPDX-License-Identifier: MIT
;; Copyright 2024 Antero Mejr <mail@antr.me>

;; Minimal changes for Chez by D. Guthrie, Glasgow, 2025

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi :252 property-testing)
  (export test-property test-property-expect-fail test-property-skip
          test-property-error test-property-error-type
          property-test-runner
          ;; Generator procedures
          boolean-generator bytevector-generator
          char-generator string-generator symbol-generator
          ;; exact number generators
          exact-complex-generator exact-integer-generator
          exact-number-generator exact-rational-generator
          exact-real-generator
          exact-integer-complex-generator
          ;; inexact number generators
          inexact-complex-generator inexact-integer-generator
          inexact-number-generator inexact-rational-generator
          inexact-real-generator
          ;; Unions of numerical generators
          complex-generator integer-generator
          number-generator rational-generator
          real-generator
          ;; Special generators
          list-generator-of pair-generator-of procedure-generator-of
          vector-generator-of)
  (import (scheme)
	  (srfi :0 cond-expand)
	  (srfi :64 testing)
	  (srfi :158 generators-and-accumulators)
	  (srfi :194 random-data-generators))

  ;; Constants

  ;; These values may be implementation-dependent, but should be reasonably
  ;; high numbers.
  ;; Number of property tests to run by default.
  (define default-runs 100)

  ;; Value range for exact random generators.
  (define max-exact (greatest-fixnum))
  (define min-exact (least-fixnum))

  ;; Value range for inexact random generators.
  (cond-expand
   ((srfi :144)
    (import (srfi :144))
    (define max-inexact fl-greatest)
    (define min-inexact fl-least))
   (else
    (define max-inexact max-exact)
    (define min-inexact min-exact)))

  ;; Maximum size for random bytevector/list/string/symbol/vector generators.
  (define max-size 1001)
  ;; Maximum character supported by integer->char.
  (define max-char #x10FFFF)

  ;; Omit values that are not distinguished in the implementation.
  (define special-number
    (list->generator
     (append
      ;; Exact integers
      '(0 1 -1)
      ;; Exact ratios
      '(1/2 -1/2)
      ;; Exact complex
      '(0+i 0-i 1+i 1-i -1+i -1-i)
      ;; Exact complex ratios
      '(1/2+1/2i 1/2-1/2i -1/2+1/2i -1/2-1/2i)
      ;; Inexact (integers and non-integers)
      '(0.0 -0.0 0.5 -0.5 1.0 -1.0)
      ;; Inexact-complex
      '(0.0+1.0i 0.0-1.0i -0.0+1.0i -0.0-1.0i
		 0.5+0.5i 0.5-0.5i -0.5+0.5i -0.5-0.5i
		 1.0+1.0i 1.0-1.0i -1.0+1.0i -1.0-1.0i
		 +inf.0+inf.0i +inf.0-inf.0i -inf.0+inf.0i -inf.0-inf.0i
		 +nan.0+nan.0i)
      ;; Other (-nan.0 not required, synonymous with +nan.0)
      '(+inf.0 -inf.0 +nan.0))))

  ;; Generator procedures

  (define (boolean-generator)
    (gcons* #t #f (make-random-boolean-generator)))

  (define (bytevector-generator)
    (let ((gen (make-random-u8-generator)))
      (gcons* (bytevector)
              (gmap (lambda (len)
                      (apply bytevector (generator->list gen len)))
                    (make-random-integer-generator 0 max-size)))))

  (define (char-generator)
    (gcons* #\nul
            (gmap integer->char
                  (gfilter (lambda (x)
                             (or (< x #xD800) (> x #xDFFF)))
                           (make-random-integer-generator 0 max-char)))))

  (define (string-generator)
    (gcons* ""
            (gmap (lambda (n)
                    (generator->string (gdrop (char-generator) 1) n))
                  (make-random-integer-generator 1 max-size))))

  (define (symbol-generator)
    (gmap string->symbol (string-generator)))

  ;; Exact number generators

  (define (exact-complex-generator)
    (gappend (gfilter (lambda (x)
                        (and (complex? x)
                             (exact? (real-part x))
                             (exact? (imag-part x))))
                      special-number)
             (gmap make-rectangular
                   (exact-real-generator)
                   (exact-real-generator))))

  (define (exact-integer-generator)
    (gappend (gfilter (lambda (x)
                        (and (exact? x) (integer? x)))
                      special-number)
             (make-random-integer-generator min-exact max-exact)))

  (define (ratio-gen)
    (gmap /
          (make-random-integer-generator min-exact max-exact)
          (gfilter (lambda (x) (not (zero? x)))
                   (make-random-integer-generator min-exact max-exact))))

  (define (exact-number-generator)
    ;; Ensure there are no repeated special values, and a random sampling
    ;; between exact ratios, complex, and integers.
    (gappend
     (gfilter exact? special-number)
     (gsampling (gmap make-rectangular
                      (exact-real-generator) (exact-real-generator))
                (ratio-gen)
                (make-random-integer-generator min-exact max-exact))))

  (define (exact-rational-generator)
    (gappend
     (gfilter (lambda (x)
                (and (rational? x) (exact? x)))
              special-number)
     (gsampling (ratio-gen)
                (make-random-integer-generator min-exact max-exact))))

  (define (exact-real-generator)
    (gappend
     (gfilter (lambda (x)
                (and (real? x) (exact? x)))
              special-number)
     (gsampling (ratio-gen)
                (make-random-integer-generator min-exact max-exact))))

  (define (exact-integer-complex-generator)
    (gappend (gfilter (lambda (x)
                        (and (complex? x)
                             (exact? (real-part x))
                             (exact? (imag-part x))
                             (integer? (real-part x))
                             (integer? (imag-part x))))
                      special-number)
             (gmap make-rectangular
                   (make-random-integer-generator min-exact max-exact)
                   (make-random-integer-generator min-exact max-exact))))

  ;; Inexact number generators

  (define (inexact-complex-generator)
    (gappend (gfilter (lambda (x)
                        (and (complex? x)
                             (inexact? (real-part x))
                             (inexact? (imag-part x))))
                      special-number)
             (make-random-rectangular-generator min-inexact max-inexact
                                                min-inexact max-inexact)))

  (define (inexact-integer-generator)
    (gmap inexact (exact-integer-generator)))

  (define (inexact-number-generator)
    (gappend (gfilter inexact? special-number)
             (gsampling (make-random-rectangular-generator
                         min-inexact max-inexact min-inexact max-inexact)
                        (make-random-real-generator min-inexact max-inexact))))

  (define (inexact-rational-generator)
    (gappend (gfilter (lambda (x)
                        (and (rational? x)
                             (inexact? x)))
                      special-number)
             (make-random-real-generator min-inexact max-inexact)))

  (define (inexact-real-generator)
    (gappend (gfilter (lambda (x)
                        (and (real? x)
                             (inexact? x)))
                      special-number)
             (make-random-real-generator min-inexact max-inexact)))

  ;; Unions of number generators

  (define (complex-generator)
    (gsampling (exact-complex-generator)
               (inexact-complex-generator)))

  (define (integer-generator)
    (gsampling (exact-integer-generator)
               (inexact-integer-generator)))

  (define (number-generator)
    ;; TODO: May need to be modified for unusual implementation-specific
    ;; number types, like Kawa's quaternion.
    (gsampling (exact-number-generator)
               (inexact-number-generator)))

  (define (rational-generator)
    (gsampling (exact-rational-generator)
               (inexact-rational-generator)))

  (define (real-generator)
    (gsampling (exact-real-generator)
               (inexact-real-generator)))

  ;; Special generators for collection types

  (define list-generator-of
    (case-lambda
     ((gen)
      (gcons* '()
              (gmap (lambda (len)
                      (generator->list gen len))
                    (make-random-integer-generator 1 max-size))))
     ((gen max-length)
      (gcons* '()
              (gmap (lambda (len)
                      (generator->list gen len))
                    (make-random-integer-generator 1 max-length))))))

  (define pair-generator-of
    (case-lambda
     ((gen1) (gmap cons gen1 gen1))
     ((gen1 gen2) (gmap cons gen2 gen2))))

  (define (procedure-generator-of gen)
    ;; Generate variadic procedures that returns a value from a generator.
    ;; Useful for testing procedures that accept procedure arguments.
    (gmap (lambda (x)
            (lambda _
              x))
          gen))

  (define vector-generator-of
    (case-lambda
     ((gen)
      (gcons* (vector)
              (gmap (lambda (len)
                      (generator->vector gen len))
                    (make-random-integer-generator 0 max-size))))
     ((gen max-length)
      (gcons* (vector)
              (gmap (lambda (len)
                      (generator->vector gen len))
                    (make-random-integer-generator 0 max-length))))))

  ;; Runner

  (define (property-test-runner)
    ;; Implementation specific.
    ;; Some implementations do not support extended test runners.
    (let ((runner (test-runner-simple)))
      ;; (test-runner-on-test-end! runner property-test-runner-on-test-end)
      ;; (test-runner-on-group-end! runner property-test-runner-on-group-end)
      runner))

  ;; Test procedures

  (define (prop-test property generators runs)
      (for-each
       (lambda (n)
         (test-assert
             (apply property
                    (let ((args (map (lambda (gen) (gen)) generators))
                          (runner (test-runner-current)))
                      (test-result-set! runner 'property-test-arguments args)
                      (test-result-set! runner 'property-test-iteration
                                        (+ n 1))
                      (test-result-set! runner 'property-test-iterations runs)
                      args))))
       (iota runs)))

  (define (prop-test-error type property generators runs)
    (for-each
     (lambda (n)
       (test-error
        type
        (apply property
               (let ((args (map (lambda (gen) (gen)) generators))
                     (runner (test-runner-current)))
                 (test-result-set! runner 'property-test-arguments args)
                 (test-result-set! runner 'property-test-iteration (+ n 1))
                 (test-result-set! runner 'property-test-iterations runs)
                 args))))
     (iota runs)))

  (define test-property-error
    (case-lambda
     ((property generators)
      (prop-test-error #t property generators default-runs))
     ((property generators n)
      (prop-test-error #t property generators n))))

  (define test-property-error-type
    (case-lambda
     ((type property generators)
      (prop-test-error type property generators default-runs))
     ((type property generators n)
      (prop-test-error type property generators n))))

  (define test-property-skip
    (case-lambda
     ((property generators)
      (begin (test-skip default-runs)
             (prop-test property generators default-runs)))
     ((property generators n)
      (begin (test-skip n)
             (prop-test property generators n)))))

  (define test-property-expect-fail
    (case-lambda
     ((property generators)
      (begin (test-expect-fail default-runs)
             (prop-test property generators default-runs)))
     ((property generators n)
      (begin (test-expect-fail n)
             (prop-test property generators n)))))

  (define test-property
    (case-lambda
     ((property generators)
      (prop-test property generators default-runs))
     ((property generators n)
      (prop-test property generators n))))
  );library
