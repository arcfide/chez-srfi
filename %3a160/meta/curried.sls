;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(library (srfi :160 meta curried)
  (export raised-with? reraise
          define/curried define/curried-case define/who&
          assert/who
          ;;
          assert-start-nat assert-end-nat
          assert-start<=end assert-bounds
          assert-index-nat assert-index-bounds
          sub-append-triple)
  (import (only (chezscheme) assertion-violationf errorf with-implicit)
          (rnrs base (6))
          (only (rnrs conditions (6))
                who-condition? condition-who
                irritants-condition? condition-irritants
                message-condition? condition-message)
          (only (rnrs control (6)) case-lambda when unless)
          (only (rnrs exceptions (6)) guard raise raise-continuable)
          (only (rnrs lists (6)) memq)
          (rnrs syntax-case (6))
	  (srfi :28 basic-format-strings)
          (only (srfi :160 meta utils) nonnegative-integer?))

  (define raised-with?
    (case-lambda
     [(ex who)
      (and (who-condition? ex)
           (eq? (condition-who ex) who))]
     [(ex who . who*)
      (and (who-condition? ex)
           (eq? (condition-who ex) (cons who who*)))]))

  #|
     Reraise error message / irritants with new `who'. The argument order is
     flipped to be more like the order of error/assertion-violation.

     By default, we raise an assertion violation with a formatted error
     message. There is no `else'.
  |#
  (define reraise
    (case-lambda
     [(who ex)
      (reraise who ex assertion-violationf)]
     [(who ex reraise-with)
      (cond [(and (irritants-condition? ex)
                  (message-condition? ex))
	     (apply reraise-with
                    who
		    (condition-message ex)
                    (condition-irritants ex))]
	    [(message-condition? ex)
             (reraise-with
              who
	      (condition-message ex))])]))

   (define-syntax (define/who& stx)
      (syntax-case stx ()
        [(k (f . u) body0 ... body* (reraise ident ...))
         (identifier? #'f)
         (with-implicit (k who)
           #'(define f
               (let ([who 'f])
                 (lambda u
                   (guard
                    (ex [(and (who-condition? ex)
                              (memq (condition-who ex) (list (quote ident) ...)))
                         (cond [(and (irritants-condition? ex)
                                     (message-condition? ex))
                                (apply assertion-violationf
                                       who
                                       (condition-message ex)
                                       (condition-irritants ex))]
                               [(message-condition? ex)
                                (assertion-violationf
                                       who
                                       (condition-message ex))]
                               [else
                                (raise-continuable ex)])]
                        [else
                         (raise-continuable ex)])
                      body0 ... body*)))))]
        [_
         (syntax-violation 'define/who& "invalid syntax" stx)]))

   (define-syntax (guarded-body stx)
     (syntax-case stx ()
       [(_ who (argU ...) body0 ... body*)
        #'(guard
           (ex [(and (who-condition? ex)
                     (memq (condition-who ex) (list (quote argU) ...)))
                (cond [(and (irritants-condition? ex) (message-condition? ex))
                       (apply assertion-violationf who (condition-message ex)
                              (condition-irritants ex))]
                      [(message-condition? ex)
                       (assertion-violationf who (condition-message ex))]
                      [else
                       (raise-continuable ex)])]
               [else
                (raise-continuable ex)])
           body0 ... body*)]))

   (define-syntax (define/curried stx)
     (syntax-case stx ()
       [(k ((generator argU ...) . argsL) body0 ... body*)
        (identifier? #'generator)
        #'(define-syntax (generator stx)
            (syntax-case stx ()
              [(_ bind-to argU ...)
               (with-implicit (k who)
                 #'(define bind-to
                     (let ([who 'bind-to])
                       (lambda argsL
                         (guarded-body who (argU ...) body0 ... body*)))))]
              [(_ bind-to p (... ...))
               (syntax-violation (quote bind-to) "incorrect argument count in call" stx)]
              [_
               (syntax-violation (quote bind-to) "invalid syntax" stx)]))]
       [_
        (syntax-violation 'define/curried "invalid syntax" stx)]))

   (define-syntax (define/curried-case stx)
     (syntax-case stx ()
       [(k (generator argU ...)
           docstring
           (fn [args0 body0 ... body*]
               ...
               [argsN bodyN ... bodyN*]))
        (and (identifier? #'generator) (identifier? #'fn))
        #'(define-syntax (generator stx)
            (syntax-case stx ()
              [(_ bind-to argU ...)
               (with-implicit (k who)
                 #'(define bind-to
                     (let ([who 'bind-to])
                       (letrec ([fn
                                 (case-lambda
                                  [args0 (guarded-body who (argU ...) body0 ... body*)]
                                  ...
                                  [argsN (guarded-body who (argU ...) bodyN ... bodyN*)])])
                         (lambda argsL
                           (apply fn argsL))))))]
              [(_ bind-to p (... ...))
               (syntax-violation (quote bind-to) "incorrect argument count in call" stx)]
              [_
               (syntax-violation (quote bind-to) "invalid syntax" stx)]))]
       [(_ (generator argU ...)
           docstring
           ([args0 body0 ... body*]
            ...
            [argsN bodyN ... bodyN*]))
        (identifier? #'generator)
        #'(define-syntax (generator stx)
            (syntax-case stx ()
              [(_ bind-to argU ...)
               (with-implicit (k who)
                 #'(define bind-to
                     (let ([who 'bind-to])
                       (case-lambda
                        [args0 (guarded-body who (argU ...) body0 ... body*)]
                        ...
                        [argsN (guarded-body who (argU ...) bodyN ... bodyN*)]))))]
              [(_ bind-to p (... ...))
               (syntax-violation (quote bind-to) "incorrect argument count in call" stx)]
              [_
               (syntax-violation (quote bind-to) "invalid syntax" stx)]))]
       [(_ (generator argU ...)
           (fn [args0 body0 ... body*] ...
               [argsN bodyN ... bodyN*]))
        (and (identifier? #'generator) (identifier? #'fn))
        #'(define/curried-case (generator argU ...)
            #f
            (fn [args0 body0 ... body*] ...  [argsN bodyN ... bodyN*]))]
       [(_ (generator argU ...)
           ([args0 body0 ... body*] ...
            [argsN bodyN ... bodyN*]))
        (identifier? #'generator)
        #'(define/curried-case (generator argU ...)
            #f
            ([args0 body0 ... body*] ... [argsN bodyN ... bodyN*]))]
       [_
        (syntax-violation 'define/curried "invalid syntax" stx)]))

   (define-syntax (assert/who stx)
    (syntax-case stx ()
      [(_ who expression message)
       #'(unless expression
	   (assertion-violationf who message))]
      [(_ who expression message irritant ...)
       #'(unless expression
           (assertion-violationf who message
                                 irritant ...))]))

   (define assert-start-nat
     (case-lambda
      [(who start)
       (assert/who who
                   (nonnegative-integer? start) "start ~a is not a non-negative integer"
                   start)]
      [(who start prefix)
       (assert/who who
                   (nonnegative-integer? start)
                   (format "~a start ~~a is not a non-negative integer" prefix) start)]))

   (define assert-end-nat
     (case-lambda
      [(who end)
       (assert/who who (nonnegative-integer? end) "end ~a is not a non-negative integer" end)]
      [(who end prefix)
       (assert/who who
                   (nonnegative-integer? end)
                   (format "~a end ~~a is not a non-negative integer" prefix) end)]))

   (define assert-start<=end
     (case-lambda
      [(who start end)
       (assert/who who (<= start end) "end ~a must be greater than or equal to start ~a" end start)]
      [(who start end anno)
       (assert/who who
                   (<= start end)
                   (format "~a end ~~a must be greater than or equal to ~a start ~~a" anno anno)
                   end start)]))

   (define (assert-bounds who end width v)
     (assert/who who (<= end width) "end ~a overflows ~a"
                 end v))

   (define assert-index-nat
     (case-lambda
      [(who k)
       (assert/who who
                   (nonnegative-integer? k)
                   "index ~a is not a non-negative integer"
                   k)]
      [(who k prefix)
       (assert/who who
                   (nonnegative-integer? k)
                   (format "~a index ~~a is not a non-negative integer" prefix) k)]))

   (define assert-index-bounds
     (lambda (who k size v)
       (assert/who who (>= size k) "index ~a overflows ~a"
                   k v)))

   (define (sub-append-triple who lst)
     (if (and (list? lst) (= (length lst) 3))
         (apply values lst)
         (assert/who who #f
                     "expected triple of vector, start and end"
                     #;lst)))

   ;;;
   ); library
