;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(library (srfi :160 test aux)
  (export fake-from-args fake-from-list
          reverse-drop-start+end
          drop-start+end
          reverse-fill-within
          fill-within
          sqrt-nearest
          char-list-segment
          list-cumulate
          ;;
          define-test-equiv
          irritants message
          define-test-property define-test-property/case
          test-property test-property-expect-fail)
  (import (only (chezscheme) with-implicit)
          (rnrs base (6))
          (only (rnrs arithmetic fixnums (6)) fx>=? fx=? fx<? fx+)
          (only (rnrs conditions (6))
                assertion-violation? who-condition? irritants-condition? message-condition?
                condition-who condition-irritants condition-message)
          (only (rnrs control (6)) case-lambda when unless)
          (only (rnrs exceptions (6)) guard)
          (only (rnrs io simple (6)) current-output-port display)
          (only (rnrs lists (6)) assq)
          (rnrs syntax-case (6))
          (only (rnrs r5rs (6)) exact->inexact)
          (srfi :28 basic-format-strings)
          (only (srfi :64 testing) test-equal test-result-alist test-runner-current)
          (only (srfi :152 strings) string-segment)
          (only (srfi :160 meta utils) compose identity thunk)
          (srfi :160 meta curried)
          (only (rename (srfi :252 property-testing)
                        (test-property test-property/unnamed)
                        (test-property-expect-fail test-property-expect-fail/unnamed))
                list-generator-of
                symbol-generator
                test-property/unnamed
                test-property-expect-fail/unnamed))

  #|
     The following two procedures are reused by various properties for the
     SRFI 160 mutators tests.
  |#
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
     [(not target-exact?)
      (map inexact lst)]
     [else
      lst]))

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

  (define (sqrt-nearest x . x*)
    "Get the nearest square root (of absolute value)"
    (call-with-values
        (thunk (exact-integer-sqrt
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

  (define-syntax define-test-equiv
    (syntax-rules ()
      [(_ bind-to to-list)
       (define bind-to
         (case-lambda
          [(expect expr)
	   (test-equal expect (to-list expr))]
	  [(name expect expr)
	   (test-equal name expect (to-list expr))]))]))

  (define-syntax (irritants stx)
    (syntax-violation #f "invalid use of auxilliary syntax" stx))

  (define-syntax (message stx)
    (syntax-violation #f "invalid use of auxilliary syntax" stx))

  (define-syntax (define-test-property stx)
    (syntax-case stx (irritants message)
      [(_ ((prop-name u0 ...) . args)
	  docstring
	  body ... body*
	  (irritants irr ...)
	  (message msg))
       (with-implicit (prop-name type-of)
                      #'(define/curried ((prop-name u0 ... type-of op-prefix op-suffix) . args)
	                  docstring
	                  (let ([op-who
		                 (string->symbol (format "~a~a~a"  op-prefix
					                 type-of op-suffix))])
	                    (guard (ex [(and (assertion-violation? ex)
			                     (who-condition? ex)
			                     (irritants-condition? ex)
			                     (message-condition? ex)
			                     (eq? (condition-who ex) op-who)
			                     msg)
			                (and (string=? (condition-message ex) msg)
			                     (equal? (condition-irritants ex) (list irr ...)))]
			               [(and (assertion-violation? ex)
			                     (who-condition? ex)
			                     (irritants-condition? ex)
			                     (eq? (condition-who ex) op-who))
			                (equal? (condition-irritants ex) (list irr ...))])
		                   body ... body*
		                   #f))))]))

  (define-syntax (define-test-property/case stx)
    (syntax-case stx (irritants message)
      [(_ (prop-name u0 ...)
	  docstring
	  (fn [args0 body ... body*]
	      ...)
          (irritants irr ...)
	  (message msg))
       (with-implicit (prop-name type-of)
                      #'(define/curried-case (prop-name u0 ... type-of op-prefix op-suffix)
	                  docstring
	                  (fn [args0
		               (let ([op-who
		                      (string->symbol (format "~a~a~a"  op-prefix
					                      type-of op-suffix))])
		                 (guard (ex [(and (assertion-violation? ex)
				                  (who-condition? ex)
				                  (irritants-condition? ex)
				                  (message-condition? ex)
				                  (eq? (condition-who ex) op-who)
				                  msg)
			                     (and (string=? (condition-message ex) msg)
				                  (equal? (condition-irritants ex) (list irr ...)))]
			                    [(and (assertion-violation? ex)
				                  (who-condition? ex)
				                  (irritants-condition? ex)
				                  (eq? (condition-who ex) op-who))
			                     (equal? (condition-irritants ex) (list irr ...))])
			                body ... body*
			                #f))]
	                      ...)))]
      [(_ (prop-name u0 ...)
	  docstring
	  (fn [args0 body ... body*]
	      ...)
          (irritants irr ...)
	  (message msg))
       (identifier? #'fn)
       (with-implicit (prop-name type-of)
                      #'(define/curried-case (prop-name u0 ... type-of op-prefix op-suffix)
	                  docstring
	                  ([args0
	                    (let ([op-who
		                   (string->symbol (format "~a~a~a"  op-prefix
					                   type-of op-suffix))])
	                      (guard (ex [(and (assertion-violation? ex)
				               (who-condition? ex)
				               (irritants-condition? ex)
				               (message-condition? ex)
				               (eq? (condition-who ex) op-who)
				               msg)
			                  (and (string=? (condition-message ex) msg)
				               (equal? (condition-irritants ex) (list irr ...)))]
			                 [(and (assertion-violation? ex)
				               (who-condition? ex)
				               (irritants-condition? ex)
				               (eq? (condition-who ex) op-who))
			                  (equal? (condition-irritants ex) (list irr ...))])
		                     body ... body*
		                     #f))]
	                   ...)))]))

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
		        (current-output-port))
               (display (format "SOURCE FILE/LINE: ~a / ~a\n"
                                (assq 'source-file resp)
                                (assq 'source-line resp))
                        (current-output-port))
               (display (format "ACTUAL ERROR:\n>>>>>>>>~a\n<<<<<<<<\n"
                                (assq 'actual-error resp))
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

  ;;;
  ); library
