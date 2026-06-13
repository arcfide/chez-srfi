;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(library (srfi :160 meta utils)
  (export format-vector-type
          ;;
          compare-lengths all-same-length? total-length
	  vectorised-subscript
          ;;
          positive-integer?
          negative-integer?
          nonnegative-integer?
          exact?
          inexact?
          exact-integer?
          inexact-integer?
          magnitude>? magnitude<?
          ;;
          identity compose thunk make-range)
  (import (rename (rnrs base (6))
                  (exact? r6rs:exact?)
                  (inexact? r6rs:inexact?))
          (rnrs syntax-case (6))
          (only (rnrs lists (6)) memq)
          (only (srfi :1 lists) iota)
          (srfi :28 basic-format-strings))

  (define like-vowels
    (string->list "faeiohlmnrstvwxyzFAEIOHLMNRSTVWXYZ"))

  (define (one-of str)
    (if (and (> (string-length str) 0)
             (memq (string-ref str 0) like-vowels))
        "an"
        "a"))

  (define (format-vector-type fmt-str type-of)
    #| We're actually formatting this twice. |#
    (let ([template (format "~a ~a" (one-of type-of) type-of)])
      (format fmt-str template)))

  (define (compare-lengths len comp v . vs)
    "Apply length and run a comparison like min/max, and return the result or #f"
    (let loop ([src vs] [prev (len v)])
      (if (null? src)
	  prev
	  (let ([res (comp prev (len (car src)))])
	    (and res
		 (loop (cdr src) res))))))

  (define (all-same-length? len v . vs)
    (apply compare-lengths
	   len
	   (lambda (prev curr)
	     (and (= prev curr)
		  curr))
	   v vs))

  (define (total-length len vs)
    (apply compare-lengths len + vs))

  (define (vectorised-subscript sub vs i)
    (map (lambda (v) (sub v i))
	 vs))

  (define (positive-integer? x)
    (and (integer? x)
         (positive? x)))

  (define (negative-integer? x)
    (and (integer? x)
         (negative? x)))

  (define (nonnegative-integer? x)
    (and (integer? x)
         (not (negative? x))))

  (define (exact? x)
    (and (number? x) (r6rs:exact? x)))

  (define (inexact? x)
    (and (number? x) (r6rs:inexact? x)))

  (define (exact-integer? x)
    (and (integer? x)
         (r6rs:exact? x)))

  (define (inexact-integer? x)
    (and (integer? x)
         (r6rs:inexact? x)))

  (define (magnitude>? x y)
    (> (magnitude x) (magnitude y)))

  (define (magnitude<? x y)
    (< (magnitude x) (magnitude y)))

  (define (compose f . rest)
    (if (null? rest)
	f
	(let ([g (apply compose rest)])
	  (lambda args
	    (call-with-values (lambda () (apply g args)) f)))))

  (define identity values)

  (define-syntax thunk
    (lambda (stx)
      (syntax-case stx ()
	[(_) #'(lambda () (values))]
	[(_ body0 body ...) #'(lambda () body0 body ...)])))

  (define (make-range A B)
    (if (or (not (integer? A)) (not (integer? B)) (< B A))
	(error 'make-range "range must be from A to B where A <= B")
	(iota (- B A) A)))

  ;;;
  ); define library
