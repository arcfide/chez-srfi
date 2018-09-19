;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: (MIT OR BSD-3-Clause OR LicenseRef-LICENSE)
#!r6rs

(import
  (rnrs)
  (srfi :14 char-sets inversion-list)
  (srfi :14 char-sets)
  (srfi :48 intermediate-format-strings)
  (srfi private include))

;; Compatibility for Scheme 48 test suites

(define-syntax define-test-suite
  (syntax-rules ()
    ((_ suite)
     (define dummy 'suite))))

(define-syntax define-test-case
  (syntax-rules ()
    ((_ test-case suite checks ...)
     (begin checks ...))))

(define (is-true) (lambda (x) x))

(define (is-false) not)

(define-syntax check
  (syntax-rules (=>)
    ((_ expr)
     (check-that expr (is-true)))
    ((_ expr => expect)
     (check-that expr (lambda (x) (equal? x expect))))
    ((_ expr (=> equal?) expect)
     (check-that expr (lambda (x) (equal? x expect))))))

(define-syntax check-that
  (syntax-rules ()
    ((_ expr ok?)
     (let ((v expr))
       (format #t "~s~%=>~%~s~%" 'expr v)
       (cond ((ok? v)
              (format #t ";; correct~%~%"))
             (else
              (format #t ";; *** failed ***~%test: ~s~%~%" 'ok?)))))))

(define is
  (case-lambda
    ((= x) (lambda (y) (= x y)))
    ((x) (if (procedure? x) x (lambda (y) (equal? x y))))))

(define (opposite f) (lambda (x) (not (f x))))

(define (all-of . fs)
  (lambda (x) (for-all (lambda (f) (f x)) fs)))

(define x->char-set ->char-set)
(define char->scalar-value char->integer)
(define scalar-value->char integer->char)

(include/resolve ("srfi" "%3a14" "char-sets") "inversion-list-check.scm")
(include/resolve ("srfi" "%3a14") "srfi-14-check.scm")
