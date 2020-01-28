;; Copyright © 2020 Göran Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

;; Tests for SRFI 5

(import
  (except (rnrs) let)
  (rnrs eval)
  (srfi :5 let)
  (srfi :64 testing))

(test-begin "let")
(test-equal (let () '())
            '())
(test-equal (let ((a 0) (b 1) (c 2))
              (list a b c))
            '(0 1 2))
(test-equal (let ((a 0) (b 1) . (c* 2 3))
              (list a b c*))
            '(0 1 (2 3)))
(test-end "let")

(test-begin "named-let")
(test-equal (let lp () '())
            '())
(test-equal (let lp ((a 0) (b 1) (c 2))
              (if (= a 10)
                  (list a b c)
                  (lp (+ a 1) (+ b 1) (+ c 1))))
            '(10 11 12))
(test-equal (let lp ((a 0) (b 1) . (c* 2 3))
              (if (= a 10)
                  (list a b c*)
                  (lp (+ a 1) (+ b 1) (+ (car c*) 1) (+ (cadr c*) 1))))
            '(10 11 (12 13)))
(test-equal (let lp ((a 0) . (x))
              (if (= a 3)
                  (list a x)
                  (apply lp (+ a 1) (cons a x))))
            '(3 (2 1 0)))

(test-end "named-let")

(test-begin "named-let-styled")
(test-equal (let (lp) '())
            '())
(test-equal (let (lp (a 0) (b 1) (c 2))
              (if (= a 10)
                  (list a b c)
                  (lp (+ a 1) (+ b 1) (+ c 1))))
            '(10 11 12))
(test-equal (let (lp (a 0) (b 1) . (c* 2 3))
              (if (= a 10)
                  (list a b c*)
                  (lp (+ a 1) (+ b 1) (+ (car c*) 1) (+ (cadr c*) 1))))
            '(10 11 (12 13)))
(test-end "named-let-styled")

;; Check if the loop variable is visible to the arguments

(test-begin "let-pitfall")
(define env (environment '(srfi :5 let)))
(test-equal (eval '(let lp (x 0) x)
                  env)
            '(0))                       ;check that eval works
(test-error #t (eval '(let lp (x lp) x)
                     env))
(test-error #t (eval '(let lp ((x 0) . (y lp)) y)
                     env))
(test-end "let-pitfall")
