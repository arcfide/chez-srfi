;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried-case (define-numeric-vector-make-generator sub len)
  "Wrap SRFI 160 `make-@vector-generator' procedures"
  (recur [(v) (recur v 0 (len v))]
         [(v start) (recur v start (len v))]
         [(v start end)
          (assert-start-nat who start)
          (assert-end-nat who end)
          (assert/who who
                      (< start end)
                      "source end (~a) must be greater than or equal to source start (~a), both less than length"
                      end start)
          (make-coroutine-generator
           (lambda (yield)
	     (let loop ([k start])
               (when (< k end)
                 (yield (sub v k))
                 (loop (+ k 1))))))]))

(define/curried-case (define-make-random-numeric-list-generator mk-genval)
  "Produce a maker for a random list generator, given random cell generator like
`(make-random-s64-generator).'
Note that this is a *maker*, since it's also taking a maker like those provided in SRFI 194.
This is also distinct from the `make-@vector-generator' type procedures defined by SRFI 160,
since it generates lists of values *which are themselves* random."
  ([() (list-generator-of (mk-genval))]
   [(min-size)
    (gfilter (lambda (lst) (<= min-size (length lst)))
	     (list-generator-of (mk-genval)))]
   [(min-size max-size)
    #| The trick is that the list generator uses inclusive min,
    but exclusive max. So it is incorrect to compare it to
    1 more than the max-size with `<='. |#
    (assert/who who
                (<= min-size max-size)
                "maximum ~a must be greater than or equal to minimum ~a"
                max-size min-size)
    (gfilter (lambda (lst) (<= min-size (length lst) max-size))
	     (list-generator-of (mk-genval)))]))

(define/curried-case (define-make-random-numeric-vector-generator mk-genl from-list)
  "Produce a random numeric vector generator based on a cell generator and from-list constructor"
  ([() (gmap from-list (mk-genl))]
   [(min-size) (gmap from-list (mk-genl min-size))]
   [(min-size max-size) (gmap from-list (mk-genl min-size max-size))]))
