#!r6rs

;; Extension to SRFI 214 that allows controlling some internal behaviors by
;; overriding parameters.

(library (srfi :214 parameters)
  (export flexvector-min-capacity
          flexvector-capacity-estimator)
  (import (rnrs)
          (srfi :39))

  ;; Check for capacity parameter.
  (define (assert-valid-capacity-value value)
    (assert (and (number? value) (> value 0)))
    value)

  ;; Parameter controlling minimal flexvector capacity.
  (define flexvector-min-capacity
    (make-parameter 4 assert-valid-capacity-value))

  ;; Check for capacity estimator.
  ;; Note that check by default calls the value to verify that
  ;; it takes correct number of arguments and returns a number, so
  ;; no side effects are expected.
  (define (assert-valid-capacity-estimator value)
    (assert (procedure? value))
    (let* ([example-current 4]
           [example-requested 8]
           [example-cap (value example-current example-requested)])
      (assert (and (number? example-cap)
                   (exact? example-cap)
                   (>= example-cap example-requested)))
      value))

  ;; Returns capacity that is returns multiple of current capacity enough to cover
  ;; requested one.
  (define (multiple-of-current-estimator current requested)
    (assert (and (number? current) (number? requested)
                 (> current 0) (> requested 0)))
    (let ([quot (exact (ceiling (/ requested current)))])
      (* quot current)))

  ;; Parameter whose value is a function, that, given current and required capacity
  ;; returns the actual capacity to use for the flexvector. Basically, estimate
  ;; how much to extend the vector to avoid reallocation in the future.
  (define flexvector-capacity-estimator
    (make-parameter multiple-of-current-estimator assert-valid-capacity-estimator)))
