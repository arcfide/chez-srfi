#!r6rs

;; This is an R6RS adaptation of the R7RS implementation by the SRFI-214 author,
;; taken from [here](https://github.com/scheme-requests-for-implementation/srfi-214/tree/master/implementation)
(library (srfi :214)
  (export
          ;; Constructors
          make-flexvector flexvector
          flexvector-unfold flexvector-unfold-right
          flexvector-copy flexvector-reverse-copy
          flexvector-append flexvector-concatenate flexvector-append-subvectors

          ;; Predicates
          flexvector? flexvector-empty? flexvector=?

          ;; Selectors
          flexvector-ref flexvector-front flexvector-back flexvector-length

          ;; Mutators
          flexvector-add! flexvector-add-front! flexvector-add-back!
          flexvector-remove! flexvector-remove-front! flexvector-remove-back!
          flexvector-add-all! flexvector-remove-range! flexvector-clear!
          flexvector-set! flexvector-swap!
          flexvector-fill! flexvector-reverse!
          flexvector-copy! flexvector-reverse-copy!
          flexvector-append!

          ;; Iteration
          flexvector-fold flexvector-fold-right
          flexvector-map flexvector-map! flexvector-map/index flexvector-map/index!
          flexvector-append-map flexvector-append-map/index
          flexvector-filter flexvector-filter! flexvector-filter/index flexvector-filter/index!
          flexvector-for-each flexvector-for-each/index
          flexvector-count flexvector-cumulate

          ;; Searching
          flexvector-index flexvector-index-right
          flexvector-skip flexvector-skip-right
          flexvector-binary-search
          flexvector-any flexvector-every flexvector-partition

          ;; Conversion
          flexvector->vector flexvector->list flexvector->string
          vector->flexvector list->flexvector string->flexvector
          reverse-flexvector->list reverse-list->flexvector
          generator->flexvector flexvector->generator)
  (import (srfi :214 impl)))
