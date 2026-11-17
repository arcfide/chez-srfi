(library (srfi :160 f64)
  (export ;; base
          f64vector? f64?
	  #;make-random-f64-generator
	  f64vector make-f64vector list->f64vector f64vector->list
	  f64vector-length f64vector-ref f64vector-set!
          ;; predicates
          f64vector-empty? f64vector=

          ;; constructors
	  f64vector-unfold f64vector-unfold-right
	  f64vector-copy f64vector-reverse-copy
	  f64vector-append f64vector-append-subvectors f64vector-concatenate

	  ;; iteration
	  f64vector-take f64vector-take-right
	  f64vector-drop f64vector-drop-right
	  f64vector-segment
	  f64vector-fold f64vector-fold-right f64vector-map f64vector-map!
	  f64vector-for-each f64vector-count f64vector-cumulate

	  ;; searching
	  f64vector-take-while f64vector-take-while-right
	  f64vector-drop-while f64vector-drop-while-right
	  f64vector-index f64vector-index-right
	  f64vector-skip f64vector-skip-right
	  f64vector-any f64vector-every
	  f64vector-partition f64vector-filter f64vector-remove

	  ;; mutators
	  f64vector-swap! f64vector-unfold! f64vector-unfold-right!
	  f64vector-fill! f64vector-reverse! f64vector-copy! f64vector-reverse-copy!

	  ;; conversion
	  reverse-f64vector->list reverse-list->f64vector
          f64vector->vector vector->f64vector

	  ;; comparators-and-generators
	  f64vector-hash
	  #;make-f64vector-comparator
	  make-f64vector-generator)

  (import (only (chezscheme) assertion-violationf with-implicit)
          (rename (rnrs base (6))
                  (exact? r6rs:exact?)
                  (inexact? r6rs:inexact?)
                  (map list-map))
          (rnrs arithmetic fixnums (6))
          (only (rnrs conditions (6))
                who-condition? condition-who
                irritants-condition? condition-irritants
                message-condition? condition-message)
          (only (rnrs control (6)) case-lambda when unless)
          (only (rnrs exceptions (6)) guard raise raise-continuable)
          (only (rnrs io simple (6)) display current-output-port)
          (only (rnrs lists (6)) fold-left memq)
          (only (rnrs r5rs (6)) exact->inexact inexact->exact)
          (rnrs syntax-case (6))
          (only (srfi :1 lists) iota)
	  (srfi :28 basic-format-strings)
	  (srfi :128 comparators)
          (only (srfi :158 generators-and-accumulators) make-coroutine-generator)
          ;;
          (srfi private include)
          (srfi :160 base))

  (include/resolve ("srfi" "%3a160") "meta.chezscheme.scm")

  (define-meta-all f64vector #t
    f64vector? f64?
    "f64vector"
    #f
    f64vector make-f64vector list->f64vector f64vector->list
    f64vector-length f64vector-ref f64vector-set!))
