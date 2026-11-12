(library (srfi :160 s16)
  (export ;; base
          s16vector? s16?
	  make-random-s16-generator
	  s16vector make-s16vector list->s16vector s16vector->list
	  s16vector-length s16vector-ref s16vector-set!
          ;; predicates
          s16vector-empty? s16vector=

          ;; constructors
	  s16vector-unfold s16vector-unfold-right
	  s16vector-copy s16vector-reverse-copy
	  s16vector-append s16vector-append-subvectors s16vector-concatenate

	  ;; iteration
	  s16vector-take s16vector-take-right
	  s16vector-drop s16vector-drop-right
	  s16vector-segment
	  s16vector-fold s16vector-fold-right s16vector-map s16vector-map!
	  s16vector-for-each s16vector-count s16vector-cumulate

	  ;; searching
	  s16vector-take-while s16vector-take-while-right
	  s16vector-drop-while s16vector-drop-while-right
	  s16vector-index s16vector-index-right
	  s16vector-skip s16vector-skip-right
	  s16vector-any s16vector-every
	  s16vector-partition s16vector-filter s16vector-remove

	  ;; mutators
	  s16vector-swap! s16vector-unfold! s16vector-unfold-right!
	  s16vector-fill! s16vector-reverse! s16vector-copy! s16vector-reverse-copy!

	  ;; conversion
	  reverse-s16vector->list reverse-list->s16vector
          s16vector->vector vector->s16vector

	  ;; comparators-and-generators
	  s16vector-hash
	  s16vector-comparator
	  make-s16vector-generator)

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
          (only (srfi :194 random-data-generators) make-random-s16-generator)
          ;;
          (srfi private include)
          (srfi :160 base))

  (include/resolve ("srfi" "%3a160") "meta.chezscheme.scm")

  (define-meta-all s16vector #t
    s16vector? s16?
    "s16vector"
    make-random-s16-generator
    s16vector make-s16vector list->s16vector s16vector->list
    s16vector-length s16vector-ref s16vector-set!))
