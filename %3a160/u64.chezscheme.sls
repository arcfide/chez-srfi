(library (srfi :160 u64)
  (export ;; base
          u64vector? u64?
	  make-random-u64-generator
	  u64vector make-u64vector list->u64vector u64vector->list
	  u64vector-length u64vector-ref u64vector-set!
          ;; predicates
          u64vector-empty? u64vector=

          ;; constructors
	  u64vector-unfold u64vector-unfold-right
	  u64vector-copy u64vector-reverse-copy
	  u64vector-append u64vector-append-subvectors u64vector-concatenate

	  ;; iteration
	  u64vector-take u64vector-take-right
	  u64vector-drop u64vector-drop-right
	  u64vector-segment
	  u64vector-fold u64vector-fold-right u64vector-map u64vector-map!
	  u64vector-for-each u64vector-count u64vector-cumulate

	  ;; searching
	  u64vector-take-while u64vector-take-while-right
	  u64vector-drop-while u64vector-drop-while-right
	  u64vector-index u64vector-index-right
	  u64vector-skip u64vector-skip-right
	  u64vector-any u64vector-every
	  u64vector-partition u64vector-filter u64vector-remove

	  ;; mutators
	  u64vector-swap! u64vector-unfold! u64vector-unfold-right!
	  u64vector-fill! u64vector-reverse! u64vector-copy! u64vector-reverse-copy!

	  ;; conversion
	  reverse-u64vector->list reverse-list->u64vector
          u64vector->vector vector->u64vector

	  ;; comparators-and-generators
	  u64vector-hash
	  u64vector-comparator
	  make-u64vector-generator)

  (import (only (chezscheme) include assertion-violationf with-implicit)
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
          (only (srfi :194 random-data-generators) make-random-u64-generator)
          ;;
          (srfi :160 base))

  (include "meta.chezscheme.scm")

  (define-meta-all u64vector #t
    u64vector? u64?
    "u64vector"
    make-random-u64-generator
    u64vector make-u64vector list->u64vector u64vector->list
    u64vector-length u64vector-ref u64vector-set!))
