(library (srfi :160 u32)
  (export ;; base
          u32vector? u32?
	  make-random-u32-generator
	  u32vector make-u32vector list->u32vector u32vector->list
	  u32vector-length u32vector-ref u32vector-set!
          ;; predicates
          u32vector-empty? u32vector=

          ;; constructors
	  u32vector-unfold u32vector-unfold-right
	  u32vector-copy u32vector-reverse-copy
	  u32vector-append u32vector-append-subvectors u32vector-concatenate

	  ;; iteration
	  u32vector-take u32vector-take-right
	  u32vector-drop u32vector-drop-right
	  u32vector-segment
	  u32vector-fold u32vector-fold-right u32vector-map u32vector-map!
	  u32vector-for-each u32vector-count u32vector-cumulate

	  ;; searching
	  u32vector-take-while u32vector-take-while-right
	  u32vector-drop-while u32vector-drop-while-right
	  u32vector-index u32vector-index-right
	  u32vector-skip u32vector-skip-right
	  u32vector-any u32vector-every
	  u32vector-partition u32vector-filter u32vector-remove

	  ;; mutators
	  u32vector-swap! u32vector-unfold! u32vector-unfold-right!
	  u32vector-fill! u32vector-reverse! u32vector-copy! u32vector-reverse-copy!

	  ;; conversion
	  reverse-u32vector->list reverse-list->u32vector
          u32vector->vector vector->u32vector

	  ;; comparators-and-generators
	  u32vector-hash
	  u32vector-comparator
	  make-u32vector-generator)

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
          (only (srfi :194 random-data-generators) make-random-u32-generator)
          ;;
          (srfi private include)
          (srfi :160 base))

  (include/resolve ("srfi" "%3a160") "meta.chezscheme.scm")

  (define-meta-all u32vector #t
    u32vector? u32?
    "u32vector"
    make-random-u32-generator
    u32vector make-u32vector list->u32vector u32vector->list
    u32vector-length u32vector-ref u32vector-set!))
