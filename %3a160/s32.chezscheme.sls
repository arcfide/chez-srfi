(library (srfi :160 s32)
  (export ;; base
          s32vector? s32?
	  make-random-s32-generator
	  s32vector make-s32vector list->s32vector s32vector->list
	  s32vector-length s32vector-ref s32vector-set!
          ;; predicates
          s32vector-empty? s32vector=

          ;; constructors
	  s32vector-unfold s32vector-unfold-right
	  s32vector-copy s32vector-reverse-copy
	  s32vector-append s32vector-append-subvectors s32vector-concatenate

	  ;; iteration
	  s32vector-take s32vector-take-right
	  s32vector-drop s32vector-drop-right
	  s32vector-segment
	  s32vector-fold s32vector-fold-right s32vector-map s32vector-map!
	  s32vector-for-each s32vector-count s32vector-cumulate

	  ;; searching
	  s32vector-take-while s32vector-take-while-right
	  s32vector-drop-while s32vector-drop-while-right
	  s32vector-index s32vector-index-right
	  s32vector-skip s32vector-skip-right
	  s32vector-any s32vector-every
	  s32vector-partition s32vector-filter s32vector-remove

	  ;; mutators
	  s32vector-swap! s32vector-unfold! s32vector-unfold-right!
	  s32vector-fill! s32vector-reverse! s32vector-copy! s32vector-reverse-copy!

	  ;; conversion
	  reverse-s32vector->list reverse-list->s32vector
          s32vector->vector vector->s32vector

	  ;; comparators-and-generators
	  s32vector-hash
	  s32vector-comparator
	  make-s32vector-generator)

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
          (only (srfi :194 random-data-generators) make-random-s32-generator)
          ;;
          (srfi :160 base))

  (include "meta.chezscheme.scm")

  (define-meta-all s32vector #t
    s32vector? s32?
    "s32vector"
    make-random-s32-generator
    s32vector make-s32vector list->s32vector s32vector->list
    s32vector-length s32vector-ref s32vector-set!))
