#!r6rs
(library (srfi :160 u8)
  (export ;; base
          u8vector? u8?
	  make-random-u8-generator
	  u8vector make-u8vector list->u8vector u8vector->list
	  u8vector-length u8vector-ref u8vector-set!
          ;; predicates
          u8vector-empty? u8vector=

          ;; constructors
	  u8vector-unfold u8vector-unfold-right
	  u8vector-copy u8vector-reverse-copy
	  u8vector-append u8vector-append-subvectors u8vector-concatenate

	  ;; iteration
	  u8vector-take u8vector-take-right
	  u8vector-drop u8vector-drop-right
	  u8vector-segment
	  u8vector-fold u8vector-fold-right u8vector-map u8vector-map!
	  u8vector-for-each u8vector-count u8vector-cumulate

	  ;; searching
	  u8vector-take-while u8vector-take-while-right
	  u8vector-drop-while u8vector-drop-while-right
	  u8vector-index u8vector-index-right
	  u8vector-skip u8vector-skip-right
	  u8vector-any u8vector-every
	  u8vector-partition u8vector-filter u8vector-remove

	  ;; mutators
	  u8vector-swap! u8vector-unfold! u8vector-unfold-right!
	  u8vector-fill! u8vector-reverse! u8vector-copy! u8vector-reverse-copy!

	  ;; conversion
	  reverse-u8vector->list reverse-list->u8vector
          u8vector->vector vector->u8vector

	  ;; comparators-and-generators
	  u8vector-hash
	  u8vector-comparator
	  make-u8vector-generator)

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
          (only (srfi :194 random-data-generators) make-random-u8-generator)
          ;;
          (srfi :160 base))

  (include "meta.chezscheme.scm")

  (define-meta-all u8vector #t
    u8vector? u8?
    "u8vector"
    make-random-u8-generator
    u8vector make-u8vector list->u8vector u8vector->list
    u8vector-length u8vector-ref u8vector-set!))
