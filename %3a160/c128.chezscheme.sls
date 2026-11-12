(library (srfi :160 c128)
  (export ;; base
          c128vector? c128?
	  #;make-random-c128-generator
	  c128vector make-c128vector list->c128vector c128vector->list
	  c128vector-length c128vector-ref c128vector-set!
          ;; predicates
          c128vector-empty? c128vector=

          ;; constructors
	  c128vector-unfold c128vector-unfold-right
	  c128vector-copy c128vector-reverse-copy
	  c128vector-append c128vector-append-subvectors c128vector-concatenate

	  ;; iteration
	  c128vector-take c128vector-take-right
	  c128vector-drop c128vector-drop-right
	  c128vector-segment
	  c128vector-fold c128vector-fold-right c128vector-map c128vector-map!
	  c128vector-for-each c128vector-count c128vector-cumulate

	  ;; searching
	  c128vector-take-while c128vector-take-while-right
	  c128vector-drop-while c128vector-drop-while-right
	  c128vector-index c128vector-index-right
	  c128vector-skip c128vector-skip-right
	  c128vector-any c128vector-every
	  c128vector-partition c128vector-filter c128vector-remove

	  ;; mutators
	  c128vector-swap! c128vector-unfold! c128vector-unfold-right!
	  c128vector-fill! c128vector-reverse! c128vector-copy! c128vector-reverse-copy!

	  ;; conversion
	  reverse-c128vector->list reverse-list->c128vector
          c128vector->vector vector->c128vector

	  ;; comparators-and-generators
	  c128vector-hash
	  c128vector-comparator
	  make-c128vector-generator)

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
          ;;
          (srfi :160 base))

  (include "meta.chezscheme.scm")

  (define-meta-all c128vector #t
    c128vector? c128?
    "c128vector"
    #f
    c128vector make-c128vector list->c128vector c128vector->list
    c128vector-length c128vector-ref c128vector-set!))
