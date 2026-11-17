(library (srfi :160 f32)
  (export ;; base
          f32vector? f32?
	  #;make-random-f32-generator
	  f32vector make-f32vector list->f32vector f32vector->list
	  f32vector-length f32vector-ref f32vector-set!
          ;; predicates
          f32vector-empty? f32vector=

          ;; constructors
	  f32vector-unfold f32vector-unfold-right
	  f32vector-copy f32vector-reverse-copy
	  f32vector-append f32vector-append-subvectors f32vector-concatenate

	  ;; iteration
	  f32vector-take f32vector-take-right
	  f32vector-drop f32vector-drop-right
	  f32vector-segment
	  f32vector-fold f32vector-fold-right f32vector-map f32vector-map!
	  f32vector-for-each f32vector-count f32vector-cumulate

	  ;; searching
	  f32vector-take-while f32vector-take-while-right
	  f32vector-drop-while f32vector-drop-while-right
	  f32vector-index f32vector-index-right
	  f32vector-skip f32vector-skip-right
	  f32vector-any f32vector-every
	  f32vector-partition f32vector-filter f32vector-remove

	  ;; mutators
	  f32vector-swap! f32vector-unfold! f32vector-unfold-right!
	  f32vector-fill! f32vector-reverse! f32vector-copy! f32vector-reverse-copy!

	  ;; conversion
	  reverse-f32vector->list reverse-list->f32vector
          f32vector->vector vector->f32vector

	  ;; comparators-and-generators
	  f32vector-hash
	  #;make-f32vector-comparator
	  make-f32vector-generator)

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

  (define-meta-all f32vector #t
    f32vector? f32?
    "f32vector"
    #f
    f32vector make-f32vector list->f32vector f32vector->list
    f32vector-length f32vector-ref f32vector-set!))
