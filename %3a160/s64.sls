(library (srfi :160 s64)
  (export ;; base
          s64vector? s64?
	  make-random-s64-generator
	  s64vector make-s64vector list->s64vector s64vector->list
	  s64vector-length s64vector-ref s64vector-set!
          ;; predicates
          s64vector-empty? s64vector= s64vector<

          ;; constructors
	  s64vector-unfold s64vector-unfold-right
	  s64vector-copy s64vector-reverse-copy
	  s64vector-append s64vector-append-subvectors s64vector-concatenate

	  ;; iteration
	  s64vector-take s64vector-take-right
	  s64vector-drop s64vector-drop-right
	  s64vector-segment
	  s64vector-fold s64vector-fold-right s64vector-map s64vector-map!
	  s64vector-for-each s64vector-count s64vector-cumulate

	  ;; searching
	  s64vector-take-while s64vector-take-while-right
	  s64vector-drop-while s64vector-drop-while-right
	  s64vector-index s64vector-index-right
	  s64vector-skip s64vector-skip-right
	  s64vector-any s64vector-every
	  s64vector-partition s64vector-filter s64vector-remove

	  ;; mutators
	  s64vector-swap! s64vector-unfold! s64vector-unfold-right!
	  s64vector-fill! s64vector-reverse! s64vector-copy! s64vector-reverse-copy!

	  ;; conversion
	  reverse-s64vector->list reverse-list->s64vector
          s64vector->vector vector->s64vector

	  ;; comparators-and-generators
	  s64vector-hash
	  s64vector-comparator
	  make-s64vector-generator)

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-s64-generator))
  (define-meta-all s64vector #t
    s64vector? s64?
    "s64vector"
    make-random-s64-generator
    s64vector make-s64vector list->s64vector s64vector->list
    s64vector-length s64vector-ref s64vector-set!))
