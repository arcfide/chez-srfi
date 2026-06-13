(library (srfi :160 s8)
  (export ;; base
          s8vector? s8?
	  make-random-s8-generator
	  s8vector make-s8vector list->s8vector s8vector->list
	  s8vector-length s8vector-ref s8vector-set!
          ;; predicates
          s8vector-empty? s8vector= s8vector<

          ;; constructors
	  s8vector-unfold s8vector-unfold-right
	  s8vector-copy s8vector-reverse-copy
	  s8vector-append s8vector-append-subvectors s8vector-concatenate

	  ;; iteration
	  s8vector-take s8vector-take-right
	  s8vector-drop s8vector-drop-right
	  s8vector-segment
	  s8vector-fold s8vector-fold-right s8vector-map s8vector-map!
	  s8vector-for-each s8vector-count s8vector-cumulate

	  ;; searching
	  s8vector-take-while s8vector-take-while-right
	  s8vector-drop-while s8vector-drop-while-right
	  s8vector-index s8vector-index-right
	  s8vector-skip s8vector-skip-right
	  s8vector-any s8vector-every
	  s8vector-partition s8vector-filter s8vector-remove

	  ;; mutators
	  s8vector-swap! s8vector-unfold! s8vector-unfold-right!
	  s8vector-fill! s8vector-reverse! s8vector-copy! s8vector-reverse-copy!

	  ;; conversion
	  reverse-s8vector->list reverse-list->s8vector
          s8vector->vector vector->s8vector

	  ;; comparators-and-generators
	  s8vector-hash
	  s8vector-comparator
	  make-s8vector-generator)

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-s8-generator))
  (define-meta-all s8vector #t
    s8vector? s8?
    "s8vector"
    make-random-s8-generator
    s8vector make-s8vector list->s8vector s8vector->list
    s8vector-length s8vector-ref s8vector-set!))
