(library (srfi :160 s16)
  (export ;; base
          s16vector? s16?
	  make-random-s16-generator
	  s16vector make-s16vector list->s16vector s16vector->list
	  s16vector-length s16vector-ref s16vector-set!
          ;; predicates
          s16vector-empty? s16vector= s16vector<

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

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-s16-generator))
  (define-meta-all s16vector #t
    s16vector? s16?
    "s16vector"
    make-random-s16-generator
    s16vector make-s16vector list->s16vector s16vector->list
    s16vector-length s16vector-ref s16vector-set!))
