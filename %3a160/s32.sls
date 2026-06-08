(library (srfi :160 s32)
  (export ;; base
          s32vector? s32?
	  make-random-s32-generator
	  s32vector make-s32vector list->s32vector s32vector->list
	  s32vector-length s32vector-ref s32vector-set!
          ;; predicates
          s32vector-empty? s32vector= s32vector<

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

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-s32-generator))
  (define-meta-all s32vector #t
    s32vector? s32?
    "s32vector"
    make-random-s32-generator
    s32vector make-s32vector list->s32vector s32vector->list
    s32vector-length s32vector-ref s32vector-set!))
