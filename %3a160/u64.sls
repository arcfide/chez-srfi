(library (srfi :160 u64)
  (export ;; base
          u64vector? u64?
	  make-random-u64-generator
	  u64vector make-u64vector list->u64vector u64vector->list
	  u64vector-length u64vector-ref u64vector-set!
          ;; predicates
          u64vector-empty? u64vector= u64vector<

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

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-u64-generator))
  (define-meta-all u64vector #t
    u64vector? u64?
    "u64vector"
    make-random-u64-generator
    u64vector make-u64vector list->u64vector u64vector->list
    u64vector-length u64vector-ref u64vector-set!))
