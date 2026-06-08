(library (srfi :160 u32)
  (export ;; base
          u32vector? u32?
	  make-random-u32-generator
	  u32vector make-u32vector list->u32vector u32vector->list
	  u32vector-length u32vector-ref u32vector-set!
          ;; predicates
          u32vector-empty? u32vector= u32vector<

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

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-u32-generator))
  (define-meta-all u32vector #t
    u32vector? u32?
    "u32vector"
    make-random-u32-generator
    u32vector make-u32vector list->u32vector u32vector->list
    u32vector-length u32vector-ref u32vector-set!))
