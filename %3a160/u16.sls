(library (srfi :160 u16)
  (export ;; base
          u16vector? u16?
	  make-random-u16-generator
	  u16vector make-u16vector list->u16vector u16vector->list
	  u16vector-length u16vector-ref u16vector-set!
          ;; predicates
          u16vector-empty? u16vector= u16vector<

          ;; constructors
	  u16vector-unfold u16vector-unfold-right
	  u16vector-copy u16vector-reverse-copy
	  u16vector-append u16vector-append-subvectors u16vector-concatenate

	  ;; iteration
	  u16vector-take u16vector-take-right
	  u16vector-drop u16vector-drop-right
	  u16vector-segment
	  u16vector-fold u16vector-fold-right u16vector-map u16vector-map!
	  u16vector-for-each u16vector-count u16vector-cumulate

	  ;; searching
	  u16vector-take-while u16vector-take-while-right
	  u16vector-drop-while u16vector-drop-while-right
	  u16vector-index u16vector-index-right
	  u16vector-skip u16vector-skip-right
	  u16vector-any u16vector-every
	  u16vector-partition u16vector-filter u16vector-remove

	  ;; mutators
	  u16vector-swap! u16vector-unfold! u16vector-unfold-right!
	  u16vector-fill! u16vector-reverse! u16vector-copy! u16vector-reverse-copy!

	  ;; conversion
	  reverse-u16vector->list reverse-list->u16vector
          u16vector->vector vector->u16vector

	  ;; comparators-and-generators
	  u16vector-hash
	  u16vector-comparator
	  make-u16vector-generator)

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-u16-generator))
  (define-meta-all u16vector #t
    u16vector? u16?
    "u16vector"
    make-random-u16-generator
    u16vector make-u16vector list->u16vector u16vector->list
    u16vector-length u16vector-ref u16vector-set!))
