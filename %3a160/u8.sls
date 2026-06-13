(library (srfi :160 u8)
  (export ;; base
          u8vector? u8?
	  make-random-u8-generator
	  u8vector make-u8vector list->u8vector u8vector->list
	  u8vector-length u8vector-ref u8vector-set!
          ;; predicates
          u8vector-empty? u8vector= u8vector<

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

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base)
	  (only (srfi :194 random-data-generators) make-random-u8-generator))
  (define-meta-all u8vector #t
    u8vector? u8?
    "u8vector"
    make-random-u8-generator
    u8vector make-u8vector list->u8vector u8vector->list
    u8vector-length u8vector-ref u8vector-set!))
