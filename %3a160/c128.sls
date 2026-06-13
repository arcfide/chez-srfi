(library (srfi :160 c128)
  (export ;; base
          c128vector? c128?
	  #;make-random-c128-generator
	  c128vector make-c128vector list->c128vector c128vector->list
	  c128vector-length c128vector-ref c128vector-set!
          ;; predicates
          c128vector-empty? c128vector= c128vector<

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

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base))

  (define-meta-all c128vector #t
    c128vector? c128?
    "c128vector"
    make-random-c128-generator
    c128vector make-c128vector list->c128vector c128vector->list
    c128vector-length c128vector-ref c128vector-set!))
