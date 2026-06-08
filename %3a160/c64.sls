(library (srfi :160 c64)
  (export ;; base
          c64vector? c64?
	  #;make-random-c64-generator
	  c64vector make-c64vector list->c64vector c64vector->list
	  c64vector-length c64vector-ref c64vector-set!
          ;; predicates
          c64vector-empty? c64vector= c64vector<

          ;; constructors
	  c64vector-unfold c64vector-unfold-right
	  c64vector-copy c64vector-reverse-copy
	  c64vector-append c64vector-append-subvectors c64vector-concatenate

	  ;; iteration
	  c64vector-take c64vector-take-right
	  c64vector-drop c64vector-drop-right
	  c64vector-segment
	  c64vector-fold c64vector-fold-right c64vector-map c64vector-map!
	  c64vector-for-each c64vector-count c64vector-cumulate

	  ;; searching
	  c64vector-take-while c64vector-take-while-right
	  c64vector-drop-while c64vector-drop-while-right
	  c64vector-index c64vector-index-right
	  c64vector-skip c64vector-skip-right
	  c64vector-any c64vector-every
	  c64vector-partition c64vector-filter c64vector-remove

	  ;; mutators
	  c64vector-swap! c64vector-unfold! c64vector-unfold-right!
	  c64vector-fill! c64vector-reverse! c64vector-copy! c64vector-reverse-copy!

	  ;; conversion
	  reverse-c64vector->list reverse-list->c64vector
          c64vector->vector vector->c64vector

	  ;; comparators-and-generators
	  c64vector-hash
	  c64vector-comparator
	  make-c64vector-generator)

  (import (rnrs base (6))
          (srfi :160 meta)
	  (srfi :160 base))

  (define-meta-all c64vector #t
    c64vector? c64?
    "c64vector"
    make-random-c64-generator
    c64vector make-c64vector list->c64vector c64vector->list
    c64vector-length c64vector-ref c64vector-set!))
