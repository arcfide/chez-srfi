;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(library (srfi :160 meta)
  (export ;; meta
          define-meta-all

          ;; predicates
          define-numeric-vector-empty?
	  define-numeric-vector=?
          define-numeric-vector<?

          ;; constructors
	  define-numeric-vector-unfold
	  (rename (define-numeric-vector-unfold define-numeric-vector-unfold-right))
	  define-numeric-vector-copy
	  (rename (define-numeric-vector-copy define-numeric-vector-reverse-copy))
	  define-numeric-vector-append
	  define-numeric-vector-sub-append
	  define-numeric-vector-concatenate

	  ;; iteration
	  define-numeric-vector-take
	  define-numeric-vector-take-right
	  define-numeric-vector-drop
	  define-numeric-vector-drop-right
	  define-numeric-vector-segment
	  define-numeric-vector-fold
	  define-numeric-vector-fold-right
	  define-numeric-vector-map
	  define-numeric-vector-map!
	  define-numeric-vector-for-each
	  define-numeric-vector-count
	  define-numeric-vector-cumulate

	  ;; searching
	  define-numeric-vector-take-while
	  define-numeric-vector-take-while-right
	  define-numeric-vector-drop-while
	  define-numeric-vector-drop-while-right
	  define-numeric-vector-index
	  define-numeric-vector-index-right
	  define-numeric-vector-skip
	  (rename (define-numeric-vector-skip define-numeric-vector-skip-right))
	  define-numeric-vector-any
	  define-numeric-vector-every
	  define-numeric-vector-partition
	  define-numeric-vector-filter
	  define-numeric-vector-remove

	  ;; mutators
	  define-numeric-vector-swap!
	  define-numeric-vector-unfold!
	  define-numeric-vector-unfold-right!
	  define-numeric-vector-fill!
	  define-numeric-vector-reverse!
	  define-numeric-vector-copy!
	  (rename (define-numeric-vector-copy! define-numeric-vector-reverse-copy!))

	  ;; conversion
	  define-numeric-vector-reverse-to-list
	  define-numeric-vector-reverse-from-list
          define-numeric-vector<->vector

	  ;; comparators
	  define-numeric-vector-hash

	  ;; generators
	  define-numeric-vector-make-generator
	  define-make-random-numeric-list-generator
	  define-make-random-numeric-vector-generator)
  ;;
  (import (only (chezscheme) include assertion-violationf inexact->exact)
	  (rnrs base (6))
          (rnrs arithmetic fixnums (6))
          (only (rnrs control (6)) case-lambda when unless)
          (only (rnrs io simple (6)) display current-output-port)
          (only (rnrs lists (6)) fold-left)
          (rnrs syntax-case (6))
	  (srfi :28 basic-format-strings)
	  (srfi :128 comparators)
          (only (srfi :158 generators-and-accumulators) make-coroutine-generator)
          (only (srfi :235 combinators) flip)
          (srfi :160 meta curried)
          (only (srfi :160 meta utils)
                format-vector-type
                nonnegative-integer?
                compare-lengths all-same-length? total-length
	        vectorised-subscript
                compose
                magnitude>? magnitude<?))
  #|
     SRFI 160 is quite extensive, so I split the procedures up into specific
     sections, where possible.

     These are `meta' procedures which define the actual exported procedures,
     including error handling and attaching docstrings.

     Of the sections in SRFI 160:
     - The base library (procedures of SRFI 4) is defined separately.
     - The `selectors' section is just `-ref' and `-length', included in the base library.
     - The `predicates' section is a bit different.
  |#
  (include/resolve ("srfi" "%3a160" "meta") "predicates.scm")
  (include/resolve ("srfi" "%3a160" "meta") "constructors.scm")
  (include/resolve ("srfi" "%3a160" "meta") "iteration.scm")
  (include/resolve ("srfi" "%3a160" "meta") "searching.scm")
  (include/resolve ("srfi" "%3a160" "meta") "mutators.scm")
  (include/resolve ("srfi" "%3a160" "meta") "conversion.scm")
  (include/resolve ("srfi" "%3a160" "meta") "comparators.scm")
  (include/resolve ("srfi" "%3a160" "meta") "generators.scm")
  (include/resolve ("srfi" "%3a160" "meta") "output.scm")

  (define-syntax (define-meta-all stx)
    (syntax-case stx ()
      [(_ variant *real?*
	  *repr?* *elem?*
	  *type-of* *gen-elem*
	  *from-args*
	  *make*
	  *from-list*
	  *to-list*
	  *length*
	  *subscript*
	  *update!*)
       (letrec* ([make-emit-ident
                  (lambda (xs)
                    (case-lambda
                     [() (emit-ident #f #f #f #f)]
                     [(suffix) (emit-ident #f #f '- suffix)]
                     [(prefix delim-pre delim-post suffix)
                      (let-values ([(prefix delim-pre delim-post suffix)
		                    (apply values (map (lambda (sym)
					                 (if (not sym)
                                                             ""
                                                             (symbol->string sym)))
				                       (list prefix delim-pre delim-post suffix)))])
	                (datum->syntax xs
	                               (string->symbol
                                        (format "~a~a~a~a~a"
                                                prefix delim-pre (syntax->datum xs)
                                                delim-post suffix))))]))]
                 [emit-ident
                  (make-emit-ident #'variant)])
         (with-syntax
	     (#| mutators
                 NB., these need to be defined first as core operations used throughout SRFI 160 |#
	      [(swap! unfold! unfold-right! fill! reverse! copy! reverse-copy!)
	       (map emit-ident '(swap! unfold! unfold-right! fill! reverse! copy! reverse-copy!))]
	      #| constructors |#
	      [(unfold unfold-right copy reverse-copy
		append sub-append concatenate)
	       (map emit-ident '(unfold unfold-right copy reverse-copy
			    append append-subvectors concatenate))]
	      #| iteration |#
	      [(take take-right drop drop-right segment
	       fold fold-right map map! for-each count cumulate)
	       (map emit-ident '(take take-right drop drop-right segment
			    fold fold-right map map! for-each count cumulate))]
	      #| searching |#
	      [(take-while take-while-right drop-while drop-while-right
		index index-right skip skip-right any every partition filter remove)
	       (map emit-ident '(take-while take-while-right drop-while drop-while-right
			    index index-right skip skip-right any every partition filter remove))]
	      #| predicates |#
	      [(empty? =? <?)
               (list (emit-ident 'empty?) (emit-ident #f #f #f '=) (emit-ident #f #f #f '<))]
              [(comp>? comp<?)
               (if (syntax->datum #'*real?*)
                   (list #'> #'<)
                   (list #'magnitude>? #'magnitude<?))]
	      #| conversion |#
	      [(reverse-to-list reverse-from-list to-vector from-vector)
	       (list (apply emit-ident '(reverse - -> list))
                     (apply emit-ident '(reverse-list -> #f #f))
                     (apply emit-ident '(#f #f -> vector))
                     (apply emit-ident '(vector -> #f #f)))]
	      #| comparators |#
	      [(hash comparator) (map emit-ident '(hash comparator))]
	      #| generators |#
	      [make-generator
               (apply emit-ident '(make - - generator))]
              #| output |#
              [write-port
               (apply emit-ident '(write- #f #f #f))])
	   ;;
	   #'(begin
	       #| mutators |#
	       (define-numeric-vector-swap!          swap!          *subscript* *update!*)
	       (define-numeric-vector-unfold!        unfold!        *length* *update!*)
	       (define-numeric-vector-unfold-right!  unfold-right!  *length* *update!*)
	       (define-numeric-vector-fill!          fill!          unfold! *length* *elem?* *type-of*)
	       (define-numeric-vector-reverse!       reverse!       swap! *length*)
	       (define-numeric-vector-copy!          copy!          unfold! *subscript* *length*)
	       (define-numeric-vector-copy!          reverse-copy!  unfold-right! *subscript* *length*)

               #| constructors |#
	       (define-numeric-vector-unfold       unfold        unfold! *make*)
	       (define-numeric-vector-unfold       unfold-right  unfold-right! *make*)
	       (define-numeric-vector-copy         copy          copy! *make* *length*)
	       (define-numeric-vector-copy         reverse-copy  reverse-copy! *make* *length*)
	       (define-numeric-vector-append       append        copy! *make* *length*)
	       (define-numeric-vector-sub-append   sub-append    copy! *make* *length*)
	       (define-numeric-vector-concatenate  concatenate   copy! *make* *length*)

               #| iteration |#
	       (define-numeric-vector-take        take        copy *length*)
	       (define-numeric-vector-take-right  take-right  copy *length*)
	       (define-numeric-vector-drop        drop        copy *length*)
	       (define-numeric-vector-drop-right  drop-right  copy *length*)
	       (define-numeric-vector-segment     segment     copy *length*)
	       (define-numeric-vector-fold        fold        *subscript* *length*)
	       (define-numeric-vector-fold-right  fold-right  *subscript* *length*)
	       (define-numeric-vector-map!        map!        unfold! *subscript* *length*)
	       (define-numeric-vector-map         map         copy map!)
	       (define-numeric-vector-for-each    for-each    fold)
	       (define-numeric-vector-count       count       fold)
	       (define-numeric-vector-cumulate    cumulate    unfold *subscript* *length*)

               #| searching |#
	       (define-numeric-vector-index             index             *length* *subscript*)
	       (define-numeric-vector-index-right       index-right       *length* *subscript*)
	       (define-numeric-vector-skip              skip              index)
	       (define-numeric-vector-skip              skip-right        index-right)
	       (define-numeric-vector-take-while        take-while        *length* skip copy)
	       (define-numeric-vector-take-while-right  take-while-right  *length* skip-right copy)
	       (define-numeric-vector-drop-while        drop-while        *length* skip copy)
	       (define-numeric-vector-drop-while-right  drop-while-right  *length* skip-right copy)
	       (define-numeric-vector-empty?     empty?            *length*); actually a predicate
	       (define-numeric-vector-any        any               fold empty?)
	       (define-numeric-vector-every      every             fold empty?)
	       (define-numeric-vector-partition  partition         *make* *subscript* *update!* *length* count)
	       (define-numeric-vector-filter     filter            *make* *subscript* *update!* *length* count)
	       (define-numeric-vector-remove     remove            filter)

               #| predicates |#
	       (define-numeric-vector=? =?    every  *length*)
               (define-numeric-vector<? <?    comp>? comp<? *subscript* *length*)

	       #| conversion |#
	       (define-numeric-vector-reverse-to-list    reverse-to-list    fold *length* *subscript*)
	       (define-numeric-vector-reverse-from-list  reverse-from-list  *make* *update!*)
               (define-numeric-vector<->vector           to-vector          make-vector *length*     *subscript* vector-set!)
               (define-numeric-vector<->vector           from-vector        *make*      vector-length vector-ref *update!*)

	       #| comparators |#
	       (define-numeric-vector-hash  hash  *length* *subscript*)
	       (define comparator (make-comparator *repr?* =? <? hash))

	       #| generators |#
	       (define-numeric-vector-make-generator         make-generator   *subscript* *length*)

               #| output |#
               (define-write-numeric-vector write-port))))]))

  ); library

