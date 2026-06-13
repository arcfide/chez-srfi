;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define (run-mutators-tests *exact?* *real?*
          ;; `base' procedures
	  *type-of*
	  *mk-gen-relem*
	  *from-args* *from-list* *to-list* *to-revlist*
	  *length* *sub* *upd*
	  ;; mutators
	  *swap!* *unfold!* *unfoldr!* *fill!* *rev!* *copy!* *copyr!*
	  ;;constructors
	  *copy*)
  (lambda  ()
    (define (~l str) (format str *type-of*))
    (define (gen-range min max) (make-random-integer-generator min max))
    (define pgen (circular-generator values)); keep around for now, but should be hard-coded into the tests
    (define-test-equiv test-equiv-to *to-list*)
    (define mk-gen-rvec
      (case-lambda
       [()
        (gmap *from-list* (list-generator-of (*mk-gen-relem*)))]
       [(min-size)
        (gfilter (lambda (vec) (<= min-size (*length* vec)))
                 (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]
       [(min-size max-size)
        (gfilter (lambda (vec) (<= min-size (*length* vec) max-size))
                 (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]))

    (define-swap!-property          swap!-property          *swap!* *copy* *to-list*)
    (define-non-vector-property     swap!/non-vec-property  *swap!* *type-of* "" "-swap!")
    (define-swap!/oob-pos1-property swap!/oob-pos1-property *swap!* *type-of* "" "-swap!")
    (define-swap!/oob-pos2-property swap!/oob-pos2-property *swap!* *type-of* "" "-swap!")
    (define-swap!/non-pos1-property swap!/non-pos1-property *swap!* *type-of* "" "-swap!")
    (define-swap!/non-pos2-property swap!/non-pos2-property *swap!* *type-of* "" "-swap!")

    (define-unfold!-property      unfold!-prop    *exact?* *unfold!* *sub* *to-list*)
    (define-unfold!-property/2    unfold!-prop/2  *exact?* *unfold!* *sub* *to-list*)
    (define-non-vector-property/1 unfold!/non-vec-prop     *unfold!* *type-of* "" "-unfold!")
    (define-neg-start-property/1  unfold!/neg-start-prop   *unfold!* *type-of* "" "-unfold!")
    (define-neg-end-property/1    unfold!/neg-end-prop     *unfold!* *type-of* "" "-unfold!")
    (define-bounds-property/1     unfold!/bounds-prop      *unfold!* *type-of* "" "-unfold!")
    (define-overflow-property/1   unfold!/overflow-prop    *unfold!* *type-of* "" "-unfold!")

    (define-unfold!-property      unfoldr!-prop   *exact?* *unfoldr!* *sub* *to-list*)
    (define-unfold!-property/2    unfoldr!-prop/2 *exact?* *unfoldr!* *sub* *to-list*)
    (define-non-vector-property/1 unfoldr!/non-vec-prop    *unfoldr!* *type-of* "" "-unfold-right!")
    (define-neg-start-property/1  unfoldr!/neg-start-prop  *unfoldr!* *type-of* "" "-unfold-right!")
    (define-neg-end-property/1    unfoldr!/neg-end-prop    *unfoldr!* *type-of* "" "-unfold-right!")
    (define-bounds-property/1     unfoldr!/bounds-prop     *unfoldr!* *type-of* "" "-unfold-right!")
    (define-overflow-property/1   unfoldr!/overflow-prop   *unfoldr!* *type-of* "" "-unfold-right!")

    (define-fill!-property                fill!-prop  *exact?* *fill!* *to-list*)
    (define-non-vector-property           fill!/non-vec-prop   *fill!* *type-of* "" "-fill!")
    (define-repeating-non-elem-property/1 fill!/non-elem-prop  *fill!* *type-of* "" "-fill!")
    (define-fill!/neg-start-property      fill!/neg-start-prop *fill!* *type-of* "" "-fill!")
    (define-fill!/neg-end-property        fill!/neg-end-prop   *fill!* *type-of* "" "-fill!")
    (define-fill!/bounds-property         fill!/bounds-prop    *fill!* *type-of* "" "-fill!")
    (define-fill!/overflow-property       fill!/overflow-prop  *fill!* *type-of* "" "-fill!")

    (define-reverse!-property   reverse!-prop  *exact?* *rev!* *length* *sub* *to-list*)
    (define-non-vector-property reverse!/non-vec-prop   *rev!* *type-of* "" "-reverse!")
    (define-neg-start-property  reverse!/neg-start-prop *rev!* *type-of* "" "-reverse!")
    (define-neg-end-property    reverse!/neg-end-prop   *rev!* *type-of* "" "-reverse!")
    (define-bounds-property     reverse!/bounds-prop    *rev!* *type-of* "" "-reverse!")
    (define-overflow-property   reverse!/overflow-prop  *rev!* *type-of* "" "-reverse!")

    (define-copy!-property               copy!-prop      *exact?* *copy!* *length* *sub* *to-list*)
    (define-copy!/neg-tgt-start-property copy!/neg-tgt-start-prop *copy!* *type-of* "" "-copy!")
    (define-copy!/neg-src-start-property copy!/neg-src-start-prop *copy!* *type-of* "" "-copy!")
    (define-copy!/neg-src-end-property   copy!/neg-src-end-prop   *copy!* *type-of* "" "-copy!")
    (define-copy!/tgt-start-property     copy!/tgt-start-prop     *copy!* *type-of* "" "-copy!")
    (define-copy!/src-end-property       copy!/src-end-prop       *copy!* *type-of* "" "-copy!")
    (define-copy!/overruns-property      copy!/overruns-prop      *copy!* *length* *type-of* "" "-copy!")
    (define-non-vector-property          copy!/non-tgt-prop       *copy!* *type-of* "" "-copy!")
    (define-copy!/non-src-property       copy!/non-src-prop       *copy!* *type-of* "" "-copy!")

    (define-reverse-copy!-property reverse-copy!-prop   *exact?* *copyr!* *length* *sub* *to-list*)

    (define copy!-test-source (fake-from-args *from-args* *exact?* *real?* '(5 10 15 20 25)))
    (define copy!-test-target (fake-from-args *from-args* *exact?* *real?* '(0 1 2 3 4)))
    (define copy!-test-case0 (fake-from-list *exact?* *real?* '(5 10 15 20 25)))
    (define copy!-test-case1 (fake-from-list *exact?* *real?* '(0 1 5 10 15)))
    (define copy!-test-case2 (fake-from-list *exact?* *real?* '(15 20 25 3 4)))
    (define copy!-test-case3 (fake-from-list *exact?* *real?* '(0 1 15 20 25)))

    ;; BEGIN THE TESTING!
    (test-begin (~l"in-place ~a-swap!"))
    (test-property (~l"~a-swap! normative behaviour") swap!-property (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range 0 24)))
    (test-property (~l"~a-swap! bad vector") swap!/non-vec-property (list (symbol-generator) (gen-range 0 24) (gen-range 0 24)))
    (test-property (~l"~a-swap! OOB position 1") swap!/oob-pos1-property (list (mk-gen-rvec 12 24) (gen-range 24 96) (gen-range 0 12)))
    (test-property (~l"~a-swap! OOB position 2") swap!/oob-pos2-property (list (mk-gen-rvec 12 24) (gen-range 0 12) (gen-range 24 96)))
    (test-property (~l"~a-swap! bad position 1") swap!/non-pos1-property (list (mk-gen-rvec 12 24) (symbol-generator) (gen-range 0 12)))
    (test-property (~l"~a-swap! bad position 2") swap!/non-pos2-property (list (mk-gen-rvec 12 24) (gen-range 0 12) (symbol-generator)))
    (test-end)

    (test-begin (~l"in-place ~a-unfold!"))
    (test-property (~l"~a-unfold! normative behaviour")          unfold!-prop (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-unfold! normative behaviour (case 2)") unfold!-prop (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-unfold! negative start") unfold!/neg-start-prop (list pgen (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold! negative end")   unfold!/neg-end-prop   (list pgen (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold! flipped bounds") unfold!/bounds-prop    (list pgen (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold! end overflows")  unfold!/overflow-prop  (list pgen (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold! bad vector")     unfold!/non-vec-prop   (list pgen (symbol-generator) (gen-range 0 12) (gen-range 12 24) (*mk-gen-relem*)))
    (test-end)

    (test-begin (~l"in-place ~a-unfold-right!"))
    (test-property (~l"~a-unfold-right! normative behaviour")          unfoldr!-prop (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-unfold-right! normative behaviour (case 2)") unfoldr!-prop (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-unfold-right! negative start") unfoldr!/neg-start-prop (list pgen (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold-right! negative end")   unfoldr!/neg-end-prop   (list pgen (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold-right! flipped bounds") unfoldr!/bounds-prop    (list pgen (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold-right! end overflows")  unfoldr!/overflow-prop  (list pgen (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold-right! bad vector")     unfoldr!/non-vec-prop   (list pgen (symbol-generator) (gen-range 0 12) (gen-range 12 24) (*mk-gen-relem*)))
    (test-end)

    (test-begin (~l"in-place ~a-fill!"))
    (test-property (~l"~a-fill! normative behaviour") fill!-prop (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-fill! negative start") fill!/neg-start-prop (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range -2048 0) (gen-range 0 24)))
    (test-property (~l"~a-fill! negative end") fill!/neg-end-prop (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range 0 24) (gen-range -2048 0)))
    (test-property (~l"~a-fill! flipped bounds") fill!/bounds-prop (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range 12 24) (gen-range 0 12)))
    (test-property (~l"~a-fill! end overflows") fill!/overflow-prop (list (mk-gen-rvec 0 24) (*mk-gen-relem*) (gen-range 0 24) (gen-range 25 999)))
    (test-property (~l"~a-fill! bad vector") fill!/non-vec-prop (list (symbol-generator) (*mk-gen-relem*) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-fill! bad repeating element") fill!/non-elem-prop (list (mk-gen-rvec 24) (symbol-generator) (gen-range 0 12) (gen-range 12 24)))
    (test-end)

    (test-begin (~l"in-place ~a-reverse!"))
    (test-property (~l"~a-reverse! normative behaviour") reverse!-prop (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-reverse! negative start") reverse!/neg-start-prop (list (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24)))
    (test-property (~l"~a-reverse! negative end") reverse!/neg-end-prop (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0)))
    (test-property (~l"~a-reverse! flipped bounds") reverse!/bounds-prop (list (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12)))
    (test-property (~l"~a-reverse! end overflows") reverse!/overflow-prop (list (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999)))
    (test-end)

    (test-begin (~l"in-place ~a-copy!"))
    (test-property (~l"~a-copy! normative behaviour") copy!-prop (list (mk-gen-rvec 24 48) (gen-range 0 24) (mk-gen-rvec 24 48) (gen-range 0 24)))

    (test-copy!-equiv (~l"~a-copy!/same")  copy!-test-case0 test-equiv-to *copy* *copy!* copy!-test-target 0 copy!-test-source 0 5)
    (test-copy!-equiv (~l"~a-copy!/tgt-snd") copy!-test-case1 test-equiv-to *copy* *copy!* copy!-test-target 2 copy!-test-source 0 3)
    (test-copy!-equiv (~l"~a-copy!/src-snd") copy!-test-case2 test-equiv-to *copy* *copy!* copy!-test-target 0 copy!-test-source 2 5)
    (test-copy!-equiv (~l"~a-copy!/src-snd") copy!-test-case3 test-equiv-to *copy* *copy!* copy!-test-target 2 copy!-test-source 2 5)

    ;; negative target start, negative source start/end
    (test-assert (copy!/neg-tgt-start-prop  copy!-test-target -12  copy!-test-source 0 5))
    (test-assert (copy!/neg-src-start-prop  copy!-test-target 0  copy!-test-source -12 5))
    (test-assert (copy!/neg-src-end-prop  copy!-test-target 0  copy!-test-source 0 -12))
    ;; target start is explicitly out of range
    (test-assert (copy!/tgt-start-prop  copy!-test-target 8  copy!-test-source 0 5))
    ;; source end is explicitly out of range
    (test-assert (copy!/src-end-prop  copy!-test-target 2  copy!-test-source 0 8))
    ;; source end is not out of range, but slice overruns
    (test-assert (copy!/overruns-prop  copy!-test-target 2  copy!-test-source 0 5))
    ;; test source end property again
    (test-assert (copy!/src-end-prop  copy!-test-target 0  copy!-test-source 2 8))
    ;; non-numeric source source or target
    (test-assert (copy!/non-src-prop  copy!-test-target 0  'abacus 0 5))
    (test-assert (copy!/non-tgt-prop  'abacus 0  copy!-test-source 0 5))
    (test-end)

    (test-begin (~l"in-place ~a-reverse-copy!"))
    (test-property (~l"~a-reverse-copy! normative behaviour") reverse-copy!-prop (list (mk-gen-rvec 24 48) (gen-range 0 24) (mk-gen-rvec 24 48) (gen-range 0 24)))
    (test-end)))

(define run-u8vector-mutators-tests
  (run-mutators-tests #t #t
     "u8vector"
     make-random-u8-generator
     u8vector list->u8vector u8vector->list reverse-u8vector->list
     u8vector-length u8vector-ref u8vector-set!
     u8vector-swap! u8vector-unfold! u8vector-unfold-right! u8vector-fill! u8vector-reverse! u8vector-copy! u8vector-reverse-copy!
     u8vector-copy))

(define run-u16vector-mutators-tests
  (run-mutators-tests #t #t
     "u16vector"
     make-random-u16-generator
     u16vector list->u16vector u16vector->list reverse-u16vector->list
     u16vector-length u16vector-ref u16vector-set!
     u16vector-swap! u16vector-unfold! u16vector-unfold-right! u16vector-fill! u16vector-reverse! u16vector-copy! u16vector-reverse-copy!
     u16vector-copy))

(define run-u32vector-mutators-tests
  (run-mutators-tests #t #t
     "u32vector"
     make-random-u32-generator
     u32vector list->u32vector u32vector->list reverse-u32vector->list
     u32vector-length u32vector-ref u32vector-set!
     u32vector-swap! u32vector-unfold! u32vector-unfold-right! u32vector-fill! u32vector-reverse! u32vector-copy! u32vector-reverse-copy!
     u32vector-copy))

(define run-u64vector-mutators-tests
  (run-mutators-tests #t #t
     "u64vector"
     make-random-u64-generator
     u64vector list->u64vector u64vector->list reverse-u64vector->list
     u64vector-length u64vector-ref u64vector-set!
     u64vector-swap! u64vector-unfold! u64vector-unfold-right! u64vector-fill! u64vector-reverse! u64vector-copy! u64vector-reverse-copy!
     u64vector-copy))

(define run-s8vector-mutators-tests
  (run-mutators-tests #t #t
     "s8vector"
     make-random-s8-generator
     s8vector list->s8vector s8vector->list reverse-s8vector->list
     s8vector-length s8vector-ref s8vector-set!
     s8vector-swap! s8vector-unfold! s8vector-unfold-right! s8vector-fill! s8vector-reverse! s8vector-copy! s8vector-reverse-copy!
     s8vector-copy))

(define run-s16vector-mutators-tests
  (run-mutators-tests #t #t
     "s16vector"
     make-random-s16-generator
     s16vector list->s16vector s16vector->list reverse-s16vector->list
     s16vector-length s16vector-ref s16vector-set!
     s16vector-swap! s16vector-unfold! s16vector-unfold-right! s16vector-fill! s16vector-reverse! s16vector-copy! s16vector-reverse-copy!
     s16vector-copy))

(define run-s32vector-mutators-tests
  (run-mutators-tests #t #t
     "s32vector"
     make-random-s32-generator
     s32vector list->s32vector s32vector->list reverse-s32vector->list
     s32vector-length s32vector-ref s32vector-set!
     s32vector-swap! s32vector-unfold! s32vector-unfold-right! s32vector-fill! s32vector-reverse! s32vector-copy! s32vector-reverse-copy!
     s32vector-copy))

(define run-s64vector-mutators-tests
  (run-mutators-tests #t #t
     "s64vector"
     make-random-s64-generator
     s64vector list->s64vector s64vector->list reverse-s64vector->list
     s64vector-length s64vector-ref s64vector-set!
     s64vector-swap! s64vector-unfold! s64vector-unfold-right! s64vector-fill! s64vector-reverse! s64vector-copy! s64vector-reverse-copy!
     s64vector-copy))

(define run-f32vector-mutators-tests
  (run-mutators-tests #f #t
     "f32vector"
     (thunk (make-random-real-generator 0 1))
     f32vector list->f32vector f32vector->list reverse-f32vector->list
     f32vector-length f32vector-ref f32vector-set!
     f32vector-swap! f32vector-unfold! f32vector-unfold-right! f32vector-fill! f32vector-reverse! f32vector-copy! f32vector-reverse-copy!
     f32vector-copy))

(define run-f64vector-mutators-tests
  (run-mutators-tests #f #t
     "f64vector"
     (thunk (make-random-real-generator 0 1))
     f64vector list->f64vector f64vector->list reverse-f64vector->list
     f64vector-length f64vector-ref f64vector-set!
     f64vector-swap! f64vector-unfold! f64vector-unfold-right! f64vector-fill! f64vector-reverse! f64vector-copy! f64vector-reverse-copy!
     f64vector-copy))

(define run-c64vector-mutators-tests
  (run-mutators-tests #f #f
     "c64vector"
     (thunk (make-random-rectangular-generator 0 1 0 1))
     c64vector list->c64vector c64vector->list reverse-c64vector->list
     c64vector-length c64vector-ref c64vector-set!
     c64vector-swap! c64vector-unfold! c64vector-unfold-right! c64vector-fill! c64vector-reverse! c64vector-copy! c64vector-reverse-copy!
     c64vector-copy))

(define run-c128vector-mutators-tests
  (run-mutators-tests #f #f
     "c128vector"
     (thunk (make-random-rectangular-generator 0 1 0 1))
     c128vector list->c128vector c128vector->list reverse-c128vector->list
     c128vector-length c128vector-ref c128vector-set!
     c128vector-swap! c128vector-unfold! c128vector-unfold-right! c128vector-fill! c128vector-reverse! c128vector-copy! c128vector-reverse-copy!
     c128vector-copy))
