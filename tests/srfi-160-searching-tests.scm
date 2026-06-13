;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define (list-index-right pred? lst)
  (let ([res (list-index pred? (reverse lst))])
    (and res
	 (- (length lst) res 1))))

(define pgen/odd+even (circular-generator odd? even?))
(define pgen/every (circular-generator exact?))

(define (run-searching-tests *exact?* *real?*
	 *type-of*
         *mk-gen-relem*
	 *from-list* *to-list* *to-revlist*
	 *len*
	 ;;
	 *take*
	 ;;
	 *takew* *takewr* *dropw* *dropwr*
	 *index* *indexr* *skip* *skipr*
	 *any* *every*
	 *part* *filter* *remove*)
  (lambda ()
    (define (~l str) (format str *type-of*))
    (define mk-gen-rvec
      (case-lambda
       [()
        (gmap *from-list* (list-generator-of (*mk-gen-relem*)))]
       [(min-size)
        (gfilter (lambda (vec) (<= min-size (*len* vec)))
                 (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]
       [(min-size max-size)
        (gfilter (lambda (vec) (<= min-size (*len* vec) max-size))
                 (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]))

    (define (x>=0.5? x) (>= x 0.5))
    (define x<0.5? (compose not x>=0.5?))
    (define (magn>0.5? x) (> (magnitude x) 0.5))
    (define (magn<0.5? x) (< (magnitude x) 0.5))

    (define pgen/sg
      (cond [*exact?*
             (circular-generator odd? even?)]
            [*real?*
             (circular-generator x>=0.5? x<0.5?)]
            [else
             (circular-generator magn>0.5? magn<0.5?)]))

    (define pgen/every
      (if *exact?*
          (circular-generator r6rs:exact?)
          (circular-generator r6rs:inexact?)))

    (define pgen/multi-every
      (if *exact?*
          (circular-generator (lambda (x y) (and (r6rs:exact? x) (r6rs:exact? y) (+ x y))))
          (circular-generator (lambda (x y) (and (r6rs:inexact? x) (r6rs:inexact? y) (+ x y))))))

    (define pgen/multi-any
      (cond [*exact?*
             (circular-generator (lambda (x y) (and (even? x) (even? y) (+ x y))))]
            [*real?*
             (circular-generator (lambda (x y) (and (x>=0.5? x) (x>=0.5? y) (+ x y))))]
            [else
             (circular-generator (lambda (x y) (and (magn>0.5? x) (magn>0.5? y) (+ x y))))]))

    (define-list-predicate-property take-while-prop *takew* take-while *to-list*)
    (define-map-non-vector-property take-while/non-vec-prop *takew* *type-of* "" "-take-while")

    (define-list-predicate-property drop-while-prop *dropw* drop-while *to-list*)
    (define-map-non-vector-property drop-while/non-vec-prop *dropw* *type-of* "" "-drop-while")

    (define-list-predicate-property take-while-right-prop *takewr* take-while *to-revlist*)
    (define-map-non-vector-property take-while-right/non-vec-prop *takewr* *type-of* "" "-take-while-right")

    (define-list-predicate-property drop-while-right-prop *dropwr* drop-while *to-revlist*)
    (define-map-non-vector-property drop-while-right/non-vec-prop *dropwr* *type-of* "" "-drop-while-right")

    (define-list-index-property index-prop *index* list-index *len* *take* *to-list*)
    (define-map-non-vector-property index/non-vec-prop *index* *type-of* "" "-index")

    (define-list-index-property index-right-prop *indexr* list-index-right *len* *take* *to-list*)
    (define-map-non-vector-property index-right/non-vec-prop *indexr* *type-of* "" "-index-right")

    (define-skip-property skip-prop *skip* list-index *len* *take* *to-list*)
    (define-map-non-vector-property skip/non-vec-prop *skip* *type-of* "" "-skip")
    (define-fold-non-vector-property/1 skip/non-vec-prop/2 *skip* *type-of* "" "-skip")

    (define-skip-property skip-right-prop *skipr* list-index-right *len* *take* *to-list*)
    (define-map-non-vector-property skip-right/non-vec-prop *skipr* *type-of* "" "-skip-right")
    (define-fold-non-vector-property/1 skip-right/non-vec-prop/2 *skipr* *type-of* "" "-skip-right")

    (define-any-property any-prop *any* *len* *take* *to-list*)
    (define-map-non-vector-property any/non-vec-prop *any* *type-of* "" "-any")
    (define-fold-non-vector-property/1 any/non-vec-prop/2 *any* *type-of* "" "-any")

    (define-every-property every-prop *every* *len* *take* *to-list*)
    (define-map-non-vector-property every/non-vec-prop *every* *type-of* "" "-every")
    (define-fold-non-vector-property/1 every/non-vec-prop/2 *every* *type-of* "" "-every")

    (define-partition-property partition-prop *part* *to-list*)
    (define-map-non-vector-property partition/non-vec-prop *part* *type-of* "" "-partition")

    (define-list-predicate-property filter-prop *filter* filter *to-list*)
    (define-map-non-vector-property filter/non-vec-prop *filter* *type-of* "" "-filter")

    (define-list-predicate-property remove-prop *remove* remove/? *to-list*)
    (define-map-non-vector-property remove/non-vec-prop *remove* *type-of* "" "-remove")

    ;; BEGIN THE TESTING!
    (test-begin (~l"~a-take-while"))
    (test-property (~l"~a-take-while normative behaviour") take-while-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-take-while non-vector") take-while/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-drop-while"))
    (test-property (~l"~a-drop-while normative behaviour") drop-while-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-drop-while non-vector") drop-while/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-take-while-right"))
    (test-property (~l"~a-take-while-right normative behaviour") take-while-right-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-take-while-right non-vector") take-while-right/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-drop-while-right"))
    (test-property (~l"~a-drop-while-right normative behaviour") drop-while-right-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-drop-while-right non-vector") drop-while-right/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-index"))
    (test-property (~l"~a-index normative behaviour") index-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-index normative behaviour (multi-valued)") index-prop (list pgen/multi-any (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-index non-vector") index/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-index-right"))
    (test-property (~l"~a-index-right normative behaviour") index-right-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-index-right non-vector") index-right/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-skip"))
    (test-property (~l"~a-skip normative behaviour") skip-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-skip normative behaviour (multi-valued)") skip-prop (list pgen/multi-any (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-skip non-vector") skip/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-property (~l"~a-skip non-vector (non-initial position)") skip/non-vec-prop/2 (list pgen/sg (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-skip-right"))
    (test-property (~l"~a-skip-right normative behaviour") skip-right-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-skip-right non-vector") skip-right/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-property (~l"~a-skip-right non-vector (non-initial position)") skip-right/non-vec-prop/2 (list pgen/sg (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-any"))
    (test-property (~l"~a-any normative behaviour (singleton case)") any-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-any normative behaviour (multi case)") any-prop (list pgen/multi-any (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-any non-vector") any/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-property (~l"~a-any non-vector (non-initial position)") any/non-vec-prop/2 (list pgen/sg (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-every"))
    (test-property (~l"~a-every normative behaviour (singleton case)") every-prop (list pgen/every (mk-gen-rvec 24)))
    (test-property (~l"~a-every normative behaviour (multi case)") every-prop (list pgen/multi-every (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-every non-vector") every/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-property (~l"~a-every non-vector (non-initial position)") every/non-vec-prop/2 (list pgen/sg (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-partition"))
    (test-property (~l"~a-partition normative behaviour") partition-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-partition non-vector") partition/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-filter"))
    (test-property (~l"~a-filter normative behaviour") filter-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-filter non-vector") filter/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-remove"))
    (test-property (~l"~a-remove normative behaviour") remove-prop (list pgen/sg (mk-gen-rvec 24)))
    (test-property (~l"~a-remove non-vector") remove/non-vec-prop (list pgen/sg (symbol-generator)))
    (test-end)))


(define run-u8vector-searching-tests
  (run-searching-tests #t #t
     "u8vector"
     make-random-u8-generator
     list->u8vector u8vector->list reverse-u8vector->list
     u8vector-length
     u8vector-take
     u8vector-take-while u8vector-take-while-right u8vector-drop-while u8vector-drop-while-right
     u8vector-index u8vector-index-right u8vector-skip u8vector-skip-right
     u8vector-any u8vector-every
     u8vector-partition u8vector-filter u8vector-remove))

(define run-u16vector-searching-tests
  (run-searching-tests #t #t
     "u16vector"
     make-random-u16-generator
     list->u16vector u16vector->list reverse-u16vector->list
     u16vector-length
     u16vector-take
     u16vector-take-while u16vector-take-while-right u16vector-drop-while u16vector-drop-while-right
     u16vector-index u16vector-index-right u16vector-skip u16vector-skip-right
     u16vector-any u16vector-every
     u16vector-partition u16vector-filter u16vector-remove))

(define run-u32vector-searching-tests
  (run-searching-tests #t #t
     "u32vector"
     make-random-u32-generator
     list->u32vector u32vector->list reverse-u32vector->list
     u32vector-length
     u32vector-take
     u32vector-take-while u32vector-take-while-right u32vector-drop-while u32vector-drop-while-right
     u32vector-index u32vector-index-right u32vector-skip u32vector-skip-right
     u32vector-any u32vector-every
     u32vector-partition u32vector-filter u32vector-remove))

(define run-u64vector-searching-tests
  (run-searching-tests #t #t
     "u64vector"
     make-random-u64-generator
     list->u64vector u64vector->list reverse-u64vector->list
     u64vector-length
     u64vector-take
     u64vector-take-while u64vector-take-while-right u64vector-drop-while u64vector-drop-while-right
     u64vector-index u64vector-index-right u64vector-skip u64vector-skip-right
     u64vector-any u64vector-every
     u64vector-partition u64vector-filter u64vector-remove))

(define run-s8vector-searching-tests
  (run-searching-tests #t #t
     "s8vector"
     make-random-s8-generator
     list->s8vector s8vector->list reverse-s8vector->list
     s8vector-length
     s8vector-take
     s8vector-take-while s8vector-take-while-right s8vector-drop-while s8vector-drop-while-right
     s8vector-index s8vector-index-right s8vector-skip s8vector-skip-right
     s8vector-any s8vector-every
     s8vector-partition s8vector-filter s8vector-remove))

(define run-s16vector-searching-tests
  (run-searching-tests #t #t
     "s16vector"
     make-random-s16-generator
     list->s16vector s16vector->list reverse-s16vector->list
     s16vector-length
     s16vector-take
     s16vector-take-while s16vector-take-while-right s16vector-drop-while s16vector-drop-while-right
     s16vector-index s16vector-index-right s16vector-skip s16vector-skip-right
     s16vector-any s16vector-every
     s16vector-partition s16vector-filter s16vector-remove))

(define run-s32vector-searching-tests
  (run-searching-tests #t #t
     "s32vector"
     make-random-s32-generator
     list->s32vector s32vector->list reverse-s32vector->list
     s32vector-length
     s32vector-take
     s32vector-take-while s32vector-take-while-right s32vector-drop-while s32vector-drop-while-right
     s32vector-index s32vector-index-right s32vector-skip s32vector-skip-right
     s32vector-any s32vector-every
     s32vector-partition s32vector-filter s32vector-remove))

(define run-s64vector-searching-tests
  (run-searching-tests #t #t
     "s64vector"
     make-random-s64-generator
     list->s64vector s64vector->list reverse-s64vector->list
     s64vector-length
     s64vector-take
     s64vector-take-while s64vector-take-while-right s64vector-drop-while s64vector-drop-while-right
     s64vector-index s64vector-index-right s64vector-skip s64vector-skip-right
     s64vector-any s64vector-every
     s64vector-partition s64vector-filter s64vector-remove))

(define run-f32vector-searching-tests
  (run-searching-tests #f #t
     "f32vector"
     (thunk (make-random-real-generator 0 1))
     list->f32vector f32vector->list reverse-f32vector->list
     f32vector-length
     f32vector-take
     f32vector-take-while f32vector-take-while-right f32vector-drop-while f32vector-drop-while-right
     f32vector-index f32vector-index-right f32vector-skip f32vector-skip-right
     f32vector-any f32vector-every
     f32vector-partition f32vector-filter f32vector-remove))

(define run-f64vector-searching-tests
  (run-searching-tests #f #t
     "f64vector"
     (thunk (make-random-real-generator 0 1))
     list->f64vector f64vector->list reverse-f64vector->list
     f64vector-length
     f64vector-take
     f64vector-take-while f64vector-take-while-right f64vector-drop-while f64vector-drop-while-right
     f64vector-index f64vector-index-right f64vector-skip f64vector-skip-right
     f64vector-any f64vector-every
     f64vector-partition f64vector-filter f64vector-remove))

(define run-c64vector-searching-tests
  (run-searching-tests #f #f
     "c64vector"
     (thunk (make-random-rectangular-generator 0 1 0 1))
     list->c64vector c64vector->list reverse-c64vector->list
     c64vector-length
     c64vector-take
     c64vector-take-while c64vector-take-while-right c64vector-drop-while c64vector-drop-while-right
     c64vector-index c64vector-index-right c64vector-skip c64vector-skip-right
     c64vector-any c64vector-every
     c64vector-partition c64vector-filter c64vector-remove))

(define run-c128vector-searching-tests
  (run-searching-tests #f #f
     "c128vector"
     (thunk (make-random-rectangular-generator 0 1 0 1))
     list->c128vector c128vector->list reverse-c128vector->list
     c128vector-length
     c128vector-take
     c128vector-take-while c128vector-take-while-right c128vector-drop-while c128vector-drop-while-right
     c128vector-index c128vector-index-right c128vector-skip c128vector-skip-right
     c128vector-any c128vector-every
     c128vector-partition c128vector-filter c128vector-remove))
