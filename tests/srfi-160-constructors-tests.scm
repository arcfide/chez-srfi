;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define (run-constructors-tests exact?
	  *type-of*
	  *mk-gen-relem*
	  *from-list* *to-list* *to-revlist*
	  *length*
	  ;;
	  *unfold* *unfoldr* *copy* *copyr*)
  (lambda ()
    (define (~l str) (format str *type-of*))
    (define gen-range make-random-integer-generator)
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

    (define-unfold-property unfold-prop exact? *unfold* *to-list*)
    (define-unfold/count-up-property unfold/count-up-prop exact? *unfold* *to-list*)
    (define-unfold/count-down-property unfold/count-down-prop exact? *unfold* *to-list*)
    (define-persistent-neg-length-property unfold/neg-length-prop *unfold* *type-of* "" "-unfold")
    (define-persistent-non-length-property unfold/non-length-prop *unfold* *type-of* "" "-unfold")

    (define-unfold-property unfoldr-prop exact? *unfoldr* *to-list*)
    (define-unfold/count-up-property unfoldr/count-up-prop exact? *unfoldr* *to-revlist*)
    (define-unfold/count-down-property unfoldr/count-down-prop exact? *unfoldr* *to-revlist*)
    (define-persistent-neg-length-property unfoldr/neg-length-prop *unfoldr* *type-of* "" "-unfold-right")
    (define-persistent-non-length-property unfoldr/non-length-prop *unfoldr* *type-of* "" "-unfold-right")

    (define-copy-property copy-prop *copy* drop-start+end *to-list*)
    (define-non-vector-property copy/non-vec-prop *copy* *type-of* "" "-copy")
    (define-neg-start-property copy/neg-start-prop *copy* *type-of* "" "-copy")
    (define-neg-end-property copy/neg-end-prop *copy* *type-of* "" "-copy")
    (define-bounds-property copy/bounds-prop *copy* *type-of* "" "-copy")
    (define-overflow-property copy/overflow-prop *copy* *type-of* "" "-copy")

    (define-copy-property copyr-prop *copyr* reverse-drop-start+end *to-list*)
    (define-non-vector-property copyr/non-vec-prop *copyr* *type-of* "" "-reverse-copy")
    (define-neg-start-property copyr/neg-start-prop *copyr* *type-of* "" "-reverse-copy")
    (define-neg-end-property copyr/neg-end-prop *copyr* *type-of* "" "-reverse-copy")
    (define-bounds-property copyr/bounds-prop *copyr* *type-of* "" "-reverse-copy")
    (define-overflow-property copyr/overflow-prop *copyr* *type-of* "" "-reverse-copy")

    (test-begin (~l "persistent ~a-unfold"))
    (test-property (~l"~a-unfold normative behaviour") unfold-prop (list (gen-range 0 120)))
    (test-property (~l"~a-unfold counting up behaviour") unfold/count-up-prop (list (gen-range 0 64) (gen-range 0 64)))
    (test-property (~l"~a-unfold counting down behaviour") unfold/count-down-prop (list (gen-range 0 64) (gen-range 64 120)))
    (test-property (~l"~a-unfold negative size") unfold/neg-length-prop (list (mk-gen-rvec 24) (gen-range -2048 0) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold non-numeric size") unfold/neg-length-prop (list (mk-gen-rvec 24) (gen-range -2048 0) (symbol-generator)))
    (test-end)

    (test-begin (~l"persistent ~a-unfold-right"))
    (test-property (~l"~a-unfold-right normative behaviour") unfoldr-prop (list (gen-range 0 120)))
    (test-property (~l"~a-unfold-right counting up behaviour") unfoldr/count-up-prop (list (gen-range 0 64) (gen-range 0 64)))
    (test-property (~l"~a-unfold-right counting down behaviour") unfoldr/count-down-prop (list (gen-range 0 64) (gen-range 64 120)))
    (test-property (~l"~a-unfold-right negative size") unfoldr/neg-length-prop (list (mk-gen-rvec 24) (gen-range -2048 0) (*mk-gen-relem*)))
    (test-property (~l"~a-unfold-right non-numeric size") unfoldr/neg-length-prop (list (mk-gen-rvec 24) (gen-range -2048 0) (symbol-generator)))
    (test-end)

    (test-begin (~l"persistent ~a-copy"))
    (test-property (~l"~a-copy normative behaviour") copy-prop (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-copy negative start") copy/neg-start-prop (list (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24)))
    (test-property (~l"~a-copy negative end") copy/neg-end-prop (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0)))
    (test-property (~l"~a-copy flipped bounds") copy/bounds-prop (list (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12)))
    (test-property (~l"~a-copy end overflows") copy/overflow-prop (list (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999)))
    (test-property (~l"~a-copy non-vector") copy/non-vec-prop (list (symbol-generator) (gen-range 0 12) (gen-range 12 24)))
    (test-end)

    (test-begin (~l"persistent ~a-reverse-copy"))
    (test-property (~l"~a-reverse-copy normative behaviour") copyr-prop (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
    (test-property (~l"~a-reverse-copy negative start") copyr/neg-start-prop (list (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24)))
    (test-property (~l"~a-reverse-copy negative end") copyr/neg-end-prop (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0)))
    (test-property (~l"~a-reverse-copy flipped bounds") copyr/bounds-prop (list (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12)))
    (test-property (~l"~a-reverse-copy end overflows") copyr/overflow-prop (list (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999)))
    (test-property (~l"~a-reverse-copy non-vector") copyr/non-vec-prop (list (symbol-generator) (gen-range 0 12) (gen-range 12 24)))
    (test-end)))

(define run-u8vector-constructors-tests
  (run-constructors-tests #t
     "u8vector"
     make-random-u8-generator
     list->u8vector u8vector->list reverse-u8vector->list
     u8vector-length
     u8vector-unfold u8vector-unfold-right u8vector-copy u8vector-reverse-copy))

(define run-u16vector-constructors-tests
  (run-constructors-tests #t
     "u16vector"
     make-random-u16-generator
     list->u16vector u16vector->list reverse-u16vector->list
     u16vector-length
     u16vector-unfold u16vector-unfold-right u16vector-copy u16vector-reverse-copy))

(define run-u32vector-constructors-tests
  (run-constructors-tests #t
     "u32vector"
     make-random-u32-generator
     list->u32vector u32vector->list reverse-u32vector->list
     u32vector-length
     u32vector-unfold u32vector-unfold-right u32vector-copy u32vector-reverse-copy))

(define run-u64vector-constructors-tests
  (run-constructors-tests #t
     "u64vector"
     make-random-u64-generator
     list->u64vector u64vector->list reverse-u64vector->list
     u64vector-length
     u64vector-unfold u64vector-unfold-right u64vector-copy u64vector-reverse-copy))

(define run-s8vector-constructors-tests
  (run-constructors-tests #t
     "s8vector"
     make-random-s8-generator
     list->s8vector s8vector->list reverse-s8vector->list
     s8vector-length
     s8vector-unfold s8vector-unfold-right s8vector-copy s8vector-reverse-copy))

(define run-s16vector-constructors-tests
  (run-constructors-tests #t
     "s16vector"
     make-random-s16-generator
     list->s16vector s16vector->list reverse-s16vector->list
     s16vector-length
     s16vector-unfold s16vector-unfold-right s16vector-copy s16vector-reverse-copy))

(define run-s32vector-constructors-tests
  (run-constructors-tests #t
     "s32vector"
     make-random-s32-generator
     list->s32vector s32vector->list reverse-s32vector->list
     s32vector-length
     s32vector-unfold s32vector-unfold-right s32vector-copy s32vector-reverse-copy))

(define run-s64vector-constructors-tests
  (run-constructors-tests #t
     "s64vector"
     make-random-s64-generator
     list->s64vector s64vector->list reverse-s64vector->list
     s64vector-length
     s64vector-unfold s64vector-unfold-right s64vector-copy s64vector-reverse-copy))

(define run-f32vector-constructors-tests
  (run-constructors-tests #f
     "f32vector"
     (thunk (make-random-real-generator 0 1))
     list->f32vector f32vector->list reverse-f32vector->list
     f32vector-length
     f32vector-unfold f32vector-unfold-right f32vector-copy f32vector-reverse-copy))

(define run-f64vector-constructors-tests
  (run-constructors-tests #f
     "f64vector"
     (thunk (make-random-real-generator 0 1))
     list->f64vector f64vector->list reverse-f64vector->list
     f64vector-length
     f64vector-unfold f64vector-unfold-right f64vector-copy f64vector-reverse-copy))

(define run-c64vector-constructors-tests
  (run-constructors-tests #f
     "c64vector"
     (thunk (make-random-rectangular-generator 0 1 0 1))
     list->c64vector c64vector->list reverse-c64vector->list
     c64vector-length
     c64vector-unfold c64vector-unfold-right c64vector-copy c64vector-reverse-copy))

(define run-c128vector-constructors-tests
  (run-constructors-tests #f
     "c128vector"
     (thunk (make-random-rectangular-generator 0 1 0 1))
     list->c128vector c128vector->list reverse-c128vector->list
     c128vector-length
     c128vector-unfold c128vector-unfold-right c128vector-copy c128vector-reverse-copy))
