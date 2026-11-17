;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define-syntax (run-constructors-tests stx)
  (syntax-case stx ()
    [(_ *exact?*
	*type-of*
	*mk-gen-relem*
	*from-list* *to-list* *to-reverse-list*
	*length*
	;;
	*unfold* *unfoldr* *copy* *reverse-copy*)
     #'(begin
         (define (~l str) (format str *type-of*))
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

         (define (make-unfold-property ufold)
           (lambda (size)
             (if *exact?*
                 (list= = (iota size) (*to-list* (ufold values size 0)))
                 (list= = (map exact->inexact (iota size))
                          (*to-list* (ufold (lambda (k _) (values (exact->inexact k) #f))
                                            size #f))))))
         (define unfold-property (make-unfold-property *unfold*))
         (define unfoldr-property (make-unfold-property *unfoldr*))

         (define (make-count-up-property ufold ->list)
           (lambda (size ini)
             (if *exact?*
                 (list= fx=? (make-range ini (fx+ ini size))
                             (->list (ufold (lambda (_k x) (values x (fx+ x 1))) size ini)))
                 (list= = (map exact->inexact (make-range ini (+ ini size)))
                          (->list (ufold (lambda (_k x) (values (exact->inexact x) (exact->inexact (+ x 1)))) size ini))))))
         (define (make-count-down-property ufold ->list)
           (lambda (size ini)
             (let ([down-from (+ 1 ini)]
	           [down-to (+ 1 (fx- ini size))])
               (if *exact?*
	           (list= fx=? (reverse (make-range down-to down-from))
                               (->list (ufold (lambda (_k x) (values x (fx- x 1))) size ini)))
                   (list= = (map exact->inexact (reverse (make-range down-to down-from)))
                            (->list (ufold (lambda (_k x) (values (exact->inexact x) (exact->inexact (- x 1)))) size ini)))))))

         (define unfold/count-up-property (make-count-up-property *unfold* *to-list*))
         (define unfold/count-down-property (make-count-down-property *unfold* *to-list*))
         (define-persistent-neg-length-property unfold/neg-length-property *unfold*)

         (define unfoldr/count-up-property (make-count-up-property *unfoldr* *to-reverse-list*))
         (define unfoldr/count-down-property (make-count-down-property *unfoldr* *to-reverse-list*))
         (define-persistent-neg-length-property unfoldr/neg-length-property *unfoldr*)

         (define (make-copy-property copy-proc drop-prop)
           (lambda (v start end)
             (list= = (drop-prop (*to-list* v) start end)
                      (*to-list* (copy-proc v start end)))))
         (define copy-property (make-copy-property *copy* drop-start+end))
         (define reverse-copy-property (make-copy-property *reverse-copy* reverse-drop-start+end))

         (define-non-vector-property copy/non-vec-property   *copy* *type-of*)
         (define-neg-start-property  copy/neg-start-property *copy*)
         (define-neg-end-property    copy/neg-end-property   *copy*)
         (define-bounds-property     copy/bounds-property    *copy*)
         (define-overflow-property   copy/overflow-property  *copy*)

         (define-non-vector-property copyr/non-vec-property   *reverse-copy* *type-of*)
         (define-neg-start-property  copyr/neg-start-property *reverse-copy*)
         (define-neg-end-property    copyr/neg-end-property   *reverse-copy*)
         (define-bounds-property     copyr/bounds-property    *reverse-copy*)
         (define-overflow-property   copyr/overflow-property  *reverse-copy*)

         (test-begin (~l "persistent ~a-unfold"))
         (test-property (~l"~a-unfold normative behaviour") unfold-property (list (gen-range 0 120)))
         (test-property (~l"~a-unfold counting up behaviour") unfold/count-up-property (list (gen-range 0 64) (gen-range 0 64)))
         (test-property (~l"~a-unfold counting down behaviour") unfold/count-down-property (list (gen-range 0 64) (gen-range 64 120)))
         (test-property (~l"~a-unfold negative size") unfold/neg-length-property (list (mk-gen-rvec 24) (gen-range -2048 0) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold non-numeric size") unfold/neg-length-property (list (mk-gen-rvec 24) (gen-range -2048 0) (symbol-generator)))
         (test-end (~l "persistent ~a-unfold"))

         (test-begin (~l"persistent ~a-unfold-right"))
         (test-property (~l"~a-unfold-right normative behaviour") unfoldr-property (list (gen-range 0 120)))
         (test-property (~l"~a-unfold-right counting up behaviour") unfoldr/count-up-property (list (gen-range 0 64) (gen-range 0 64)))
         (test-property (~l"~a-unfold-right counting down behaviour") unfoldr/count-down-property (list (gen-range 0 64) (gen-range 64 120)))
         (test-property (~l"~a-unfold-right negative size") unfoldr/neg-length-property (list (mk-gen-rvec 24) (gen-range -2048 0) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold-right non-numeric size") unfoldr/neg-length-property (list (mk-gen-rvec 24) (gen-range -2048 0) (symbol-generator)))
         (test-end (~l"persistent ~a-unfold-right"))

         (test-begin (~l"persistent ~a-copy"))
         (test-property (~l"~a-copy normative behaviour") copy-property (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-copy negative start") copy/neg-start-property (list (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24)))
         (test-property (~l"~a-copy negative end") copy/neg-end-property (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0)))
         (test-property (~l"~a-copy flipped bounds") copy/bounds-property (list (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12)))
         (test-property (~l"~a-copy end overflows") copy/overflow-property (list (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999)))
         (test-property (~l"~a-copy non-vector") copy/non-vec-property (list (symbol-generator) (gen-range 0 12) (gen-range 12 24)))
         (test-end (~l"persistent ~a-copy"))

         (test-begin (~l"persistent ~a-reverse-copy"))
         (test-property (~l"~a-reverse-copy normative behaviour") reverse-copy-property (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-reverse-copy negative start") copyr/neg-start-property (list (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24)))
         (test-property (~l"~a-reverse-copy negative end") copyr/neg-end-property (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0)))
         (test-property (~l"~a-reverse-copy flipped bounds") copyr/bounds-property (list (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12)))
         (test-property (~l"~a-reverse-copy end overflows") copyr/overflow-property (list (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999)))
         (test-property (~l"~a-reverse-copy non-vector") copyr/non-vec-property (list (symbol-generator) (gen-range 0 12) (gen-range 12 24)))
         (test-end (~l"persistent ~a-reverse-copy"))
         )]))

(run-constructors-tests #t
  "u8vector"
  make-random-u8-generator
  list->u8vector u8vector->list reverse-u8vector->list
  u8vector-length
  u8vector-unfold u8vector-unfold-right u8vector-copy u8vector-reverse-copy)

(run-constructors-tests #t
  "u16vector"
  make-random-u16-generator
  list->u16vector u16vector->list reverse-u16vector->list
  u16vector-length
  u16vector-unfold u16vector-unfold-right u16vector-copy u16vector-reverse-copy)

(run-constructors-tests #t
  "u32vector"
  make-random-u32-generator
  list->u32vector u32vector->list reverse-u32vector->list
  u32vector-length
  u32vector-unfold u32vector-unfold-right u32vector-copy u32vector-reverse-copy)

(run-constructors-tests #t
  "u64vector"
  make-random-u64-generator
  list->u64vector u64vector->list reverse-u64vector->list
  u64vector-length
  u64vector-unfold u64vector-unfold-right u64vector-copy u64vector-reverse-copy)

(run-constructors-tests #t
  "s8vector"
  make-random-s8-generator
  list->s8vector s8vector->list reverse-s8vector->list
  s8vector-length
  s8vector-unfold s8vector-unfold-right s8vector-copy s8vector-reverse-copy)

(run-constructors-tests #t
  "s16vector"
  make-random-s16-generator
  list->s16vector s16vector->list reverse-s16vector->list
  s16vector-length
  s16vector-unfold s16vector-unfold-right s16vector-copy s16vector-reverse-copy)

(run-constructors-tests #t
  "s32vector"
  make-random-s32-generator
  list->s32vector s32vector->list reverse-s32vector->list
  s32vector-length
  s32vector-unfold s32vector-unfold-right s32vector-copy s32vector-reverse-copy)

(run-constructors-tests #t
  "s64vector"
  make-random-s64-generator
  list->s64vector s64vector->list reverse-s64vector->list
  s64vector-length
  s64vector-unfold s64vector-unfold-right s64vector-copy s64vector-reverse-copy)

(run-constructors-tests #f
  "f32vector"
  (lambda () (make-random-real-generator 0 1))
  list->f32vector f32vector->list reverse-f32vector->list
  f32vector-length
  f32vector-unfold f32vector-unfold-right f32vector-copy f32vector-reverse-copy)

(run-constructors-tests #f
  "f64vector"
  (lambda () (make-random-real-generator 0 1))
  list->f64vector f64vector->list reverse-f64vector->list
  f64vector-length
  f64vector-unfold f64vector-unfold-right f64vector-copy f64vector-reverse-copy)

(run-constructors-tests #f
  "c64vector"
  (lambda () (make-random-rectangular-generator 0 1 0 1))
  list->c64vector c64vector->list reverse-c64vector->list
  c64vector-length
  c64vector-unfold c64vector-unfold-right c64vector-copy c64vector-reverse-copy)

(run-constructors-tests #f
  "c128vector"
  (lambda () (make-random-rectangular-generator 0 1 0 1))
  list->c128vector c128vector->list reverse-c128vector->list
  c128vector-length
  c128vector-unfold c128vector-unfold-right c128vector-copy c128vector-reverse-copy)
