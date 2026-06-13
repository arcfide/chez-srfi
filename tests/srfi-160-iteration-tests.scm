;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define (run-iteration-tests *exact?* *real?*
          *type-of* %max%
          *mk-gen-relem*
	  *from-list* *to-list* *to-revlist*
	  *len*
	  *take* *taker* *drop* *dropr*
	  *seg* *fold* *foldr*
	  *map!* *map* *for-each*
	  *count* *cumul*)
  (lambda ()
    (define (~l str) (format str *type-of*))
    (define pgen (circular-generator values))
    (define nulgen (circular-generator '()))
    (define gen-range make-random-integer-generator)
    (define gen-char
      (let* ([points
              (if *exact?*
                  (iota (min %max% 55295))
                  (iota 55295))]
	     [code-points (map integer->char points)]
	     [language (list->string code-points)])
	(make-random-char-generator language)))
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

    (define-take/drop-property take-prop *take* take *to-list*)
    (define-non-vector-property take/non-vec-prop *take* *type-of* "" "-take")
    (define-iter-neg-index-property take/neg-index-prop *take* *type-of* "" "-take")
    (define-iter-overflow-property take/overflow-prop *take* *type-of* "" "-take")

    (define-take/drop-property taker-prop *taker* take *to-revlist*)
    (define-non-vector-property taker/non-vec-prop *taker* *type-of* "" "-take-right")
    (define-iter-neg-index-property taker/neg-index-prop *taker* *type-of* "" "-take-right")
    (define-iter-overflow-property taker/overflow-prop *taker* *type-of* "" "-take-right")

    (define-take/drop-property drop-prop *drop* drop *to-list*)
    (define-non-vector-property drop/non-vec-prop *drop* *type-of* "" "-drop")
    (define-iter-neg-index-property drop/neg-index-prop *drop* *type-of* "" "-drop")
    (define-iter-overflow-property drop/overflow-prop *drop* *type-of* "" "-drop")

    (define-take/drop-property dropr-prop *dropr* drop *to-revlist*)
    (define-non-vector-property dropr/non-vec-prop *dropr* *type-of* "" "-drop-right")
    (define-iter-neg-index-property dropr/neg-index-prop *dropr* *type-of* "" "-drop-right")
    (define-iter-overflow-property dropr/overflow-prop *dropr* *type-of* "" "-drop-right")

    (define-segment-property segment-prop *exact?* *seg* *from-list* *to-list*)
    (define-non-vector-property segment/non-vec-prop *seg* *type-of* "" "-segment")
    (define-segment-index-property segment/neg-index-prop *seg* *type-of* "" "-segment")

    (define-fold-property fold-prop *fold* *len* *take* (flip cons*) *to-list*)
    (define-fold-non-vector-property/1 fold/non-vec-prop *fold* *type-of* "" "-fold")
    (define-fold-non-vector-property/2 fold/non-vec-prop/2 *fold* *type-of* "" "-fold")

    (define-fold-property foldr-prop *foldr* *len* *take* (flip cons*) *to-revlist*)
    (define-fold-non-vector-property/1 foldr/non-vec-prop *foldr* *type-of* "" "-fold-right")
    (define-fold-non-vector-property/2 foldr/non-vec-prop/2 *foldr* *type-of* "" "-fold-right")

    (define-map!-property map!-prop *exact?* *map!* *len* *take* *to-list*)
    (define-map-non-vector-property map!/non-vec-prop *map!* *type-of* "" "-map!")
    (define-fold-non-vector-property/1 map!/non-vec-prop/2 *map!* *type-of* "" "-map!")

    (define-map-property map-prop *exact?* *map* *len* *take* *to-list*)
    (define-map-non-vector-property map/non-vec-prop *map* *type-of* "" "-map")
    (define-fold-non-vector-property/1 map/non-vec-prop/2 *map* *type-of* "" "-map")

    (define-for-each-property for-each-prop *exact?* *for-each* *len* *take* *to-list*)
    (define-map-non-vector-property for-each/non-vec-prop *for-each* *type-of* "" "-for-each")
    (define-fold-non-vector-property/1 for-each/non-vec-prop/2 *for-each* *type-of* "" "-for-each")

    (define-count-property count-prop *count* *len* *take* *to-list*)
    (define-map-non-vector-property count/non-vec-prop *count* *type-of* "" "-count")
    (define-fold-non-vector-property/1 count/non-vec-prop/2 *count* *type-of* "" "-count")

    (define-cumulate-property cumulate-prop *exact?* *cumul* *to-list*)
    (define-fold-non-vector-property/1 cumulate/non-vec-prop *cumul* *type-of* "" "-cumulate")

    (test-begin (~l"~a-take"))
    (test-property (~l"~a-take normative behaviour") take-prop (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-take non-vector") take/non-vec-prop (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-take non-index") take/neg-index-prop (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-take overflowing index") take/overflow-prop (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-take-right"))
    (test-property (~l"~a-take-right normative behaviour") taker-prop (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-take-right non-vector") taker/non-vec-prop (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-take-right non-index") taker/neg-index-prop (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-take-right overflowing index") taker/overflow-prop (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-drop"))
    (test-property (~l"~a-drop normative behaviour") drop-prop (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-drop non-vector") drop/non-vec-prop (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-drop non-index") drop/neg-index-prop (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-drop overflowing index") drop/overflow-prop (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-drop-right"))
    (test-property (~l"~a-drop-right normative behaviour") dropr-prop (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-drop-right non-vector") dropr/non-vec-prop (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-drop-right non-index") dropr/neg-index-prop (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-drop-right overflowing index") dropr/overflow-prop (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-segment"))
    (test-property (~l"~a-segment normative behaviour") segment-prop (list (list-generator-of gen-char) (gen-range 1 24)))
    (test-property (~l"~a-segment non-vector") segment/non-vec-prop (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-segment non-index") segment/neg-index-prop (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-segment non-index") segment/neg-index-prop (list (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-fold"))
    (test-property (~l"~a-fold normative behaviour") fold-prop (list (mk-gen-rvec 24)))
    (test-property (~l"~a-fold normative behaviour (multi-valued case)") fold-prop (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-fold non-vector") fold/non-vec-prop (list pgen nulgen (symbol-generator)))
    (test-property (~l"~a-fold non-vector (non-initial position)") fold/non-vec-prop/2 (list pgen nulgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-fold-right"))
    (test-property (~l"~a-fold-right normative behaviour") foldr-prop (list (mk-gen-rvec 24)))
    (test-property (~l"~a-fold-right normative behaviour (multi-valued case)") foldr-prop (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-fold-right non-vector") foldr/non-vec-prop (list pgen nulgen (symbol-generator)))
    (test-property (~l"~a-fold-right non-vector (non-initial position)") foldr/non-vec-prop/2 (list pgen nulgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-in-place map"))
    (test-property (~l"~a-in-place map normative behaviour") map!-prop (list (mk-gen-rvec 24)))
    (test-property (~l"~a-in-place map normative behaviour (multivalued case)") map!-prop (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-in-place map non-vector") map!/non-vec-prop (list pgen (symbol-generator)))
    (test-property (~l"~a-in-place map non-vector (non-initial position)") map!/non-vec-prop/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-persistent map"))
    (test-property (~l"~a-persistent map normative behaviour") map-prop (list (mk-gen-rvec 24)))
    (test-property (~l"~a-persistent map normative behaviour (multivalued case)") map-prop (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-map non-vector") map/non-vec-prop (list pgen (symbol-generator)))
    (test-property (~l"~a-map non-vector (non-initial position)") map/non-vec-prop/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-for-each"))
    (test-property (~l"~a-for-each normative behaviour") for-each-prop (list (mk-gen-rvec 24)))
    (test-property (~l"~a-for-each normative behaviour (multivalued case)") for-each-prop (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-for-each non-vector") for-each/non-vec-prop (list pgen (symbol-generator)))
    (test-property (~l"~a-for-each non-vector (non-initial position)") for-each/non-vec-prop/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-count"))
    (cond [*exact?*
           (test-property (~l"~a-count normative behaviour") count-prop (list (circular-generator odd? even?) (mk-gen-rvec 24)))
           (test-property (~l"~a-count normative behaviour (multivalued case)") count-prop (list (circular-generator >) (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))]
          [*real?*
           (let* ([x>=0.5? (lambda (x) (>= x 0.5))]
                  [x<0.5? (compose not x>=0.5?)])
             (test-property (~l"~a-count normative behaviour") count-prop (list (circular-generator x>=0.5? x<0.5?) (mk-gen-rvec 24)))
             (test-property (~l"~a-count normative behaviour (multivalued case)") count-prop (list (circular-generator >) (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24))))]
          [else
           (let* ([magn>0.5? (lambda (x) (> (magnitude x) 0.5))]
                  [magn<0.5? (lambda (x) (< (magnitude x) 0.5))]
                  [magn>? (lambda args (apply > (map magnitude args)))])
             (test-property (~l"~a-count normative behaviour") count-prop (list (circular-generator magn>0.5? magn<0.5?) (mk-gen-rvec 24)))
             (test-property (~l"~a-count normative behaviour (multivalued case)") count-prop (list (circular-generator magn>?) (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24))))])
    (test-property (~l"~a-count non-vector") count/non-vec-prop (list pgen (symbol-generator)))
    (test-property (~l"~a-count non-vector (non-initial position)") count/non-vec-prop/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-cumulate"))
    (test-property (~l"~a-cumulate normative behaviour") cumulate-prop (list (mk-gen-rvec 24)))
    (test-property (~l"~a-cumulate non-vector") cumulate/non-vec-prop (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)))

(define run-u8vector-iteration-tests
  (run-iteration-tests #t #t
     "u8vector" 255
     make-random-u8-generator
     list->u8vector u8vector->list reverse-u8vector->list
     u8vector-length
     u8vector-take u8vector-take-right u8vector-drop u8vector-drop-right
     u8vector-segment u8vector-fold u8vector-fold-right
     u8vector-map! u8vector-map u8vector-for-each
     u8vector-count u8vector-cumulate))

(define run-u16vector-iteration-tests
  (run-iteration-tests #t #t
     "u16vector" 65535
     make-random-u16-generator
     list->u16vector u16vector->list reverse-u16vector->list
     u16vector-length
     u16vector-take u16vector-take-right u16vector-drop u16vector-drop-right
     u16vector-segment u16vector-fold u16vector-fold-right
     u16vector-map! u16vector-map u16vector-for-each
     u16vector-count u16vector-cumulate))

(define run-u32vector-iteration-tests
  (run-iteration-tests #t #t
     "u32vector" 4294967295
     make-random-u32-generator
     list->u32vector u32vector->list reverse-u32vector->list
     u32vector-length
     u32vector-take u32vector-take-right u32vector-drop u32vector-drop-right
     u32vector-segment u32vector-fold u32vector-fold-right
     u32vector-map! u32vector-map u32vector-for-each
     u32vector-count u32vector-cumulate))

(define run-u64vector-iteration-tests
  (run-iteration-tests #t #t
     "u64vector" 18446744073709551615
     make-random-u64-generator
     list->u64vector u64vector->list reverse-u64vector->list
     u64vector-length
     u64vector-take u64vector-take-right u64vector-drop u64vector-drop-right
     u64vector-segment u64vector-fold u64vector-fold-right
     u64vector-map! u64vector-map u64vector-for-each
     u64vector-count u64vector-cumulate))

(define run-s8vector-iteration-tests
  (run-iteration-tests #t #t
     "s8vector" 127
     make-random-s8-generator
     list->s8vector s8vector->list reverse-s8vector->list
     s8vector-length
     s8vector-take s8vector-take-right s8vector-drop s8vector-drop-right
     s8vector-segment s8vector-fold s8vector-fold-right
     s8vector-map! s8vector-map s8vector-for-each
     s8vector-count s8vector-cumulate))

(define run-s16vector-iteration-tests
  (run-iteration-tests #t #t
     "s16vector" 32767
     make-random-s16-generator
     list->s16vector s16vector->list reverse-s16vector->list
     s16vector-length
     s16vector-take s16vector-take-right s16vector-drop s16vector-drop-right
     s16vector-segment s16vector-fold s16vector-fold-right
     s16vector-map! s16vector-map s16vector-for-each
     s16vector-count s16vector-cumulate))

(define run-s32vector-iteration-tests
  (run-iteration-tests #t #t
     "s32vector" 2147483647
     make-random-s32-generator
     list->s32vector s32vector->list reverse-s32vector->list
     s32vector-length
     s32vector-take s32vector-take-right s32vector-drop s32vector-drop-right
     s32vector-segment s32vector-fold s32vector-fold-right
     s32vector-map! s32vector-map s32vector-for-each
     s32vector-count s32vector-cumulate))

(define run-s64vector-iteration-tests
  (run-iteration-tests #t #t
     "s64vector" 9223372036854775807
     make-random-s64-generator
     list->s64vector s64vector->list reverse-s64vector->list
     s64vector-length
     s64vector-take s64vector-take-right s64vector-drop s64vector-drop-right
     s64vector-segment s64vector-fold s64vector-fold-right
     s64vector-map! s64vector-map s64vector-for-each
     s64vector-count s64vector-cumulate))

(define run-f32vector-iteration-tests
  (run-iteration-tests #f #t
     "f32vector" #f
     (thunk (make-random-real-generator 0 1))
     list->f32vector f32vector->list reverse-f32vector->list
     f32vector-length
     f32vector-take f32vector-take-right f32vector-drop f32vector-drop-right
     f32vector-segment f32vector-fold f32vector-fold-right
     f32vector-map! f32vector-map f32vector-for-each
     f32vector-count f32vector-cumulate))

(define run-f64vector-iteration-tests
  (run-iteration-tests #f #t
     "f64vector" #f
     (thunk (make-random-real-generator 0 1))
     list->f64vector f64vector->list reverse-f64vector->list
     f64vector-length
     f64vector-take f64vector-take-right f64vector-drop f64vector-drop-right
     f64vector-segment f64vector-fold f64vector-fold-right
     f64vector-map! f64vector-map f64vector-for-each
     f64vector-count f64vector-cumulate))

(define run-c64vector-iteration-tests
  (run-iteration-tests #f #f
     "c64vector" #f
     (thunk (make-random-rectangular-generator 0 1 0 1))
     list->c64vector c64vector->list reverse-c64vector->list
     c64vector-length
     c64vector-take c64vector-take-right c64vector-drop c64vector-drop-right
     c64vector-segment c64vector-fold c64vector-fold-right
     c64vector-map! c64vector-map c64vector-for-each
     c64vector-count c64vector-cumulate))

(define run-c128vector-iteration-tests
  (run-iteration-tests #f #f
     "c128vector" #f
     (thunk (make-random-rectangular-generator 0 1 0 1))
     list->c128vector c128vector->list reverse-c128vector->list
     c128vector-length
     c128vector-take c128vector-take-right c128vector-drop c128vector-drop-right
     c128vector-segment c128vector-fold c128vector-fold-right
     c128vector-map! c128vector-map c128vector-for-each
     c128vector-count c128vector-cumulate))
