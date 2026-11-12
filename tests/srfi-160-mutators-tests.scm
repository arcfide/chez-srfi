;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define-syntax (run-mutators-tests stx)
  (syntax-case stx ()
    [(_ *exact?* *real?*
        ;; `base' procedures
	*type-of*
	*mk-gen-relem*
	*from-args* *from-list* *to-list*
	*length* *subscript* *update!*
	;; mutators
	*swap!* *unfold!* *unfoldr!* *fill!* *reverse!* *copy!* *reverse-copy!*
	;;constructors
	*copy*)
     #'(begin
         (define (~l str) (format str *type-of*))
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

         (define (swap!-property v i j)
           (let ([kept (*copy* v)])
             (*swap!* v i j)
             (*swap!* v i j)
             (equal? (*to-list* v) (*to-list* kept))))

         (define-non-vector-property swap!/non-vec-property *swap!* *type-of*)
         (define-test-property (swap!/oob-pos1-property v i j)
           (*swap!* v i j)
           (raises *swap!*) (irritants i v) (message "~a is not a valid index for ~a"))
         (define-test-property (swap!/oob-pos2-property v i j)
           (*swap!* v i j)
           (raises *swap!*) (irritants j v) (message "~a is not a valid index for ~a"))
         (define-test-property (swap!/non-pos1-property v i j)
           (*swap!* v i j)
           (raises *swap!*) (irritants i) (message "index ~a is not a non-negative fixnum"))
         (define-test-property (swap!/non-pos2-property v i j)
           (*swap!* v i j)
           (raises *swap!*) (irritants j) (message "index ~a is not a non-negative fixnum"))

         (define (make-unfold!-property ufold!)
           (lambda (v start end)
             (let* ([slice (fill-within *exact?* (*to-list* v) start end sqrt-nearest identity)]
                    [seed (if *exact?* 0 0.0)]
                    [unfold-with (if *exact?*
                                     (lambda (k x) (values (sqrt-nearest k) seed))
                                     (lambda (k x) (values (exact->inexact (sqrt-nearest k)) seed)))])
               (ufold! unfold-with v start end seed)
               (list= = slice (*to-list* v)))))

         (define unfold!-property (make-unfold!-property *unfold!*))
         (define unfoldr!-property (make-unfold!-property *unfoldr!*))

         (define-non-vector-property/1 unfold!/non-vec-property     *unfold!* *type-of*)
         (define-neg-start-property/1  unfold!/neg-start-property   *unfold!*)
         (define-neg-end-property/1    unfold!/neg-end-property     *unfold!*)
         (define-bounds-property/1     unfold!/bounds-property      *unfold!*)
         (define-overflow-property/1   unfold!/overflow-property    *unfold!*)

         (define-non-vector-property/1 unfoldr!/non-vec-property    *unfoldr!* *type-of*)
         (define-neg-start-property/1  unfoldr!/neg-start-property  *unfoldr!*)
         (define-neg-end-property/1    unfoldr!/neg-end-property    *unfoldr!*)
         (define-bounds-property/1     unfoldr!/bounds-property     *unfoldr!*)
         (define-overflow-property/1   unfoldr!/overflow-property   *unfoldr!*)

         (define (fill!-property v rep start end)
           (let* ([rep (if *exact?* rep (exact->inexact rep))]
                  [slice (fill-within *exact?*
                                      (*to-list* v) start end
			              (lambda (_) rep) identity)])
             (*fill!* v rep start end)
             (if *exact?*
                 (list= = slice (*to-list* v))
                 (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
                        slice
                        (*to-list* v)))))

         (define-non-vector-property fill!/non-vec-property   *fill!* *type-of*)
         (define-test-property (fill!/non-elem-property vec rep . rest)
           (apply *fill!* vec rep rest)
           (raises *fill!*) (irritants rep) (message (format-vector-type "repeating element ~~a cannot be contained within ~a" *type-of*)))
         (define-test-property (fill!/neg-start-property vec rep start end)
           (*fill!* vec rep start end)
           (raises *fill!*) (irritants start) (message "start ~a is not a non-negative fixnum"))
         (define-test-property (fill!/neg-end-property vec rep start end)
           (*fill!* vec rep start end)
           (raises *fill!*) (irritants end) (message "end ~a is not a non-negative fixnum"))
         (define-test-property (fill!/bounds-property vec rep start end)
           (*fill!* vec rep start end)
           (raises *fill!*) (irritants end start) (message "end ~a must be greater than or equal to start ~a"))
         (define-test-property (fill!/overflow-property vec rep start end)
           (*fill!* vec rep start end)
           (raises *fill!*) (irritants end vec) (message "end ~a overflows ~a"))

         (define (reverse!-property v start end)
           (let* ([absolute-max (*length* v)]
	          [sub-list-max (fx- end start)]
	          [swp (lambda (k)
		         (let* ([relative-k (fx- k start)]
		                [swap-with-k (fx- (fx- sub-list-max relative-k) 1)]
		                [swap-absolute (fx+ swap-with-k start)])
		           (*subscript* v swap-absolute)))]
	          [slice (fill-within *exact?* (*to-list* v) start end swp identity)])
             (*reverse!* v start end)
             (equal? slice (*to-list* v))))

         (define-non-vector-property reverse!/non-vec-property   *reverse!* *type-of*)
         (define-neg-start-property  reverse!/neg-start-property *reverse!*)
         (define-neg-end-property    reverse!/neg-end-property   *reverse!*)
         (define-bounds-property     reverse!/bounds-property    *reverse!*)
         (define-overflow-property   reverse!/overflow-property  *reverse!*)

         (define (copy!-property tgt tgt-start src src-start)
           (let* ([slice-length (random (fxmin (fx- (*length* tgt) tgt-start)
                                               (fx- (*length* src) src-start)))]
	          [tgt-end (fx+ tgt-start slice-length)]
	          [src-end (fx+ src-start slice-length)]
	          [from-tgt
	           (lambda (k)
	             (let* ([tgt-rel-k (fx- k tgt-start)]
		            [src-abs-k (fx+ src-start tgt-rel-k)])
	               (*subscript* src src-abs-k)))]
	          [slice (fill-within *exact?* (*to-list* tgt) tgt-start tgt-end from-tgt identity)])
             (*copy!* tgt tgt-start src src-start src-end)
             (equal? slice (*to-list* tgt))))

         (define-test-property (copy!/neg-tgt-start-property tgt tgt-start src src-start src-end)
           (*copy!* tgt tgt-start src src-start src-end)
           (raises *copy!*) (irritants tgt-start) (message "target start ~a is not a non-negative fixnum"))

         (define-test-property (copy!/neg-src-start-property tgt tgt-start src src-start src-end)
           (*copy!* tgt tgt-start src src-start src-end)
           (raises *copy!*) (irritants src-start) (message "source start ~a is not a non-negative fixnum"))

         (define-test-property (copy!/neg-src-end-property tgt tgt-start src src-start src-end)
           (*copy!* tgt tgt-start src src-start src-end)
           (raises *copy!*) (irritants src-end) (message "source end ~a is not a non-negative fixnum"))

         (define-test-property (copy!/tgt-start-property tgt tgt-start src src-start src-end)
           (*copy!* tgt tgt-start src src-start src-end)
           (raises *copy!*) (irritants tgt-start tgt) (message "target start ~a exceeds length of target ~a"))

         (define-test-property (copy!/src-end-property tgt tgt-start src src-start src-end)
           (*copy!* tgt tgt-start src src-start src-end)
           (raises *copy!*) (irritants src-end src) (message "source end ~a exceeds length of source ~a"))

         (define-test-property (copy!/overruns-property tgt tgt-start src src-start src-end)
           (*copy!* tgt tgt-start src src-start src-end)
           (raises *copy!*) (irritants (fx- (fx+ tgt-start (fx- src-end src-start)) (*length* tgt))) (message "slice source overruns target by ~a elements"))

         (define-non-vector-property copy!/non-tgt-property *copy!* *type-of*)

         (define-test-property (copy!/non-src-property tgt tgt-start src src-start src-end)
           (*copy!* tgt tgt-start src src-start src-end)
           (raises *copy!*) (irritants src) (message (format "~~a is not of type ~a" *type-of*)))

         (define (reverse-copy!-property tgt tgt-start src src-start)
           (let* ([slice-length (random (fxmin (fx- (*length* tgt) tgt-start)
		                               (fx- (*length* src) src-start)))]
	          [tgt-end (fx+ tgt-start slice-length)]
	          [src-end (fx+ src-start slice-length)]
	          [from-tgt
	           (lambda (k)
	             (let* ([tgt-rel-k (fx- k tgt-start)]
		            [swap-with-src-k (fx- (fx- slice-length tgt-rel-k) 1)]
		            [swap-absolute (fx+ swap-with-src-k src-start)])
	               (*subscript* src swap-absolute)))]
	          [slice
	           (fill-within *exact?* (*to-list* tgt) tgt-start tgt-end from-tgt identity)])
             (*reverse-copy!* tgt tgt-start src src-start src-end)
             (equal? slice (*to-list* tgt))))

         (define copy!-test-source (fake-from-args *from-args* *exact?* *real?* '(5 10 15 20 25)))
         (define copy!-test-target (fake-from-args *from-args* *exact?* *real?* '(0 1 2 3 4)))
         (define copy!-test-case0 (fake-from-list *exact?* *real?* '(5 10 15 20 25)))
         (define copy!-test-case1 (fake-from-list *exact?* *real?* '(0 1 5 10 15)))
         (define copy!-test-case2 (fake-from-list *exact?* *real?* '(15 20 25 3 4)))
         (define copy!-test-case3 (fake-from-list *exact?* *real?* '(0 1 15 20 25)))
         (define (test-copy!-equiv name expect test-equiv tgt tgt-start src src-start src-end)
           (let ([tgt/c (*copy* tgt)])
             (*copy!* tgt/c tgt-start src src-start src-end)
             (test-equiv name expect tgt/c)))

         ;; ;; BEGIN THE TESTING!
         (test-begin (~l"in-place ~a-swap!"))
         (test-property (~l"~a-swap! normative behaviour") swap!-property (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range 0 24)))
         (test-property (~l"~a-swap! bad vector") swap!/non-vec-property (list (symbol-generator) (gen-range 0 24) (gen-range 0 24)))

         (test-property (~l"~a-swap! OOB position 1") swap!/oob-pos1-property (list (mk-gen-rvec 12 24) (gen-range 24 96) (gen-range 0 12)))
         (test-property (~l"~a-swap! OOB position 2") swap!/oob-pos2-property (list (mk-gen-rvec 12 24) (gen-range 0 12) (gen-range 24 96)))
         (test-property (~l"~a-swap! bad position 1") swap!/non-pos1-property (list (mk-gen-rvec 12 24) (symbol-generator) (gen-range 0 12)))
         (test-property (~l"~a-swap! bad position 2") swap!/non-pos2-property (list (mk-gen-rvec 12 24) (gen-range 0 12) (symbol-generator)))
         (test-end (~l"in-place ~a-swap!"))

         (test-begin (~l"in-place ~a-unfold!"))
         (test-property (~l"~a-unfold! normative behaviour") unfold!-property (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-unfold! negative start") unfold!/neg-start-property (list pgen (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold! negative end")   unfold!/neg-end-property   (list pgen (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold! flipped bounds") unfold!/bounds-property    (list pgen (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12) (*mk-gen-relem*)))
          (test-property (~l"~a-unfold! end overflows")  unfold!/overflow-property  (list pgen (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold! bad vector")     unfold!/non-vec-property   (list pgen (symbol-generator) (gen-range 0 12) (gen-range 12 24) (*mk-gen-relem*)))
         (test-end (~l"in-place ~a-unfold!"))

         (test-begin (~l"in-place ~a-unfold-right!"))
         (test-property (~l"~a-unfold-right! normative behaviour") unfoldr!-property (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-unfold-right! normative behaviour (case 2)") unfoldr!-property (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-unfold-right! negative start") unfoldr!/neg-start-property (list pgen (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold-right! negative end")   unfoldr!/neg-end-property   (list pgen (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold-right! flipped bounds") unfoldr!/bounds-property    (list pgen (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold-right! end overflows")  unfoldr!/overflow-property  (list pgen (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999) (*mk-gen-relem*)))
         (test-property (~l"~a-unfold-right! bad vector")     unfoldr!/non-vec-property   (list pgen (symbol-generator) (gen-range 0 12) (gen-range 12 24) (*mk-gen-relem*)))
         (test-end (~l"in-place ~a-unfold-right!"))

         (test-begin (~l"in-place ~a-fill!"))
         (test-property (~l"~a-fill! normative behaviour") fill!-property (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-fill! negative start") fill!/neg-start-property (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range -2048 0) (gen-range 0 24)))
         (test-property (~l"~a-fill! negative end") fill!/neg-end-property (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range 0 24) (gen-range -2048 0)))
         (test-property (~l"~a-fill! flipped bounds") fill!/bounds-property (list (mk-gen-rvec 24) (*mk-gen-relem*) (gen-range 12 24) (gen-range 0 12)))
         (test-property (~l"~a-fill! end overflows") fill!/overflow-property (list (mk-gen-rvec 0 24) (*mk-gen-relem*) (gen-range 0 24) (gen-range 25 999)))
         (test-property (~l"~a-fill! bad vector") fill!/non-vec-property (list (symbol-generator) (*mk-gen-relem*) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-fill! bad repeating element") fill!/non-elem-property (list (mk-gen-rvec 24) (symbol-generator) (gen-range 0 12) (gen-range 12 24)))
         (test-end (~l"in-place ~a-fill!"))

         (test-begin (~l"in-place ~a-reverse!"))
         (test-property (~l"~a-reverse! normative behaviour") reverse!-property (list (mk-gen-rvec 24) (gen-range 0 12) (gen-range 12 24)))
         (test-property (~l"~a-reverse! negative start") reverse!/neg-start-property (list (mk-gen-rvec 24) (gen-range -2048 0) (gen-range 0 24)))
         (test-property (~l"~a-reverse! negative end") reverse!/neg-end-property (list (mk-gen-rvec 24) (gen-range 0 24) (gen-range -2048 0)))
         (test-property (~l"~a-reverse! flipped bounds") reverse!/bounds-property (list (mk-gen-rvec 24) (gen-range 12 24) (gen-range 0 12)))
         (test-property (~l"~a-reverse! end overflows") reverse!/overflow-property (list (mk-gen-rvec 0 24) (gen-range 0 24) (gen-range 25 999)))
         (test-end (~l"in-place ~a-reverse!"))

         (test-begin (~l"in-place ~a-copy!"))
         (test-property (~l"~a-copy! normative behaviour") copy!-property (list (mk-gen-rvec 24 48) (gen-range 0 24) (mk-gen-rvec 24 48) (gen-range 0 24)))

         (test-copy!-equiv (~l"~a-copy!/same")     copy!-test-case0 test-equiv-to copy!-test-target 0 copy!-test-source 0 5)
         (test-copy!-equiv (~l"~a-copy!/tgt-snd")  copy!-test-case1 test-equiv-to copy!-test-target 2 copy!-test-source 0 3)
         (test-copy!-equiv (~l"~a-copy!/src-snd")  copy!-test-case2 test-equiv-to copy!-test-target 0 copy!-test-source 2 5)
         (test-copy!-equiv (~l"~a-copy!/src-snd")  copy!-test-case3 test-equiv-to copy!-test-target 2 copy!-test-source 2 5)

         ;; negative target start, negative source start/end
         (test-assert (copy!/neg-tgt-start-property  copy!-test-target -12  copy!-test-source 0 5))
         (test-assert (copy!/neg-src-start-property  copy!-test-target 0  copy!-test-source -12 5))
         (test-assert (copy!/neg-src-end-property  copy!-test-target 0  copy!-test-source 0 -12))
         ;; target start is explicitly out of range
         (test-assert (copy!/tgt-start-property  copy!-test-target 8  copy!-test-source 0 5))
         ;; source end is explicitly out of range
         (test-assert (copy!/src-end-property  copy!-test-target 2  copy!-test-source 0 8))
         ;; source end is not out of range, but slice overruns
         (test-assert (copy!/overruns-property  copy!-test-target 2  copy!-test-source 0 5))
         ;; test source end property again
         (test-assert (copy!/src-end-property  copy!-test-target 0  copy!-test-source 2 8))
         ;; non-numeric source source or target
         (test-assert (copy!/non-src-property  copy!-test-target 0  'abacus 0 5))
         (test-assert (copy!/non-tgt-property  'abacus 0  copy!-test-source 0 5))
         (test-end (~l"in-place ~a-copy!"))

         (test-begin (~l"in-place ~a-reverse-copy!"))
         (test-property (~l"~a-reverse-copy! normative behaviour") reverse-copy!-property (list (mk-gen-rvec 24 48) (gen-range 0 24) (mk-gen-rvec 24 48) (gen-range 0 24)))
         (test-end (~l"in-place ~a-reverse-copy!"))
         )]))

(run-mutators-tests #t #t
  "u8vector"
  make-random-u8-generator
  u8vector list->u8vector u8vector->list
  u8vector-length u8vector-ref u8vector-set!
  u8vector-swap! u8vector-unfold! u8vector-unfold-right! u8vector-fill! u8vector-reverse! u8vector-copy! u8vector-reverse-copy!
  u8vector-copy)

(run-mutators-tests #t #t
  "u16vector"
  make-random-u16-generator
  u16vector list->u16vector u16vector->list
  u16vector-length u16vector-ref u16vector-set!
  u16vector-swap! u16vector-unfold! u16vector-unfold-right! u16vector-fill! u16vector-reverse! u16vector-copy! u16vector-reverse-copy!
  u16vector-copy)

(run-mutators-tests #t #t
  "u32vector"
  make-random-u32-generator
  u32vector list->u32vector u32vector->list
  u32vector-length u32vector-ref u32vector-set!
  u32vector-swap! u32vector-unfold! u32vector-unfold-right! u32vector-fill! u32vector-reverse! u32vector-copy! u32vector-reverse-copy!
  u32vector-copy)

(run-mutators-tests #t #t
  "u64vector"
  make-random-u64-generator
  u64vector list->u64vector u64vector->list
  u64vector-length u64vector-ref u64vector-set!
  u64vector-swap! u64vector-unfold! u64vector-unfold-right! u64vector-fill! u64vector-reverse! u64vector-copy! u64vector-reverse-copy!
  u64vector-copy)

(run-mutators-tests #t #t
  "s8vector"
  make-random-s8-generator
  s8vector list->s8vector s8vector->list
  s8vector-length s8vector-ref s8vector-set!
  s8vector-swap! s8vector-unfold! s8vector-unfold-right! s8vector-fill! s8vector-reverse! s8vector-copy! s8vector-reverse-copy!
  s8vector-copy)

(run-mutators-tests #t #t
  "s16vector"
  make-random-s16-generator
  s16vector list->s16vector s16vector->list
  s16vector-length s16vector-ref s16vector-set!
  s16vector-swap! s16vector-unfold! s16vector-unfold-right! s16vector-fill! s16vector-reverse! s16vector-copy! s16vector-reverse-copy!
  s16vector-copy)

(run-mutators-tests #t #t
  "s32vector"
  make-random-s32-generator
  s32vector list->s32vector s32vector->list
  s32vector-length s32vector-ref s32vector-set!
  s32vector-swap! s32vector-unfold! s32vector-unfold-right! s32vector-fill! s32vector-reverse! s32vector-copy! s32vector-reverse-copy!
  s32vector-copy)

(run-mutators-tests #t #t
  "s64vector"
  make-random-s64-generator
  s64vector list->s64vector s64vector->list
  s64vector-length s64vector-ref s64vector-set!
  s64vector-swap! s64vector-unfold! s64vector-unfold-right! s64vector-fill! s64vector-reverse! s64vector-copy! s64vector-reverse-copy!
  s64vector-copy)

(run-mutators-tests #f #t
  "f32vector"
  (lambda () (make-random-real-generator 0 1))
  f32vector list->f32vector f32vector->list
  f32vector-length f32vector-ref f32vector-set!
  f32vector-swap! f32vector-unfold! f32vector-unfold-right! f32vector-fill! f32vector-reverse! f32vector-copy! f32vector-reverse-copy!
  f32vector-copy)

(run-mutators-tests #f #t
  "f64vector"
  (lambda () (make-random-real-generator 0 1))
  f64vector list->f64vector f64vector->list
  f64vector-length f64vector-ref f64vector-set!
  f64vector-swap! f64vector-unfold! f64vector-unfold-right! f64vector-fill! f64vector-reverse! f64vector-copy! f64vector-reverse-copy!
  f64vector-copy)

(run-mutators-tests #f #f
  "c64vector"
  (lambda () (make-random-rectangular-generator 0 1 0 1))
  c64vector list->c64vector c64vector->list
  c64vector-length c64vector-ref c64vector-set!
  c64vector-swap! c64vector-unfold! c64vector-unfold-right! c64vector-fill! c64vector-reverse! c64vector-copy! c64vector-reverse-copy!
  c64vector-copy)

(run-mutators-tests #f #f
  "c128vector"
  (lambda () (make-random-rectangular-generator 0 1 0 1))
  c128vector list->c128vector c128vector->list
  c128vector-length c128vector-ref c128vector-set!
  c128vector-swap! c128vector-unfold! c128vector-unfold-right! c128vector-fill! c128vector-reverse! c128vector-copy! c128vector-reverse-copy!
  c128vector-copy)
