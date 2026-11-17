;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define-syntax (run-iteration-tests stx)
  (syntax-case stx ()
    [(_ *exact?* *real?*
        *type-of* %max%
        *mk-gen-relem*
	*from-list* *to-list* *to-reverse-list*
	*length*
	*take* *take-right* *drop* *drop-right*
	*segment* *fold* *fold-right*
	*map!* *map* *for-each*
	*count* *cumulate*)
 #'(begin
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
        (gfilter (lambda (vec) (<= min-size (*length* vec)))
                 (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]
       [(min-size max-size)
        (gfilter (lambda (vec) (<= min-size (*length* vec) max-size))
                 (gmap *from-list* (list-generator-of (*mk-gen-relem*))))]))

    (define (make-take/drop-property op list-op ->list)
      (lambda (v n)
        (list= =
               (list-op (->list v) n)
               (->list (op v n)))))

    (define take-property (make-take/drop-property       *take*       take *to-list*))
    (define take-right-property (make-take/drop-property *take-right* take *to-reverse-list*))
    (define drop-property (make-take/drop-property       *drop*       drop *to-list*))
    (define drop-right-property (make-take/drop-property *drop-right* drop *to-reverse-list*))

    (define-non-vector-property     take/non-vec-property   *take* *type-of*)
    (define-iter-neg-index-property take/neg-index-property *take*)
    (define-iter-overflow-property  take/overflow-property  *take*)

    (define-non-vector-property     take-right/non-vec-property   *take-right* *type-of*)
    (define-iter-neg-index-property take-right/neg-index-property *take-right*)
    (define-iter-overflow-property  take-right/overflow-property  *take-right*)

    (define-non-vector-property     drop/non-vec-property   *drop* *type-of*)
    (define-iter-neg-index-property drop/neg-index-property *drop*)
    (define-iter-overflow-property  drop/overflow-property  *drop*)

    (define-non-vector-property     drop-right/non-vec-property   *drop-right* *type-of*)
    (define-iter-neg-index-property drop-right/neg-index-property *drop-right*)
    (define-iter-overflow-property  drop-right/overflow-property  *drop-right*)

    (define (segment-property lst n)
      (let* ([cps (map char->integer lst)]
	     [vec (if *exact?*
                      (*from-list* cps)
                      (*from-list* (map exact->inexact cps)))])
        (for-all (lambda (A B)
	           (list= = (map char->integer A) (*to-list* B)))
	         (char-list-segment lst n)
	         (*segment* vec n))))

    (define-non-vector-property segment/non-vec-property *segment* *type-of*)

    (define-test-property (segment/non-index-property vec k . rest)
      (apply *segment* vec k rest)
      (raises *segment*) (irritants k) (message "segment ~a is not a non-negative fixnum"))

    (define (make-fold-property fold ->list)
      (lambda (v . vs)
        (let ([id '()]
              [snoc* (flip cons*)])
          (if (null? vs)
	      (list= = (fold-left snoc* id (->list v))
                       (fold      snoc* id v))
	(let* ([diff/min-len (apply compare-lengths *length* min v vs)]
	       [nvs (map (lambda (v)
			   (if (fx=? (*length* v) diff/min-len)
			       v
			       (*take* v diff/min-len)))
			 (cons v vs))])
	  (list= = (apply fold-left snoc* id (map ->list nvs))
                   (apply fold      snoc* id nvs)))))))

    (define fold-property (make-fold-property *fold* *to-list*))
    (define fold-right-property (make-fold-property *fold-right* *to-reverse-list*))

    (define-fold-non-vector-property/1 fold/non-vec-property   *fold* *type-of*)
    (define-fold-non-vector-property/2 fold/non-vec-property/2 *fold* *type-of*)

    (define-fold-non-vector-property/1 fold-right/non-vec-property   *fold-right* *type-of*)
    (define-fold-non-vector-property/2 fold-right/non-vec-property/2 *fold-right* *type-of*)

    (define map!-property
      (case-lambda
       [(v)
        (let* ([op (if *exact?* sqrt-nearest *)]
               [slice (map op (*to-list* v))])
          (*map!* op v)
          (list= = (*to-list* v) slice))]
       [(v . vs)
        (let* ([diff/min-len (apply compare-lengths *length* fxmin v vs)]
	       [nvs (map (lambda (v)
			   (if (fx=? (*length* v) diff/min-len)
			       v
			       (*take* v diff/min-len)))
		         (cons v vs))]
               [op (if *exact?* sqrt-nearest *)]
	       [slice (apply map op (map *to-list* nvs))])
          (apply *map!* op nvs)
          ;; There's no way to check the second part as it is, likely
          ;; that nvs is actually truncated to the smallest length.
          (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
                 (*to-list* (car nvs))
	         slice))]))

    (define map-property
      (case-lambda
       [(v)
        (let ([op (if *exact?* sqrt-nearest *)])
          (list= = (*to-list* (*map* op v))
                   (map op (*to-list* v))))]
       [(v . vs)
        (let* ([diff/min-len (apply compare-lengths *length* fxmin v vs)]
	       [nvs (map (lambda (v)
		           (if (fx=? (*length* v) diff/min-len)
			       v
			       (*take* v diff/min-len)))
		         (cons v vs))]
               [op (if *exact?* sqrt-nearest *)])
          (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
                 (*to-list* (apply *map* op nvs))
	         (apply map op (map *to-list* nvs))))]))

    (define-map-non-vector-property map!/non-vec-property *map!* *type-of*)
    (define-fold-non-vector-property/1 map!/non-vec-property/2 *map!* *type-of*)

    (define-map-non-vector-property map/non-vec-property *map* *type-of*)
    (define-fold-non-vector-property/1 map/non-vec-property/2 *map* *type-of*)

    (define for-each-property
      (case-lambda
       [(v)
        (let* ([slots '()]
               [op (if *exact?* sqrt-nearest *)]
	       [slice (map op (*to-list* v))])
          (*for-each* (lambda (x) (set! slots (cons (op x) slots)))
		      v)
	  (list= = (reverse slots) slice))]
       [(v . vs)
        (let* ([diff/min-len (apply compare-lengths *length* fxmin v vs)]
	       [nvs (map (lambda (v)
		           (if (fx=? (*length* v) diff/min-len)
			       v
			       (*take* v diff/min-len)))
		         (cons v vs))]
	       [slots '()]
               [op (if *exact?* sqrt-nearest *)]
	       [slice (apply map op (map *to-list* nvs))])
          (apply *for-each* (lambda x*
			      (set! slots (cons (apply op x*) slots)))
	         nvs)
          (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
                 (reverse slots)
                 slice))]))

    (define-map-non-vector-property for-each/non-vec-property *for-each* *type-of*)
    (define-fold-non-vector-property/1 for-each/non-vec-property/2 *for-each* *type-of*)

    (define count-property
      (case-lambda
       [(pred? v)
        (fx=? (count pred? (*to-list* v))
              (*count* pred? v))]
       [(pred? v . vs)
        #| There's actually a bug in the SRFI 1 of the Chez SRFI grab-bag:
           counting doesn't stop on the shortest list, as specified.
           The `take' approach like before, below, is the workaround. |#
        (let* ([diff/min-len (apply compare-lengths *length* fxmin v vs)]
	       [nvs (map (lambda (v)
		           (if (fx=? (*length* v) diff/min-len)
			       v
			       (*take* v diff/min-len)))
		         (cons v vs))])
          (fx=? (apply count pred? (map *to-list* nvs))
	        (apply *count* pred? nvs)))]))

    (define-map-non-vector-property count/non-vec-property *count* *type-of*)
    (define-fold-non-vector-property/1 count/non-vec-property/2 *count* *type-of*)

    (define (cumulate-property v)
      (if *exact?*
          (list= =
                 (list-cumulate sqrt-nearest 12 (*to-list* v))
	         (*to-list* (*cumulate* sqrt-nearest 12 v)))
          (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
                 (list-cumulate * 12 (*to-list* v))
	         (*to-list* (*cumulate* * 12 v)))))

    (define-fold-non-vector-property/1 cumulate/non-vec-property *cumulate* *type-of*)

    (test-begin (~l"~a-take"))
    (test-property (~l"~a-take normative behaviour") take-property (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-take non-vector") take/non-vec-property (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-take non-index") take/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-take overflowing index") take/overflow-property (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-take-right"))
    (test-property (~l"~a-take-right normative behaviour") take-right-property (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-take-right non-vector") take-right/non-vec-property (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-take-right non-index") take-right/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-take-right overflowing index") take-right/overflow-property (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-drop"))
    (test-property (~l"~a-drop normative behaviour") drop-property (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-drop non-vector") drop/non-vec-property (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-drop non-index") drop/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-drop overflowing index") drop/overflow-property (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-drop-right"))
    (test-property (~l"~a-drop-right normative behaviour") drop-right-property (list (mk-gen-rvec 24) (gen-range 0 24)))
    (test-property (~l"~a-drop-right non-vector") drop-right/non-vec-property (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-drop-right non-index") drop-right/neg-index-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-drop-right overflowing index") drop-right/overflow-property (list (mk-gen-rvec 0 24) (gen-range 26 2048)))
    (test-end)

    (test-begin (~l"~a-segment"))
    (test-property (~l"~a-segment normative behaviour") segment-property (list (list-generator-of gen-char) (gen-range 1 24)))
    (test-property (~l"~a-segment non-vector") segment/non-vec-property (list (symbol-generator) (gen-range 0 24)))
    (test-property (~l"~a-segment negative index") segment/non-index-property (list (mk-gen-rvec 24) (gen-range -2048 0)))
    (test-property (~l"~a-segment non-index") segment/non-index-property (list (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-fold"))
    (test-property (~l"~a-fold normative behaviour") fold-property (list (mk-gen-rvec 24)))
    (test-property (~l"~a-fold normative behaviour (multi-valued case)") fold-property (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-fold non-vector") fold/non-vec-property (list pgen nulgen (symbol-generator)))
    (test-property (~l"~a-fold non-vector (non-initial position)") fold/non-vec-property/2 (list pgen nulgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-fold-right"))
    (test-property (~l"~a-fold-right normative behaviour") fold-right-property (list (mk-gen-rvec 24)))
    (test-property (~l"~a-fold-right normative behaviour (multi-valued case)") fold-right-property (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-fold-right non-vector") fold-right/non-vec-property (list pgen nulgen (symbol-generator)))
    (test-property (~l"~a-fold-right non-vector (non-initial position)") fold-right/non-vec-property/2 (list pgen nulgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-in-place map"))
    (test-property (~l"~a-in-place map normative behaviour") map!-property (list (mk-gen-rvec 24)))
    (test-property (~l"~a-in-place map normative behaviour (multivalued case)") map!-property (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-in-place map non-vector") map!/non-vec-property (list pgen (symbol-generator)))
    (test-property (~l"~a-in-place map non-vector (non-initial position)") map!/non-vec-property/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-persistent map"))
    (test-property (~l"~a-persistent map normative behaviour") map-property (list (mk-gen-rvec 24)))
    (test-property (~l"~a-persistent map normative behaviour (multivalued case)") map-property (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-map non-vector") map/non-vec-property (list pgen (symbol-generator)))
    (test-property (~l"~a-map non-vector (non-initial position)") map/non-vec-property/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-for-each"))
    (test-property (~l"~a-for-each normative behaviour") for-each-property (list (mk-gen-rvec 24)))
    (test-property (~l"~a-for-each normative behaviour (multivalued case)") for-each-property (list (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))
    (test-property (~l"~a-for-each non-vector") for-each/non-vec-property (list pgen (symbol-generator)))
    (test-property (~l"~a-for-each non-vector (non-initial position)") for-each/non-vec-property/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-cumulate"))
    (test-property (~l"~a-cumulate normative behaviour") cumulate-property (list (mk-gen-rvec 24)))
    (test-property (~l"~a-cumulate non-vector") cumulate/non-vec-property (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)

    (test-begin (~l"~a-count"))
    (cond [*exact?*
           (test-property (~l"~a-count normative behaviour") count-property (list (circular-generator odd? even?) (mk-gen-rvec 24)))
           (test-property (~l"~a-count normative behaviour (multivalued case)") count-property (list (circular-generator >) (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24)))]
          [*real?*
           (let* ([x>=0.5? (lambda (x) (>= x 0.5))]
                  [x<0.5? (compose not x>=0.5?)])
             (test-property (~l"~a-count normative behaviour") count-property (list (circular-generator x>=0.5? x<0.5?) (mk-gen-rvec 24)))
             (test-property (~l"~a-count normative behaviour (multivalued case)") count-property (list (circular-generator >) (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24))))]
          [else
           (let* ([magn>0.5? (lambda (x) (> (magnitude x) 0.5))]
                  [magn<0.5? (lambda (x) (< (magnitude x) 0.5))]
                  [magn>? (lambda args (apply > (map magnitude args)))])
             (test-property (~l"~a-count normative behaviour") count-property (list (circular-generator magn>0.5? magn<0.5?) (mk-gen-rvec 24)))
             (test-property (~l"~a-count normative behaviour (multivalued case)") count-property (list (circular-generator magn>?) (mk-gen-rvec 24) (mk-gen-rvec 24) (mk-gen-rvec 24))))])
    (test-property (~l"~a-count non-vector") count/non-vec-property (list pgen (symbol-generator)))
    (test-property (~l"~a-count non-vector (non-initial position)") count/non-vec-property/2 (list pgen (mk-gen-rvec 24) (symbol-generator)))
    (test-end)
    )]))

(run-iteration-tests #t #t "u8vector" 255
   make-random-u8-generator
   list->u8vector u8vector->list reverse-u8vector->list
   u8vector-length
   u8vector-take u8vector-take-right u8vector-drop u8vector-drop-right
   u8vector-segment u8vector-fold u8vector-fold-right
   u8vector-map! u8vector-map u8vector-for-each
   u8vector-count u8vector-cumulate)

(run-iteration-tests #t #t "u16vector" 65535
   make-random-u16-generator
   list->u16vector u16vector->list reverse-u16vector->list
   u16vector-length
   u16vector-take u16vector-take-right u16vector-drop u16vector-drop-right
   u16vector-segment u16vector-fold u16vector-fold-right
   u16vector-map! u16vector-map u16vector-for-each
   u16vector-count u16vector-cumulate)

(run-iteration-tests #t #t "u32vector" 4294967295
   make-random-u32-generator
   list->u32vector u32vector->list reverse-u32vector->list
   u32vector-length
   u32vector-take u32vector-take-right u32vector-drop u32vector-drop-right
   u32vector-segment u32vector-fold u32vector-fold-right
   u32vector-map! u32vector-map u32vector-for-each
   u32vector-count u32vector-cumulate)

(run-iteration-tests #t #t "u64vector" 18446744073709551615
   make-random-u64-generator
   list->u64vector u64vector->list reverse-u64vector->list
   u64vector-length
   u64vector-take u64vector-take-right u64vector-drop u64vector-drop-right
   u64vector-segment u64vector-fold u64vector-fold-right
   u64vector-map! u64vector-map u64vector-for-each
   u64vector-count u64vector-cumulate)

(run-iteration-tests #t #t "s8vector" 127
   make-random-s8-generator
   list->s8vector s8vector->list reverse-s8vector->list
   s8vector-length
   s8vector-take s8vector-take-right s8vector-drop s8vector-drop-right
   s8vector-segment s8vector-fold s8vector-fold-right
   s8vector-map! s8vector-map s8vector-for-each
   s8vector-count s8vector-cumulate)

(run-iteration-tests #t #t "s16vector" 32767
   make-random-s16-generator
   list->s16vector s16vector->list reverse-s16vector->list
   s16vector-length
   s16vector-take s16vector-take-right s16vector-drop s16vector-drop-right
   s16vector-segment s16vector-fold s16vector-fold-right
   s16vector-map! s16vector-map s16vector-for-each
   s16vector-count s16vector-cumulate)

(run-iteration-tests #t #t "s32vector" 2147483647
   make-random-s32-generator
   list->s32vector s32vector->list reverse-s32vector->list
   s32vector-length
   s32vector-take s32vector-take-right s32vector-drop s32vector-drop-right
   s32vector-segment s32vector-fold s32vector-fold-right
   s32vector-map! s32vector-map s32vector-for-each
   s32vector-count s32vector-cumulate)

(run-iteration-tests #t #t "s64vector" 9223372036854775807
   make-random-s64-generator
   list->s64vector s64vector->list reverse-s64vector->list
   s64vector-length
   s64vector-take s64vector-take-right s64vector-drop s64vector-drop-right
   s64vector-segment s64vector-fold s64vector-fold-right
   s64vector-map! s64vector-map s64vector-for-each
   s64vector-count s64vector-cumulate)

(run-iteration-tests #f #t "f32vector" #f
  (lambda () (make-random-real-generator 0 1))
  list->f32vector f32vector->list reverse-f32vector->list
  f32vector-length
  f32vector-take f32vector-take-right f32vector-drop f32vector-drop-right
  f32vector-segment f32vector-fold f32vector-fold-right
  f32vector-map! f32vector-map f32vector-for-each
  f32vector-count f32vector-cumulate)

(run-iteration-tests #f #t "f64vector" #f
  (lambda () (make-random-real-generator 0 1))
  list->f64vector f64vector->list reverse-f64vector->list
  f64vector-length
  f64vector-take f64vector-take-right f64vector-drop f64vector-drop-right
  f64vector-segment f64vector-fold f64vector-fold-right
  f64vector-map! f64vector-map f64vector-for-each
  f64vector-count f64vector-cumulate)

(run-iteration-tests #f #f "c64vector" #f
  (lambda () (make-random-rectangular-generator 0 1 0 1))
  list->c64vector c64vector->list reverse-c64vector->list
  c64vector-length
  c64vector-take c64vector-take-right c64vector-drop c64vector-drop-right
  c64vector-segment c64vector-fold c64vector-fold-right
  c64vector-map! c64vector-map c64vector-for-each
  c64vector-count c64vector-cumulate)

(run-iteration-tests #f #f "c128vector" #f
  (lambda () (make-random-rectangular-generator 0 1 0 1))
  list->c128vector c128vector->list reverse-c128vector->list
  c128vector-length
  c128vector-take c128vector-take-right c128vector-drop c128vector-drop-right
  c128vector-segment c128vector-fold c128vector-fold-right
  c128vector-map! c128vector-map c128vector-for-each
  c128vector-count c128vector-cumulate)
