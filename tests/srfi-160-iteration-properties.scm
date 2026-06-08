;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

#| Some general properties for error behaviour |#

(define-test-property ((define-iter-neg-index-property op) vec k . rest)
  "Holds true if start is not non-negative"
  (apply op vec k rest)
  (irritants k)
  (message "index ~a is not a non-negative integer"))

(define-test-property ((define-iter-overflow-property op) vec k . rest)
  "Holds true if end overflows vector"
  (apply op vec k rest)
  (irritants k vec)
  (message "index ~a overflows ~a"))

#| take and drop |#

(define/curried ((define-take/drop-property op list-op ->list) v n)
  "define property to test `@vector-take' normative behaviour"
  (list= =
         (list-op (->list v) n)
	 (->list (op v n))))

#| segment |#

(define/curried ((define-segment-property exact? seg from-list to-list) lst n)
  "Define property to test `@vector-segment' normative behaviour.
It's feasible to use faster fixnum arithmetic here as the characters
(and hence integers) are limited to that of the R6RS unicode range."
  (let* ([cps (map char->integer lst)]
	 [vec
          (if exact?
              (from-list cps)
              (from-list (map exact->inexact cps)))])
    (for-all (lambda (A B)
	       (list= =
		      (map char->integer A)
		      (to-list B)))
	     (char-list-segment lst n)
	     (seg vec n))))

(define-test-property ((define-segment-index-property op) vec k . rest)
  "Holds true if end overflows vector"
  (apply op vec k rest)
  (irritants k)
  (message "segment ~a is not a non-negative integer"))

#| map |#

(define-test-property ((define-map-non-vector-property op) pre v . vs)
  "Holds true if last arguments fails to pass as vectors"
  (apply op pre v vs)
  (irritants v)
  (message (format "~~a is not of type ~a" type-of)))

#| fold |#

(define/curried ((define-fold-property fold len take kons ->list) v . vs)
  "define property to test `@vector-fold' normative behaviour"
  (let ([id '()])
    (if (null? vs)
	(list= = (fold-left kons id (->list v))
	       (fold      kons id v))
	(let* ([diff/min-len (apply compare-lengths len min v vs)]
	       [nvs (map (lambda (v)
			   (if (fx=? (len v) diff/min-len)
			       v
			       (take v diff/min-len)))
			 (cons v vs))])
	  (list= = (apply fold-left kons id (map ->list nvs))
                   (apply fold      kons id nvs))))))

(define-test-property ((define-fold-non-vector-property/1 op) proc id v . vs)
  "Holds true if last arguments fails to pass as vectors"
  (apply op proc id v vs)
  (irritants v)
  (message (format "~~a is not of type ~a" type-of)))

(define-test-property ((define-fold-non-vector-property/2 op) proc id v vv . vs)
  "Holds true if last arguments fails to pass as vectors"
  (apply op proc id v vv vs)
  (irritants vv)
  (message (format "~~a is not of type ~a" type-of)))

#| map, map! |#

(define/curried-case (define-map!-property exact? vmap! len take ->list)
  "Define property to test `@vector-map!' normative behaviour.
This is in-place so the existing property skeletons aren't readily applicable.
For representations like f32vector, we simplify the procedure and map *."
   ([(v)
     (let* ([op (if exact? sqrt-nearest *)]
            [slice (map op (->list v))])
       (vmap! op v)
       (list= = (->list v) slice))]
    [(v . vs)
     (let* ([diff/min-len (apply compare-lengths len min v vs)]
	    [nvs (map (lambda (v)
			(if (fx=? (len v) diff/min-len)
			    v
			    (take v diff/min-len)))
		      (cons v vs))]
            [op (if exact? sqrt-nearest *)]
	    [slice (apply map op (map ->list nvs))])
       (apply vmap! op nvs)
       ;; There's no way to check the second part as it is, likely
       ;; that nvs is actually truncated to the smallest length.
       (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
              (->list (car nvs))
	      slice))]))

(define/curried-case (define-map-property exact? vmap len take ->list)
  "define property to test `@vector-map' normative behaviour"
  ([(v)
    (let ([op (if exact? sqrt-nearest *)])
      (list= =
             (->list (vmap op v))
	     (map op (->list v))))]
   [(v . vs)
    (let* ([diff/min-len (apply compare-lengths len min v vs)]
	   [nvs (map (lambda (v)
		       (if (fx=? (len v) diff/min-len)
			   v
			   (take v diff/min-len)))
		     (cons v vs))]
           [op (if exact? sqrt-nearest *)])
      (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
             (->list (apply vmap op nvs))
	     (apply map op (map ->list nvs))))]))

#| for-each |#

(define/curried-case (define-for-each-property exact? for-each len take ->list)
  "define property to test `@vector-fors-each' normative behaviour"
  ([(v)
    (let* ([slots '()]
           [op (if exact? sqrt-nearest *)]
	   [slice (map op (->list v))])
      (for-each (lambda (x)
		  (set! slots (cons (op x) slots)))
		v)
	(list= = (reverse slots) slice))]
   [(v . vs)
    (let* ([diff/min-len (apply compare-lengths len min v vs)]
	   [nvs (map (lambda (v)
		       (if (fx=? (len v) diff/min-len)
			   v
			   (take v diff/min-len)))
		     (cons v vs))]
	   [slots '()]
           [op (if exact? sqrt-nearest *)]
	   [slice (apply map op (map ->list nvs))])
      (apply for-each (lambda x*
			(set! slots (cons (apply op x*) slots)))
	     nvs)
      (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
             (reverse slots)
             slice))]))

#| count |#

(define/curried-case (define-count-property vcount len take ->list)
  "define property to test `@vector-count' normative behaviour"
  ([(pred? v)
    (= (count pred? (->list v))
       (vcount pred? v))]
   [(pred? v . vs)
    #| There's actually a bug in the SRFI 1 of the Chez SRFI grab-bag:
       counting doesn't stop on the shortest list, as specified.
       The `take' approach like before, below, is the workaround. |#
    (let* ([diff/min-len (apply compare-lengths len min v vs)]
	   [nvs (map (lambda (v)
		       (if (fx=? (len v) diff/min-len)
			   v
			   (take v diff/min-len)))
		     (cons v vs))])
      (= (apply count pred? (map ->list nvs))
	 (apply vcount pred? nvs)))]))

#| cumulate |#

(define/curried ((define-cumulate-property exact? cumul ->list) v)
  "define property to test `@vector-cumulate' normative behaviour"
  (if exact?
      (list= =
             (list-cumulate sqrt-nearest 12 (->list v))
	     (->list (cumul sqrt-nearest 12 v)))
      (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
             (list-cumulate * 12 (->list v))
	     (->list (cumul * 12 v)))))
