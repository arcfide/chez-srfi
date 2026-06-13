;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define/curried ((define-list-predicate-property op list-op ->list) pred? v)
  "define `@vector-take-while' normative behaviour"
  (list= = (list-op pred? (->list v))
	   (->list (op pred? v))))

(define/curried-case (define-list-index-property op list-op len take ->list)
  "define `@vector-index' normative behaviour"
  ([(pred? v)
    (eq? (list-op pred? (->list v))
	 (op pred? v))]
   [(pred? v . vs)
    (let* ([diff/min-len (apply compare-lengths len min v vs)]
	   [nvs (map (lambda (v)
		       (if (fx=? (len v) diff/min-len)
			   v
			   (take v diff/min-len)))
		     (cons v vs))])
      (eq? (apply list-op pred? (map ->list nvs))
	   (apply op pred? nvs)))]))

(define/curried-case (define-skip-property op list-op len take ->list)
  "define `@vector-skip' normative behaviour"
  ([(pred? v)
    (eq? (list-op (compose not pred?) (->list v))
	 (op pred? v))]
   [(pred? v . vs)
    (let* ([diff/min-len (apply compare-lengths len min v vs)]
	   [nvs (map (lambda (v)
		       (if (fx=? (len v) diff/min-len)
			   v
			   (take v diff/min-len)))
		     (cons v vs))])
      (eq? (apply list-op (compose not pred?) (map ->list nvs))
	    (apply op pred? nvs)))]))

(define/curried-case (define-any-property any len take ->list)
  "Define `@vector-any' normative behaviour. This additionally makes sure to
check returning true if the shortest vector is of length zero."
  ([(pred? v)
    (if (= 0 (len v))
	(any pred? v)
	(and (exists pred? (->list v))
	     (any pred? v)))]
   [(pred? v . vs)
    (let* ([diff/min-len (apply compare-lengths len min v vs)]
	   [nvs (map (lambda (v)
		       (if (fx=? (len v) diff/min-len)
			   v
			   (take v diff/min-len)))
		     (cons v vs))])
      (if (= 0 diff/min-len)
	  (apply any pred? nvs)
	  (and (apply exists pred? (map ->list nvs))
		(apply any pred? nvs))))]))

(define/curried-case (define-every-property every len take ->list)
  "Define `@vector-every' normative behaviour, this time expecting false,
unless the smallest vector is empty. This additionally makes sure to
check returning true if the shortest vector is of length zero."
  ([(pred? v)
    (if (= 0 (len v))
	(every pred? v)
	(and (for-all pred? (->list v))
	     (every pred? v)))]
   [(pred? v . vs)
    (let* ([diff/min-len (apply compare-lengths len min v vs)]
	   [nvs (map (lambda (v)
		       (if (fx=? (len v) diff/min-len)
			   v
			   (take v diff/min-len)))
		     (cons v vs))])
      (if (= 0 diff/min-len)
	  (apply every pred? nvs)
	  (and (apply for-all pred? (map ->list nvs))
		(apply every pred? nvs))))]))

(define/curried ((define-partition-property part ->list) pred? v)
  "Define @vector-partition normative behaviour"
  (let*-values ([(reordered point) (part pred? v)]
		[(lpart1 lpart2) (partition pred? (->list v))]
		[(vpart1 vpart2) (split-at (->list reordered) point)])
    (and (list= = lpart1 vpart1)
	 (list= = lpart2 vpart2))))
