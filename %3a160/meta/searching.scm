;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-numeric-vector-take-while len skip copy) pred? v)
  "Wrap SRFI 160 `@vector-take-while' procedures.
   To do this, get the index of the first element which violates `pred?',
   and then call `@vector-copy' to create a new vector."
  (let* ([width (len v)]
	 [idx (skip pred? v)]
	 [idx* (if idx idx width)])
    (copy v 0 idx*)))

(define/curried ((define-numeric-vector-take-while-right len skip copy) pred? v)
  "Wrap SRFI 160 `@vector-take-while-right' procedures"
  (let* ([width (len v)]
	 [idx (skip pred? v)]
	 [idx* (if idx (+ idx 1) 0)])
    (copy v idx* width)))

(define/curried ((define-numeric-vector-drop-while len skip copy) pred? v)
  "Wrap SRFI 160 `@vector-drop-while' procedures"
  (let* ([width (len v)]
	 [idx (skip pred? v)]
	 [idx* (if idx idx width)])
    (copy v idx* width)))

(define/curried ((define-numeric-vector-drop-while-right len skip copy) pred? v)
  "Wrap SRFI 160 `@vector-drop-while-right' procedures"
  (let* ([width (len v)]
	 [idx (skip pred? v)]
	 [idx* (if idx idx -1)])
    (copy v 0 (+ idx* 1))))

(define/curried-case (define-numeric-vector-index len sub)
  "wrap SRFI 160 `@vector-index' and procedures"
   ([(pred? v)
     (let ([width (len v)])
       (let loop ([i 0])
	 (cond [(= i width) #f]
	       [(pred? (sub v i)) i]
	       [else (loop (+ i 1))])))]
    [(pred? v . vs)
     (let* ([vecs (cons v vs)]
	    [width (apply min (map len vecs))])
       (let loop ([i 0])
	 (cond [(= i width) #f]
	       [(apply pred? (vectorised-subscript sub vecs i)) i]
	       [else (loop (+ i 1))])))]))

;; Copied these almost verbatim from the SRFI 160 reference
(define/curried-case (define-numeric-vector-index-right len sub)
  "wrap SRFI 160 `@vector-index-right' and procedures"
  ([(pred? v)
    (let ([width (len v)])
      (let loop ([i (- width 1)])
	(cond [(negative? i) #f]
	      [(pred? (sub v i)) i]
	      [else (loop (- i 1))])))]
   [(pred? v . vs)
    (let* ([vecs (cons v vs)]
	   [width (apply min (map len vecs))])
      (let loop ([i (- width 1)])
	(cond [(negative? i) #f]
	      [(apply pred? (vectorised-subscript sub vecs i)) i]
	      [else (loop (- i 1))])))]))

;; skip-right is the same, just provide some index-right as the indexing procedure
(define/curried-case (define-numeric-vector-skip index)
  "wrap SRFI 160 `@vector-skip' and `@vector-skip-right' procedures"
  ([(pred? v)
    (index (compose not pred?) v)]
   [(pred? v . vs)
    (apply index (lambda vs
		   (not (apply pred? vs))) v vs)]))

(define/curried-case (define-numeric-vector-any fold empty?)
  "wrap SRFI 160 `@vector-any' procedures"
  ([(pred? v)
    (or (empty? v)
	(call/cc
	 (lambda (break)
	   (fold (lambda (_ c . cs)
		   (let ([res (pred? c)])
		     (if res (break res) #f)))
		 #f v))))]
   [(pred? v . vs)
    (or (empty? v)
	(call/cc
	 (lambda (break)
	   (apply fold
		  (lambda (_ c . cs)
		    (let ([res (apply pred? c cs)])
		      (if res (break res) #f)))
		  #f v vs))))]))


(define/curried-case (define-numeric-vector-every fold empty?)
  "wrap SRFI 160 `@vector-every' procedures"
  ([(pred? v)
    (or (empty? v)
	(call/cc
	 (lambda (break)
	   (fold (lambda (_ c . cs)
		   (let ([res (pred? c)])
		     (or res (break #f))))
		 #t v))))]
   [(pred? v . vs)
    (or (empty? v)
	(call/cc
	 (lambda (break)
	   (apply fold
		  (lambda (_ c . cs)
		    (let ([res (apply pred? c cs)])
		      (or res (break #f))))
		   #t v vs))))]))

;; Translated more or less verbatim from Chez SRFI 133 source
;; It can't be implemented with `unfold!' as far as I can see,
;; due to the setting different indices depending on whether
;; the predicate holds. `unfold!' doesn't allow one to vary the
;; position which is set.
(define/curried ((define-numeric-vector-partition mk sub upd! len count) pred? v)
  "wrap SRFI 160 `@vector-partition' procedures"
  (let* ([wdth (len v)]
	 [cnt (count pred? v)]
	 [slots (mk wdth)])
    (let loop ([i 0] [yes 0] [no cnt])
      (if (= i wdth)
	  (values slots cnt)
	  (let ([elem (sub v i)])
	    (if (pred? elem)
		(begin
		  (upd! slots yes elem)
		  (loop (+ i 1) (+ yes 1) no))
		(begin
		  (upd! slots no elem)
		      (loop (+ i 1) yes (+ no 1)))))))))

(define/curried ((define-numeric-vector-filter mk sub upd! len count) pred? v)
  "wrap SRFI 160 `@vector-filter' procedures"
  (let* ([wdth (len v)]
	 [cnt (count pred? v)]
	 [slots (mk cnt)])
    (let loop ([src-index 0] [tgt-index 0])
      (if (= tgt-index cnt)
	  slots
	  (let ([elem (sub v src-index)])
	    (if (pred? elem)
		(begin (upd! slots tgt-index elem)
		       (loop (+ src-index 1) (+ tgt-index 1)))
		    (loop (+ src-index 1) tgt-index)))))))

(define/curried ((define-numeric-vector-remove filter) pred? v)
  "wrap SRFI 160 `@vector-remove' procedures"
  (filter (compose not pred?) v))
