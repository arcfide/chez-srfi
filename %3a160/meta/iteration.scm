;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-numeric-vector-take copy len) v k)
  "wrap SRFI 160 `@vector-take' procedures"
  (let ([size (len v)])
    (assert-index-nat who k)
    (assert-index-bounds who k size v)
    (copy v 0 k)))

(define/curried ((define-numeric-vector-take-right copy len) v k)
  "wrap SRFI 160 `@vector-take-right' procedures"
  (let ([size (len v)])
    (assert-index-nat who k)
    (assert-index-bounds who k size v)
    (copy v (- size k) size)))

(define/curried ((define-numeric-vector-drop copy len) v k)
  "wrap SRFI 160 `@vector-drop' procedures"
  (let ([size (len v)])
    (assert-index-nat who k)
    (assert-index-bounds who k size v)
    (copy v k size)))

(define/curried ((define-numeric-vector-drop-right copy len) v k)
  "wrap SRFI 160 `@vector-drop-right' procedures"
  (let ([size (len v)])
    (assert-index-nat who k)
    (assert-index-bounds who k size v)
    (copy v 0 (- (len v) k))))

#| Lifted verbatim from SRFI 160 reference. I have no idea
   how it's actually meant to work. |#
(define/curried ((define-numeric-vector-segment copy len) v n)
  "wrap SRFI 160 `@vector-cumulate' procedures"
  (assert/who who
    (nonnegative-integer? n)
    "segment ~a is not a non-negative integer" n)
  (let loop ([acc '()] [i 0] [remain (len v)])
    (if (<= remain 0)
	(reverse acc)
	(let ([size (min n remain)])
          (loop (cons (copy v i (+ i size)) acc)
		(+ i size)
		(- remain size))))))

(define/curried ((define-numeric-vector-fold sub len) proc id v . vs)
  "wrap SRFI 160 `@vector-fold' procedures"
  (let* ([vecs (cons v vs)]
	 [width (apply compare-lengths len min vecs)])
    (let loop ([i 0] [acc id])
      (if (fx=? i width)
	  acc
	  (let* ([indices (map (lambda (v) (sub v i)) vecs)]
		 [processed (apply proc acc indices)])
	    (loop (fx+ i 1) processed))))))


(define/curried ((define-numeric-vector-fold-right sub len) proc id v . vs)
  "wrap SRFI 160 `@vector-fold-right' procedures"
  (let* ([vecs (cons v vs)]
	 [width (apply compare-lengths len min vecs)])
    (let loop ([i (fx- width 1)] [acc id])
      (if (fx<? i 0)
	  acc
	  (let* ([indices (map (lambda (v) (sub v i)) vecs)]
		 [processed (apply proc acc indices)])
	    (loop (fx- i 1) processed))))))

(define/curried ((define-numeric-vector-map copy map!) proc v . vs)
  "wrap SRFI 160 `@vector-map' procedures"
  (let ([slots (copy v)])
    (apply map! proc
	   slots ; copied and updated in-place
	   vs)   ; unaffected
    slots))

(define/curried ((define-numeric-vector-map! unfold! sub len) proc v . vs)
  "wrap SRFI 160 `@vector-map! procedures'"
  (let* ([vecs (cons v vs)]
	 ;; compare-lengths len comp v . vs
	 [width (apply compare-lengths len fxmin vecs)])
    (unfold! (lambda (i _)
	       (let ([xs (map (lambda (v) (sub v i)) vecs)])
		 (values (apply proc xs) #f)))
	     v 0 width #f)))

(define/curried ((define-numeric-vector-for-each fold) proc v . vs)
  "wrap SRFI 160 `@vector-for-each' procedures"
  (apply fold
	 (lambda (_ v . vs) (apply proc v vs))
	 #f v vs))

(define/curried ((define-numeric-vector-count fold) pred? v . vs)
  "wrap SRFI 160 `@vector-count' procedures"
  (apply fold
	 (lambda (tally x . xs)
	   (if (apply pred? x xs)
	       (fx+ tally 1)
	       tally))
	 0 v vs))

(define/curried ((define-numeric-vector-cumulate unfold sub len) proc ini v)
  "wrap SRFI 160 `@vector-cumulate' procedures"
  (unfold
   (lambda (i prev)
     (let ([res (proc prev (sub v i))])
       (values res res)))
   (len v) ini))
