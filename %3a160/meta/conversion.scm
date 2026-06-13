;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried-case (define-numeric-vector-reverse-to-list fold len sub)
  "Wrap SRFI 160 `reverse-@vector->list' procedures"
  (recur [(v) (fold (flip cons) '() v)]
         [(v start) (recur v start (len v))]
         [(v start end)
          (assert-start-nat who start)
          (assert-end-nat who end)
          (assert-start<=end who start end)
          (assert-bounds who end (len v) v)
	  (let loop ([i start] [acc '()])
	    (if (= i end)
	        acc
	        (loop (+ i 1)
		      (cons (sub v i) (sub v i)))))]))

(define/curried ((define-numeric-vector-reverse-from-list mk upd!) xs)
  "Wrap SRFI 160 `reverse-list->@vector' procedures"
  (if (list? xs)
      (let* ([width (length xs)]
	     [slots (mk width)])
        (let loop ([k (- width 1)] [acc xs])
          (if (negative? k)
	      slots
	      (begin (upd! slots k (car acc))
		     (loop (- k 1) (cdr acc)))))
	slots)
      (assertion-violationf (quote who) "~a is not a proper list" xs)))

(define/curried-case (define-numeric-vector<->vector mk len sub upd!)
  "Wrap SRFI 160 `@vector->vector' and `vector->@vector' procedures"
  (recur [(v) (recur v 0 (len v))]
         [(v start) (recur v start (len v))]
         [(v start end)
          (assert-start-nat who start)
          (assert-end-nat who end)
          (assert-start<=end who start end)
          (let* ([source-size (len v)]
                 [target-size (- end start)]
                 [slots (mk target-size)])
            (assert-bounds who end source-size v)
            (let loop ([k start] [tgt 0])
              (if (= k end)
                  slots
                  (begin
                    (upd! slots tgt (sub v k))
                    (loop (+ k 1) (+ tgt 1))))))]))
