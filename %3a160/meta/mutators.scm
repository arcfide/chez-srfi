;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-numeric-vector-swap! sub upd!) vec i j)
  "wrap SRFI 160 `@vector-swap!' procedures"
  (let ([i@v (sub vec i)]
	[j@v (sub vec j)])
    (upd! vec i j@v)
    (upd! vec j i@v)))

(define/curried ((define-numeric-vector-unfold! len upd!) proc vec start end ini)
  "wrap SRFI 160 `@vector-unfold!' procedures"
  (assert-start-nat who start)
  (assert-end-nat who end)
  (assert-start<=end who start end)
  (assert-bounds who end (len vec) vec)
  (let loop ([i start] [seed ini])
    (when (< i end)
      (let-values ([(x next) (proc i seed)])
	(upd! vec i x)
	(loop (+ i 1) next)))))

(define/curried ((define-numeric-vector-unfold-right! len upd!) proc vec start end ini)
  "wrap SRFI 160 `@vector-unfold-right!' procedures"
  (assert-start-nat who start)
  (assert-end-nat who end)
  (assert-start<=end who start end)
  (assert-bounds who end (len vec) vec)
  (let loop ((i (- end 1)) (seed ini))
    (when (>= i start)
      (let-values (((x next) (proc i seed)))
	(upd! vec i x)
	(loop (- i 1) next)))))

(define/curried-case (define-numeric-vector-fill! unfold! len valid? type-of)
  "wrap SRFI 160 `@vector-fill!' procedures"
  (recur
   [(vec rep) (recur vec rep 0 (len vec))]
   [(vec rep start) (recur vec rep start (len vec))]
   [(vec rep start end)
    (assert/who who
      (valid? rep)
      (format-vector-type "repeating element ~~a cannot be contained within ~a" type-of)
      rep)
    (unfold!
     (lambda _ (values rep rep))
     vec start end #f)]))

;; (0 1 2 3 4 5)
;; swap 0 with 5
;; (5 1 2 3 4 0)
;; swap 1 with 4
;; (5 4 2 3 1 0)
;; swap 2 with 3
;; (5 4 3 2 1 0)
;; done beacuse we've reached index 3 (len / 2)

(define/curried-case (define-numeric-vector-reverse! swap! len)
  "Wrap SRFI 160 `@vector-reverse!' procedures. This is about reversing
specific portions of an element, so the treatment of start/end is somewhat
subtle.
Reversing with start 2 and end 2: zero elements to reverse (no change)
Reversing with start 2 and end 3: one element to reverse (no change)
Reversing with start 2 and end 2+N: at least two elements to reverse (changes)"
  (recur
   [(vec) (recur vec 0 (len vec))]
   [(vec start) (recur vec start (len vec))]
   [(vec start end)
    (assert-start-nat who start)
    (assert-end-nat who end)
    (assert-start<=end who start end)
    (assert-bounds who end (len vec) vec)
    (let loop ([i start] [j (- end 1)])
      (when (< i j)
	(swap! vec i j)
	(loop (+ i 1) (- j 1))))]))

(define/curried-case (define-numeric-vector-copy! unfold! sub len)
  "wrap SRFI 160 `@vector-copy!' procedures"
  (recur
   [(tgt tgt-start src) (recur tgt tgt-start src 0 (len src))]
   [(tgt tgt-start src src-start) (recur tgt tgt-start src src-start (len src))]
   [(tgt tgt-start src src-start src-end)
    (assert-start-nat who tgt-start "target")
    (assert-start-nat who src-start "source")
    (assert-end-nat who src-end "source")
    (assert-start<=end who src-start src-end "source")
    (let ([src-length (len src)]
	  [tgt-length (len tgt)])
      (assert/who who (<= src-end src-length)
                  "source end ~a exceeds length of source ~a"
                  src-end src)
      (assert/who who (<= tgt-start tgt-length)
                  "target start ~a exceeds length of target ~a"
                  tgt-start tgt)
      (let ([adjusted-tgt-end (+ tgt-start (- src-end src-start))]
	    [unfold-with (lambda (tgt-i src-i)
			   (values (sub src src-i) (+ src-i 1)))])
        (assert/who who (<= adjusted-tgt-end tgt-length)
                    "slice source overruns target by ~a elements"
                    (- adjusted-tgt-end tgt-length))
	(unfold! unfold-with
		 tgt
		 tgt-start
		 adjusted-tgt-end
		 src-start)))]))

;; reverse-copy! should be possible using an unfold-right! procedure
