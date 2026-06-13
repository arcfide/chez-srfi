;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define-test-property ((define-neg-start-property op) vec start . rest)
  "Holds true if start is not non-negative"
  (apply op vec start rest)
  (irritants start)
  (message "start ~a is not a non-negative integer"))

(define-test-property ((define-neg-start-property/1 op) proc vec start . rest)
  "Holds true if start is not non-negative"
  (apply op proc vec start rest)
  (irritants start)
  (message "start ~a is not a non-negative integer"))

(define-test-property ((define-neg-end-property op) vec start end . rest)
  "Holds true if end is not non-negative"
  (apply op vec start end rest)
  (irritants end)
  (message "end ~a is not a non-negative integer"))

(define-test-property ((define-neg-end-property/1 op) proc vec start end . rest)
  "Holds true if end is not non-negative"
  (apply op proc vec start end rest)
  (irritants end)
  (message "end ~a is not a non-negative integer"))

(define-test-property ((define-bounds-property op) vec start end . rest)
  "Holds true if start is greater than end"
  (apply op vec start end rest)
  (irritants end start)
  (message "end ~a must be greater than or equal to start ~a"))

(define-test-property ((define-bounds-property/1 op) proc vec start end . rest)
  "Holds true if start is greater than end"
  (apply op proc vec start end rest)
  (irritants end start)
  (message "end ~a must be greater than or equal to start ~a"))

(define-test-property ((define-overflow-property op) vec start end . rest)
  "Holds true if end overflows vector"
  (apply op vec start end rest)
  (irritants end vec)
  (message "end ~a overflows ~a"))

(define-test-property ((define-overflow-property/1 op) proc vec start end . rest)
  "Holds true if end overflows vector"
  (apply op proc vec start end rest)
  (irritants end vec)
  (message "end ~a overflows ~a"))


#| in-place fill! |#

(define-test-property ((define-fill!/neg-start-property op) vec rep start end)
  "Holds true if start is not non-negative"
  (op vec rep start end)
  (irritants start)
  (message "start ~a is not a non-negative integer"))

(define-test-property ((define-fill!/neg-end-property op) vec rep start end)
  "Holds true if end is not non-negative"
  (op vec rep start end)
  (irritants end)
  (message "end ~a is not a non-negative integer"))

(define-test-property ((define-fill!/bounds-property op) vec rep start end)
  "Holds true if start is greater than end"
  (op vec rep start end)
  (irritants end start)
  (message "end ~a must be greater than or equal to start ~a"))

(define-test-property ((define-fill!/overflow-property op) vec rep start end)
  "Holds true if end overflows vector"
  (op vec rep start end)
  (irritants end vec)
  (message "end ~a overflows ~a"))

#| in-place swap! |#

(define/curried ((define-swap!-property swap! copy ->list) v i j)
  "define property to test `@vector-swap!' normative behaviour"
  (let ([kept (copy v)])
    (swap! v i j)
    (swap! v i j)
    (equal? (->list v) (->list kept))))

(define-test-property ((define-swap!/oob-pos1-property swap!) v i j)
  "define property to test out-of-bounds `@vector-swap!' index at position 1"
  (swap! v i j)
  (irritants i v)
  (message "~a is not a valid index for ~a"))

(define-test-property ((define-swap!/oob-pos2-property swap!) v i j)
  "define property to test out-of-bounds `@vector-swap!' index at position 2"
  (swap! v i j)
  (irritants j v)
  (message "~a is not a valid index for ~a"))

(define-test-property ((define-swap!/non-pos1-property swap!) v i j)
  "define property to test invalid `@vector-swap!' index at position 1"
  (swap! v i j)
  (irritants i)
  (message "index ~a is not a non-negative integer"))

(define-test-property ((define-swap!/non-pos2-property swap!) v i j)
  "define property to test invalid `@vector-swap!' index at position 2"
  (swap! v i j)
  (irritants j)
  (message "index ~a is not a non-negative integer"))

#| in-place unfold! |#

(define/curried ((define-unfold!-property exact? unfold! sub ->list)
                   v start end)
  "define property to test `@vector-unfold!' normative behaviour"
  (let* ([slice (fill-within exact?
                             (->list v) start end
			     (lambda (k) (sqrt-nearest k))
			     identity)]
         [seed (if exact? 0 0.0)])
    (unfold! (lambda (k x)
               (if exact?
                   (values (sqrt-nearest k) seed)
                   (values (exact->inexact (sqrt-nearest k)) seed)))
	     v start end seed)
    (list= = slice
	    (->list v))))

(define/curried ((define-unfold!-property/2 exact? unfold! sub ->list)
                   v start end)
  "Define property to test `@vector-unfold!' normative behaviour. This time, use both the index and the value."
  (let ([slice (fill-within exact?
                            (->list v) start end
			    (lambda (k) (sqrt-nearest (- k (sub v k))))
			    identity)]
        [seed (if exact? 0 0.0)])
    (unfold! (lambda (k x)
               (if exact?
                   (values (sqrt-nearest (- k x)) seed)
                   (values (exact->inexact (sqrt-nearest (- k x))) seed)))
	     v start end seed)
    (list= = slice
	   (->list v))))

#| in-place fill! |#

(define/curried ((define-fill!-property exact? fill! ->list)
                   v rep start end)
  "define property to test `@vector-fill!' normative behaviour"
  (let* ([rep (if exact? rep (exact->inexact rep))]
         [slice (fill-within exact?
                             (->list v) start end
			     (lambda (_) rep) identity)])
    (fill! v rep start end)
    (if exact?
        (list= = slice (->list v))
        (list= (lambda (p q) (< (magnitude (- p q)) 0.1))
               slice
               (->list v)))))

#| in-place reverse!

For some arbitary start -> end:
1. Transform absolute index k into its position on the sub-list,
   i.e. subtract k from start.
2. Get the sub-list max, which is the difference between end and start.
3. Work out the index that k will be swapped with,, i.e. subtract sub-list
   max from k's position on sub-list.
4. Finally, transform this relative index back into an absolute index, and
subscript the vector. |#

(define/curried ((define-reverse!-property exact? reverse! len sub ->list)
                   v start end)
  "define property to test `@vector-reverse!' normative behaviour"
  (let* ([absolute-max (len v)]
	 [sub-list-max (- end start)]
	 [swp (lambda (k)
		(let* ([relative-k (- k start)]
		       [swap-with-k (- sub-list-max relative-k 1)]
		       [swap-absolute (+ swap-with-k start)])
		  (sub v swap-absolute)))]
	 [slice (fill-within exact? (->list v) start end swp identity)])
    ;;
    (reverse! v start end)
    (equal? slice
		(->list v))))

#| in-place copy!

`fill-within' is being run on the *target*, but it's not being run on the source start and end.
Instead, it's being run from the target start, to target start + length of source slice.
The length of the source slice is, source end minus source start.
Start by getting the relative position on the *target* like for `reverse!'.
The relative position on the target, is used to get the relative position on the source.
The absolute position on the source is just the source relative position plus source start.

The constraint is that given a source end and a source start,
don't exceed the length of the target vector, which otherwise
has no restriction on where to stop.

The way this works is to take a *slice* of the source vector.
This is of length source end minus source start.
The restriction on this length is that it cannot be more than target length minus start.
So the slice length can be generated simply as (random (- (len tgt) tgt-start))
Using this, it's possible to get the actual source end, and the actual target end.
|#

(define/curried ((define-copy!-property exact? copy! len sub ->list)
                         tgt tgt-start src src-start)
  "define property to test `@vector-copy!' normative behaviour"
  (let* ([slice-length
	  (random (min (- (len tgt) tgt-start)
		       (- (len src) src-start)))]
	 [tgt-end (+ tgt-start slice-length)]
	 [src-end (+ src-start slice-length)]
	 [from-tgt
	  (lambda (k)
	    (let* ([tgt-rel-k (- k tgt-start)]
		   [src-abs-k (+ src-start tgt-rel-k)])
	      (sub src src-abs-k)))]
	 [slice (fill-within exact? (->list tgt) tgt-start tgt-end from-tgt identity)])
    ;;
    (copy! tgt tgt-start src src-start src-end)
    (equal? slice
	    (->list tgt))))

(define/curried ((define-reverse-copy!-property exact? rcopy! len sub ->list)
                         tgt tgt-start src src-start)
  "define property to test `@vector-reverse-copy!' normative behaviour"
  (let* ([slice-length
	  (random (min (- (len tgt) tgt-start)
		       (- (len src) src-start)))]
	 [tgt-end (+ tgt-start slice-length)]
	 [src-end (+ src-start slice-length)]
	 [from-tgt
	  (lambda (k)
	    (let* ([tgt-rel-k (- k tgt-start)]
		   [swap-with-src-k (- slice-length tgt-rel-k 1)]
		   [swap-absolute (+ swap-with-src-k src-start)])
	      (sub src swap-absolute)))]
	 [slice
	  (fill-within exact? (->list tgt) tgt-start tgt-end from-tgt identity)])
    ;;
    (rcopy! tgt tgt-start src src-start src-end)
    (equal? slice
	    (->list tgt))))

(define-test-property ((define-copy!/neg-tgt-start-property copy!)
                         tgt tgt-start src src-start src-end)
  "Holds true if target start is not a non-negative integer"
  (copy! tgt tgt-start src src-start src-end)
  (irritants tgt-start)
  (message "target start ~a is not a non-negative integer"))

(define-test-property ((define-copy!/neg-src-start-property copy!)
                         tgt tgt-start src src-start src-end)
  "Holds true if source start is not a non-negative-integer"
  (copy! tgt tgt-start src src-start src-end)
  (irritants src-start)
  (message "source start ~a is not a non-negative integer"))

(define-test-property ((define-copy!/neg-src-end-property copy!)
                         tgt tgt-start src src-start src-end)
  "Holds true if source end is not a non-negative integer"
  (copy! tgt tgt-start src src-start src-end)
  (irritants src-end)
  (message "source end ~a is not a non-negative integer"))

(define-test-property ((define-copy!/tgt-start-property copy!)
                         tgt tgt-start src src-start src-end)
  "Holds true if target start is out of range"
  (copy! tgt tgt-start src src-start src-end)
  (irritants tgt-start tgt)
  (message "target start ~a exceeds length of target ~a"))

(define-test-property ((define-copy!/src-end-property copy!)
                         tgt tgt-start src src-start src-end)
  "Holds true if the source end is out of range"
  (copy! tgt tgt-start src src-start src-end)
  (irritants src-end src)
  (message "source end ~a exceeds length of source ~a"))

(define-test-property ((define-copy!/overruns-property copy! len)
                         tgt tgt-start src src-start src-end)
  "Holds true if source end is at most the length of the source, but if in-place copy overruns the target."
  (copy! tgt tgt-start src src-start src-end)
  (irritants (- (+ tgt-start (- src-end src-start)) (len tgt)))
  (message "slice source overruns target by ~a elements"))

(define-test-property ((define-copy!/non-src-property copy!)
                         tgt tgt-start src src-start src-end)
  "Holds true if source is not a valid vector"
  (copy! tgt tgt-start src src-start src-end)
  (irritants src)
  (message (format "~~a is not of type ~a" type-of)))

(define-test-property ((define-copy!/bounds-property copy!) tgt tgt-start src src-start src-end)
  "Holds true if bounds are flipped"
  (copy! tgt tgt-start src src-start src-end)
  (irritants src-end src-start)
  (message "source end ~a must be greater than or equal to source start ~a"))

(define (test-copy!-equiv name expect test-equiv copy copy! tgt tgt-start src src-start src-end)
  "Helper procedure for manual cases, which actually calls persistent copy as a precondition"
  (let ([tgt/c (copy tgt)])
    (copy! tgt/c tgt-start src src-start src-end)
    (test-equiv name expect tgt/c)))
