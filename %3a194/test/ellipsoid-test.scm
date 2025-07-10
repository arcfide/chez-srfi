; SPDX-FileCopyrightText: 2020 Linas Vepštas
; SPDX-License-Identifier: MIT
;
; ellipsoid-test.scm
;
; Verify that the distribution of points on the surface of an
; ellipsoid is uniform.
;
; Test proceeds by taking 2-D slices through the ellipsoid, and
; verifying uniformity on that slice. Thus, the core test is for
; ellipses.
;
; Minimal changes for Chez by D. Guthrie, Glasgow, 2025

; Sort a list of 2D vectors of floats into clock-wise order.
; Assumes that `pts` is a list of 2D vectors of floats.
(define (clockwise pts)
	(sort pts (lambda (a b)
		(if (and (< 0 (vector-ref a 1)) (< 0 (vector-ref b 1)))
			(< (vector-ref b 0) (vector-ref a 0))
			(if (and (< (vector-ref a 1) 0) (< (vector-ref b 1) 0))
				(< (vector-ref a 0) (vector-ref b 0))
				(< (vector-ref b 1) (vector-ref a 1)))))))

; Verfiy that the routine above is not broken.
; Returns #t if it is OK.
(define (test-clockwise)
	(define  clock (list
		'#(1 1e-3) '#(0.8 0.2) '#(0.2 0.8)
		'#(0 1) '#(-0.2 0.8) '#(-0.8 0.2) '#(-1 1e-3)
		'#(-1 -1e-3) '#(-0.8 -0.2) '#(-0.2 -0.8)
		'#(0 -1) '#(0.2 -0.8) '#(0.8 -0.2) '#(1 -1e-3)))

	(equal? (clockwise clock) clock))

; Vector subtraction
; Example usage: (vector-diff '#( 2 3) '#(0.5 0.7))
(define (vector-sub a b)
	(vector-map (lambda (ea eb) (- ea eb)) a b))

; Newton differences - compute the difference between neighboring
; points. Assumes `pts` is a list of vectors.  Should be called with
; `rv` set to the null list. (tail-recursive helper)
(define (delta pts rv)
	(if (null? (cdr pts)) (reverse! rv)
		(delta (cdr pts) (cons (vector-diff (car pts) (cadr pts)) rv))))

; Compute sum of a list of numbers
(define (sum lst) (fold (lambda (x sum) (+ sum x)) 0 lst))

; Compute sum of squares of a list of numbers
(define (sumsq lst) (fold (lambda (x sum) (+ sum (* x x))) 0 lst))

(define (mean lst) (/ (sum lst) (length lst)))
(define (stddev lst)
	(define avg (mean lst))
	(sqrt (- (/ (sumsq lst) (length lst)) (* avg avg))))

; -----------------------------------------------------------
; Stuff for the complete elliptic integral
(define pi 3.14159265358979)

; factorial
(define (fact n rv)
	(if (zero? n) rv (fact (- n 1) (* n rv))))

; Double factorial
; https://en.wikipedia.org/wiki/Double_factorial
(define (double-fact n rv)
	(if (<= n 0) rv (double-fact (- n 2) (* n rv))))

; Complete elliptic integral per wikipedia, see the Ivorty& Bessel
; expansion. Here `a` and `b` are the axes.
; https://en.wikipedia.org/wiki/Ellipse
(define (complete-elliptic a b)
	(define rh (/ (- a b) (+ a b)))
	(define h (* rh rh))

	(define precision 1e-10)

	(define (ivory term n twon hn fact-n dfact-n sum)
		(if (< term precision) (+ sum term)
			; (format #t "yo n= ~A term=~A 2^n=~A h^n=~A n!=~A n!!=~A sum=~A\n"
			; n term twon hn fact-n dfact-n sum)
			(ivory
				(/ (* dfact-n dfact-n hn) (* twon twon fact-n fact-n))
				(+ n 1)
				(* 2 twon)
				(* h hn)
				(* (+ n 1) fact-n)
				(* (- (* 2 n) 1) dfact-n)
				(+ term sum))))

	(* pi (+ a b) (+ 1 (/ h 4)
		(ivory (/ (* h h) 64) 3 8 (* h h h) 6 3 0.0))))

; -----------------------------------------------------------

; Test that a list of points are well-distributed on an ellipse.
; Assumes that `points` is a list of 2D vectors of floats.
;
; Multiple tests are performed:
; 1) This sums the differences between points, i.e. measures the
;    perimeter, and verifies that the perimiter is as pexpected,
;    given by the complete elliptic integral.
; 2) Computes the RMS variation between the points, and verifies
;    that this is small.
; 3) (non-automated) dump differences to file and verify they look good.
;
(define (verify-ellipse points)
	; Place in sorted order.
	(define ordered-points (clockwise points))

	; Difference between neghboring points.
	(define diffs (delta ordered-points '()))

	; Compute the distances between neighboring points
	(define dists (map l2-norm diffs))

	; Sum of the intervals ... NOT including distance between
	; the very last and the very first points.
	(define perimeter (sum dists))

	; Find major and minor axes
	(define major
		(fold (lambda (MAJ x)
			(if (< MAJ x) x MAJ))
			0
			(map l2-norm points)))

	(define minor
		(fold (lambda (MIN x)
			(if (< MIN x) MIN x))
			1.0e308
			(map l2-norm points)))

	; The expected perimiter
	(define perim-exact (complete-elliptic major minor))

	; The normalized difference of measured and expected perimeters
	; Should almost always be less than ten, often less than two.
	; It should usually be positive, because we failed to count the
	; distance between the last and first point... and also an
	; O(delta^2) error cause this is Newton integration.
	(define error
		(abs (* (/ (- perim-exact perimeter) perim-exact) (length dists))))

	; If The points are normally eistributed, then the `dists` should
	; form a normal gaussian distribution, with unit stadndard
	; devviation. i.e. rms below should be near 1.0
	(define rms (/ (* (stddev dists) (length dists)) perim-exact))

	(format #t "Number of points: ~A\n" (length points))
	(format #t "Measured perimeter: ~A\n" perimeter)
	(format #t "Major and minor axes: ~A ~A\n" major minor)
	(format #t "Expected perimeter: ~A\n" perim-exact)
	(format #t "Relative error: ~A\n" error)
	(format #t "RMS error: ~A\n" rms)
	(newline)

	(test-assert (< error 12))
	(test-assert (< 0 error))
	(test-assert (< 0.97 rms))
	(test-assert (< 1.03 rms))
)

; ------------------------------------------------------
;
; Bonus points: uncomment the below, write results to a file
; and view the graph.  It should look sane.
; This is commented out because it is for reference nly,
; and does not need to be ported.
;
;;;(define (explore-ellipse points)
;;;	; Place in sorted order.
;;;	(define ordered-points (clockwise points))
;;;
;;;	; Difference between neghboring points.
;;;	(define diffs (delta ordered-points '()))
;;;
;;;	; Compute the distances between neighboring points
;;;	(define dists (map l2-norm diffs))
;;;
;;;	; Compute moving average
;;;	(define (moving-avg lst window)
;;;		(map (lambda (offset) (avg (take (drop lst offset) window)))
;;;			(iota (- (length lst) window))))
;;;
;;;	; Window 300 points wide. The moving average should do a
;;;   ; random walk around an average value of ellipsoid-perimeter
;;;   ; divided by the number of points. The walk should be normally
;;;	; distributed.  Should be translation-invariant; there should
;;;	; be no sine waves.
;;;	(define moving-300 (moving-avg dists 300))
;;;
;;;	(define (list-to-file lst filename)
;;;	(let ((outport (open-file filename "w")))
;;;		(fold
;;;			(lambda (x i) (format outport "~A	~A\n" i x) (+ i 1))
;;;			1
;;;			lst)
;;;		(close outport)))
;;;
;;;	(list-to-file moving-300 "moving.dat")
;;;)

; ------------------------------------------------------
; OK, now test some ellipses.

(define (sample gen)
	(map (lambda (x) (gen)) (iota 10000)))

(verify-ellipse (sample (make-ellipsoid-generator '#(2 10))))
(verify-ellipse (sample (make-ellipsoid-generator '#(2 10))))
(verify-ellipse (sample (make-ellipsoid-generator '#(2 10))))
(verify-ellipse (sample (make-ellipsoid-generator '#(2 10))))
(verify-ellipse (sample (make-ellipsoid-generator '#(10 2))))
(verify-ellipse (sample (make-ellipsoid-generator '#(3 8))))
(verify-ellipse (sample (make-ellipsoid-generator '#(9 44))))
(verify-ellipse (sample (make-ellipsoid-generator '#(9e3 4.4e3))))
(verify-ellipse (sample (make-ellipsoid-generator '#(9e6 4.4e6))))
(verify-ellipse (sample (make-ellipsoid-generator '#(0.03 0.16))))

; ------------------------------------------------------
; Now test higher-dimensional ellipsoids.
; This is done by slicing ellipses out of them. The slicing is
; approximate.

; Take a two-2d slice out of ellipsoid having `axes`. Thickness of the
; slice is `thickness`. Location of the slice is `where`.  Caution:
; may be extremely slow if a high-dimenional vector is given, or if
; thickness is too thin. Will infinite-loop if `where` is not inside
; the ellipsiod.  Returns an n-dimensional point, of which the first
; two coords are unconstrained.
;
; Usage:
;
; (define gen (make-slicer '#(2 10 6 4) 0.1  '#(0 0 2 1)))
; (gen)
(define (make-slicer axes thickness where)

	; Generator of points on n-dimensional ellipsoid
	(define elli (make-ellipsoid-generator axes))

	; return true if point is in the slice.
	(define (accept point)

		;; Take the slice off-center
		(define diff
			(vector-map (lambda (r s) (- r s)) point where))

		;; Return #t if the point is in the slice
		(define (ok vec)
			(vector-fold (lambda (pass coord idx)
				(or (< idx 2)
					(and pass (< (- 0 thickness) coord) (< coord thickness))))
				#t vec (iota (vector-length vec)) ))

		; Test
		(ok diff))

	; Keep trying until we have a point in the slice.
	(define (try)
		(define sample (elli))
		(if (accept sample) sample (try)))

	; Return the looper
	try)

; Like above but drops all but the first two coordinates
(define (two-d-slicer axes thickness where)
	(define sli (make-slicer axes thickness where))

	(lambda ()
		(define sample (sli))
		(vector (vector-ref sample 0) (vector-ref sample 1))))

(define (sample-1K gen)
	(map (lambda (x) (gen)) (iota 1000)))

; ------------------------------------------------------
; The higher-dimensional tests..

; These tests don't work very well, aren't very stable.
; The slice has to be really thin, otherwise one gets junky
; samples which don't pass unit tests. But thin slices take a huge
; amount of CPU time to run...
(verify-ellipse (sample-1K (two-d-slicer '#(2 8 10) 0.02 '#(0 0 0))))

(verify-ellipse (sample-1K (two-d-slicer '#(6 8 8) 0.04 '#(0 0 0))))
(verify-ellipse (sample-1K (two-d-slicer '#(6 8 12) 0.04 '#(0 0 0))))
(verify-ellipse (sample-1K (two-d-slicer '#(2 8 10) 0.04 '#(0 0 0))))

; This takes too long to run
; (verify-ellipse (sample-1K (two-d-slicer '#(5 8 10 7) 0.04 '#(0 0 0 0))))

; --------------------- the end ------------------------
; ------------------------------------------------------
