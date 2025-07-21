;;; SPDX-FileCopyrightText: 2020 Arvydas Silanskas
;;; SPDX-FileCopyrightText: 2024 Bradley J Lucier
;;; SPDX-License-Identifier: MIT
;;;
;;; Minimal changes for Chez by D. Guthrie, Glasgow, 2025
;;;
;;; Take REPS samples from unit sphere, verify random distribution.
;;;
;;; This test checks that:
;;; * Every sample has unit length, within numerical tolerance.
;;; * The REPS samples are uniformly distributed.
;;; * Rotations of the REPS samples are uniformly distributed.

(define (test-sphere sphereg dim-sizes REPS rotate?)
  (define random-int (random-source-make-integers (current-random-source)))
  (define random-real (random-source-make-reals (current-random-source)))
  (define N (- (vector-length dim-sizes) 1))

  ;; Fix list of samples
  (define samples
    (let ((taken (gtake sphereg REPS)))
      (generator->list taken)))

  (define (l2-norm VEC)
    (sqrt (vector-fold
           (lambda (sum x l)
	     (+ sum (/ (* x x)
                       (* l l))))
            0
            VEC
            dim-sizes)))

  ;; Rotate the j'th amnd k'th coordinates of a vector VEC
  ;; by cosine co and sine si
  (define (pair-rot VEC j k co si)
    (define oj (vector-ref VEC j))
    (define ok (vector-ref VEC k))
    (define nj (+ (* co oj) (* si ok)))
    (define nk (+ (* (- si) oj) (* co ok)))
    (list->vector
      (map (lambda (index)
             (cond
               ((= index j) nj)
               ((= index k) nk)
               (else (vector-ref VEC index))))
           (iota (vector-length VEC)))))

  ;; Apply a random rotation to a collection of vectors
  (define how-many-rots (if (< 10 N) 10 N))

  (define (arb-rot VEC-LIST)
    (define j (random-int N))
    (define k (+ j 1 (random-int (- N j))))
    (define theta (* 3.14 (random-real)))
    (define co (cos theta))
    (define si (sin theta))
    (define rvl
      (map (lambda (vec)
             (pair-rot vec j k co si))
           VEC-LIST))
    (if (not (= 0 (random-int how-many-rots)))
        (arb-rot rvl)
        rvl))

  ;; Expect a vector approaching zero. That is, each individual
  ;; coordinate should be uniformly randomly distributed in the
  ;; interval [-1,1]. The sum of REPS samples of these should
  ;; converge to zero. The standard deviation of a uniform
  ;; distribution is sqrt(REPS/12).
  ;; https://en.wikipedia.org/wiki/Continuous_uniform_distribution
  ;; So setting max bound of 9 stddev should allow it to usually
  ;; pass.
  (define (converge-to-zero samples)
    (fold-left (lambda (acc sample)
		 (vector-map! + acc sample)
		 acc)
               (make-vector REPS 0.0)
               samples))

  (define (should-be-zero samples)
    (l2-norm (converge-to-zero samples)))

  (define (norm-should-be-zero samples)
    (/ (should-be-zero samples) (sqrt (/ REPS 12.0))))

  (define (check-zero samples)
    (define num-stddev 9.0)
    (define zz (norm-should-be-zero samples))
    (test-assert (< zz num-stddev)))

  ;; maximum allowed tolerance for radius deviation
  (define EPS (* 2e-15 (sqrt N)))

  ;; Each individual sphere radius should be 1.0 to within float
  ;; tolerance.
  (for-each
   (lambda (SAMP)
     (test-approximate 1.0 (l2-norm SAMP) EPS))
   samples)

  ;; The distribution should be zero
  (check-zero samples)

  ;; Rotate wildly. Should still be uniform.
  (when rotate?
    (for-each
      (lambda (junk) (check-zero (arb-rot samples)))
      (make-list 12))))

(define (test-ball ballg dim-sizes)
  (define (l2-norm VEC)
    (sqrt (vector-fold
            (lambda (sum x l) (+ sum (/ (* x x)
                                        (* l l))))
            0
            VEC
            dim-sizes)))

  (define (test-ball-generates-on-radius radius err)
    (test-assert
      (generator-any
        (lambda (vec)
          (define n (l2-norm vec))
          (and (> n (- radius err))
               (< n (+ radius err))))
        (gtake ballg 10000))))

  (define (test-ball-avg-zero N)
    (define vec-sum
      (generator-fold
        (lambda (vec acc)
          (vector-map + vec acc)
	  vec)
        (make-vector (vector-length dim-sizes) 0.0)
        (gtake ballg N)))
    (define avg-vec
      (vector-map
        (lambda (e) (/ e N))
        vec-sum))
    (define n (l2-norm avg-vec))
    (test-assert (< n 1)))

  (test-ball-generates-on-radius 0.0 0.1)
  (test-ball-generates-on-radius 0.5 0.1)
  (test-ball-generates-on-radius 1.0 0.1)

  (test-ball-avg-zero 5000))

  ;; Number of standard deviation difference from the expected value
  ;; before we say a test failed.  If set to 2, then one out of
  ;; 20 tests will fail even if the code is correct.  Setting it to
  ;; 3 means that only one out of a 1000 tests will fail even if the
  ;; code is correct.

  (define STDs 3)

  (define (test-ellipsoid a b N)

    (define epsilon 1e-10)

    (define pi (* 4 (atan 1)))

    (define g (make-ellipsoid-generator (vector a b)))

    (define points (generator->list (gtake g N)))

    ;; The points on the "top" of the ellipse, with
    ;; x between -a/2 and a/2

    (define top
      (filter (lambda (v)
                (and (<  (- (/ a 2)) (vector-ref v 0) (/ a 2))
                     (<  0 (vector-ref v 1))))
              points))

    ;; The points on the "right" of the ellipse, with
    ;; y between -b/2 and b/2

    (define right
      (filter (lambda (v)
                (and (< (- (/ b 2)) (vector-ref v 1) (/ b 2))
                     (< 0 (vector-ref v 0))))
              points))

    (define (arc-length a b)
      ;; parametrization: (a\cos t, b\sin t)
      ;; this is the norm of the derivative
      (lambda (t)
        (sqrt (+ (square (* a (sin t)))
                 (square (* b (cos t)))))))

    (define (simpsons-rule f t0 t1 N)
      ;; O(Delta^4) numerical integration
      ;; integrate f between t0 and t1 with N intervals
      (let* ((Delta (/ (- t1 t0) N))
             (sum1 (do ((i 0 (+ i 1))
                        (sum 0. (+ sum
                                   (f (+ t0 (* i Delta)))
                                   (f (+ t0 (* (+ i 1) Delta))))))
                       ((= i N) sum)))
             (sum2 (do ((i 0 (+ i 1))
                        (sum 0. (+ sum (f (+ t0 (* (+ i 0.5) Delta))))))
                       ((= i N) sum))))
        (* Delta (/ (+ (* 4 sum2) sum1) 6))))

    (define p-right
      (/ (simpsons-rule (arc-length a b) (- (* pi 1/6)) (* pi 1/6) 100)
         (simpsons-rule (arc-length a b) 0 (* pi 2) 100)))

    (define p-top
      (/ (simpsons-rule (arc-length a b) (* pi 1/3) (* pi 2/3) 100)
         (simpsons-rule (arc-length a b) 0 (* pi 2) 100)))

    ;; test that they're all more-or-less on the ellipse

    (test-assert (every (lambda (p)
                          (< (abs (- (sqrt (+ (square (/ (vector-ref p 0) a))
                                              (square (/ (vector-ref p 1) b))))
                                     1))
                             epsilon))
                        points))

    (for-each (lambda (p m)
                ;; p = probability of landing in arc, m = measured number
                (test-assert (< (abs (- (* p N) m))
                                (* STDs (sqrt (* N p (- 1 p)))))))
              (list p-right p-top)
              (map length (list right top)))
    )

  (define (test-ellipse a b N)

    ;; This test with two-dimensional ellipses stands in for all
    ;; dimensions, as the code to generate points is independent
    ;; of dimension

    (define pi (* 4 (atan 1)))

    (define interior-points
      (generator->list (gtake (make-ball-generator (vector a b)) N)))

    (define (in-ellipse? point)
      (< (+ (square (/ (vector-ref point 0) a))
            (square (/ (vector-ref point 1) b)))
         1))

    ;; Find points inside rectangles inside various parts
    ;; of the ellipse

    (define center
      (filter (lambda (v)
                (and (< (- (/ a 4))
                        (vector-ref v 0)
                        (/ a 4))
                     (< (- (/ b 4))
                        (vector-ref v 1)
                        (/ b 4))))
              interior-points))

    (define right-x
      ;; (right-x b/4) is on the ellipse
      (* a (sqrt 15/16)))

    (define right
      (filter (lambda (v)
                (and (< (- right-x (/ a 2))
                        (vector-ref v 0)
                        right-x)
                     (< (- (/ b 4))
                        (vector-ref v 1)
                        (/ b 4))))
              interior-points))

    (define top-y
      ;; (a/4 top-y) is on the ellipse
      (* b (sqrt 15/16)))

    (define top
      (filter (lambda (v)
                (and (< (- (/ a 4))
                        (vector-ref v 0)
                        (/ a 4))
                     (< (- top-y (/ b 2))
                        (vector-ref v 1)
                        top-y)))
              interior-points))

    ;; p is he fraction of the area in the ellipse
    ;; contained in the rectangles (it's all the same).

    #;(define p
    (/ (* (/ a 2) (/ b 2))
    (* pi a b)))

    (define p (/ (* 4 pi)))

    ;; check that all the points are truly inside the ellipse.
    (test-assert (every in-ellipse? interior-points))

    (for-each (lambda (p m)
                ;; p = probability of landing in rectangle,
                ;; m = measured number of events
                (test-assert (< (abs (- (* p N) m))
                                (* STDs (sqrt (* N p (- 1 p)))))))
              (list p p p)
              (map length (list center right top))))
