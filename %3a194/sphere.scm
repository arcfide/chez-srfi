;;; SPDX-FileCopyrightText: 2020 Arvydas Silanskas
;;; SPDX-FileCopyrightText: 2020 Linas Vepštas
;;; SPDX-FileCopyrightText: 2024 Bradley J Lucier
;;; SPDX-License-Identifier: MIT
;;;
;;; Minimal changes for Chez by D. Guthrie, Glasgow, 2025
;;;
;;; sphere.scm
;;; Uniform distributions on a sphere, and a ball.
;;; Submitted for inclusion in srfi-194
;;;
;;; Algorithm based on BoxMeuller as described in
;;; http://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/
;;;
;;; make-sphere-generator N - return a generator of points uniformly
;;; distributed on an N-dimensional sphere.
;;; This implements the BoxMeuller algorithm, that is, of normalizing
;;; N+1 Gaussian random variables.

(define (make-sphere-generator arg)
  (cond
   ((and (integer? arg)
         (exact? arg)
         (positive? arg))
    (make-ellipsoid-generator* (make-vector (+ 1 arg) 1.0)))
   (else
    (error 'make-sphere-generator
	   "argument must be a positive exact integer" arg))))

(define (make-ellipsoid-generator arg)

  (define (return-error)
    (error 'make-ellipsoid-generator
	   "argument must be a vector of real numbers that are finite and positive when converted to inexact"
	   arg))

  (if (and (vector? arg)
           (vector-every real? arg))
      (let ((inexact-arg (vector-map inexact arg)))
        (if (vector-every (lambda (x)
                            (and (positive? x) (finite? x)))
                          inexact-arg)
            (make-ellipsoid-generator* inexact-arg)
            (return-error)))
      (return-error)))

;;; -----------------------------------------------
;;; Generator of points uniformly distributed on an N-dimensional ellipsoid.
;;;
;;; The `axes` should be a vector of floats, specifying the axes of the
;;; ellipsoid. The algorithm used is an accept/reject sampling algo,
;;; wherein the acceptance rate is proportional to the measure of a
;;; surface element on the ellipsoid. The measure is straight-forward to
;;; arrive at, and the 3D case is described by `mercio` in detail at
;;; https://math.stackexchange.com/questions/973101/how-to-generate-points-uniformly-distributed-on-the-surface-of-an-ellipsoid
;;;
;;; Note that sampling means that complexity goes as
;;; O(B/A x C/A x D/A x ...) where `A` is the shorest axis,
;;; and `B`, `C`, `D`, ... are the other axes. Maximum performance
;;; is achieved on spheres, which is the case used in make-ball-generator

(define (make-ellipsoid-generator* axes)
  (let ((gauss (make-normal-generator))
        (uniform (make-random-real-generator 0. 1.)) ;; should really be from a separate stream
        (min-axis (vector-fold min +inf.0 axes)))

    (define (sphere)
      (let* ((point
              (vector-map (lambda (_) (gauss)) axes))
             (norm-inverse
              (/ (sqrt (vector-fold (lambda (sum x)
                                      (+ sum (square x)))
                                    0.
                                    point)))))
        (vector-map (lambda (x) (* x norm-inverse)) point)))

    (define (ellipsoid-distance ray)
      (sqrt (vector-fold
             (lambda (sum x a) (+ sum (square (/ x a))))
             0. ray axes)))

    (define (keep point)
      (< (uniform)
         (* min-axis (ellipsoid-distance point))))

    (define (sample)
      (let ((point (sphere)))
        (if (keep point)
            point
            (sample))))

    (lambda ()
      (vector-map * (sample) axes))))



;;;-----------------------------------------------
;;; make-ball-generator N - return a generator of points
;;; inside an N-dimensional ball.  It's based on the algorithm in
;;;
;;; An Efficient Method for Generating Points
;;; Uniformly Distributed in Hyperellipsoids
;;; Jean Dezert and Christian Musso
;;; https://www.onera.fr/sites/default/files/297/C013_-_Dezert_-_YBSTributeMonterey2001.pdf
;;;
;;; which in turn is based on the Harman-Lacko-Voelker Dropped Coordinate method for
;;; generating points uniformly inside the unit ball in N dimensions.

(define (make-ball-generator arg)

  (define (return-error)
    (error 'make-ball-generator
	   "argument must be either an exact, positive, integer or a vector of real numbers that are positive and finite when converted to inexact"
	   arg))

  (if (and (integer? arg)
           (exact? arg)
           (positive? arg))
      (make-ball-generator* (make-vector arg 1.0))
      (if (and (vector? arg)
               (vector-every real? arg))
          (let ((inexact-arg (vector-map inexact arg)))
            (if (vector-every (lambda (x)
                                (and (positive? x)
                                     (finite? x)))
                              inexact-arg)
                (make-ball-generator* inexact-arg)
                (return-error)))
          (return-error))))

(define (make-ball-generator* axes)
  (let ((sphere-generator
         ;; returns vectors with (vector-length axes) + 2 elements
	 (make-sphere-generator (+ (vector-length axes) 1))))
    (lambda ()
      (let ((delayed (vector-copy (sphere-generator)
				  0 (vector-length axes))))
	;; returns vectors with (vector-length axes) elements
	(vector-map (lambda (el axis)
                      (* el axis))
                    delayed
		    axes)))))
