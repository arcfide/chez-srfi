; SPDX-FileCopyrightText: 2020 Arvydas Silanskas
; SPDX-FileCopyrightText: 2020 Linas Vepštas
; SPDX-License-Identifier: MIT
;
; zipf-test.scm
; Unit tests for the Zipf (zeta) distribution.
;
; Created by Linas Vepstas 10 July 2020
; Minimal changes for Chez by D. Guthrie, Glasgow, 2025
; Part of srfi-194

; srfi-133 compatible API
(define zvector-map vector-map)
(define z2vector-map vector-map)
(define zvector-fold vector-fold)

; srfi-43 compatible API
;(define (zvector-map f vec)
;  (vector-map (lambda (i x) (f x)) vec))
;(define (z2vector-map f veca vecb)
;  (vector-map (lambda (i x y) (f x y)) veca vecb))
;(define (zvector-fold f knil vec)
;  (vector-fold (lambda (i x elt) (f x elt)) knil vec))

; ------------------------------------------------------------------
; Debug utility for gnuplot graphing.
; You can use this to dump a vector to a tab-delimited file.
(define (vector-to-file vec filename)
  (define (write-vec)
    (for-each
      (lambda (i)
        (define index (+ i 1))
        (define val (vector-ref vec i))
        (display index)
        (display "  ")
        (display val)
        (newline))
      (iota (vector-length vec))))
  (with-output-to-file filename write-vec))

; ------------------------------------------------------------------
; Test harness for exploring the Zipf (Riemann/Hurwicz zeta) distribution
; parameter space.
;
;     (test-zipf TEST-ID NVOCAB ESS QUE REPS TOL)
;
; * TEST-ID -- String ID, for debugging.
; * The next three parameters are presented to the generator as
;     (make-zipf-generator NVOCAB ESS QUE)
;   ++  NVOCAB -- Size of vocabulary to select from.
;   ++  ESS -- The Riemann zeta "s" exponent.
;   ++  QUE -- The Hurwicz zeta "q" offset.
; * REPS -- The number of samples to draw from the distribution.
; * TOL -- The test tolerance, governing the expected failure rate.
;
; The algorithm is roughly:
;   Take REPS samples (make-zipf-generator NVOCAB ESS QUE)
;   Accumulate them into NVOCAB histogram bins.
;   Normalize counts to unit probability (i.e. divide by NVOCAB)
;
; The resulting distribution should uniformly converge to C/(k+q)^s
; for 1 <= k <= NVOCAB where C is a normalization constant.
;
; This compares the actual distribution to the expected convergent
; and reports an error if it is not within TOL of the convergent.
; i.e. it computes the Banach l_0 norm of (distribution-convergent)
; TOL is to be given in units of standard deviations. So, for example,
; setting TOL to 6 gives a six-sigma bandpass, allowing the tests to
; usually pass.
;
; The keyword here is "usually". The tail of the Zipf distribution is
; generally quite thin, and experiences a lot of statistical variation.
; There does not seem to be any published theory of exactly how the
; central limit theorm can be applied to estimate the distribution of
; the tail. The code below assumes an approximate gaussian distribution,
; computes it and tests it; however certain parameter ranges violate
; this assumption. Thus, this test will fail from time to time,
; depending on the luck of the draw. If it fails, it should be repeated
; once or twice, ubtil it passes.
;
(define (test-zipf-once TEST-ID NVOCAB ESS QUE REPS TOL)

  ; Default random number generator
  (define ZGEN make-zipf-generator)

  ; Bin-counter containing accumulated histogram.
  (define bin-counts
    (let ((bin-counts (make-vector NVOCAB 0)))
     ; Accumulate samples into the histogram.
     (generator-for-each
       (lambda (SAMP)
         (define offset (- SAMP 1))
         (vector-set! bin-counts offset (+ 1 (vector-ref bin-counts offset))))
       (gtake (ZGEN NVOCAB ESS QUE) REPS))
     bin-counts))

  ; Verify the distribution is within tolerance.
  ; This is written out long-hand for easier debuggability.

  ; Frequency is normalized to be 0.0 to 1.0
  (define frequency (zvector-map (lambda (n) (/ n REPS)) bin-counts))
  (define probility (zvector-map (lambda (n) (inexact n)) frequency))

  ; Sequence 1..NVOCAB
  (define seq (list->vector (iota NVOCAB 1)))

  ; Sequence  1/(k+QUE)^ESS
  (define inv-pow
    (zvector-map (lambda (k) (expt (+ k QUE) (- (inexact ESS)))) seq))

  ; Hurwicz harmonic number sum_1..NVOCAB 1/(k+QUE)^ESS
  (define hnorm
    (zvector-fold
      (lambda (sum cnt) (+ sum cnt)) 0 inv-pow))

  ; The expected distribution.
  (define expect
    (zvector-map (lambda (x) (/ x hnorm)) inv-pow))

  ; Convert to floating point.
  (define prexpect (zvector-map (lambda (x) (inexact x)) expect))

  ; The difference.
  (define diff (z2vector-map (lambda (x y) (- x y)) probility prexpect))

  ; Re-weight the tail by k^{s/2}. This seems give a normal error
  ; distribution. ... at least, for small q. Problems for large q
  ; and with undersampling; so we hack around that.
  (define err-dist
    (if (< 10 QUE) diff
        (z2vector-map (lambda (i x) (* x (expt (+ i 1) (* 0.5 ESS))))
                    (list->vector (iota (vector-length diff)))
                    diff)))

  ; Normalize to unit root-mean-square.
  (define rms (/ 1 (sqrt (* 2 3.141592653 REPS))))
  (define norm-dist (zvector-map (lambda (x) (/ x rms)) err-dist))

  ; Maximum deviation from expected distribution (l_0 norm)
  (define l0-norm
    (zvector-fold
      (lambda (sum x) (if (< sum (abs x)) (abs x) sum)) 0 norm-dist))

  ; The mean.
  (define mean
    (/ (zvector-fold (lambda (sum x) (+ sum x)) 0 norm-dist)
       NVOCAB))

  (define root-mean-square
    (sqrt (/ (zvector-fold (lambda (sum x) (+ sum (* x x))) 0 norm-dist)
             NVOCAB)))

  ; Test for uniform convergence.
  ; (test-assert TEST-ID (<= l0-norm TOL))
  (define tol-result (<= l0-norm TOL))

  ; Should not random walk too far away.
  ; Could tighten this with a proper theory of the error distribution.
  ; (test-assert TEST-ID (< (abs mean) 3))
  (define mean-result (< (abs mean) 3))
  ; I don't understand the error distribution ....
  ; (test-assert (and (< 0.4 root-mean-square) (< root-mean-square 1.5)))

  ; Sanity check. The total counts in the bins should be equal to REPS.
  ; If this fails, the test harness itself is broken.
  (test-assert TEST-ID
    (equal? REPS
            (zvector-fold
              (lambda (sum cnt) (+ sum cnt)) 0 bin-counts)))

  ; Utility debug printing
  ;(vector-to-file probility "probility.dat")
  ;(vector-to-file prexpect "prexpect.dat")
  ;(vector-to-file diff "diff.dat")

  (list tol-result mean-result))

; ------------------------------------------------------------------
; Sometimes the Zip test fails, due to random variation. It should
; pass, if attempted a second time. This gives it three chances.
; If it fails three times, then something bad is happening.
(define (test-zipf TEST-ID NVOCAB ESS QUE REPS TOL)

  ;; Three strikes, you're out.
  (define RETRY 3)

  (define num-failures
    (list-index
     (lambda (n)
       (define rc (test-zipf-once TEST-ID NVOCAB ESS QUE REPS TOL))
       (and (first rc) (second rc)))
     (iota RETRY)))

  ; `num-failures` will be #f if it failed each and every time.
  ;(if (number? num-failures)
  ;  (if (< 0 num-failures)
  ;    (format #t "Test '~A' out of bounds ~A times.\n" TEST-ID num-failures))
  ;  (format #t "Error: Test '~A' failed every time!\n" TEST-ID))

  ; Announce excessive repeated failures.
  (test-assert TEST-ID num-failures)
)

; ------------------------------------------------------------------
; Explore the parameter space.
(define (zipf-test-group)
  ; (test-begin "srfi-194-zipf")

  ; The unit test computes something that is "almost" a standard
  ; deviation for the error distribution. Except, maybe not quite,
  ; I don't fully understand the theory. So most tests seem to come
  ; in fine in well-under a six-sigma deviation, but some of the wilder
  ; parameter choices misbehave, so six-sigma doesn't always work.
  ; Also, when the number of bins is large, its easy to under-sample;
  ; some bins end up empty and the std-dev is thrown off as a result.
  ; Thus, the tolerance bounds below are hand-adjusted.
  (define six-sigma 6.0)

  (define hack-que 3.0)

  ; Zoom into s->1
  (test-zipf "zoom-1"   30 1.1     0 1000 six-sigma)
  (test-zipf "zoom-2"   30 1.01    0 1000 six-sigma)
  (test-zipf "zoom-3"   30 1.001   0 1000 six-sigma)
  (test-zipf "zoom-4"   30 1.0001  0 1000 six-sigma)
  (test-zipf "zoom-5"   30 1.00001 0 1000 six-sigma)

  (test-zipf "zoom-6"   30 (+ 1 1e-6)  0 1000 six-sigma)
  (test-zipf "zoom-8"   30 (+ 1 1e-8)  0 1000 six-sigma)
  (test-zipf "zoom-10"  30 (+ 1 1e-10) 0 1000 six-sigma)
  (test-zipf "zoom-12"  30 (+ 1 1e-12) 0 1000 six-sigma)
  (test-zipf "zoom-14"  30 (+ 1 1e-14) 0 1000 six-sigma)
  (test-zipf "zoom-inf" 30 1           0 1000 six-sigma)

  ; Verify improving uniform convergence
  (test-zipf "uniform-1" 30 1  0 10000   six-sigma)
  (test-zipf "uniform-2" 30 1  0 100000  six-sigma)

  ; Larger vocabulary
  (test-zipf "mid-voc-1" 300 1.1     0 10000 six-sigma)
  (test-zipf "mid-voc-2" 300 1.01    0 10000 six-sigma)
  (test-zipf "mid-voc-3" 300 1.001   0 10000 six-sigma)
  (test-zipf "mid-voc-4" 300 1.0001  0 10000 six-sigma)
  (test-zipf "mid-voc-5" 300 1.00001 0 10000 six-sigma)

  ; Larger vocabulary. Take more samples....
  (test-zipf "large-voc-1" 3701 1.1     0 40000 six-sigma)
  (test-zipf "large-voc-2" 3701 1.01    0 40000 six-sigma)
  (test-zipf "large-voc-3" 3701 1.001   0 40000 six-sigma)
  (test-zipf "large-voc-4" 3701 1.0001  0 40000 six-sigma)
  (test-zipf "large-voc-5" 3701 1.00001 0 40000 six-sigma)

  ; Huge vocabulary; few samples. Many bins will be empty,
  ; causing the std-dev to get large.
  (test-zipf "huge-voc-1" 43701 (+ 1 1e-6)  0 60000 9.5)
  (test-zipf "huge-voc-2" 43701 (+ 1 1e-7)  0 60000 9.5)
  (test-zipf "huge-voc-3" 43701 (+ 1 1e-9)  0 60000 9.5)
  (test-zipf "huge-voc-4" 43701 (+ 1 1e-12) 0 60000 9.5)
  (test-zipf "huge-voc-5" 43701 1           0 60000 9.5)

  ; Large s, small range
  (test-zipf "big-s-lo-1" 5 1.1     0 1000 six-sigma)
  (test-zipf "big-s-lo-2" 5 2.01    0 1000 six-sigma)
  (test-zipf "big-s-lo-3" 5 4.731   0 1000 six-sigma)
  (test-zipf "big-s-lo-4" 5 9.09001 0 1000 six-sigma)
  (test-zipf "big-s-lo-5" 5 13.45   0 1000 8.0)

  ; Large s, larger range. Most histogram bins will be empty
  ; so allow much larger error margins. There are excessively
  ; frequent large failures in this bunch.
  (test-zipf "bis-mid-1" 130 1.5     0 30000 six-sigma)
  (test-zipf "bis-mid-2" 130 2.03    0 30000 9.0)
  (test-zipf "bis-mid-3" 130 4.5     0 30000 36.0) ; This one is problematic
  (test-zipf "bis-mid-4" 130 6.66    0 30000 24.0)

  ; Verify that accuracy improves with more samples.
  (test-zipf "samp-bi-1" 129 1.1     0 10000 six-sigma)
  (test-zipf "samp-bi-2" 129 1.01    0 10000 six-sigma)
  (test-zipf "samp-bi-3" 129 1.001   0 10000 six-sigma)
  (test-zipf "samp-bi-4" 129 1.0001  0 10000 six-sigma)
  (test-zipf "samp-bi-5" 129 1.00001 0 10000 six-sigma)

  ; Non-zero Hurwicz parameter
  (test-zipf "hurw-1" 131 1.1     0.3    10000 six-sigma)
  (test-zipf "hurw-2" 131 1.1     1.3    10000 six-sigma)
  (test-zipf "hurw-3" 131 1.1     6.3    10000 six-sigma)
  (test-zipf "hurw-4" 131 1.1     20.23  10000 six-sigma)

  ; Negative Hurwicz parameter. Must be greater than branch point at -0.5.
  (test-zipf "hneg-1" 81 1.1     -0.1   1000 six-sigma)
  (test-zipf "hneg-2" 81 1.1     -0.3   1000 six-sigma)
  (test-zipf "hneg-3" 81 1.1     -0.4   1000 six-sigma)
  (test-zipf "hneg-4" 81 1.1     -0.499 1000 six-sigma)

  ; A walk into a stranger corner of the parameter space.
  (test-zipf "big-h-1" 131 1.1     41.483 10000 hack-que)
  (test-zipf "big-h-2" 131 2.1     41.483 10000 hack-que)
  (test-zipf "big-h-3" 131 6.1     41.483 10000 hack-que)
  (test-zipf "big-h-4" 131 16.1    41.483 10000 hack-que)
  (test-zipf "big-h-5" 131 46.1    41.483 10000 hack-que)
  (test-zipf "big-h-6" 131 96.1    41.483 10000 hack-que)

  ; A still wilder corner of the parameter space.
  (test-zipf "huhu-1" 131 1.1     1841.4 10000 hack-que)
  (test-zipf "huhu-2" 131 1.1     1.75e6 10000 hack-que)
  (test-zipf "huhu-3" 131 2.1     1.75e6 10000 hack-que)
  (test-zipf "huhu-4" 131 12.1    1.75e6 10000 hack-que)
  (test-zipf "huhu-5" 131 42.1    1.75e6 10000 hack-que)

  ; Lets try s less than 1
  (test-zipf "small-s-1" 35 0.9     0 1000 six-sigma)
  (test-zipf "small-s-2" 35 0.99    0 1000 six-sigma)
  (test-zipf "small-s-3" 35 0.999   0 1000 six-sigma)
  (test-zipf "small-s-4" 35 0.9999  0 1000 six-sigma)
  (test-zipf "small-s-5" 35 0.99999 0 1000 six-sigma)

  ; Attempt to force an overflow
  (test-zipf "ovfl-1" 437 (- 1 1e-6)  0 1000 six-sigma)
  (test-zipf "ovfl-2" 437 (- 1 1e-7)  0 1000 six-sigma)
  (test-zipf "ovfl-3" 437 (- 1 1e-9)  0 1000 six-sigma)
  (test-zipf "ovfl-4" 437 (- 1 1e-12) 0 1000 six-sigma)

  ; Almost flat distribution
  (test-zipf "flat-1" 36 0.8     0 1000 six-sigma)
  (test-zipf "flat-2" 36 0.5     0 1000 six-sigma)
  (test-zipf "flat-3" 36 0.1     0 1000 six-sigma)

  ; A visit to crazy-town -- increasing, not decreasing exponent
  (test-zipf "neg-s-1" 36 0.0     0 1000 six-sigma)
  (test-zipf "neg-s-2" 36 -0.1    0 1000 six-sigma)
  (test-zipf "neg-s-3" 36 -1.0    0 1000 six-sigma)
  (test-zipf "neg-s-4" 36 -3.0    0 1000 six-sigma)

  ; More crazy with some Hurwicz on top.
  (test-zipf "neg-shu-1" 16 0.0     0.5 1000 six-sigma)
  (test-zipf "neg-shu-2" 16 -0.2    2.5 1000 six-sigma)
  (test-zipf "neg-shu-3" 16 -1.3    10  1000 six-sigma)
  (test-zipf "neg-shu-4" 16 -2.9    100 1000 six-sigma)

  ; (test-end "srfi-194-zipf")
  )
