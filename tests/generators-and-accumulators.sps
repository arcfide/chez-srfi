(import
 (rnrs)
 (only (chezscheme) bytevector with-input-from-string)
 (only (srfi :1 lists) unfold)
 (only (srfi :141 integer-division) truncate/)
 (srfi :158 generators-and-accumulators)
 (srfi :64 testing))

(test-begin "generators")
;; generators/constructors

(define test-000
  (test-equal "generator-000" '() (generator->list (generator))))

(define test-001
  (test-equal "generator-001" '(1 2 3) (generator->list (generator 1 2 3))))

(define test-001-1
  (test-equal "generator-001-1"
               '(1 2 3 1 2)
               (generator->list (circular-generator 1 2 3) 5)))

(define test-002
  (test-equal "generator-002" '(8 9 10) (generator->list (make-iota-generator 3 8))))

(define test-003
  (test-equal "generator-003" '(8 10 12) (generator->list (make-iota-generator 3 8 2))))

(define test-004
  (test-equal "generator-004" '(3 4 5 6) (generator->list (make-range-generator 3) 4)))

(define test-005
  (test-equal "generator-005" '(3 4 5 6 7) (generator->list (make-range-generator 3 8))))

(define test-006
  (test-equal "generator-006" '(3 5 7) (generator->list (make-range-generator 3 8 2))))

(define test-007
  (let ()
    (define g
      (make-coroutine-generator
       (lambda (yield) (let loop ((i 0))
                         (when (< i 3) (yield i) (loop (+ i 1)))))))
    (test-equal "generator-007" '(0 1 2) (generator->list g))))

(define test-008
  (test-equal "generator-008" '(1 2 3 4 5) (generator->list (list->generator '(1 2 3 4 5)))))

(define test-009
  (test-equal "generator-009" '(1 2 3 4 5) (generator->list (vector->generator '#(1 2 3 4 5)))))

(define test-010
  (test-equal "generator-010" '#(0 0 1 2 4)
         (let ((v (make-vector 5 0)))
           (generator->vector! v 2 (generator 1 2 4))
           v)))

(define test-011
  (test-equal "generator-011" '(5 4 3 2 1) (generator->list (reverse-vector->generator '#(1 2 3 4 5)))))

(define test-012
  (test-equal "generator-012" '(#\a #\b #\c #\d #\e) (generator->list (string->generator "abcde"))))

(define test-013
  (test-equal "generator-013" '(10 20 30) (generator->list (bytevector->generator (bytevector 10 20 30)))))

(define test-014
  (let ()
    (define (for-each-digit proc n)
      (when (> n 0)
        (let-values (((div rem) (truncate/ n 10)))
          (proc rem)
          (for-each-digit proc div))))
    (test-equal "generator-014" '(5 4 3 2 1) (generator->list
                                              (make-for-each-generator for-each-digit
                                                                       12345)))))

(define test-015
  (test-equal "generator-015" '(0 2 4 6 8 10) (generator->list
                          (make-unfold-generator
                           (lambda (s) (> s 5))
                           (lambda (s) (* s 2))
                           (lambda (s) (+ s 1))
                           0))))

;; generators/operators

(define test-016
  (test-equal "generator-016" '(a b 0 1) (generator->list (gcons* 'a 'b (make-range-generator 0 2)))))

(define test-017
  (test-equal "generator-017" '(0 1 2 0 1) (generator->list (gappend (make-range-generator 0 3)
                                                (make-range-generator 0 2)))))

(define test-018
  (test-equal "generator-018" '() (generator->list (gappend))))

(define test-019
  (let ()
    (define g1 (generator 1 2 3))
    (define g2 (generator 4 5 6 7))
    (define (proc . args) (values (apply + args) (apply + args)))
    (test-equal "generator-019" '(15 22 31) (generator->list (gcombine proc 10 g1 g2)))))

(define test-020
  (test-equal "generator-020" '(1 3 5 7 9) (generator->list (gfilter
                                        odd?
                                        (make-range-generator 1 11)))))

(define test-021
  (test-equal "generator-021" '(2 4 6 8 10) (generator->list (gremove
                                         odd?
                                         (make-range-generator 1 11)))))

(define g (make-range-generator 1 5))

(define test-022
  (test-equal "generator-022" '(1 2 3) (generator->list (gtake g 3))))

(define test-023
  (test-equal "generator-023" '(4) (generator->list g)))

(define test-024
  (test-equal "generator-024" '(1 2) (generator->list (gtake (make-range-generator 1 3) 3))))

(define test-025
  (test-equal "generator-025" '(1 2 0) (generator->list (gtake (make-range-generator 1 3) 3 0))))

(define test-026
  (test-equal "generator-026" '(3 4) (generator->list (gdrop (make-range-generator 1 5) 2))))

(define g2 (make-range-generator 1 5))

(define (small? x) (< x 3))

(define test-027
  (test-equal "generator-027" '(1 2) (generator->list (gtake-while small? g2))))

(define g3 (make-range-generator 1 5))

(define test-028
  (test-equal "generator-028" '(3 4) (generator->list (gdrop-while small? g3))))

(define test-029
  (test-equal "generator-029" '() (generator->list (gdrop-while (lambda args #t) (generator 1 2 3)))))

(define test-030
  (test-equal "generator-030" '(0.0 1.0 0 2) (generator->list (gdelete 1
                                                  (generator 0.0 1.0 0 1 2)))))

(define test-031
  (test-equal "generator-031" '(0.0 0 2) (generator->list (gdelete 1
                                              (generator 0.0 1.0 0 1 2)
                                              =))))

(define test-032
  (test-equal "generator-032" '(a c e) (generator->list (gindex (list->generator '(a b c d e f))
                                           (list->generator '(0 2 4))))))

(define test-033
  (test-equal "generator-033" '(a d e) (generator->list (gselect (list->generator '(a b c d e f))
                                            (list->generator '(#t #f #f #t #t #f))))))

(define test-034
  (test-equal "generator-034" '(1 2 3) (generator->list (gdelete-neighbor-dups
                                    (generator 1 1 2 3 3 3)
                                    =))))

(define test-035
  (test-equal "generator-035" '(1) (generator->list (gdelete-neighbor-dups
                                (generator 1 2 3)
                                (lambda args #t)))))

(define test-036
  (test-equal "generator-036" '(1 2 3 a b c)
         (generator->list
          (gflatten (generator '(1 2 3) '(a b c))))))

(define test-037
  (test-equal "generator-037" '((1 2 3) (4 5 6) (7 8))
         (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3))))

(define test-038
  (test-equal "generator-038" '((1 2 3) (4 5 6) (7 8 0))
         (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3 0))))

(define test-039
  (test-equal "generator-039" '(1 2 3)
         (generator->list (gmerge < (generator 1 2 3)))))

(define test-040
  (test-equal "generator-040" '(1 2 3 4 5 6)
         (generator->list (gmerge < (generator 1 2 3) (generator 4 5 6)))))

(define test-041
  (test-equal "generator-041" '(1 2 3 4 4 5 6)
         (generator->list (gmerge <
                                  (generator 1 2 4 6)
                                  (generator)
                                  (generator 3 4 5)))))

(define test-042
  (test-equal "generator-042" '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
         (generator->list (gmerge <
                                  (generator 1 10 11)
                                  (generator 2 9 12)
                                  (generator 3 8 13)
                                  (generator 4 7 14)
                                  (generator 5 6 15)))))

(define test-043
  ;; check the tie-break rule
  (test-equal "generator-043" '((1 a) (1 e) (1 b) (1 c) (1 d))
         (generator->list (gmerge (lambda (x y) (< (car x) (car y)))
                                  (generator '(1 a) '(1 e))
                                  (generator '(1 b))
                                  (generator '(1 c) '(1 d))))))

(define test-044
  (test-equal "generator-044" '(-1 -2 -3 -4 -5)
         (generator->list (gmap - (generator 1 2 3 4 5)))))

(define test-045
  (test-equal "generator-045" '(7 9 11 13)
         (generator->list (gmap +
                                (generator 1 2 3 4 5)
                                (generator 6 7 8 9)))))

(define test-046
  (test-equal "generator-046" '(54 140 264)
         (generator->list (gmap *
                                (generator 1 2 3 4 5)
                                (generator 6 7 8)
                                (generator 9 10 11 12 13)))))

(define test-047
  (test-equal "generator-047" '(a c e g i)
         (generator->list
          (gstate-filter
           (lambda (item state) (values (even? state) (+ 1 state)))
           0
           (generator 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))))

;; generators/consumers

(define test-048
  ;; no test-equal for plain generator->list (used throughout)
  (test-equal "generator-048" '(1 2 3) (generator->list (generator 1 2 3 4 5) 3)))

(define test-049
  (test-equal "generator-049" '(5 4 3 2 1) (generator->reverse-list (generator 1 2 3 4 5))))

(define test-050
  (test-equal "generator-050" '#(1 2 3 4 5) (generator->vector (generator 1 2 3 4 5))))

(define test-051
  (test-equal "generator-051" '#(1 2 3) (generator->vector (generator 1 2 3 4 5) 3)))

(define test-052
  (test-equal "generator-052" "abc" (generator->string (generator #\a #\b #\c))))

(define test-052-1
  (test-equal "generator-052-1"
              '(e d c b a . z)
              (with-input-from-string "a b c d e"
                (lambda () (generator-fold cons 'z read)))))

(define test-053
  (test-equal "generator-053" 6 (let ()
             (define n 0)
             (generator-for-each (lambda values (set! n (apply + values)))
                                 (generator 1) (generator 2) (generator 3))
             n)))

(define test-054
  (test-equal "generator-054" '(6 15)
         (generator-map->list (lambda values (apply + values))
                              (generator 1 4) (generator 2 5) (generator 3 6))))

(define test-055
  (test-equal "generator-055" 3 (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5))))

(define test-056
  (test-equal "generator-056" 2 (generator-count odd? (make-range-generator 1 5))))

(define test-057
  (test-equal "generator-057" #t
         (let ()
           (define g (make-range-generator 2 5))
           (and
            (equal? #t (generator-any odd? g))
            (equal? '(4) (generator->list g))))))

(define test-058
  (test-equal "generator-058" #t
         (let ()
           (define g (make-range-generator 2 5))
           (and
            (equal? #f (generator-every odd? g))
            (equal? '(3 4) (generator->list g))))))

(define test-059
  (test-equal "generator-059" '(#\a #\b #\c)
         (generator-unfold (make-for-each-generator string-for-each "abc") unfold)))

(test-end "generators")

(test-begin "accumulators")
;; accumulators

(define test-060
  (test-equal "accumulator-060" -8
         (let ((a (make-accumulator * 1 -)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-061
  (test-equal "accumulator-061" 3
         (let ((a (count-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-062
  (test-equal "accumulator-062" '(1 2 4)
         (let ((a (list-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-063
  (test-equal "accumulator-063" '(4 2 1)
         (let ((a (reverse-list-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-064
  (test-equal "accumulator-064" '#(1 2 4)
         (let ((a (vector-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-065
  (test-equal "accumulator-065" '#(0 0 1 2 4)
         (let* ((v (vector 0 0 0 0 0))
                (a (vector-accumulator! v 2)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-066
  (test-equal "accumulator-066" '#vu8(0 0 1 2 4)
         (let* ((v (bytevector 0 0 0 0 0))
                (a (bytevector-accumulator! v 2)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-067
  (test-equal "accumulator-067" '#(4 2 1)
         (let ((a (reverse-vector-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-068
  (test-equal "accumulator-068" "abc"
         (let ((a (string-accumulator)))
           (a #\a)
           (a #\b)
           (a #\c)
           (a (eof-object)))))

(define test-069
  (test-equal "accumulator-069" #vu8(1 2 4)
         (let ((a (bytevector-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-070
  (test-equal "accumulator-070" 7
         (let ((a (sum-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(define test-071
  (test-equal "accumulator-071" 8
         (let ((a (product-accumulator)))
           (a 1)
           (a 2)
           (a 4)
           (a (eof-object)))))

(test-end "accumulators")
