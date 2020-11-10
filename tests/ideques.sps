;; Copyright © 2016 John Cowan
;; Copyright © 2020 Amirouche Boubekki
;; SPDX-License-Identifier: MIT
#!r6rs

(import
 (except (rnrs) define-record-type)
 (srfi :158 generators-and-accumulators)
 (srfi :134 ideques)
 (only (srfi :1 lists) take drop split-at iota)
 (srfi :64 testing))

(define-syntax receive
  (syntax-rules ()
    ((receive ?formals ?producer ?body1 ?body2 ...)
     (call-with-values (lambda () ?producer)
       (lambda ?formals ?body1 ?body2 ...)))))

(define (pk . args)
  (write args)(newline)
  (car (reverse args)))

(test-begin "ideques")

(define check-000
  (test-equal "ideque-000" '() (ideque->list (ideque))))

(define check-001
  (test-equal "ideque-001" '() (ideque->list (list->ideque '()))))

(define check-002
  (test-equal "ideque-002" '(1 2 3) (ideque->list (ideque 1 2 3))))

(define check-003
  (test-equal "ideque-003" '(4 5 6 7) (ideque->list (list->ideque '(4 5 6 7)))))

(define check-004
  (test-equal "ideque-004" '(10 9 8 7 6 5 4 3 2 1)
              (ideque->list (ideque-unfold zero? values (lambda (n) (- n 1)) 10))))

(define check-005
  (test-equal "ideque-005" '(1 2 3 4 5 6 7 8 9 10)
              (ideque->list (ideque-unfold-right zero? values (lambda (n) (- n 1)) 10))))

(define check-006
  (test-equal "ideque-006" '(0 2 4 6 8 10)
              (ideque->list (ideque-tabulate 6 (lambda (n) (* n 2))))))

;; corner cases
(define check-007
  (test-equal "ideque-007" '() (ideque->list
                                (ideque-unfold (lambda (n) #t) values (lambda (n) (+ n 1)) 0))))

(define check-008
  (test-equal "ideque-008" '() (ideque->list
                                (ideque-unfold-right (lambda (n) #t) values (lambda (n) (+ n 1)) 0))))

(define check-009
  (test-equal "ideque-009" '() (ideque->list (ideque-tabulate 0 values))))

(define check-010
  (test-equal "ideque-010" #t (ideque? (ideque))))

(define check-011
  (test-equal "ideque-011" #t (not (ideque? 1))))

(define check-012
  (test-equal "ideque-012" #t (ideque-empty? (ideque))))

(define check-013
  (test-equal "ideque-013" #t (not (ideque-empty? (ideque 1)))))

(define check-014
  (test-equal "ideque-014" #t (ideque= eq?)))

(define check-015
  (test-equal "ideque-015" #t (ideque= eq? (ideque 1))))

(define check-016
  (test-equal "ideque-016" #t (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B))))

(define check-017
  (test-equal "ideque-017" #t (ideque= char-ci=? (ideque) (ideque))))

(define check-018
  (test-equal "ideque-018" #t (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B #\c)))))

(define check-019
  (test-equal "ideque-019" #t (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A)))))

(define check-020
  (test-equal "ideque-020" #t (ideque= char-ci=? (ideque) (ideque) (ideque))))

(define check-021
  (test-equal "ideque-021" #t (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\a #\B))))

(define check-022
  (test-equal "ideque-022" #t (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A) (ideque #\a #\B)))))

(define check-023
  (test-equal "ideque-023" #t (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B) (ideque #\A #\B #\c)))))

(define check-024
  (test-equal "ideque-024" 'singleton (guard (ex (else 'singleton)) (ideque-front (ideque)))))

(define check-025
  (let ((singleton '(singleton)))
    (test-equal "ideque-025" singleton (guard (ex (else singleton)) (ideque-back (ideque))))))

(define check-026
  (test-equal "ideque-026" 1 (ideque-front (ideque 1 2 3))))

(define check-027
  (test-equal "ideque-027" 3 (ideque-back (ideque 1 2 3))))

(define check-028
  (test-equal "ideque-028" 2 (ideque-front (ideque-remove-front (ideque 1 2 3)))))

(define check-029
  (test-equal "ideque-029" 2 (ideque-back (ideque-remove-back (ideque 1 2 3)))))

(define check-030
  (test-equal "ideque-030" 1 (ideque-front (ideque-remove-back (ideque 1 2 3)))))

(define check-031
  (test-equal "ideque-031" 3 (ideque-back (ideque-remove-front (ideque 1 2 3)))))

(define check-032
  (test-equal "ideque-032" #t (ideque-empty? (ideque-remove-front (ideque 1)))))

(define check-033
  (test-equal "ideque-033" #t (ideque-empty? (ideque-remove-back (ideque 1)))))

(define check-034
  (test-equal "ideque-034" 0 (ideque-front (ideque-add-front (ideque 1 2 3) 0))))

(define check-035
  (test-equal "ideque-035" 0 (ideque-back (ideque-add-back (ideque 1 2 3) 0))))

;; TODO: uncomment and adapt

(define (check* name ideque-op list-op n)
  (let* ((lis (iota n))
         (dq (list->ideque lis)))
    (for-each (lambda (i)
                (test-equal name
                            (receive xs (list-op lis i) xs)
                            (receive xs (ideque-op dq i)
                              (map ideque->list xs))))
              lis)))

(define check-036 (check* "ideque-036" ideque-take take 7))
(define check-037 (check* "ideque-037" ideque-drop drop 6))
(define check-038 (check* "ideque-038" ideque-split-at split-at 8))

;; out-of-range conditions
(define check-039
  (let ((singleton '(singleton)))
    (test-equal "ideque-039"
                singleton
                (guard (ex (else singleton))
                       (split-at (lambda (x) #t) (ideque->list (ideque-take (ideque 1 2 3 4 5 6 7) 10)))))))

(define check-040
  (let ((singleton '(singleton)))
    (test-equal "test-040"
                singleton
                (guard (ex (else singleton))
                       (ideque->list (ideque-take-right (ideque 1 2 3 4 5 6 7) 10))))))

(define check-041
  (let ((singleton '(singleton)))
    (test-equal "ideque-041"
                singleton
                (guard (ex (else singleton))
                       (ideque-split-at (ideque 1 2 3 4 5 6 7) 10)))))

(define check-042
  (test-equal "ideque-42" '(3 2 1) (map (lambda (n) (ideque-ref (ideque 3 2 1) n)) '(0 1 2))))

(define check-043
  (let ((singleton '(singleton)))
    (test-equal "ideque-043"
                singleton
                (guard (ex (else singleton)) (ideque-ref (ideque 3 2 1) -1)))))

(define check-044
  (let ((singleton '(singleton)))
    (test-equal "ideque-044"
                singleton
                (guard (ex (else singleton))
                       (ideque-ref (ideque 3 2 1) 3)))))

(define check-045
  (test-equal "ideque-045" 7 (ideque-length (ideque 1 2 3 4 5 6 7))))

(define check-046
  (test-equal "ideque-046" 0 (ideque-length (ideque))))

(define check-047
  (test-equal "ideque-047" '() (ideque->list (ideque-append))))

(define check-048
  (test-equal "ideque-048" '() (ideque->list (ideque-append (ideque) (ideque)))))

(define check-049
  (test-equal "ideque-049" '(1 2 3 a b c d 5 6 7 8 9)
              (ideque->list (ideque-append (ideque 1 2 3)
                                           (ideque 'a 'b 'c 'd)
                                           (ideque)
                                           (ideque 5 6 7 8 9)))))

(define check-050
  (test-equal "ideque-050" '() (ideque->list (ideque-reverse (ideque)))))

(define check-051
  (test-equal "ideque-051" '(5 4 3 2 1) (ideque->list (ideque-reverse (ideque 1 2 3 4 5)))))

(define check-052
  (test-equal "ideque-052" 0 (ideque-count odd? (ideque))))

(define check-053
  (test-equal "ideque-053" 3 (ideque-count odd? (ideque 1 2 3 4 5))))

(define check-054
  (test-equal "ideque-054" '((1 a) (2 b) (3 c))
              (ideque->list (ideque-zip (ideque 1 2 3) (ideque 'a 'b 'c 'd 'e)))))

(define check-055
  (test-equal "ideque-055" '((1 a x) (2 b y) (3 c z))
              (ideque->list (ideque-zip (ideque 1 2 3 4 5)
                                        (ideque 'a 'b 'c 'd 'e)
                                        (ideque 'x 'y 'z)))))

(define check-056
  (test-equal "ideque-056" '((1) (2) (3))
              (ideque->list (ideque-zip (ideque 1 2 3)))))

(define check-057
  (test-equal "ideque-057" '()
              (ideque->list (ideque-zip (ideque 1 2 3) (ideque)))))

(define check-058
  (test-equal "ideque-058" #t (ideque-empty? (ideque-map list (ideque)))))

(define check-059
  (test-equal "ideque-059" '(-1 -2 -3 -4 -5) (ideque->list (ideque-map - (ideque 1 2 3 4 5)))))

(define check-060
  (test-equal "ideque-060" '(-1 -3 5 -8)
              (ideque->list (ideque-filter-map (lambda (x) (and (number? x) (- x)))
                                               (ideque 1 3 'a -5 8)))))

(define check-061
  (test-equal "ideque-061" '(5 4 3 2 1)
              (let ((r '()))
                (ideque-for-each (lambda (n) (set! r (cons n r)))
                                 (ideque 1 2 3 4 5))
                r)))

(define check-062
  (test-equal "ideque-062" '(1 2 3 4 5)
              (let ((r '()))
                (ideque-for-each-right (lambda (n) (set! r (cons n r)))
                                       (ideque 1 2 3 4 5))
                r)))

(define check-063
  (test-equal "ideque-063" '(5 4 3 2 1 . z)
              (ideque-fold cons 'z (ideque 1 2 3 4 5))))

(define check-064
  (test-equal "ideque-064" '(1 2 3 4 5 . z)
              (ideque-fold-right cons 'z (ideque 1 2 3 4 5))))

(define check-065
  (test-equal "ideque-065" '(a a b b c c)
              (ideque->list (ideque-append-map (lambda (x) (list x x))
                                               (ideque 'a 'b 'c)))))

(define check-066
  (test-equal "ideque-066" '(1 3 5)
              (ideque->list (ideque-filter odd? (ideque 1 2 3 4 5)))))

(define check-067
  (test-equal "ideque-067" '(2 4)
              (ideque->list (ideque-remove odd? (ideque 1 2 3 4 5)))))

(define check-068
  (test-equal "ideque-068" '((1 3 5) (2 4))
              (receive xs (ideque-partition odd? (ideque 1 2 3 4 5))
                (map ideque->list xs))))

(define check-069
  (test-equal "ideque-069" 3 (ideque-find number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo))))

(define check-070
  (test-equal "ideque-070" 'boo (ideque-find number? (ideque 'a 'b 'c 'd) (lambda () 'boo))))

(define check-071
  (test-equal "ideque-071" #f (ideque-find number? (ideque 'a 'b 'c 'd))))

(define check-072
  (test-equal "ideque-072" 4 (ideque-find-right number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo))))

(define check-073
  (test-equal "ideque-073" 'boo (ideque-find-right number? (ideque 'a 'b 'c 'd) (lambda () 'boo))))

(define check-074
  (test-equal "ideque-074" #f (ideque-find-right number? (ideque 'a 'b 'c 'd))))

(define check-075
  (test-equal "ideque-075" '(1 3 2)
              (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                               (ideque 1 3 2 5 8 4 6 3 4 2)))))

(define check-076
  (test-equal "ideque-076" '(5 8 4 6 3 4 2)
              (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                               (ideque 1 3 2 5 8 4 6 3 4 2)))))

(define check-077
  (test-equal "ideque-077" '(3 4 2)
              (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                                     (ideque 1 3 2 5 8 4 6 3 4 2)))))

(define check-078
  (test-equal "ideque-078" '(1 3 2 5 8 4 6)
              (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                                     (ideque 1 3 2 5 8 4 6 3 4 2)))))

(define check-079
  (test-equal "ideque-079" '()
              (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                               (ideque 5 8 4 6 3 4 2 9)))))

(define check-080
  (test-equal "ideque-080" '()
              (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                               (ideque 1 4 3 2 3 4 2 1)))))

(define check-081
  (test-equal "ideque-081" '()
              (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                                     (ideque 5 8 4 6 3 4 2 9)))))

(define check-082
  (test-equal "ideque-082" '()
              (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                                     (ideque 1 3 2 4 3 2 3 2)))))

(define check-083
  (test-equal "ideque-083" '((1 3 2) (5 8 4 6 3 4 2))
              (receive xs (ideque-span (lambda (n) (< n 5))
                                       (ideque 1 3 2 5 8 4 6 3 4 2))
                (map ideque->list xs))))

(define check-084
  (test-equal "ideque-084" '((5 8) (4 6 3 4 2 9))
              (receive xs (ideque-break (lambda (n) (< n 5))
                                        (ideque 5 8 4 6 3 4 2 9))
                (map ideque->list xs))))

(define check-085
  (test-equal "ideque-085" 3 (ideque-any (lambda (x) (and (number? x) x))
                                         (ideque 'a 3 'b 'c 4 'd 'e))))

(define check-086
  (test-equal "ideque-086" 5 (ideque-any (lambda (x) (and (number? x) x))
                                         (ideque 'a 'b 'c 'd 'e 5))))

(define check-087
  (test-equal "ideque-087" #f (ideque-any (lambda (x) (and (number? x) x))
                                          (ideque 'a 'b 'c 'd 'e))))

(define check-088
  (test-equal "ideque-088" 9 (ideque-every (lambda (x) (and (number? x) x))
                                           (ideque 1 5 3 2 9))))

(define check-089
  (test-equal "ideque-089" #f (ideque-every (lambda (x) (and (number? x) x))
                                            (ideque 1 5 'a 2 9))))

(define check-090
  ;; check if we won't see further once we found the result
  (test-equal "ideque-090" 1 (ideque-any (lambda (x) (and (odd? x) x))
                                         (ideque 2 1 'a 'b 'c 'd))))

(define check-091
  (test-equal "ideque-091" #f (ideque-every (lambda (x) (and (odd? x) x))
                                            (ideque 1 2 'a 'b 'c 'd))))

(define check-092
  (test-equal "ideque-092" '(1 2 3) (generator->list (ideque->generator (ideque 1 2 3)))))

(define check-093
  (test-equal "ideque-093" '() (generator->list (ideque->generator (ideque)))))

(define check-094
  (test-equal "ideque-094" '(1 2 3) (ideque->list (generator->ideque (generator 1 2 3)))))

(define check-095
  (test-equal "ideque-095" '() (ideque->list (generator->ideque (generator)))))

(test-end "ideques")
