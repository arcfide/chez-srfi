;; Copyright © 2015 Jeremy Steward
;; Copyright © 2017 Peter Lane
;; Copyright © 2020 Amirouche Boubekki
;; SPDX-License-Identifier: MIT
#!r6rs

;; Tests for SRFI 116

(import
 (rnrs)
 (srfi :116 ilists)
 (srfi :64 testing))

(define (pk . args)
  (write args)(newline)
  (car (reverse args)))

(test-begin "ilists")

(define abc (ilist 'a 'b 'c))

(define check-000
  (test-equal "ilist-000" 'a (icar abc)))

(define check-001
  (test-equal "ilist-001" 'b (icadr abc)))

(define check-002
  (test-equal "ilist-002" 'c (icaddr abc)))

(define check-003
  (test-equal "ilist-003" #t (ilist= (ipair 2 1) (xipair 1 2))))

(define abc-dot-d (ipair* 'a 'b 'c 'd))

(define check-004
  (test-equal "ilist-004" 'd (icdddr abc-dot-d)))

(define check-005
  (test-equal "ilist-005" #t (ilist= (iq c c c c) (make-ilist 4 'c))))

(define check-006
  (test-equal "ilist-006" #t (ilist= (iq 0 1 2 3) (ilist-tabulate 4 values))))

(define check-007
  (test-equal "ilist-007" #t (ilist= (iq 0 1 2 3 4) (iiota 5))))

(define abc-copy (ilist-copy abc))

(define check-008
  (test-equal "ilist-008" #t (ilist= abc abc-copy)))

(define check-009
  (test-equal "ilist-009" #t (not (eq? abc abc-copy))))

(define check-010
  (test-equal "ilist-010" #t (ipair? (ipair 1 2))))

(define check-011
  (test-equal "ilist-011" #t (proper-ilist? '())))

(define check-012
  (test-equal "ilist-012" #t (proper-ilist? (iq 1 2 3))))

(define check-013
  (test-equal "ilist-013" #t (ilist? '())))

(define check-014
  (test-equal "ilist-014" #t (ilist? (iq 1 2 3))))

(define check-015
  (test-equal "ilist-015" #t (dotted-ilist? (ipair 1 2))))

(define check-016
  (test-equal "ilist-016" #t (dotted-ilist? 2)))

(define check-017
  (test-equal "ilist-017" #t (null-ilist? '())))

(define check-018
  (test-equal "ilist-018" #t (not (null-ilist? (iq 1 2 3)))))

(define check-019
  (let ((singleton '(singleton)))
    (test-equal singleton (guard (ex (else singleton)) (null-ilist? 'a)))))

(define check-020
  (test-equal "ilist-020" #t (not-ipair? 'a)))

(define check-021
  (test-equal "ilist-021" #t (not (not-ipair? (ipair 'a 'b)))))

(define check-022
  (test-equal "ilist-022" #t (ilist= = (iq 1 2 3) (iq 1 2 3))))

(define check-023
  (test-equal "ilist-023" #t (ilist= = (iq 1 2 3) (iq 1 2 3) (iq 1 2 3))))

(define check-024
  (test-equal "ilist-024" #t (not (ilist= = (iq 1 2 3 4) (iq 1 2 3)))))

(define check-025
  (test-equal "ilist-025" #t (not (ilist= = (iq 1 2 3) (iq 1 2 3 4)))))

(define check-026
  (test-equal "ilist-026" #t (ilist= = (iq 1 2 3) (iq 1 2 3))))

(define check-027
  (test-equal "ilist-027" #t (not (ilist= = (iq 1 2 3) (iq 1 2 3 4) (iq 1 2 3 4)))))

(define check-028
  (test-equal "ilist-028" #t (not (ilist= = (iq 1 2 3) (iq 1 2 3) (iq 1 2 3 4)))))

(define ab (ipair 'a 'b))
(define cd (ipair 'c 'd))
(define ef (ipair 'e 'f))
(define gh (ipair 'g 'h))
(define abcd (ipair ab cd))
(define efgh (ipair ef gh))
(define abcdefgh (ipair abcd efgh))
(define ij (ipair 'i 'j))
(define kl (ipair 'k 'l))
(define mn (ipair 'm 'n))
(define op (ipair 'o 'p))
(define ijkl (ipair ij kl))
(define mnop (ipair mn op))
(define ijklmnop (ipair ijkl mnop))
(define abcdefghijklmnop (ipair abcdefgh ijklmnop))

(define check-029
  (test-equal "ilist-029" 'a (icaar abcd)))

(define check-030
  (test-equal "ilist-030" 'b (icdar abcd)))

(define check-031
  (test-equal "ilist-031" 'c (icadr abcd)))

(define check-032
  (test-equal "ilist-032" 'd (icddr abcd)))

(define check-033
  (test-equal "ilist-033" 'a (icaaar abcdefgh)))

(define check-034
  (test-equal "ilist-034" 'b (icdaar abcdefgh)))

(define check-035
  (test-equal "ilist-035" 'c (icadar abcdefgh)))

(define check-036
  (test-equal "ilist-036" 'd (icddar abcdefgh)))

(define check-037
  (test-equal "ilist-037" 'e (icaadr abcdefgh)))

(define check-038
  (test-equal "ilist-038" 'f (icdadr abcdefgh)))

(define check-039
  (test-equal "ilist-039" 'g (icaddr abcdefgh)))

(define check-040
  (test-equal "ilist-040" 'h (icdddr abcdefgh)))

(define check-041
  (test-equal "ilist-041" 'a (icaaaar abcdefghijklmnop)))

(define check-042
  (test-equal "ilist-042" 'b (icdaaar abcdefghijklmnop)))

(define check-043
  (test-equal "ilist-043" 'c (icadaar abcdefghijklmnop)))

(define check-044
  (test-equal "ilist-044" 'd (icddaar abcdefghijklmnop)))

(define check-045
  (test-equal "ilist-045" 'e (icaadar abcdefghijklmnop)))

(define check-046
  (test-equal "ilist-046" 'f (icdadar abcdefghijklmnop)))

(define check-047
  (test-equal "ilist-047" 'g (icaddar abcdefghijklmnop)))

(define check-048
  (test-equal "ilist-048" 'h (icdddar abcdefghijklmnop)))

(define check-049
  (test-equal "ilist-049" 'i (icaaadr abcdefghijklmnop)))

(define check-050
  (test-equal "ilist-050" 'j (icdaadr abcdefghijklmnop)))

(define check-051
  (test-equal "ilist-051" 'k (icadadr abcdefghijklmnop)))

(define check-052
  (test-equal "ilist-052" 'l (icddadr abcdefghijklmnop)))

(define check-053
  (test-equal "ilist-053" 'm (icaaddr abcdefghijklmnop)))

(define check-054
  (test-equal "ilist-054" 'n (icdaddr abcdefghijklmnop)))

(define check-055
  (test-equal "ilist-055" 'o (icadddr abcdefghijklmnop)))

(define check-056
  (test-equal "ilist-056" 'p (icddddr abcdefghijklmnop)))

(define check-057
  (test-equal "ilist-057" 'c (ilist-ref (iq a b c d) 2)))

(define ten (ilist 1 2 3 4 5 6 7 8 9 10))

(define check-058
  (test-equal "ilist-058" 1 (ifirst ten)))

(define check-059
  (test-equal "ilist-059" 2 (isecond ten)))

(define check-060
  (test-equal "ilist-060" 3 (ithird ten)))

(define check-061
  (test-equal "ilist-061" 4 (ifourth ten)))

(define check-062
  (test-equal "ilist-062" 5 (ififth ten)))

(define check-063
  (test-equal "ilist-063" 6 (isixth ten)))

(define check-064
  (test-equal "ilist-064" 7 (iseventh ten)))

(define check-065
  (test-equal "ilist-065" 8 (ieighth ten)))

(define check-066
  (test-equal "ilist-066" 9 (ininth ten)))

(define check-067
  (test-equal "ilist-067" 10 (itenth ten)))

(define check-068
  (let ((singleton '(singleton)))
    (test-equal "ilist-068" singleton (guard (ex (else singleton)) (ilist-ref '() 2)))))

(define check-069
  (test-equal "ilist-69" '(1 2) (call-with-values (lambda () (icar+icdr (ipair 1 2))) list)))

(define abcde (iq a b c d e))

(define dotted (ipair 1 (ipair 2 (ipair 3 'd))))

(define check-070
  (test-equal "ilist-070" #t (ilist= (iq a b) (itake abcde 2))))

(define check-071
  (test-equal "ilist-071" #t (ilist= (iq c d e) (idrop abcde 2))))

(define check-072
  (test-equal "ilist-072" #t (ilist= (iq c d e) (ilist-tail abcde 2))))

(define check-073
  (test-equal "ilist-073" #t (ilist= (iq 1 2) (itake dotted 2))))

(define check-074
  (test-equal "ilist-074" #t (ilist= (ipair 3 'd) (idrop dotted 2))))

(define check-075
  (test-equal "ilist-075" #t (ilist= (ipair 3 'd) (ilist-tail dotted 2))))

(define check-076
  (test-equal "ilist-076" 'd (idrop dotted 3)))

(define check-077
  (test-equal "ilist-077" 'd (ilist-tail dotted 3)))

(define check-078
  (test-equal "ilist-078" #t (ilist= abcde (iappend (itake abcde 4) (idrop abcde 4)))))

(define check-079
  (test-equal "ilist-079" #t (ilist= (iq d e) (itake-right abcde 2))))

(define check-080
  (test-equal "ilist-080" #t (ilist= (iq a b c) (idrop-right abcde 2))))

(define check-081
  (test-equal "ilist-081" #t (ilist= (ipair 2 (ipair 3 'd)) (itake-right dotted 2))))

(define check-082
  (test-equal "ilist-082" #t (ilist= (iq 1) (idrop-right dotted 2))))

(define check-083
  (test-equal "ilist-083" 'd (itake-right dotted 0)))

(define check-084
  (test-equal "ilist-084" #t (ilist= (iq 1 2 3) (idrop-right dotted 0))))

(define check-085
  (test-equal "ilist-085" #t (ilist= abcde (call-with-values (lambda () (isplit-at abcde 3)) iappend))))

(define check-086
  (test-equal "ilist-086" 'c (ilast (iq a b c))))

(define check-087
  (test-equal "ilist-087" #t (ilist= (iq c) (last-ipair (iq a b c)))))

(define check-088
  (test-equal "ilist-088" 0 (ilength '())))

(define check-089
  (test-equal "ilist-089" 3 (ilength (iq 1 2 3))))

(define check-090
  (test-equal "ilist-090" #t (ilist= (iq x y) (iappend (iq x) (iq y)))))

(define check-091
  (test-equal "ilist-091" #t (ilist= (iq a b c d) (iappend (iq a b) (iq c d)))))

(define check-092
  (test-equal "ilist-092" #t (ilist= (iq a) (iappend '() (iq a)))))

(define check-093
  (test-equal "ilist-093" #t (ilist= (iq x y) (iappend (iq x y)))))

(define check-094
  (test-equal "ilist-094" '() (iappend)))

(define check-095
  (test-equal "ilist-095" #t (ilist= (iq a b c d) (iconcatenate (iq (a b) (c d))))))

(define check-096
  (test-equal "ilist-096" #t (ilist= (iq c b a) (ireverse (iq a b c)))))

(define check-097
  (test-equal "ilist-097" #t (ilist= (iq (e (f)) d (b c) a) (ireverse (iq a (b c) d (e (f)))))))

(define check-098
  (test-equal "ilist-098" #t (ilist= (ipair 2 (ipair 1 'd)) (iappend-reverse (iq 1 2) 'd))))

(define check-099
  (test-equal "ilist-099"
              #t (ilist=
                  (iq (one 1 odd) (two 2 even) (three 3 odd))
                  (izip (iq one two three) (iq 1 2 3) (iq odd even odd)))))

(define check-100
  (test-equal "ilist-100" #t (ilist= (iq (1) (2) (3)) (izip (iq 1 2 3)))))

(define check-101
  (test-equal "ilist-101" #t (ilist= (iq 1 2 3) (iunzip1 (iq (1) (2) (3))))))

(define check-102
  (test-equal "ilist-102" #t
              (ilist=
               (iq (1 2 3) (one two three))
               (call-with-values
                   (lambda () (iunzip2 (iq (1 one) (2 two) (3 three))))
                 ilist))))

(define check-103
  (test-equal "ilist-103"
              #t
              (ilist= (iq (1 2 3) (one two three) (a b c))
                      (call-with-values
                          (lambda () (iunzip3 (iq (1 one a) (2 two b) (3 three c))))
                        ilist))))

(define check-104
  (test-equal "ilist-104"
              #t
              (ilist= (iq (1 2 3) (one two three) (a b c) (4 5 6))
                      (call-with-values
                          (lambda () (iunzip4 (iq (1 one a 4) (2 two b 5) (3 three c 6))))
                        ilist))))

(define check-105
  (test-equal "ilist-105"
              #t
              (ilist= (iq (1 2 3) (one two three) (a b c) (4 5 6) (#t #f #t))
                      (call-with-values
                          (lambda () (iunzip5 (iq (1 one a 4 #t) (2 two b 5 #f) (3 three c 6 #t))))
                        ilist))))

(define check-106
  (test-equal "ilist-106" 3 (icount even? (iq 3 1 4 1 5 9 2 5 6))))

(define check-107
  (test-equal "ilist-107" 3 (icount < (iq 1 2 4 8) (iq 2 4 6 8 10 12 14 16))))

;; We have to be careful to test both single-list and multiple-list
;; code paths, as they are different in this implementation.

(define lis (iq 1 2 3))

(define check-108
  (test-equal "ilist-108" 6 (ifold + 0 lis)))

(define check-109
  (test-equal "ilist-109"
              #t (ilist= (iq 3 2 1) (ifold ipair '() lis))))

(define check-110
  (test-equal "ilist-110" 2 (ifold
                             (lambda (x count) (if (symbol? x) (+ count 1) count))
                             0
                             (iq a 0 b))))

(define check-111
  (test-equal "ilist-111" 4 (ifold
                             (lambda (s max-len) (max max-len (string-length s)))
                             0
                             (iq "ab" "abcd" "abc"))))

(define check-112
  (test-equal "ilist-112" 32 (ifold
                              (lambda (a b ans) (+ (* a b) ans))
                              0
                              (iq 1 2 3)
                              (iq 4 5 6))))

(define (z x y ans) (ipair (ilist x y) ans))

(define check-113
  (test-equal "ilist-113" #t (ilist= (iq (b d) (a c))
                                  (ifold z '() (iq a b) (iq c d)))))

(define check-114
  (test-equal "ilist-114" #t (ilist= lis (ifold-right ipair '() lis))))

(define check-115
  (test-equal "ilist-115" #t (ilist= (iq 0 2 4) (ifold-right
                                             (lambda (x l) (if (even? x) (ipair x l) l))
                                             '()
                                             (iq 0 1 2 3 4)))))

(define check-116
  (test-equal "ilist-116" #t (ilist= (iq (a c) (b d))
                                     (ifold-right z '() (iq a b) (iq c d)))))

(define check-117
  (test-equal "ilist-117" #t (ilist= (iq (c) (b c) (a b c))
                                  (ipair-fold ipair '() (iq a b c)))))

(define check-118
  (test-equal "ilist-118" #t (ilist= (iq ((b) (d)) ((a b) (c d)))
                                  (ipair-fold z '() (iq a b) (iq c d)))))

(define check-119
  (test-equal "ilist-119" #t (ilist= (iq (a b c) (b c) (c))
                                  (ipair-fold-right ipair '() (iq a b c)))))

(define check-120
  (test-equal "ilist-120" #t (ilist= (iq ((a b) (c d)) ((b) (d)))
                                  (ipair-fold-right z '() (iq a b) (iq c d)))))

(define check-121
  (test-equal "ilist-121" 5 (ireduce max 0 (iq 1 3 5 4 2 0))))

(define check-122
  (test-equal "ilist-122" 1 (ireduce - 0 (iq 1 2))))

(define check-123
  (test-equal "ilist-123" -1 (ireduce-right - 0 (iq 1 2))))

(define squares (iq 1 4 9 16 25 36 49 64 81 100))

(define check-124
  (test-equal "ilist-124" #t (ilist= squares
                                     (iunfold (lambda (x) (> x 10))
                                              (lambda (x) (* x x))
                                              (lambda (x) (+ x 1))
                                              1))))

(define check-125
  (test-equal "ilist-125" #t (ilist= squares
                                     (iunfold-right zero?
                                                    (lambda (x) (* x x))
                                                    (lambda (x) (- x 1))
                                                    10))))

(define check-126
  (test-equal "ilist-126" #t (ilist= (iq 1 2 3) (iunfold null-ilist? icar icdr (iq 1 2 3)))))

(define check-127
  (test-equal "ilist-127" #t (ilist= (iq 3 2 1) (iunfold-right null-ilist? icar icdr (iq 1 2 3)))))

(define check-128
  (test-equal "ilist-128" #t (ilist= (iq 1 2 3 4)
                                     (iunfold null-ilist? icar icdr (iq 1 2) (lambda (x) (iq 3 4))))))

(define check-129
  (test-equal "ilist-129" #t (ilist= (iq b e h) (imap icadr (iq (a b) (d e) (g h))))))

(define check-130
  (test-equal "ilist-130" #t (ilist= (iq b e h) (imap-in-order icadr (iq (a b) (d e) (g h))))))

(define check-131
  (test-equal "ilist-131" #t (ilist= (iq 5 7 9) (imap + (iq 1 2 3) (iq 4 5 6)))))

(define check-132
  (test-equal "ilist-132" #t (ilist= (iq 5 7 9) (imap-in-order + (iq 1 2 3) (iq 4 5 6)))))

(define z1 (let ((count 0)) (lambda (ignored) (set! count (+ count 1)) count)))

(define check-133
  (test-equal "ilist-133" #t (ilist= (iq 1 2) (imap-in-order z1 (iq a b)))))

(define check-134
  (test-equal "ilist-134" '#(0 1 4 9 16)
              (let ((v (make-vector 5)))
                (ifor-each (lambda (i)
                             (vector-set! v i (* i i)))
                           (iq 0 1 2 3 4))
                v)))

(define check-135
  (test-equal "ilist-135" '#(5 7 9 11 13)
              (let ((v (make-vector 5)))
                (ifor-each (lambda (i j)
                             (vector-set! v i (+ i j)))
                           (iq 0 1 2 3 4)
                           (iq 5 6 7 8 9))
                v)))

(define check-136
  (test-equal "ilist-136" #t (ilist= (iq 1 -1 3 -3 8 -8)
                                     (iappend-map (lambda (x) (ilist x (- x))) (iq 1 3 8)))))

(define check-137
  (test-equal "ilist-137" #t (ilist= (iq 1 4 2 5 3 6)
                                     (iappend-map ilist (iq 1 2 3) (iq 4 5 6)))))
(define check-138
  (test-equal "ilist-138"
              (vector (list 0 1 2 3 4) (list 1 2 3 4) (list 2 3 4) (list 3 4) (list 4))
              (let ((v (make-vector 5)))
                (ipair-for-each (lambda (lis) (vector-set! v (icar lis) (ilist->list lis))) (iq 0 1 2 3 4))
                v)))

(define check-139
  (test-equal "ilist-139" (vector (list 5 6 7 8 9) (list 6 7 8 9) (list 7 8 9) (list 8 9) (list 9))
              (let ((v (make-vector 5)))
                (ipair-for-each (lambda (i j) (vector-set! v (icar i) (ilist->list j)))
                                (iq 0 1 2 3 4)
                                (iq 5 6 7 8 9))
                v)))

(define check-140
  (test-equal "ilist-140" #t (ilist= (iq 1 9 49)
                                     (ifilter-map (lambda (x) (and (number? x) (* x x))) (iq a 1 b 3 c 7)))))

(define check-141
  (test-equal "ilist-141" #t (ilist= (iq 5 7 9)
                                     (ifilter-map
                                      (lambda (x y) (and (number? x) (number? y) (+ x y)))
                                      (iq 1 a 2 b 3 4)
                                      (iq 4 0 5 y 6 z1)))))

(define check-142
  (test-equal "ilist-142" #t (ilist= (iq 0 8 8 -4) (ifilter even? (iq 0 7 8 8 43 -4)))))

(define check-143
  (test-equal "ilist-143"
              (list (list 'one 'four 'five) (list 2 3 6))
              (call-with-values (lambda () (ipartition symbol? (iq one 2 3 four five 6)))
                (lambda (a b) (list (ilist->list a) (ilist->list b))))))

(define check-144
  (test-equal "ilist-144" #t (ilist= (iq 7 43) (iremove even? (iq 0 7 8 8 43 -4)))))

(define check-145
  (test-equal "ilist-145" 2 (ifind even? (iq 1 2 3))))

(define check-146
  (test-equal "ilist-146" #t (iany even? (iq 1 2 3))))

(define check-147
  (test-equal "ilist-147" #f (ifind even? (iq 1 7 3))))

(define check-148
  (test-equal "ilist-148" #f (iany  even? (iq 1 7 3))))

(define check-149
  (let ((singleton '(singleton)))
    (test-equal "ilist-149"
                singleton
                (guard (ex (else singleton)) (ifind even? (ipair 1 (ipair 3 'x)))))))

(define check-150
  (let ((singleton '(singleton)))
    (test-equal "ilist-150"
                 singleton
                 (guard (ex (else singleton)) (iany even? (ipair 1 (ipair 3 'x)))))))

(define check-151
  (test-equal "ilist-151" 4 (ifind even? (iq 3 1 4 1 5 9))))

(define check-152
  (test-equal "ilist-152" #t (ilist= (iq -8 -5 0 0) (ifind-tail even? (iq 3 1 37 -8 -5 0 0)))))

(define check-153
  (test-equal "ilist-153" #t (ilist= (iq 2 18) (itake-while even? (iq 2 18 3 10 22 9)))))

(define check-154
  (test-equal "ilist-154" #t (ilist= (iq 3 10 22 9) (idrop-while even? (iq 2 18 3 10 22 9)))))

(define check-155
  (test-equal "ilist-155" (list (list 2 18) (list 3 10 22 9))
              (call-with-values
                  (lambda () (ispan even? (iq 2 18 3 10 22 9)))
                (lambda (a b) (list (ilist->list a) (ilist->list b))))))

(define check-156
  (test-equal "ilist-156" (list (list 3 1) (list 4 1 5 9))
              (call-with-values
                  (lambda () (ibreak even? (iq 3 1 4 1 5 9)))
                (lambda (a b) (list (ilist->list a) (ilist->list b))))))

(define check-157
  (test-equal "ilist-157" #t (iany integer? (iq a 3 b 2.7))))

(define check-158
  (test-equal "ilist-158" #f (iany integer? (iq a 3.1 b 2.7))))

(define check-159
  (test-equal "ilist-159" #t (iany < (iq 3 1 4 1 5) (iq 2 7 1 8 2))))

(define check-160
  (test-equal "ilist-160" #t (ievery integer? (iq 1 2 3 4 5))))

(define check-161
  (test-equal "ilist-161" #f (ievery integer? (iq 1 2 3 4.5 5))))

(define check-162
  (test-equal "ilist-162" #t (ievery (lambda (a b) (< a b)) (iq 1 2 3) (iq 4 5 6))))

(define check-163
  (test-equal "ilist-163" 2 (ilist-index even? (iq 3 1 4 1 5 9))))

(define check-164
  (test-equal "ilist-164" 1 (ilist-index < (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2))))

(define check-165
  (test-equal "ilist-165" #f (ilist-index = (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2))))

(define check-166
  (test-equal "ilist-166" #t (ilist= (iq a b c) (imemq 'a (iq a b c)))))

(define check-167
  (test-equal "ilist-167" #t (ilist= (iq b c) (imemq 'b (iq a b c)))))

(define check-168
  (test-equal "ilist-168" #f (imemq 'a (iq b c d))))

(define check-169
  (test-equal "ilist-169" #f (imemq (ilist 'a) (iq b (a) c))))

(define check-170
  (test-equal "ilist-170" #t (ilist= (iq (a) c) (imember (ilist 'a) (iq b (a) c)))))

(define check-171
  (test-equal "ilist-171" #t (ilist= (iq 101 102) (imemv 101 (iq 100 101 102)))))

(define check-172
  (test-equal "ilist-172" #t (ilist= (iq 1 2 4 5) (idelete 3 (iq 1 2 3 4 5)))))

(define check-173
  (test-equal "ilist-173" #t (ilist= (iq 3 4 5) (idelete 5 (iq 3 4 5 6 7) <))))

(define check-174
  (test-equal "ilist-174" #t (ilist= (iq a b c z1) (idelete-duplicates (iq a b a c a b c z1)))))

(define e (iq (a 1) (b 2) (c 3)))

(define check-175
  (test-equal "ilist-175" #t (ilist= (iq a 1) (iassq 'a e))))

(define check-176
  (test-equal "ilist-176" #t (ilist= (iq b 2) (iassq 'b e))))

(define check-177
  (test-equal "ilist-177" #f (iassq 'd e)))

(define check-178
  (test-equal "ilist-178" #f (iassq (ilist 'a) (iq ((a)) ((b)) ((c))))))

(define check-179
  (test-equal "ilist-179" #t (ilist= (iq (a)) (iassoc (ilist 'a) (iq ((a)) ((b)) ((c)))))))

(define e2 (iq (2 3) (5 7) (11 13)))

(define check-180
  (test-equal "ilist-180" #t (ilist= (iq 5 7) (iassv 5 e2))))

(define check-181
  (test-equal "ilist-181" #t (ilist= (iq 11 13) (iassoc 5 e2 <))))

(define check-182
  (test-equal "ilist-182" #t (ilist= (ipair (iq 1 1) e2) (ialist-cons 1 (ilist 1) e2))))

(define check-183
  (test-equal "ilist-183" #t (ilist= (iq (2 3) (11 13)) (ialist-delete 5 e2))))

(define check-184
  (test-equal "ilist-184" #t (ilist= (iq (2 3) (5 7)) (ialist-delete 5 e2 <))))

(define check-185
  (test-equal "ilist-185" #t (ilist= (ipair 1 3) (replace-icar (ipair 2 3) 1))))

(define check-186
  (test-equal "ilist-186" #t (ilist= (ipair 1 3) (replace-icdr (ipair 1 2) 3))))

(define check-187
  (test-equal "ilist-187" #t (ilist= (ipair 1 2) (pair->ipair '(1 . 2)))))

(define check-188
  (test-equal "ilist-188" '(1 . 2) (ipair->pair (ipair 1 2))))

(define check-189
  (test-equal "ilist-189" #t (ilist= (iq 1 2 3) (list->ilist '(1 2 3)))))

(define check-190
  (test-equal "ilist-190" '(1 2 3) (ilist->list (iq 1 2 3))))

(define check-191
  (test-equal "ilist-191" #t (ilist= (ipair 1 (ipair 2 3)) (list->ilist '(1 2 . 3)))))

(define check-192
  (test-equal "ilist-192" '(1 2 . 3) (ilist->list (ipair 1 (ipair 2 3)))))

(define check-193
  (test-equal "ilist-193" #t (ilist= (ipair (ipair 1 2) (ipair 3 4)) (tree->itree '((1 . 2) . (3 . 4))))))

(define check-194
  (test-equal "ilist-194" '((1 . 2) . (3 . 4)) (itree->tree (ipair (ipair 1 2) (ipair 3 4)))))

(define check-195
  (test-equal "ilist-195" #t (ilist= (ipair (ipair 1 2) (ipair 3 4)) (gtree->itree (cons (ipair 1 2) (ipair 3 4))))))

(define check-196
  (test-equal "ilist-196" '((1 . 2) . (3 . 4)) (gtree->tree (cons (ipair 1 2) (ipair 3 4)))))

(define check-197
  (test-equal "ilist-197" 6 (iapply + (iq 1 2 3))))

(define check-198
  (test-equal "ilist-198" 15 (iapply + 1 2 (iq 3 4 5))))

(test-end "ilists")
