(import (rnrs)
        (only (rnrs r5rs) modulo)
        (srfi private define-values)
        (srfi :19)
        (srfi :113)
        (srfi :128)
        (srfi :64))

(define (error* msg . args)
  (error 'sets-and-bags-tests msg args))

;; Below are some default comparators provided by SRFI-114,
;; but not SRFI-128, which this SRFI has transitioned to
;; depend on. See the rationale for SRFI-128 as to why it is
;; preferred in usage compared to SRFI-114.

;; Most if not all of this code is taken from SRFI-114

(define (make-comparison=/< = <)
  (lambda (a b)
    (cond
     ((= a b) 0)
     ((< a b) -1)
     (else 1))))

;; Comparison procedure for real numbers only
(define (real-comparison a b)
  (cond
   ((< a b) -1)
   ((> a b) 1)
   (else 0)))

;; Comparison procedure for non-real numbers.
(define (complex-comparison a b)
  (let ((real-result (real-comparison (real-part a) (real-part b))))
    (if (= real-result 0)
        (real-comparison (imag-part a) (imag-part b))
        real-result)))

(define number-comparator
  (make-comparator number? = complex-comparison number-hash))

(define char-comparison (make-comparison=/< char=? char<?))

(define char-comparator
  (make-comparator char? char=? char-comparison char-hash))

;; Makes a hash function that works vectorwise
(define limit (expt 2 20))

(define (make-vectorwise-hash hash length ref)
  (lambda (obj)
    (let loop ((index (- (length obj) 1)) (result 5381))
      (if (= index 0)
          result
          (let* ((prod (modulo (* result 33) limit))
                 (sum (modulo (+ prod (hash (ref obj index))) limit)))
            (loop (- index 1) sum))))))

(define string-comparison (make-comparison=/< string=? string<?))

(define string-ci-comparison (make-comparison=/< string-ci=? string-ci<?))

(define string-comparator
  (make-comparator string? string=? string-comparison string-hash))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

(define eq-comparator
  (make-comparator
   #t
   eq?
   #f
   default-hash))

(define eqv-comparator
  (make-comparator
   #t
   eqv?
   #f
   default-hash))

(define equal-comparator
  (make-comparator
   #t
   equal?
   #f
   default-hash))


(define (big x) (> x 5))


(test-begin "sets-and-bags")

(define nums (set number-comparator))

(define syms (set eq-comparator 'a 'b 'c 'd))

(define nums2 (set-copy nums))

(define syms2 (set-copy syms))

(define esyms (set eq-comparator))

(define check-000
  (test-equal "sets-and-bags-000" #t (set-empty? esyms)))

(define total 0)

(define check-001
  (test-equal "sets-and-bags-001" #t (set? nums)))

(define check-002
  (test-equal "sets-and-bags-002" #t (set? syms)))

(define check-003
  (test-equal "sets-and-bags-003" #t (set? nums2)))

(define check-004
  (test-equal "sets-and-bags-004" #t (set? syms2)))

(define check-005
  (test-equal "sets-and-bags-005" #t (not (set? 'a))))

(define check-006
  (test-equal "sets-and-bags-006"
              4
              (begin (set-adjoin! nums 2)
                     (set-adjoin! nums 3)
                     (set-adjoin! nums 4)
                     (set-adjoin! nums 4)
                     (set-size (set-adjoin nums 5)))))
(define check-007
  (test-equal "sets-and-bags-007" 3 (set-size nums)))

(define check-008
  (test-equal "sets-and-bags-008" 3 (set-size (set-delete syms 'd))))

(define check-009
  (test-equal "sets-and-bags-009" 2 (set-size (set-delete-all syms '(c d)))))

(define check-010
  (test-equal "sets-and-bags-010" 4 (set-size syms)))

(define check-011
  (test-equal "sets-and-bags-011" 4 (begin (set-adjoin! syms 'e 'f) (set-size (set-delete-all! syms '(e f))))))

(define check-012
  (test-equal "sets-and-bags-012" 0 (set-size nums2)))

(define check-013
  (test-equal "sets-and-bags-013" 4 (set-size syms2)))

(define check-014
  (test-equal "sets-and-bags-014" 2 (begin (set-delete! nums 2) (set-size nums))))

(define check-015
  (test-equal "sets-and-bags-015" 2 (begin (set-delete! nums 1)
                                           (set-size nums))))

(define check-016
  (test-equal "sets-and-bags-016" #t (begin (set! nums2 (set-map number-comparator (lambda (x) (* 10 x)) nums))
                                            (set-contains? nums2 30))))

(define check-017
  (test-equal "sets-and-bags-017" #t (not (set-contains? nums2 3))))

(define check-018
  (test-equal "sets-and-bags-08" 70 (begin   (set-for-each (lambda (x) (set! total (+ total x))) nums2)
                                             total)))

(define check-019
  (test-equal "sets-and-bags-019" 10 (set-fold + 3 nums)))

(define check-020
  (test-equal "sets-and-bags-020"
              #t
              (begin
                (set! nums (set eqv-comparator 10 20 30 40 50))

                (set=? nums (set-unfold
                             (lambda (i) (= i 0))
                             (lambda (i) (* i 10))
                             (lambda (i) (- i 1))
                             5
                             eqv-comparator)))))

(define check-021
  (test-equal "sets-and-bags-021" '(a) (set->list (set eq-comparator 'a))))

(define check-022
  (test-equal "sets-and-bags-022" 2 (begin
                                      (set! syms2 (list->set eq-comparator '(e f)))
                                      (set-size syms2))))

(define check-023
  (test-equal "sets-and-bags-023" #t (set-contains? syms2 'e)))

(define check-024
  (test-equal "sets-and-bags-024" #t (set-contains? syms2 'f)))

(define check-025
  (test-equal "sets-and-bags-025" 4 (begin
                                      (list->set! syms2 '(a b))
                                      (set-size syms2))))

(define yam (set char-comparator #\y #\a #\m))

(define (failure/insert insert ignore)
  (insert 1))

(define (failure/ignore insert ignore)
  (ignore 2))

(define (success/update element update remove)
  (update #\b 3))

(define (success/remove element update remove)
  (remove 4))

(define yam! (set char-comparator #\y #\a #\m #\!))

(define bam (set char-comparator #\b #\a #\m))

(define ym (set char-comparator #\y #\m))

(define-values (set1 obj1)
  (set-search! (set-copy yam) #\! failure/insert error*))

(define check-026
  (test-equal "sets-and-bags-026" #t (set=? yam! set1)))

(define check-027
  (test-equal "sets-and-bags-027" 1 obj1))

(define-values (set2 obj2)
  (set-search! (set-copy yam) #\! failure/ignore error*))

(define check-028
  (test-equal "sets-and-bags-028" #t (set=? yam set2)))

(define check-029
  (test-equal "sets-and-bags-029" 2 obj2))

(define-values (set3 obj3)
  (set-search! (set-copy yam) #\y error* success/update))

(define check-030
  (test-equal "sets-and-bags-030" #t (set=? bam set3)))

(define check-031
  (test-equal "sets-and-bags-031" 3 obj3))

(define-values (set4 obj4)
  (set-search! (set-copy yam) #\a error* success/remove))

(define check-032
  (test-equal "sets-and-bags-032" #t (set=? ym set4)))

(define check-033
  (test-equal "sets-and-bags-033" 4 obj4))

(define set2-bis (set number-comparator 1 2))
(define other-set2 (set number-comparator 1 2))
(define set3-bis (set number-comparator 1 2 3))
(define set4-bis (set number-comparator 1 2 3 4))
(define sety (set number-comparator 1 2 4 5))
(define setx (set number-comparator 10 20 30 40))

(define check-034
  (test-equal "sets-and-bags-034" #t (set=? set2-bis other-set2)))

(define check-035
  (test-equal "sets-and-bags-035" #t (not (set=? set2-bis set3-bis))))

(define check-036
  (test-equal "sets-and-bags-036" #t (not (set=? set2-bis set3-bis other-set2))))

(define check-037
  (test-equal "sets-and-bags-037" #t (set<? set2-bis set3-bis set4-bis)))

(define check-038
  (test-equal "sets-and-bags-038" #t (not (set<? set2-bis other-set2))))

(define check-039
  (test-equal "sets-and-bags-039" #t (set<=? set2-bis other-set2 set3-bis)))

(define check-040
  (test-equal "sets-and-bags-040" #t (not (set<=? set2-bis set3-bis other-set2))))

(define check-041
  (test-equal "sets-and-bags-041" #t (set>? set4-bis set3-bis set2-bis)))

(define check-042
  (test-equal "sets-and-bags-042" #t (not (set>? set2-bis other-set2))))

(define check-043
  (test-equal "sets-and-bags-043" #t (set>=? set3-bis other-set2 set2-bis)))

(define check-044
  (test-equal "sets-and-bags-044" #t (not (set>=? other-set2 set3-bis set2-bis))))

(define check-045
  (test-equal "sets-and-bags-045" #t (not (set<? set2-bis other-set2))))

(define check-046
  (test-equal "sets-and-bags-046" #t (not (set<? set2-bis setx))))

(define check-047
  (test-equal "sets-and-bags-047" #t (not (set<=? set2-bis setx))))

(define check-048
  (test-equal "sets-and-bags-048" #t (not (set>? set2-bis setx))))

(define check-049
  (test-equal "sets-and-bags-049" #t (not (set>=? set2-bis setx))))

(define check-050
  (test-equal "sets-and-bags-050" #t (not (set<?  set3-bis sety))))

(define check-051
  (test-equal "sets-and-bags-051" #t (not (set<=? set3-bis sety))))

(define check-052
  (test-equal "sets-and-bags-052" #t (not (set>?  set3-bis sety))))

(define check-053
  (test-equal "sets-and-bags-053" #t (not (set>=? set3-bis sety))))

(define abcd (set eq-comparator 'a 'b 'c 'd))
(define efgh (set eq-comparator 'e 'f 'g 'h))
(define abgh (set eq-comparator 'a 'b 'g 'h))
(define other-abcd (set eq-comparator 'a 'b 'c 'd))
(define other-efgh (set eq-comparator 'e 'f 'g 'h))
(define other-abgh (set eq-comparator 'a 'b 'g 'h))
(define all (set eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
(define none (set eq-comparator))
(define ab (set eq-comparator 'a 'b))
(define cd (set eq-comparator 'c 'd))
(define ef (set eq-comparator 'e 'f))
(define gh (set eq-comparator 'g 'h))
(define cdgh (set eq-comparator 'c 'd 'g 'h))
(define abcdgh (set eq-comparator 'a 'b 'c 'd 'g 'h))
(define abefgh (set eq-comparator 'a 'b 'e 'f 'g 'h))

(define check-054
  (test-equal "sets-and-bags-54" #t (set-disjoint? abcd efgh)))

(define check-055
  (test-equal "sets-and-bags-055" #t (not (set-disjoint? abcd ab))))

(define check-056
  (test-equal "sets-and-bags-053" #t (set=? abcd (set-union abcd))))

(define check-057
  (test-equal "sets-and-bags-057" #t (set=? all (set-union abcd efgh))))

(define check-058
  (test-equal "sets-and-bags-058" #t (set=? abcdgh (set-union abcd abgh))))

(define check-059
  (test-equal "sets-and-bags-059" #t (set=? abefgh (set-union efgh abgh))))


(define efgh2 (set-copy efgh))

(define check-060
  (test-equal "sets-and-bags-060" #t (set=? efgh (begin (set-union! efgh2) efgh2))))

(define check-061
  (test-equal "sets-and-bags-061" #t (set=? abefgh (begin (set-union! efgh2 abgh) efgh2))))

(define check-062
  (test-equal "sets-and-bags-062" #t (set=? abcd (set-intersection abcd))))

(define check-063
  (test-equal "sets-and-bags-063" #t (set=? none (set-intersection abcd efgh))))

(define abcd2 (set-copy abcd))

(define check-064
  (test-equal "sets-and-bags-64"
              #t
              (set=?
               abcd
               (begin
                 (set-intersection! abcd2)
                 abcd2))))

(define check-065
  (test-equal "sets-and-bags-065" #t (set=? none (begin (set-intersection! abcd2 efgh) abcd2))))

(define check-066
  (test-equal "sets-and-bags-066" #t (set=? ab (set-intersection abcd abgh))))

(define check-067
  (test-equal "sets-and-bags-067" #t (set=? ab (set-intersection abgh abcd))))

(define check-068
  (test-equal "sets-and-bags-068" #t (set=? abcd (set-difference abcd))))

(define check-069
  (test-equal "sets-and-bags-069" #t (set=? cd (set-difference abcd ab))))

(define check-070
  (test-equal "sets-and-bags-070" #t (set=? abcd (set-difference abcd gh))))

(define check-071
  (test-equal "sets-and-bags-071" #t (set=? none (set-difference abcd abcd))))


(define abcd3 (set-copy abcd))

(define check-072
  (test-equal "sets-and-bags-072"
              #t
              (set=?
               abcd
               (begin
                 (set-difference! abcd3)
                 abcd3))))

(define check-073
  (test-equal "sets-and-bags-073"
              #t
              (set=? none (begin (set-difference! abcd3 abcd)
                                 abcd3))))

(define check-074
  (test-equal "sets-and-bags-074"
              #t
              (set=? cdgh (set-xor abcd abgh))))

(define check-075
  (test-equal "sets-and-bags-075"
              #t (set=? all (set-xor abcd efgh))))

(define check-076
  (test-equal "sets-and-bags-076" #t (set=? none (set-xor abcd other-abcd))))

(define abcd4 (set-copy abcd))
(define check-077
  (test-equal "sets-and-bags-077" #t (set=? none (set-xor! abcd4 other-abcd))))

(define check-078
  (test-equal "sets-and-bags-078" #t (set=? other-abcd abcd)))

(define check-079
  (test-equal "sets-and-bags-079" #t (set=? other-efgh efgh)))

(define check-080
  (test-equal "sets-and-bags-080" #t (set=? other-abgh abgh)))

(define nums3 (set number-comparator 1 2 3))
(define syms3 (set eq-comparator 'a 'b 'c))

(define check-081
  (test-error "sets-and-bags-081" (set=? nums3 syms3)))

(define check-082
  (test-error "sets-and-bags-082" (set<? nums3 syms3)))

(define check-083
  (test-error "sets-and-bags-083" (set<=? nums3 syms3)))

(define check-084
  (test-error "sets-and-bags-084" (set>? nums3 syms3)))

(define check-085
  (test-error "sets-and-bags-085" (set>=? nums3 syms3)))

(define check-086
  (test-error "sets-and-bags-086" (set-union nums3 syms3)))

(define check-087
  (test-error "sets-and-bags-087" (set-intersection nums3 syms3)))

(define check-088
  (test-error "sets-and-bags-088" (set-difference nums3 syms3)))

(define check-089
  (test-error "sets-and-bags-089" (set-xor nums3 syms3)))

(define check-090
  (test-error "sets-and-bags-090" (set-union! nums3 syms3)))

(define check-091
  (test-error "sets-and-bags-091" (set-intersection! nums3 syms3)))

(define check-092
  (test-error "sets-and-bags-092" (set-difference! nums3 syms3)))

(define check-093
  (test-error "sets-and-bags-093" (set-xor! nums3 syms3)))

(define whole (set eqv-comparator 1 2 3 4 5 6 7 8 9 10))
(define whole2 (set-copy whole))
(define whole3 (set-copy whole))
(define whole4 (set-copy whole))
(define bottom (set eqv-comparator 1 2 3 4 5))
(define top (set eqv-comparator 6 7 8 9 10))
(define-values (topx bottomx)
  (set-partition big whole))

(define check-094
  (test-equal "sets-and-bags-94" #t (set=? top (begin
                                                 (set-partition! big whole4)
                                                 (set-filter big whole)))))

(define check-095
  (test-equal "sets-and-bags-95" #t (set=? bottom (set-remove big whole))))

(define check-096
  (test-equal "sets-and-bags-96" #t (begin
                                    (set-filter! big whole2)
                                    (not (set-contains? whole2 1)))))

(define check-097
  (test-equal "sets-and-bags-97" #t (begin
                                    (set-remove! big whole3)
                                    (not (set-contains? whole3 10)))))

(define check-098
  (test-equal "sets-and-bags-98" #t (set=? top topx)))

(define check-099
  (test-equal "sets-and-bags-99" #t (set=? bottom bottomx)))

(define check-100
  (test-equal "sets-and-bags-100" #t (set=? top whole4)))

(define check-101
  (test-equal "sets-and-bags-101" 5 (set-count big whole)))

(define hetero (set eqv-comparator 1 2 'a 3 4))
(define homo (set eqv-comparator 1 2 3 4 5))

(define check-102
  (test-equal "sets-and-bags-102" 'a (set-find symbol? hetero (lambda () (error* "wrong")))))

(define check-103
  (test-error "sets-and-bags-103" (set-find symbol? homo (lambda () (error* "wrong")))))

(define check-104
  (test-equal "sets-and-bags-104" #t (set-any? symbol? hetero)))

(define check-105
  (test-equal "sets-and-bags-105" #t (set-any? number? hetero)))

(define check-106
  (test-equal "sets-and-bags-106" #t (not (set-every? symbol? hetero))))

(define check-107
  (test-equal "sets-and-bags-107" #t (not (set-every? number? hetero))))

(define check-108
  (test-equal "sets-and-bags-108" #t (not (set-any? symbol? homo))))

(define check-109
  (test-equal "sets-and-bags-109" #t (set-every? number? homo)))

(define bucket (set string-ci-comparator "abc" "def"))

(define check-110
  (test-equal "sets-and-bags-110" string-ci-comparator (set-element-comparator bucket)))

(define check-111
  (test-equal "sets-and-bags-111" #t (set-contains? bucket "abc")))

(define check-112
  (test-equal "sets-and-bags-112" #t (set-contains? bucket "ABC")))

(define check-113
  (test-equal "sets-and-bags-113" "def" (set-member bucket "DEF" "fqz")))

(define check-114
  (test-equal "sets-and-bags-114" "fqz" (set-member bucket "lmn" "fqz")))

(define nums4 (set number-comparator 1 2 3))

(define nums42 (set-replace nums4 2.0))

(define check-115
  (test-equal "sets-and-bags-115" #t (set-any? inexact? nums42)))

(define check-116
  (test-equal "sets-and-bags-116" #t (begin (set-replace! nums4 2.0)
                                            (set-any? inexact? nums4))))

(define sos
  (set set-comparator
       (set equal-comparator '(2 . 1) '(1 . 1) '(0 . 2) '(0 . 0))
       (set equal-comparator '(2 . 1) '(1 . 1) '(0 . 0) '(0 . 2))))

(define check-117
  (test-equal "sets-and-bags-117" 1 (set-size sos)))

(define nums5 (bag number-comparator))
(define syms-bis (bag eq-comparator 'a 'b 'c 'd))
(define nums52 (bag-copy nums5))
(define syms2-bis (bag-copy syms-bis))
(define esyms-bis (bag eq-comparator))

(define check-118
  (test-equal "sets-and-bags-118" #t (bag-empty? esyms-bis)))

(define total-bis 0)

(define check-119
  (test-equal "sets-and-bags-119" #t (bag? nums5)))

(define check-120
  (test-equal "sets-and-bags-120" #t (bag? syms-bis)))

(define check-121
  (test-equal "sets-and-bags-121" #t (bag? nums52)))

(define check-122
  (test-equal "sets-and-bags-122" #t (bag? syms2-bis)))

(define check-123
  (test-equal "sets-and-bags-123" #t (not (bag? 'a))))

(define check-124
  (test-equal "sets-and-bags-124" 4 (begin
                                      (bag-adjoin! nums5 2)
                                      (bag-adjoin! nums5 3)
                                      (bag-adjoin! nums5 4)

                                      (bag-size (bag-adjoin nums5 5)))))

(define check-125
  (test-equal "sets-and-bags-125" 3 (bag-size nums5)))

(define check-126
  (test-equal "sets-and-bags-126" 3 (bag-size (bag-delete syms-bis 'd))))

(define check-127
  (test-equal "sets-and-bags-127" 2 (bag-size (bag-delete-all syms-bis '(c d)))))

(define check-128
  (test-equal "sets-and-bags-128" 4 (bag-size syms-bis)))

(define check-129
  (test-equal "sets-and-bags-129" 4 (begin (bag-adjoin! syms-bis 'e 'f)
                                        (bag-size (bag-delete-all! syms-bis '(e f))))))

(define check-130
  (test-equal "sets-and-bags-130" 3 (bag-size nums5)))

(define check-131
  (test-equal "sets-and-bags-131" 3 (begin (bag-delete! nums5 1) (bag-size nums5))))

(define check-132
  (test-equal "sets-and-bags-132" #t (begin   (set! nums52 (bag-map number-comparator (lambda (x) (* 10 x)) nums5))
                                           (bag-contains? nums52 30))))

(define check-133
  (test-equal "sets-and-bags-133" #t (not (bag-contains? nums52 3))))

(define check-134
  (test-equal "sets-and-bags-134" 90 (begin (bag-for-each (lambda (x) (set! total-bis (+ total-bis x))) nums52)
                                         total-bis)))
(define check-135
  (test-equal "sets-and-bags-135" 12 (bag-fold + 3 nums5)))

(define check-136
  (test-equal "sets-and-bags-136"
   #t
   (begin
     (set! nums5 (bag eqv-comparator 10 20 30 40 50))
     (bag=? nums5 (bag-unfold
                   (lambda (i) (= i 0))
                   (lambda (i) (* i 10))
                   (lambda (i) (- i 1))
                   5
                   eqv-comparator)))))

(define check-137
  (test-equal "sets-and-bags-137" '(a) (bag->list (bag eq-comparator 'a))))

(define check-138
  (test-equal "sets-and-bags-138" 2 (begin (set! syms2-bis (list->bag eq-comparator '(e f))) (bag-size syms2-bis))))

(define check-139
  (test-equal "sets-and-bags-139" #t (bag-contains? syms2-bis 'e)))

(define check-140
  (test-equal "sets-and-bags-140" #t (bag-contains? syms2-bis 'f)))

(define check-141
  (test-equal "sets-and-bags-141" 4 (begin (list->bag! syms2-bis '(e f))
                                        (bag-size syms2-bis))))

(define yam2 (bag char-comparator #\y #\a #\m))

(define (failure/insert/bis insert ignore)
  (insert 1))

(define (failure/ignore/bis insert ignore)
  (ignore 2))

(define (success/update/bis element update remove)
  (update #\b 3))

(define (success/remove/bis element update remove)
  (remove 4))

(define yam2! (bag char-comparator #\y #\a #\m #\!))

(define bam2 (bag char-comparator #\b #\a #\m))

(define ym-bis (bag char-comparator #\y #\m))

(define-values (bag1 obj1-bis)
  (bag-search! (bag-copy yam2) #\! failure/insert/bis error*))

(define check-142
  (test-equal "sets-and-bags-142" #t (bag=? yam2! bag1)))

(define check-143
  (test-equal "sets-and-bags-143" 1 obj1-bis))

(define-values (bag2 obj2-bis)
  (bag-search! (bag-copy yam2) #\! failure/ignore/bis error*))

(define check-144
  (test-equal "sets-and-bags-144" #t (bag=? yam2 bag2)))

(define check-145
  (test-equal "sets-and-bags-145" 2 obj2-bis))

(define-values (bag3 obj3-bis)
  (bag-search! (bag-copy yam2) #\y error* success/update/bis))

(define check-146
  (test-equal "sets-and-bags-146" #t (bag=? bam2 bag3)))

(define check-147
  (test-equal "sets-and-bags-147" 3 obj3-bis))

(define-values (bag4 obj4-bis)
  (bag-search! (bag-copy yam2) #\a error* success/remove/bis))

(define check-148
  (test-equal "sets-and-bags-148" #t (bag=? ym-bis bag4)))

(define check-149
  (test-equal "sets-and-bags-149" 4 obj4-bis))

(define mybag (bag eqv-comparator 1 1 1 1 1 2 2))

(define check-150
  (test-equal "sets-and-bags-150" 5 (bag-element-count mybag 1)))

(define check-151
  (test-equal "sets-and-bags-151" 0 (bag-element-count mybag 3)))

(define bag2-bis (bag number-comparator 1 2))
(define other-bag2 (bag number-comparator 1 2))
(define bag3-bis (bag number-comparator 1 2 3))
(define bag4-bis (bag number-comparator 1 2 3 4))
(define bagx (bag number-comparator 10 20 30 40))
(define bagy (bag number-comparator 10 20 20 30 40))

(define check-152
  (test-equal "sets-and-bags-152" #t (bag=? bag2-bis other-bag2)))

(define check-153
  (test-equal "sets-and-bags-153" #t (not (bag=? bag2-bis bag3-bis))))

(define check-154
  (test-equal "sets-and-bags-154" #t (not (bag=? bag2-bis bag3-bis other-bag2))))

(define check-155
  (test-equal "sets-and-bags-155" #t (bag<? bag2-bis bag3-bis bag4-bis)))

(define check-156
  (test-equal "sets-and-bags-156" #t (not (bag<? bag2-bis other-bag2))))

(define check-157
  (test-equal "sets-and-bags-157" #t (bag<=? bag2-bis other-bag2 bag3-bis)))

(define check-158
  (test-equal "sets-and-bags-158" #t (not (bag<=? bag2-bis bag3-bis other-bag2))))

(define check-159
  (test-equal "sets-and-bags-159" #t (bag>? bag4-bis bag3-bis bag2-bis)))

(define check-160
  (test-equal "sets-and-bags-160" #t (not (bag>? bag2-bis other-bag2))))

(define check-161
  (test-equal "sets-and-bags-161" #t (bag>=? bag3-bis other-bag2 bag2-bis)))

(define check-162
  (test-equal "sets-and-bags-162" #t (not (bag>=? other-bag2 bag3-bis bag2-bis))))

(define check-163
  (test-equal "sets-and-bags-163" #t (not (bag<? bag2-bis other-bag2))))

(define check-164
  (test-equal "sets-and-bags-164" #t (bag<=? bagx bagy)))

(define check-165
  (test-equal "sets-and-bags-165" #t (not (bag<=? bagy bagx))))

(define check-166
  (test-equal "sets-and-bags-166" #t (bag<? bagx bagy)))

(define check-167
  (test-equal "sets-and-bags-167" #t (not (bag<? bagy bagx))))

(define check-168
  (test-equal "sets-and-bags-168" #t (bag>=? bagy bagx)))

(define check-169
  (test-equal "sets-and-bags-169" #t (not (bag>=? bagx bagy))))

(define check-170
  (test-equal "sets-and-bags-170" #t (bag>? bagy bagx)))

(define check-171
  (test-equal "sets-and-bags-171" #t (not (bag>? bagx bagy))))

(define one (bag eqv-comparator 10))

(define two (bag eqv-comparator 10 10))

(define check-172
  (test-equal "sets-and-bags-172" #t (not (bag=? one two))))

(define check-173
  (test-equal "sets-and-bags-173" #t (bag<? one two)))

(define check-174
  (test-equal "sets-and-bags-174" #t (not (bag>? one two))))

(define check-175
  (test-equal "sets-and-bags-175" #t (bag<=? one two)))

(define check-176
  (test-equal "sets-and-bags-176" #t (not (bag>? one two))))

(define check-177
  (test-equal "sets-and-bags-177" #t (bag=? two two)))

(define check-178
  (test-equal "sets-and-bags-178" #t (not (bag<? two two))))

(define check-179
  (test-equal "sets-and-bags-179" #t (not (bag>? two two))))

(define check-180
  (test-equal "sets-and-bags-180" #t (bag<=? two two)))

(define check-181
  (test-equal "sets-and-bags-181" #t (bag>=? two two)))

(define check-182
  (test-equal "sets-and-bags-182" '((10 . 2))
              (let ((result '()))
                (bag-for-each-unique
                 (lambda (x y) (set! result (cons (cons x y) result)))
                 two)
                result)))

(define check-183
  (test-equal "sets-and-bags-183" 25 (bag-fold + 5 two)))

(define check-184
  (test-equal "sets-and-bags-184" 12 (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two)))

(define bag-abcd (bag eq-comparator 'a 'b 'c 'd))
(define bag-efgh (bag eq-comparator 'e 'f 'g 'h))
(define bag-abgh (bag eq-comparator 'a 'b 'g 'h))
(define bag-other-abcd (bag eq-comparator 'a 'b 'c 'd))
(define bag-other-efgh (bag eq-comparator 'e 'f 'g 'h))
(define bag-other-abgh (bag eq-comparator 'a 'b 'g 'h))
(define bag-all (bag eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
(define bag-none (bag eq-comparator))
(define bag-ab (bag eq-comparator 'a 'b))
(define bag-cd (bag eq-comparator 'c 'd))
(define bag-ef (bag eq-comparator 'e 'f))
(define bag-gh (bag eq-comparator 'g 'h))
(define bag-cdgh (bag eq-comparator 'c 'd 'g 'h))
(define bag-abcdgh (bag eq-comparator 'a 'b 'c 'd 'g 'h))
(define bag-abefgh (bag eq-comparator 'a 'b 'e 'f 'g 'h))

(define check-185
  (test-equal "sets-and-bags-185" #t (bag-disjoint? bag-abcd bag-efgh)))

(define check-186
  (test-equal "sets-and-bags-186" #t (not (bag-disjoint? bag-abcd bag-ab))))

(define check-187
  (test-equal "sets-and-bags-187" #t (bag=? bag-abcd (bag-union bag-abcd))))

(define check-188
  (test-equal "sets-and-bags-188" #t (bag=? bag-all (bag-union bag-abcd bag-efgh))))

(define check-189
  (test-equal "sets-and-bags-189" #t (bag=? bag-abcdgh (bag-union bag-abcd bag-abgh))))

(define check-190
  (test-equal "sets-and-bags-190" #t (bag=? bag-abefgh (bag-union bag-efgh bag-abgh))))


(define bag-efgh2-bis (bag-copy bag-efgh))

(define check-191
  (test-equal "sets-and-bags-191" #t (bag=? bag-efgh (begin (bag-union! bag-efgh2-bis)
                                                            bag-efgh2-bis))))

(define check-192
  (test-equal "sets-and-bags-192" #t (bag=? bag-abefgh (begin (bag-union! bag-efgh2-bis bag-abgh) bag-efgh2-bis))))

(define check-193
  (test-equal "sets-and-bags-193" #t (bag=? bag-abcd (bag-intersection bag-abcd))))

(define check-194
  (test-equal "sets-and-bags-194" #t (bag=? bag-none (bag-intersection bag-abcd bag-efgh))))

(define bag-abcd2-bis (bag-copy bag-abcd))

(define check-195
  (test-equal "sets-and-bags-195" #t (bag=? bag-abcd (begin (bag-intersection! bag-abcd2-bis) bag-abcd2-bis))))

(define check-196
  (test-equal "sets-and-bags-196" #t (bag=? bag-none (begin (bag-intersection! bag-abcd2-bis bag-efgh) bag-abcd2-bis))))

(define check-197
  (test-equal "sets-and-bags-197" #t (bag=? bag-ab (bag-intersection bag-abcd bag-abgh))))

(define check-198
  (test-equal "sets-and-bags-198" #t (bag=? bag-ab (bag-intersection bag-abgh bag-abcd))))

(define check-199
  (test-equal "sets-and-bags-199" #t (bag=? bag-abcd (bag-difference bag-abcd))))

(define check-200
  (test-equal "sets-and-bags-200" #t (bag=? bag-cd (bag-difference bag-abcd bag-ab))))

(define check-201
  (test-equal "sets-and-bags-201" #t (bag=? bag-abcd (bag-difference bag-abcd bag-gh))))

(define check-202
  (test-equal "sets-and-bags-202" #t (bag=? bag-none (bag-difference bag-abcd bag-abcd))))

(define bag-abcd3-bis (bag-copy bag-abcd))

(define check-203
  (test-equal "sets-and-bags-203" #t (bag=? bag-abcd (begin (bag-difference! bag-abcd3-bis) bag-abcd3-bis))))

(define check-204
  (test-equal "sets-and-bags-204" #t (bag=? bag-none (begin (bag-difference! bag-abcd3-bis bag-abcd) bag-abcd3-bis))))

(define check-205
  (test-equal "sets-and-bags-205" #t (bag=? bag-cdgh (bag-xor bag-abcd bag-abgh))))

(define check-206
  (test-equal "sets-and-bags-206" #t (bag=? bag-all (bag-xor bag-abcd bag-efgh))))

(define check-207
  (test-equal "sets-and-bags-207" #t (bag=? bag-none (bag-xor bag-abcd bag-other-abcd))))

(define bag-abcd4-bis (bag-copy bag-abcd))

(define check-208
  (test-equal "sets-and-bags-208" #t (bag=? bag-none (bag-xor! bag-abcd4-bis bag-other-abcd))))

(define bag-abab (bag eq-comparator 'a 'b 'a 'b))

(define check-209
  (test-equal "sets-and-bags-209" #t (bag=? bag-ab (bag-sum bag-ab))))

(define bag-ab2 (bag-copy bag-ab))

(define check-210
  (test-equal "sets-and-bags-210" #t (bag=? bag-ab (bag-sum! bag-ab2))))

(define check-211
  (test-equal "sets-and-bags-211" #t (bag=? bag-abab (bag-sum! bag-ab2 bag-ab))))

(define check-212
  (test-equal "sets-and-bags-212" #t (bag=? bag-abab bag-ab2)))

(define check-213
  (test-equal "sets-and-bags-213" #t (bag=? bag-abab (bag-product 2 bag-ab))))

(define bag-ab3 (bag-copy bag-ab))

(define check-214
  (test-equal "sets-and-bags-214" #t (bag=? bag-abab (begin (bag-product! 2 bag-ab3) bag-ab3))))

(define check-215
  (test-equal "sets-and-bags-215" #t (bag=? bag-other-abcd bag-abcd)))

(define check-216
  (test-equal "sets-and-bags-216" #t (bag=? bag-other-abcd bag-abcd)))

(define check-217
  (test-equal "sets-and-bags-217" #t (bag=? bag-other-efgh bag-efgh)))

(define check-218
  (test-equal "sets-and-bags-218" #t (bag=? bag-other-abgh bag-abgh)))

(define bag-nums (bag number-comparator 1 2 3))

(define bag-syms (bag eq-comparator 'a 'b 'c))

(define check-219
  (test-error "sets-and-bags-219" (bag=? bag-nums bag-syms)))

(define check-220
  (test-error "sets-and-bags-220" (bag<? bag-nums bag-syms)))

(define check-221
  (test-error "sets-and-bags-221" (bag<=? bag-nums bag-syms)))

(define check-222
  (test-error "sets-and-bags-222" (bag>? bag-nums bag-syms)))

(define check-223
  (test-error "sets-and-bags-223" (bag>=? bag-nums bag-syms)))

(define check-224
  (test-error "sets-and-bags-224" (bag-union bag-nums bag-syms)))

(define check-225
  (test-error "sets-and-bags-225" (bag-intersection bag-nums bag-syms)))

(define check-226
  (test-error "sets-and-bags-226" (bag-difference bag-nums bag-syms)))

(define check-227
  (test-error "sets-and-bags-227" (bag-xor bag-nums bag-syms)))

(define check-228
  (test-error "sets-and-bags-228" (bag-union! bag-nums bag-syms)))

(define check-229
  (test-error "sets-and-bags-229" (bag-intersection! bag-nums bag-syms)))

(define check-230
  (test-error "sets-and-bags-230" (bag-difference! bag-nums bag-syms)))

(define bag-whole (bag eqv-comparator 1 2 3 4 5 6 7 8 9 10))
(define bag-whole2 (bag-copy bag-whole))
(define bag-whole3 (bag-copy bag-whole))
(define bag-whole4 (bag-copy bag-whole))
(define bag-bottom (bag eqv-comparator 1 2 3 4 5))
(define bag-top (bag eqv-comparator 6 7 8 9 10))

(define-values (topx-bis bottomx-bis)
  (bag-partition big bag-whole))

(define check-231
  (test-equal "sets-and-bags-231" #t (bag=? bag-top (begin (bag-partition! big bag-whole4) (bag-filter big bag-whole)))))

(define check-232
  (test-equal "sets-and-bags-232" #t (bag=? bag-bottom (bag-remove big bag-whole))))

(define check-233
  (test-equal "sets-and-bags-233" #t (begin (bag-filter! big bag-whole2) (not (bag-contains? bag-whole2 1)))))

(define check-234
  (test-equal "sets-and-bags-234" #t (begin
                                    (bag-remove! big bag-whole3)
                                    (not (bag-contains? bag-whole3 10)))))

(define check-235
  (test-equal "sets-and-bags-235" #t (bag=? bag-top topx-bis)))

(define check-236
  (test-equal "sets-and-bags-236" #t (bag=? bag-bottom bottomx-bis)))

(define check-237
  (test-equal "sets-and-bags-237" #t (bag=? bag-top bag-whole4)))

(define check-238
  (test-equal "sets-and-bags-238" 5 (bag-count big bag-whole)))

(define hetero-bis (bag eqv-comparator 1 2 'a 3 4))
(define homo-bis (bag eqv-comparator 1 2 3 4 5))

(define check-239
  (test-equal "sets-and-bags-239" 'a (bag-find symbol? hetero-bis (lambda () (error* "wrong")))))

(define check-240
  (test-error "sets-and-bags-240" (bag-find symbol? homo-bis (lambda () (error* "wrong")))))

(define check-241
  (test-equal "sets-and-bags-241" #t (bag-any? symbol? hetero-bis)))

(define check-242
  (test-equal "sets-and-bags-242" #t (bag-any? number? hetero-bis)))

(define check-243
  (test-equal "sets-and-bags-243" #t (not (bag-every? symbol? hetero-bis))))

(define check-244
  (test-equal "sets-and-bags-244" #t (not (bag-every? number? hetero-bis))))

(define check-245
  (test-equal "sets-and-bags-245" #t (not (bag-any? symbol? homo-bis))))

(define check-246
  (test-equal "sets-and-bags-246" #t (bag-every? number? homo-bis)))

(define bag-bucket (bag string-ci-comparator "abc" "def"))

(define check-247
  (test-equal "sets-and-bags-247" string-ci-comparator (bag-element-comparator bag-bucket)))

(define check-248
  (test-equal "sets-and-bags-248" #t (bag-contains? bag-bucket "abc")))

(define check-249
  (test-equal "sets-and-bags-249" #t (bag-contains? bag-bucket "ABC")))

(define check-250
  (test-equal "sets-and-bags-250" "def" (bag-member bag-bucket "DEF" "fqz")))

(define check-251
  (test-equal "sets-and-bags-251" "fqz" (bag-member bag-bucket "lmn" "fqz")))

(define ter-nums (bag number-comparator 1 2 3))

(define ter-nums2 (bag-replace ter-nums 2.0))

(define check-252
  (test-equal "sets-and-bags-252" #t (bag-any? inexact? ter-nums2)))

(define check-253
  (test-equal "sets-and-bags-253" #t (begin (bag-replace! ter-nums 2.0) (bag-any? inexact? ter-nums))))

(define bob
  (bag bag-comparator
       (bag eqv-comparator 1 2)
       (bag eqv-comparator 1 2)))

(define check-254
  (test-equal "sets-and-bags-254" 2 (bag-size bob)))

(define mybag-bis (bag number-comparator 1 2))

(define check-255
  (test-equal "sets-and-bags-255" 2 (bag-size mybag-bis)))

(define check-256
  (test-equal "sets-and-bags-256" 3 (begin
                                   (bag-adjoin! mybag-bis 1)
                                   (bag-size mybag-bis))))

(define check-257
  (test-equal "sets-and-bags-257" 2 (bag-unique-size mybag-bis)))

(define check-258
  (test-equal "sets-and-bags-258" 2 (begin
                                   (bag-delete! mybag-bis 2)
                                   (bag-delete! mybag-bis 2)
                                   (bag-size mybag-bis))))

(define check-259
  (test-equal "sets-and-bags-259" 5 (begin
                                   (bag-increment! mybag-bis 1 3)
                                   (bag-size mybag-bis))))

(define side-effect (bag-decrement! mybag-bis 1 2))

(define check-261
  (test-equal "sets-and-bags-261" 3 (bag-size mybag-bis)))

(define check-262
  (test-equal "sets-and-bags-262" 0 (begin
                                   (bag-decrement! mybag-bis 1 5)
                                   (bag-size mybag-bis))))

(define multi (bag eqv-comparator 1 2 2 3 3 3))
(define single (bag eqv-comparator 1 2 3))
(define singleset (set eqv-comparator 1 2 3))
(define minibag (bag eqv-comparator 'a 'a))
(define alist '((a . 2)))

(define check-263
  (test-equal "sets-and-bags-263" alist (bag->alist minibag)))

(define check-264
  (test-equal "sets-and-bags-264" #t (bag=? minibag (alist->bag eqv-comparator alist))))

(define check-265
  (test-equal "sets-and-bags-265" #t (set=? singleset (bag->set single))))

(define check-266
  (test-equal "sets-and-bags-266" #t (set=? singleset (bag->set multi))))

(define check-267
  (test-equal "sets-and-bags-267" #t (bag=? single (set->bag singleset))))

(define check-268
  (test-equal "sets-and-bags-268" #t (not (bag=? multi (set->bag singleset)))))

(define check-269
  (test-equal "sets-and-bags-269" #t (begin (set->bag! minibag singleset)
                                         (bag-contains? minibag 1))))

(define abb (bag eq-comparator 'a 'b 'b))

(define aab (bag eq-comparator 'a 'a 'b))

(define total-ter (bag-sum abb aab))

(define check-270
  (test-equal "sets-and-bags-270" 3 (bag-count (lambda (x) (eqv? x 'a)) total-ter)))

(define check-271
  (test-equal "sets-and-bags-271" 3 (bag-count (lambda (x) (eqv? x 'b)) total-ter)))

(define check-272
  (test-equal "sets-and-bags-272" 12 (bag-size (bag-product 2 total-ter))))

(define bag1-bis (bag eqv-comparator 1))

(define check-273
  (test-equal "sets-and-bags-273" 2 (begin (bag-sum! bag1-bis bag1-bis) (bag-size bag1-bis))))

(define check-274
  (test-equal "sets-and-bags-274" 4 (begin (bag-product! 2 bag1-bis)
                                        (bag-size bag1-bis))))

(define a (set number-comparator 1 2 3))
(define b (set number-comparator 1 2 4))
(define aa (bag number-comparator 1 2 3))
(define bb (bag number-comparator 1 2 4))

(define check-275
  (test-equal "sets-and-bags-275" #t (not (=? set-comparator a b))))

(define check-276
  (test-equal "sets-and-bags-276" #t (=? set-comparator a (set-copy a))))

(define check-277
  (test-error "sets-and-bags-277" (<? set-comparator a b)))

(define check-278
  (test-equal "sets-and-bags-278" #t (not (=? bag-comparator aa bb))))

(define check-279
  (test-equal "sets-and-bags-279" #t (=? bag-comparator aa (bag-copy aa))))

(define check-280
  (test-error "sets-and-bags-280" (<? bag-comparator aa bb)))

(define check-281
  (test-equal "sets-and-bags-281" #t (not (=? (make-default-comparator) a aa))))

(test-end "sets-and-bags")
