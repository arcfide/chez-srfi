#!r6rs
(import
  (rnrs)
  (srfi :127 lazy-sequences)
  (srfi :64 testing))

(test-begin "lseqs")

;; Make-generator for tests cloned from SRFI 121
(define (make-generator . args)
  (lambda () (if (null? args)
                 (eof-object)
                 (let ((next (car args)))
                   (set! args (cdr args))
                   next))))

;; Make-lseq creates an lseq, like list, but guarantees the use of a generator
(define (make-lseq . args) (generator->lseq (apply make-generator args)))


(define one23 (make-lseq 1 2 3))

(define check-001
  (test-equal "lseq-1" 1 (car one23)))

(define check-002
  (test-equal "lseq-2" #t (procedure? (cdr one23))))

(define check-003
  (test-equal "lseq-3" '(1 2 3) (lseq-realize one23)))

(define check-004
  (test-equal "lseq-4" #t (lseq? '())))

(define check-005
  (test-equal "lseq-5" #t (lseq? '(1 2 3))))

(define check-006
  (test-equal "lseq-6" #t (lseq? (make-lseq 1 2 3))))

(define check-007
  (test-equal "lseq-7" #t (lseq? (cons 'x (lambda () 'x)))))

(define check-008
  (test-equal "lseq-8" #t (lseq=? = '() '())))

(define check-009
  (test-equal "lseq-9" #t (lseq=? = '(1 2 3) '(1 2 3))))

(define check-010
  (test-equal "lseq-10" #t (lseq=? = (make-lseq 1 2 3)
                         (make-lseq 1 2 3))))

(define check-011
  (test-equal "lseq-11" #t (lseq=? = (make-lseq 1 2 3) '(1 2 3))))
(define check-012
  (test-error "lseq-12" (lseq-car (make-generator))))

(define check-013
  (test-equal "lseq-13" 1 (lseq-car (make-lseq 1 2 3))))

(define check-014
  (test-equal "lseq-14" 1 (lseq-car '(1 2 3))))

(define check-015
  (test-error "lseq-15" (lseq-car 2)))

(define check-016
  (test-error "lseq-16" #t (lseq-first (make-generator))))

(define check-017
  (test-equal "lseq-17" 1 (lseq-first (make-lseq 1 2 3))))

(define check-018
  (test-equal "lseq-18" 1 (lseq-first '(1 2 3))))

(define check-019
  (test-error "lseq-19" (lseq-first 2)))

(define check-020
  (test-error "lseq-20" (lseq-cdr (make-generator))))

(define check-021
  (test-equal "lseq-21" 2 (lseq-cdr '(1 . 2))))

(define check-022
  (test-equal "lseq-22" 2 (lseq-car (lseq-cdr '(1 2 3)))))

(define check-023
  (test-equal "lseq-23" 2 (lseq-car (lseq-cdr (make-lseq 1 2 3)))))

(define check-024
  (test-error "lseq-24" (lseq-rest (make-generator))))

(define check-025
  (test-equal "lseq-25" 2 (lseq-rest '(1 . 2))))

(define check-026
  (test-equal "lseq-26" 2 (lseq-car (lseq-rest '(1 2 3)))))

(define check-027
  (test-equal "lseq-27" 2 (lseq-car (lseq-rest (make-lseq 1 2 3)))))

(define check-028
  (test-error "lseq-28" (lseq-rest 2)))

(define check-029
  (test-error "lseq-29" (lseq-ref '() 0)))

(define check-030
  (test-equal "lseq-30" 1 (lseq-ref '(1) 0)))

(define check-031
  (test-equal "lseq-31" 2 (lseq-ref '(1 2) 1)))

(define check-032
  (test-error "lseq-32" (lseq-ref (make-lseq) 0)))

(define check-033
  (test-equal "lseq-33" 1 (lseq-ref (make-lseq 1) 0)))

(define check-034
  (test-equal "lseq-34" 1 (lseq-ref (make-lseq 1 2) 0)))

(define check-035
  (test-equal "lseq-35" 2 (lseq-ref (make-lseq 1 2) 1)))

(define check-036
  (test-error "lseq-36" (lseq-take '() 1)))

(define check-037
  (test-error "lseq-37" (lseq-take (make-lseq) 1)))

(define check-038
  ;; test laziness
  (test-equal "lseq-38" #t (procedure? (cdr (lseq-take '(1 2 3 4 5) 3)))))

(define check-039
  (test-equal "lseq-39" '(1 2 3) (lseq-realize (lseq-take '(1 2 3 4 5) 3))))

(define check-040
  (test-error "lseq-40" (lseq-drop '() 1)))

(define check-041
  (test-error "lseq-41" (lseq-drop (make-lseq 1) 2)))

(define check-042
  (test-equal "lseq-42" '(3 4 5) (lseq-realize (lseq-drop '(1 2 3 4 5) 2))))

(define check-043
  (test-equal "lseq-43" '(3 4 5) (lseq-realize (lseq-drop (make-lseq 1 2 3 4 5) 2))))

(define check-044
  (test-equal "lseq-44" '() (lseq-realize '())))

(define check-045
  (test-equal "lseq-45" '(1 2 3) (lseq-realize '(1 2 3))))

(define check-046
  (test-equal "lseq-46" '() (lseq-realize (make-lseq))))

(define check-047
  (test-equal "lseq-47" '(1 2 3) (lseq-realize (make-lseq 1 2 3))))

(define g (lseq->generator '(1 2 3)))

(define check-048 (test-equal "lseq-48" 1 (g)))

(define check-049 (test-equal "lseq-49" 2 (g)))

(define check-050 (test-equal "lseq-50" 3 (g)))
(define check-051 (test-equal "lseq-51" #t (eof-object? (g))))

(define g1 (lseq->generator (make-lseq 1 2 3)))

(define check-052
  (test-equal "lseq-52" 1 (g1)))

(define check-053
  (test-equal "lseq-53" 2 (g1)))

(define check-054
  (test-equal "lseq-54" 3 (g1)))

(define check-055
  (test-equal "lseq-55" #t (eof-object? (g))))

(define check-056
  (test-equal "lseq-56" 0 (lseq-length '())))

(define check-057
  (test-equal "lseq-57" 3 (lseq-length '(1 2 3))))

(define check-058
  (test-equal "lseq-58" 3 (lseq-length (make-lseq 1 2 3))))

(define check-059
  (test-equal "lseq-59" '(1 2 3 a b c) (lseq-realize (lseq-append '(1 2 3) '(a b c)))))

(define one23abc (lseq-append (make-lseq 1 2 3) (make-lseq 'a 'b 'c)))

(define check-060
  (test-equal "lseq-60" #t (procedure? (cdr one23abc))))

(define check-061
  (test-equal "lseq-61" '(1 2 3 a b c) (lseq-realize one23abc)))

(define one2345 (make-lseq 1 2 3 4 5))
(define oddeven (make-lseq 'odd 'even 'odd 'even 'odd 'even 'odd 'even))

(define check-062
  (test-equal "lseq-62" '((one 1 odd) (two 2 even) (three 3 odd))
              (lseq-realize (lseq-zip '(one two three) one2345 oddeven))))

(define check-063
  (test-equal "lseq-63" '() (lseq-map - '())))

(define check-064
  (test-equal "lseq-64" '(-1 -2 -3) (lseq-realize (lseq-map - '(1 2 3)))))

(define check-065
  (test-equal "lseq-65" '(-1 -2 -3) (lseq-realize (lseq-map - (make-lseq 1 2 3)))))

(define check-066
  (test-equal "lseq-66" #t (procedure? (cdr (lseq-map - '(1 2 3))))))

(define output '())
(define out! (lambda (x) (set! output (cons x output))))

(define check-068
  (test-equal "lseq-68" output (begin (lseq-for-each out! '()) '())))

(define check-069
  (test-equal "lseq-69" '(c b a) (begin (lseq-for-each out! '(a b c)) output)))

(define check-070
  (test-equal "lseq-70" '(3 2 1 c b a) (begin (lseq-for-each out! (make-lseq 1 2 3)) output)))

(define check-071
  (test-equal "lseq-71" '() (lseq-filter odd? '())))

(define odds (lseq-filter odd? '(1 2 3 4 5)))

(define check-072
  (test-equal "lseq-72" #t (procedure? (cdr odds))))

(define check-073
  (test-equal "lseq-73" '(1 3 5) (lseq-realize odds)))

(define check-074
  (test-equal "lseq-74" '(1 3 5) (lseq-realize (lseq-filter odd? (make-lseq 1 2 3 4 5)))))

(define check-075
  (test-equal "lseq-75" '() (lseq-remove even? '())))

(define odds1 (lseq-remove even? '(1 2 3 4 5)))

(define check-076
  (test-equal "lseq-76" #t (procedure? (cdr odds1))))

(define check-077
  (test-equal "lseq-77" '(1 3 5) (lseq-realize odds1)))

(define check-078
  (test-equal "lseq-78" '(1 3 5) (lseq-realize (lseq-remove even? (make-lseq 1 2 3 4 5)))))

(define check-079
  (test-equal "lseq-79" 4 (lseq-find even? '(3 1 4 1 5 9 2 6))))

(define check-080
  (test-equal "lseq-80" 4 (lseq-find even? (make-lseq 3 1 4 1 5 9 2 6))))

(define check-081
  (test-equal "lseq-81" #f (lseq-find negative? (make-lseq 1 2 3 4 5))))

(define check-082
  (test-equal "lseq-82" '(-8 -5 0 0) (lseq-realize (lseq-find-tail even? '(3 1 37 -8 -5 0 0)))))

(define check-083
  (test-equal "lseq-83" '(-8 -5 0 0) (lseq-realize (lseq-find-tail even?
                                                    (make-lseq 3 1 37 -8 -5 0 0)))))

(define check-084
  (test-equal "lseq-84" #f (lseq-find-tail even? '())))

(define check-085
  (test-equal "lseq-85" #f (lseq-find-tail negative? (make-lseq 1 2 3 4 5))))

(define check-086
  (test-equal "lseq-86" '(2 18) (lseq-realize (lseq-take-while even? '(2 18 3 10 22 9)))))

(define check-087
  (test-equal "lseq-87" '(2 18) (lseq-realize (lseq-take-while even?
                                                (make-lseq 2 18 3 10 22 9)))))

(define check-088
  (test-equal "lseq-88" '(2 18) (lseq-realize (lseq-take-while even?
                                                (make-lseq 2 18 3 10 22 9)))))

(define check-089
  (test-equal "lseq-89" '(3 10 22 9) (lseq-drop-while even? '(2 18 3 10 22 9))))

(define check-090
  (test-equal "lseq-90" '(3 10 22 9) (lseq-realize (lseq-drop-while even?
                                                     (make-lseq 2 18 3 10 22 9)))))

(define check-091
  (test-equal "lseq-91" #t (lseq-any integer? '(a 3 b 2.7))))

(define check-092
  (test-equal "lseq-92" #t (lseq-any integer? (make-lseq 'a 3 'b 2.7))))

(define check-093
  (test-equal "lseq-93" #f (lseq-any integer? '(a 3.1 b 2.7))))

(define check-094
  (test-equal "lseq-94" #f (lseq-any integer? (make-lseq 'a 3.1 'b 2.7))))

(define check-095
  (test-equal "lseq-95" #t (lseq-any < '(3 1 4 1 5) '(2 7 1 8 2))))

(define (factorial n)
  (cond
   ((< n 0) #f)
   ((= n 0) 1)
   (else (* n (factorial (- n 1))))))

(define check-096
  (test-equal "lseq-96" 6 (lseq-any factorial '(-1 -2 3 4))))

(define check-097
  (test-equal "lseq-97" 6 (lseq-any factorial (make-lseq -1 -2 3 4))))

(define check-098
  (test-equal "lseq-98" 24 (lseq-every factorial '(1 2 3 4))))

(define check-099
  (test-equal "lseq-99" 24 (lseq-every factorial (make-lseq 1 2 3 4))))

(define check-100
  (test-equal "lseq-100" 2 (lseq-index even? '(3 1 4 1 5 9))))

(define check-101
  (test-equal "lseq-101" 1 (lseq-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

(define check-102
  (test-equal "lseq-102" #f (lseq-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

(define check-103
  (test-equal "lseq-103" '(a b c) (lseq-realize (lseq-memq 'a '(a b c)))))

(define check-104
  (test-equal "lseq-104" '(a b c) (lseq-realize (lseq-memq 'a (make-lseq 'a 'b 'c)))))

(define check-105
  (test-equal "lseq-105" #f (lseq-memq 'a (make-lseq 'b 'c 'd))))

(define check-106
  (test-equal "lseq-106" #f (lseq-memq (list 'a) '(b c d))))

(define check-107
  (test-equal "lseq-107" #f (lseq-memq (list 'a) (make-lseq 'b 'c 'd))))

(define check-108
  (test-equal "lseq-108" '(101 102) (lseq-realize (lseq-memv 101 (make-lseq 100 101 102)))))

(define check-109
  (test-equal "lseq-109" '((a) c) (lseq-realize (lseq-member (list 'a) (make-lseq 'b '(a) 'c)))))

(define check-110
  (test-equal "lseq-110" '(2 3) (lseq-realize (lseq-member 2.0 (make-lseq 1 2 3) =))))

(test-end "lseqs")
