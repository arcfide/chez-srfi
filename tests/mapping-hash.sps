;; Copyright (C) Marc Nieper-Wißkirchen (2016, 2017).  All Rights
;; Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (except (rnrs)
                define-record-type assoc filter find
                fold-right for-each map member partition
                remove)
        (srfi :1)
        (srfi :8)
        (srfi :128)
        (srfi :146 hash)
        (srfi :64))

(test-begin "mapping-hash")

(define (error* msg . args)
  (error 'mapping-hash msg args))

(define comparator (make-default-comparator))

;; SRFI 146: Hashmaps

;; Predicates
(define hashmap0 (hashmap comparator))
(define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap2 (hashmap comparator 'c 1 'd 2 'e 3))
(define hashmap3 (hashmap comparator 'd 1 'e 2 'f 3))

(define check-000
  (test-equal #t
         (hashmap? (hashmap comparator))))

(define check-001
  (test-equal #t
         (not (hashmap? (list 1 2 3)))))

(define check-002
  (test-equal #t
         (hashmap-empty? hashmap0)))

(define check-003
  (test-equal #t
         (not (hashmap-empty? hashmap1))))

(define check-004
  (test-equal #t
         (hashmap-contains? hashmap1 'b)))

(define check-005
  (test-equal #t
         (not (hashmap-contains? hashmap1 '2))))

(define check-006
  (test-equal #t
         (hashmap-disjoint? hashmap1 hashmap3)))

(define check-007
  (test-equal #t
         (not (hashmap-disjoint? hashmap1 hashmap2))))

;; Accessors
(define hashmap11 (hashmap comparator 'a 1 'b 2 'c 3))

(define check-008
  (test-equal
   2
   (hashmap-ref hashmap11 'b)))

(define check-009
  (test-equal
   42
   (hashmap-ref hashmap11 'd (lambda () 42))))

(define check-009-1 (test-error (hashmap-ref hashmap1 'd)))

(define check-010
  (test-equal
   (* 2 2)
   (hashmap-ref hashmap11 'b (lambda () #f) (lambda (x) (* x x)))))

(define check-011
  (test-equal
   3
   (hashmap-ref/default hashmap11 'c 42)))

(define check-012
  (test-equal
   42
   (hashmap-ref/default hashmap11 'd 42)))

(define check-013
  (test-equal
   comparator
   (hashmap-key-comparator hashmap11)))

;; Updaters
(define hashmap21 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap22 (hashmap-set hashmap21 'c 4 'd 4 'd 5))
(define hashmap23 (hashmap-update hashmap21 'b (lambda (x) (* x x))))
(define hashmap24 (hashmap-update/default hashmap21 'd (lambda (x) (* x x)) 4))
(define hashmap25 (hashmap-adjoin hashmap21 'c 4 'd 4 'd 5))
(define hashmap20 (hashmap comparator))

(define check-014
  (test-equal
   3
   (hashmap-ref hashmap25 'c)))

(define check-015
  (test-equal
   4
   (hashmap-ref hashmap25 'd)))

(define check-016
  (test-equal
   4
   (hashmap-ref hashmap22 'c)))

(define check-017
  (test-equal
   5
   (hashmap-ref hashmap22 'd)))

(define check-018
  (test-equal
   #f
   (hashmap-ref/default (hashmap-replace hashmap21 'd 4) 'd #f)))

(define check-019
  (test-equal
   6
   (hashmap-ref (hashmap-replace hashmap21 'c 6) 'c)))

(define check-020
  (test-equal
   42
   (hashmap-ref/default (hashmap-delete hashmap21 'b) 'b 42)))

(define check-021
  (test-equal
   42
   (hashmap-ref/default (hashmap-delete-all hashmap21 '(a b)) 'b 42)))

(define check-022
  (test-equal
   (list hashmap21 2)
   (receive result
       (hashmap-intern hashmap21 'b (lambda () (error* "should not have been invoked")))
     result)))

(define check-023
  (test-equal
   (list 42 42)
   (receive (hashmap value)
       (hashmap-intern hashmap21 'd (lambda () 42))
     (list value (hashmap-ref hashmap 'd)))))

(define check-024
  (test-equal
   4
   (hashmap-ref hashmap23 'b)))

(define check-025
  (test-equal
   16
   (hashmap-ref hashmap24 'd)))

(define check-026
  (test-equal
   'empty
   (hashmap-pop hashmap20 (lambda () 'empty))))

;; TODO: convert to a check
;; (test-equal-assert "hashmap-pop: non-empty hashmap"
;;   (member
;;    (receive (hashmap key value)
;;        (hashmap-pop hashmap1)
;;      (list (hashmap-size hashmap) key value))
;;    '((2 a 1) (2 b 2) (2 c 3)))))

;; The whole hashmap
(define hashmap30 (hashmap comparator))
(define hashmap31 (hashmap comparator 'a 1 'b 2 'c 3))

(define check-027
  (test-equal
   0
   (hashmap-size hashmap30)))

(define check-028
  (test-equal
   3
   (hashmap-size hashmap31)))

(define check-029
  (test-equal
   (list 'b 2)
   (receive result
       (hashmap-find (lambda (key value)
                       (and (eq? key 'b)
                            (= value 2)))
                     hashmap31
                     (lambda () (error* "should not have been called")))
     result)))

(define check-030
  (test-equal
   (list 42)
   (receive result
       (hashmap-find (lambda (key value)
                       (eq? key 'd))
                     hashmap31
                     (lambda ()
                       42))
     result)))

(define check-031
  (test-equal
   2
   (hashmap-count (lambda (key value)
                    (>= value 2))
                  hashmap31)))

(define check-032
  (test-equal #t
         (hashmap-any? (lambda (key value)
                         (= value 3))
                       hashmap31)))

(define check-033
  (test-equal #t
         (not (hashmap-any? (lambda (key value)
                              (= value 4))
                            hashmap31))))

(define check-034
  (test-equal #t
         (hashmap-every? (lambda (key value)
                           (<= value 3))
                         hashmap31)))

(define check-035
  (test-equal #t
         (not (hashmap-every? (lambda (key value)
                                (<= value 2))
                              hashmap31))))

(define check-036
  (test-equal
   3
   (length (hashmap-keys hashmap31))))

(define check-037
  (test-equal
   6
   (fold + 0 (hashmap-values hashmap31))))

(define check-038
  (test-equal
   (list 3 6)
   (receive (keys values)
       (hashmap-entries hashmap31)
     (list (length keys) (fold + 0 values)))))

;; Hashmap and folding
(define hashmap41 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap42 (hashmap-map (lambda (key value)
                                 (values (symbol->string key)
                                         (* 10 value)))
                               comparator
                               hashmap41))

(define check-039
  (test-equal
   20
   (hashmap-ref hashmap42 "b")))

(define check-040
  (test-equal
   6
   (let ((counter 0))
     (hashmap-for-each (lambda (key value)
                         (set! counter (+ counter value)))
                       hashmap41)
     counter)))

(define check-041
  (test-equal
   6
   (hashmap-fold (lambda (key value acc)
                   (+ value acc))
                 0
                 hashmap41)))

(define check-042
  (test-equal
   (+ (* 1 1) (* 2 2) (* 3 3))
   (fold + 0 (hashmap-map->list (lambda (key value)
                                  (* value value))
                                hashmap41))))

(define check-043
  (test-equal
   2
   (hashmap-size (hashmap-filter (lambda (key value)
                                   (<= value 2))
                                 hashmap41))))

(define check-044
  (test-equal
   1
   (hashmap-size (hashmap-remove (lambda (key value)
                                   (<= value 2))
                                 hashmap41))))

(define check-045
  (test-equal
   (list 1 2)
   (receive result
       (hashmap-partition (lambda (key value)
                            (eq? 'b key))
                          hashmap41)
     (map hashmap-size result))))

;; Copying and conversion
(define hashmap51 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap52 (alist->hashmap comparator '((a . 1) (b . 2) (c . 3))))
(define hashmap53 (alist->hashmap! (hashmap-copy hashmap51) '((d . 4) '(c . 5))))

(define check-046
  (test-equal
   3
   (hashmap-size (hashmap-copy hashmap51))))

(define check-047
  (test-equal
   comparator
   (hashmap-key-comparator (hashmap-copy hashmap51))))

(define check-048
  (test-equal
   (cons 'b 2)
   (assq 'b (hashmap->alist hashmap51))))

(define check-049
  (test-equal
   2
   (hashmap-ref hashmap52 'b)))

(define check-050
  (test-equal
   4
   (hashmap-ref hashmap53 'd)))

(define check-051
  (test-equal
   3
   (hashmap-ref hashmap53 'c)))

;; Subhashmaps

(define hashmap61 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap62 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap63 (hashmap comparator 'a 1 'c 3))
(define hashmap64 (hashmap comparator 'a 1 'c 3 'd 4))
(define hashmap65 (hashmap comparator 'a 1 'b 2 'c 6))
(define hashmap66 (hashmap (make-comparator (comparator-type-test-predicate comparator)
                                            (comparator-equality-predicate comparator)
                                            (comparator-ordering-predicate comparator)
                                            (comparator-hash-function comparator))
                           'a 1 'b 2 'c 3))

(define check-052
  (test-equal #t
         (hashmap=? comparator hashmap61 hashmap62)))

(define check-053
  (test-equal #t
         (not (hashmap=? comparator hashmap61 hashmap64))))

(define check-054
  (test-equal #t
         (not (hashmap=? comparator hashmap61 hashmap66))))

(define check-055
  (test-equal #t
         (hashmap<? comparator hashmap63 hashmap61)))

(define check-056
  (test-equal #t
         (not (hashmap<? comparator hashmap63 hashmap61 hashmap62))))

(define check-057
  (test-equal #t
         (hashmap>? comparator hashmap62 hashmap63)))

(define check-058
  (test-equal #t
         (not (hashmap>? comparator hashmap61 hashmap62 hashmap63))))

(define check-059
  (test-equal #t
         (hashmap<=? comparator hashmap63 hashmap62 hashmap61)))

(define check-060
  (test-equal #t
         (not (hashmap<=? comparator hashmap63 hashmap65))))

(define check-061
  (test-equal #t
         (not (hashmap<=? comparator hashmap62 hashmap64))))

(define check-062
  (test-equal #t
         (hashmap>=? comparator hashmap64 hashmap63)))

(define check-063
  (test-equal #t
         (not (hashmap>=? comparator hashmap65 hashmap63))))

;; Set theory operations
(define hashmap71 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap72 (hashmap comparator 'a 1 'b 2 'd 4))
(define hashmap73 (hashmap comparator 'a 1 'b 2))
(define hashmap74 (hashmap comparator 'a 1 'b 2 'c 4))
(define hashmap75 (hashmap comparator 'a 1 'c 3))
(define hashmap76 (hashmap comparator 'd 4 'e 5 'f 6))

(define check-064
  (test-equal
   4
   (hashmap-ref (hashmap-union hashmap71 hashmap72) 'd)))

(define check-065
  (test-equal
   3
   (hashmap-ref (hashmap-union hashmap71 hashmap74) 'c)))

(define check-066
  (test-equal
   6
   (hashmap-size (hashmap-union hashmap71 hashmap72 hashmap76))))

(define check-067
  (test-equal
   3
   (hashmap-ref (hashmap-intersection hashmap71 hashmap74) 'c)))

(define check-068
  (test-equal
   42
   (hashmap-ref/default (hashmap-intersection hashmap71 hashmap75) 'b 42)))

(define check-069
  (test-equal
   2
   (hashmap-size (hashmap-difference hashmap72 hashmap76))))

(define check-070
  (test-equal
   4
   (hashmap-size (hashmap-xor hashmap72 hashmap76))))

;; Comparators

(define hashmap81 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap82 (hashmap comparator 'a 1 'b 2 'c 3))
(define hashmap83 (hashmap comparator 'a 1 'b 2))
(define hashmap84 (hashmap comparator 'a 1 'b 2 'c 4))
(define hashmap85 (hashmap comparator 'a 1 'c 3))
(define hashmap80 (hashmap comparator
                           hashmap81 "a"
                           hashmap82 "b"
                           hashmap83 "c"
                           hashmap84 "d"
                           hashmap85 "e"))

(define check-071
  (test-equal #t
         (comparator? hashmap-comparator)))

(define check-072
  (test-equal
   (list "a" "a" "c" "d" "e")
   (list (hashmap-ref hashmap80 hashmap81)
         (hashmap-ref hashmap80 hashmap82)
         (hashmap-ref hashmap80 hashmap83)
         (hashmap-ref hashmap80 hashmap84)
         (hashmap-ref hashmap80 hashmap85)
         )))

;; Ordering comparators
(define check-073
  (test-equal #t
         (=? comparator hashmap81 hashmap82)))

(define check-074
  (test-equal #t
         (not (=? comparator hashmap81 hashmap84))))

(test-end "mapping-hash")
