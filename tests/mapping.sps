;; Copyright (C) Marc Nieper-Wißkirchen (2016, 2017).
;; Copyright (C) Amirouche Boubekki (2020).
;;
;; All Rights Reserved.

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
        (srfi :146)
        (srfi :64))

(define (error* msg)
  (error 'mapping msg))

(test-begin "mappings")

(define comparator (make-default-comparator))

(define mapping0 (mapping comparator))
(define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping2 (mapping comparator 'c 1 'd 2 'e 3))
(define mapping3 (mapping comparator 'd 1 'e 2 'f 3))

(define check-000
  (test-equal #t (mapping? (mapping comparator))))

(define check-001
  (test-equal #t (not (mapping? (list 1 2 3)))))

(define check-002
  (test-equal #t (mapping-empty? mapping0)))

(define check-003
  (test-equal #t (not (mapping-empty? mapping1))))

(define check-004
  (test-equal #t (mapping-contains? mapping1 'b)))

(define check-005
  (test-equal #t (not (mapping-contains? mapping1 '2))))

(define check-006
  (test-equal #t (mapping-disjoint? mapping1 mapping3)))

(define check-007
  (test-equal #t (not (mapping-disjoint? mapping1 mapping2))))

;; Accessors
(define mapping11 (mapping comparator 'a 1 'b 2 'c 3))

(define check-008
  (test-equal 2 (mapping-ref mapping11 'b)))

(define check-009
  (test-equal
   42
   (mapping-ref mapping11 'd (lambda () 42))))

(define check-009-1
  (test-error (mapping-ref mapping11 'd)))

(define check-010
  (test-equal
   (* 2 2)
   (mapping-ref mapping11 'b (lambda () #f) (lambda (x) (* x x)))))

(define check-011
  (test-equal
   3
   (mapping-ref/default mapping11 'c 42)))

(define check-012
  (test-equal
   42
   (mapping-ref/default mapping11 'd 42)))

(define check-013
  (test-equal
   comparator
   (mapping-key-comparator mapping11)))

;; Updaters
(define mapping21 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping22 (mapping-set mapping1 'c 4 'd 4 'd 5))
(define mapping23 (mapping-update mapping1 'b (lambda (x) (* x x))))
(define mapping24 (mapping-update/default mapping1 'd (lambda (x) (* x x)) 4))
(define mapping25 (mapping-adjoin mapping1 'c 4 'd 4 'd 5))
(define mapping20 (mapping comparator))

(define check-014
  (test-equal
   3
   (mapping-ref mapping25 'c)))

(define check-015
  (test-equal
   4
   (mapping-ref mapping25 'd)))

(define check-016
  (test-equal
   4
   (mapping-ref mapping22 'c)))

(define check-017
  (test-equal
   5
   (mapping-ref mapping22 'd)))

(define check-018
  (test-equal
   #f
   (mapping-ref/default (mapping-replace mapping21 'd 4) 'd #f)))

(define check-019
  (test-equal
   6
   (mapping-ref (mapping-replace mapping21 'c 6) 'c)))

(define check-020
  (test-equal
   42
   (mapping-ref/default (mapping-delete mapping21 'b) 'b 42)))

(define check-021
  (test-equal
   42
   (mapping-ref/default (mapping-delete-all mapping21 '(a b)) 'b 42)))

(define check-022
  (test-equal
   (list mapping21 2)
   (receive result
       (mapping-intern mapping21 'b (lambda () (error* "should not have been invoked")))
     result)))

(define check-023
  (test-equal
   (list 42 42)
   (receive (mapping value)
       (mapping-intern mapping21 'd (lambda () 42))
     (list value (mapping-ref mapping 'd)))))

(define check-024
  (test-equal
   4
   (mapping-ref mapping23 'b)))

(define check-025
  (test-equal
   16
   (mapping-ref mapping24 'd)))

(define check-026
  (test-equal
   'empty
   (mapping-pop mapping20 (lambda () 'empty))))

(define check-027
  (test-equal
   (list 2 'a 1)
   (receive (mapping key value)
       (mapping-pop mapping21)
     (list (mapping-size mapping) key value))))

(define check-028
  (test-equal
   '("success updated"
     "failure ignored"
     ((0 . "zero") (1 . "one") (2 . "two [seen]") (3 . "three")
      (4 . "four") (5 . "five")))
   (let ((m1 (mapping (make-default-comparator)
                      1 "one"
                      3 "three"
                      0 "zero"
                      4 "four"
                      2 "two"
                      5 "five")))
     (define (f/ignore insert ignore)
       (ignore "failure ignored"))
     (define (s/update key val update remove)
       (update key
               (string-append val " [seen]")
               "success updated"))
     (let*-values (((m2 v2) (mapping-search m1 2 f/ignore s/update))
                   ((m3 v3) (mapping-search m2 42 f/ignore s/update)))
       (list v2 v3 (mapping->alist m3))))))

;; The whole mapping

(define mapping30 (mapping comparator))
(define mapping31 (mapping comparator 'a 1 'b 2 'c 3))

(define check-029
  (test-equal
   0
   (mapping-size mapping30)))

(define check-030
  (test-equal
   3
   (mapping-size mapping31)))

(define check-031
  (test-equal
   (list 'b 2)
   (receive result
       (mapping-find (lambda (key value)
                       (and (eq? key 'b)
                            (= value 2)))
                     mapping31
                     (lambda () (error* "should not have been called")))
     result)))

(define check-032
  (test-equal
   (list 42)
   (receive result
       (mapping-find (lambda (key value)
                       (eq? key 'd))
                     mapping31
                     (lambda ()
                       42))
     result)))

(define check-033
  (test-equal
   2
   (mapping-count (lambda (key value)
                    (>= value 2))
                  mapping31)))

(define check-034
  (test-equal #t
              (mapping-any? (lambda (key value)
                              (= value 3))
                            mapping31)))

(define check-035
  (test-equal #t
              (not (mapping-any? (lambda (key value)
                                   (= value 4))
                                 mapping31))))

(define check-036
  (test-equal #t
              (mapping-every? (lambda (key value)
                                (<= value 3))
                              mapping31)))

(define check-037
  (test-equal #t
              (not (mapping-every? (lambda (key value)
                                     (<= value 2))
                                   mapping31))))

(define check-038
  (test-equal
   3
   (length (mapping-keys mapping31))))

(define check-039
  (test-equal
   6
   (fold + 0 (mapping-values mapping31))))

(define check-040
  (test-equal
   (list 3 6)
   (receive (keys values)
       (mapping-entries mapping31)
     (list (length keys) (fold + 0 values)))))

(define mapping41 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping42 (mapping-map (lambda (key value)
                                 (values (symbol->string key)
                                         (* 10 value)))
                               comparator
                               mapping1))
(define check-041
  (test-equal
   20
   (mapping-ref mapping42 "b")))

(define check-042
  (test-equal
   6
   (let ((counter 0))
     (mapping-for-each (lambda (key value)
                         (set! counter (+ counter value)))
                       mapping41)
     counter)))

(define check-043
  (test-equal
   6
   (mapping-fold (lambda (key value acc)
                   (+ value acc))
                 0
                 mapping41)))

(define check-044
  (test-equal
   (+ (* 1 1) (* 2 2) (* 3 3))
   (fold + 0 (mapping-map->list (lambda (key value)
                                  (* value value))
                                mapping41))))

(define check-045
  (test-equal
   2
   (mapping-size (mapping-filter (lambda (key value)
                                   (<= value 2))
                                 mapping41))))

(define check-046
  (test-equal
   1
   (mapping-size (mapping-remove (lambda (key value)
                                   (<= value 2))
                                 mapping41))))

(define check-047
  (test-equal
   (list 1 2)
   (receive result
       (mapping-partition (lambda (key value)
                            (eq? 'b key))
                          mapping41)
     (map mapping-size result))))

;; Copying and conversion
(define mapping51 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping52 (alist->mapping comparator '((a . 1) (b . 2) (c . 3))))
(define mapping53 (alist->mapping! (mapping-copy mapping1) '((d . 4) '(c . 5))))

(define check-048
  (test-equal
   3
   (mapping-size (mapping-copy mapping51))))

(define check-049
  (test-equal
   comparator
   (mapping-key-comparator (mapping-copy mapping51))))

(define check-050
  (test-equal
   (cons 'b 2)
   (assq 'b (mapping->alist mapping51))))

(define check-051
  (test-equal
   2
   (mapping-ref mapping52 'b)))

(define check-052
  (test-equal
   4
   (mapping-ref mapping53 'd)))

(define check-053
  (test-equal
   3
   (mapping-ref mapping53 'c)))

;; Submappings
(define mapping61 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping62 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping63 (mapping comparator 'a 1 'c 3))
(define mapping64 (mapping comparator 'a 1 'c 3 'd 4))
(define mapping65 (mapping comparator 'a 1 'b 2 'c 6))
(define mapping66 (mapping (make-comparator (comparator-type-test-predicate comparator)
                                            (comparator-equality-predicate comparator)
                                            (comparator-ordering-predicate comparator)
                                            (comparator-hash-function comparator))
                           'a 1 'b 2 'c 3))


(define check-054
  (test-equal #t
              (mapping=? comparator mapping61 mapping62)))

(define check-055
  (test-equal #t
              (not (mapping=? comparator mapping61 mapping64))))

(define check-056
  (test-equal #t
              (not (mapping=? comparator mapping61 mapping66))))

(define check-057
  (test-equal #t
              (mapping<? comparator mapping63 mapping61)))

(define check-058
  (test-equal #t
              (not (mapping<? comparator mapping3 mapping61 mapping62))))

(define check-059
  (test-equal #t
              (mapping>? comparator mapping62 mapping63)))

(define check-060
  (test-equal #t
              (not (mapping>? comparator mapping1 mapping62 mapping63))))

(define check-061
  (test-equal #t
              (mapping<=? comparator mapping63 mapping62 mapping61)))

(define check-062
  (test-equal #t
              (not (mapping<=? comparator mapping63 mapping65))))

(define check-063
  (test-equal #t
              (not (mapping<=? comparator mapping62 mapping64))))

(define check-064
  (test-equal #t
              (mapping>=? comparator mapping64 mapping63)))

(define check-065
  (test-equal #t
              (not (mapping>=? comparator mapping65 mapping63))))

;; Set theory operations
(define mapping71 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping72 (mapping comparator 'a 1 'b 2 'd 4))
(define mapping73 (mapping comparator 'a 1 'b 2))
(define mapping74 (mapping comparator 'a 1 'b 2 'c 4))
(define mapping75 (mapping comparator 'a 1 'c 3))
(define mapping76 (mapping comparator 'd 4 'e 5 'f 6))

(define check-066
  (test-equal
   4
   (mapping-ref (mapping-union mapping71 mapping72) 'd)))

(define check-067
  (test-equal
   3
   (mapping-ref (mapping-union mapping71 mapping74) 'c)))

(define check-068
  (test-equal
   6
   (mapping-size (mapping-union mapping71 mapping72 mapping76))))

(define check-069
  (test-equal
   3
   (mapping-ref (mapping-intersection mapping71 mapping74) 'c)))

(define check-070
  (test-equal
   42
   (mapping-ref/default (mapping-intersection mapping71 mapping75) 'b 42)))

(define check-071
  (test-equal
   2
   (mapping-size (mapping-difference mapping72 mapping76))))

(define check-072
  (test-equal
   4
   (mapping-size (mapping-xor mapping72 mapping76))))

;; Additional procedures for mappings with ordered keys

(define mapping81 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping82 (mapping comparator 'a 1 'b 2 'c 3 'd 4))
(define mapping83 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5))
(define mapping84 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6))
(define mapping85 (mapping comparator 'f 6 'g 7 'h 8))

(define check-073
  (test-equal
   '(a a a a)
   (map mapping-min-key (list mapping81 mapping82 mapping83 mapping84))))

(define check-074
  (test-equal
   '(c d e f)
   (map mapping-max-key (list mapping81 mapping82 mapping83 mapping84))))

(define check-075
  (test-equal
   '(1 1 1 1)
   (map mapping-min-value (list mapping81 mapping82 mapping83 mapping84))))

(define check-076
  (test-equal
   '(3 4 5 6)
   (map mapping-max-value (list mapping81 mapping82 mapping83 mapping84))))

(define check-077
  (test-equal
   '(c d d d)
   (map (lambda (mapping)
          (mapping-key-predecessor mapping 'e (lambda () #f)))
        (list mapping81 mapping82 mapping83 mapping84))))

(define check-078
  (test-equal
   '(#f #f e e)
   (map (lambda (mapping)
          (mapping-key-successor mapping 'd (lambda () #f)))
        (list mapping81 mapping82 mapping83 mapping84))))

(define check-079
  (test-equal '(4)
              (mapping-values (mapping-range= mapping84 'd))))

(define check-080
  (test-equal
   '()
   (mapping-values (mapping-range= mapping84 'z))))

(define check-081
  (test-equal '(1 2 3)
              (mapping-values (mapping-range< mapping84 'd))))

(define check-082
  (test-equal
   '(1 2 3 4)
   (mapping-values (mapping-range<= mapping84 'd))))

(define check-083
  (test-equal
   '(5 6)
   (mapping-values (mapping-range> mapping84 'd))))

(define check-084
  (test-equal
   '(4 5 6)
   (mapping-values (mapping-range>= mapping84 'd))))

(define check-085
  (test-equal
   '((1 2 3) (1 2 3 4) (4) (4 5 6) (5 6))
   (receive mappings
       (mapping-split mapping84 'd)
     (map mapping-values mappings))))

(define check-086
  (test-equal
   '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8))
   (mapping->alist (mapping-catenate comparator mapping82 'e 5 mapping85))))

(define check-087
  (test-equal
   '((1 . 1) (2 . 4) (3 . 9))
   (mapping->alist
    (mapping-map/monotone (lambda (key value)
                            (values value (* value value)))
                          comparator
                          mapping81))))

(define check-088
  (test-equal
   '(1 2 3)
   (mapping-fold/reverse (lambda (key value acc)
                           (cons value acc))
                         '() mapping81)))

;; Comparators

(define mapping91 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping92 (mapping comparator 'a 1 'b 2 'c 3))
(define mapping93 (mapping comparator 'a 1 'b 2))
(define mapping94 (mapping comparator 'a 1 'b 2 'c 4))
(define mapping95 (mapping comparator 'a 1 'c 3))
(define mapping90 (mapping comparator mapping91 "a" mapping92 "b" mapping93 "c" mapping94 "d" mapping95 "e"))

(define check-089
  (test-equal #t
              (comparator? mapping-comparator)))

(define check-090
  (test-equal
   (list "a" "a" "c" "d" "e")
   (list (mapping-ref mapping90 mapping91)
         (mapping-ref mapping90 mapping92)
         (mapping-ref mapping90 mapping93)
         (mapping-ref mapping90 mapping94)
         (mapping-ref mapping90 mapping95))))

;; Ordering comparators
(define check-091
  (test-equal #t
              (=? comparator mapping91 mapping92)))

(define check-092
  (test-equal #t
              (not (=? comparator mapping91 mapping94))))

(define check-093
  (test-equal #t
              (<? comparator mapping93 mapping94)))

(define check-094
  (test-equal #t
              (<? comparator mapping91 mapping94)))

(define check-095
  (test-equal #t
              (<? comparator mapping91 mapping95)))

(test-end "mappings")
