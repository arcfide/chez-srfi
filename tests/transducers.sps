#!r6rs

(import
  (rnrs)
  (srfi :64 testing)
  (srfi :171 transducers))

(define (add1 x) (+ x 1))

(define numeric-list (iota 5))
(define numeric-vec (list->vector numeric-list))
(define example-string "0123456789abcdef")
(define list-of-chars (string->list example-string))
(define replace-alist '((1 . s) (2 . c) (3 . h) (4 . e) (5 . m)))

(test-begin "transducers")
(test-equal '(1 2 3 4 5) (list-transduce (tmap add1) rcons numeric-list))
(test-equal '(0 2 4) (list-transduce (tfilter even?) rcons numeric-list))
(test-equal '(1 3 5) (list-transduce (compose (tfilter even?) (tmap add1)) rcons numeric-list))

(test-equal (string-transduce (tmap char->integer) rcons example-string) (list-transduce (tmap char->integer) rcons list-of-chars))
(test-equal 6 (string-transduce (tfilter char-alphabetic?) rcount example-string))
(test-equal (list-transduce (tremove char-alphabetic?) rcount list-of-chars) (string-transduce (tremove char-alphabetic?) rcount example-string))
(test-equal '(s c h e m e  r o c k s) (list-transduce (treplace replace-alist) rcons '(1 2 3 4 5 4 r o c k s) ))

(test-equal 6 (list-transduce (ttake 4) + numeric-list))
(test-equal 7 (list-transduce (tdrop 3) + numeric-list))

(test-equal '(3 4) (list-transduce (tdrop-while (lambda (x) (< x 3))) rcons numeric-list))

(test-equal '(0 1 2) (list-transduce (ttake-while (lambda (x) (< x 3))) rcons numeric-list))

(test-equal '(0 1 2 3 4) (list-transduce tconcatenate rcons '((0 1) (2 3) (4))))

(test-equal '(1 2 2 4 3 6) (list-transduce (tappend-map (lambda (x) (list x (* x 2)))) rcons '(1 2 3)))

(test-equal '(1 2 1 2 3) (list-transduce (tdelete-neighbor-dupes) rcons '(1 1 1 2 2 1 2 3 3)))

(test-equal '(1 2 3 4) (list-transduce (tdelete-duplicates) rcons '(1 1 2 1 2 3 3 1 2 3 4 4)))

(test-equal '(1 2 3 4 5 6 7 8 9) (list-transduce tflatten rcons '((1 2) 3 (4 (5 6) 7) 8 (9))))

(test-equal '((1 1 1 1) (2 2 2 2) (3 3 3) (4 4 4 4)) (list-transduce (tpartition even?) rcons '(1 1 1 1 2 2 2 2 3 3 3 4 4 4 4)))

(test-equal '((0 1) (2 3) (4)) (vector-transduce (tsegment 2) rcons numeric-vec))

(test-equal '(0 and 1 and 2 and 3 and 4) (list-transduce (tinterpose 'and) rcons numeric-list))

(test-equal '((-1 . 0) (0 . 1) (1 . 2) (2 . 3) (3 . 4)) (list-transduce (tenumerate (- 1)) rcons numeric-list))

(test-end "transducers")

