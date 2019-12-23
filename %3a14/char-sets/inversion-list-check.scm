; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite inversion-lists-tests)

(define-test-case creation/membership inversion-lists-tests
  (check-that (inversion-list-member? 5 (make-empty-inversion-list 0 1000)) (is-false))
  (check (inversion-list-member? 5 (number->inversion-list 0 1000 5)))
  (check-that (inversion-list-member? 4 (number->inversion-list 0 1000 5)) (is-false))
  (check-that (inversion-list-member? 6 (number->inversion-list 0 1000 5)) (is-false))
  (check-that (inversion-list-member? 6 (range->inversion-list 0 1000 500 1000)) (is-false))
  (check-that (inversion-list-member? 499 (range->inversion-list 0 1000 500 1000)) (is-false))
  (check (inversion-list-member? 500 (range->inversion-list 0 1000 500 1000)))
  (check (inversion-list-member? 1000 (range->inversion-list 0 1000 500 1000))))

(define-test-case complement/1 inversion-lists-tests
  (check-that
   (inversion-list-complement
    (inversion-list-complement
     (range->inversion-list 0 1000 5 10)))
   (is inversion-list=?
       (range->inversion-list 0 1000 5 10))))

(define-test-case complement/2 inversion-lists-tests
  (check-that
   (inversion-list-complement
    (inversion-list-complement
     (range->inversion-list 0 1000 0 1000)))
   (is inversion-list=?
       (range->inversion-list 0 1000 0 1000))))

(define-test-case union/1 inversion-lists-tests
  (check-that
   (inversion-list-union (range->inversion-list 0 1000 5 10)
			 (range->inversion-list 0 1000 20 30))
   (is inversion-list=?
       (ranges->inversion-list 0 1000 '(5 . 10) '(20 . 30)))))

(define-test-case union/2 inversion-lists-tests
  (check-that
   (inversion-list-union (range->inversion-list 0 1000 5 10)
			 (range->inversion-list 0 1000 7 8))
   (is inversion-list=?
       (range->inversion-list 0 1000 5 10))))
  
(define-test-case union/3 inversion-lists-tests
  (check-that
   (inversion-list-union (range->inversion-list 0 1000 5 10)
			 (range->inversion-list 0 1000 7 15))
   (is inversion-list=?
       (range->inversion-list 0 1000 5 15))))

(define-test-case union/4 inversion-lists-tests
  (check-that
   (inversion-list-union (range->inversion-list 0 1000 500 1000)
			 (range->inversion-list 0 1000 0 500))
   (is inversion-list=?
       (range->inversion-list 0 1000 0 1000))))

(define-test-case intersection/1 inversion-lists-tests
  (check-that
   (inversion-list-intersection (range->inversion-list 0 1000 5 10)
				(range->inversion-list 0 1000 20 30))
   (is inversion-list=?
       (make-empty-inversion-list 0 1000))))
  
(define-test-case intersection/2 inversion-lists-tests
  (check-that
   (inversion-list-intersection (range->inversion-list 0 1000 5 10)
				(range->inversion-list 0 1000 7 8))
   (is inversion-list=?
       (range->inversion-list 0 1000 7 8))))

(define-test-case intersection/3 inversion-lists-tests
  (check-that
   (inversion-list-intersection (range->inversion-list 0 1000 5 10)
				(range->inversion-list 0 1000 7 15))
   (is inversion-list=?
       (range->inversion-list 0 1000 7 10))))

(define-test-case intersection/4 inversion-lists-tests
  (check-that
   (inversion-list-intersection (range->inversion-list 0 1000 500 1000)
				(range->inversion-list 0 1000 0 501))
   (is inversion-list=?
       (range->inversion-list 0 1000 500 501))))

(define-test-case intersection/5 inversion-lists-tests
  (check-that
   (inversion-list-intersection (range->inversion-list 0 1000 500 1000)
				(range->inversion-list 0 1000 501 505))
   (is inversion-list=?
       (range->inversion-list 0 1000 501 505))))

(define-test-case adjoin inversion-lists-tests
  (check-that
   (inversion-list-adjoin (range->inversion-list 0 1000 0 999) 999)
   (is inversion-list=?
       (range->inversion-list 0 1000 0 1000))))

(define-test-case remove inversion-lists-tests
  (check-that
   (inversion-list-remove (range->inversion-list 0 1000 0 1000) 999)
   (is inversion-list=?
       (range->inversion-list 0 1000 0 999))))

(define-test-case size inversion-lists-tests
  (check
   (inversion-list-size
    (ranges->inversion-list 0 1000 '(5 . 10) '(15 . 20) '(500 . 1000)))
   => 510))

(define-test-case copy inversion-lists-tests
  (check-that
   (inversion-list-copy
    (ranges->inversion-list 0 1000 '(5 . 10) '(15 . 20) '(500 . 1000)))
   (is inversion-list=?
       (ranges->inversion-list 0 1000 '(5 . 10) '(15 . 20) '(500 . 1000)))))

(define-test-case fold/done? inversion-lists-tests
  (check
   (inversion-list-fold/done?
    (lambda (n sum)
      (+ n sum))
    0
    (lambda (sum)
      (> sum 250000))
    (ranges->inversion-list 0 1000 '(5 . 10) '(15 . 20) '(500 . 1000)))
   =>
   250781))

(define (i-list-sum i-list)
  (let loop ((cursor (inversion-list-cursor i-list))
	     (sum 0))
    (if (inversion-list-cursor-at-end? cursor)
	sum
	(loop (inversion-list-cursor-next i-list cursor)
	      (+ (inversion-list-cursor-ref cursor)
		 sum)))))
    
(define-test-case cursor inversion-lists-tests
  (check
   (i-list-sum (ranges->inversion-list 0 1000 '(5 . 10) '(15 . 20) '(500 . 1000)))
   => 374870))

(define-test-case hash inversion-lists-tests 
  (check-that
   (inversion-list-hash (ranges->inversion-list 0 1000 '(5 . 10) '(15 . 20) '(500 . 1000)) 1031)
   (opposite (is =
		 (inversion-list-hash (ranges->inversion-list 0 1000 '(5 . 10) '(500 . 1000)) 1031)))))
