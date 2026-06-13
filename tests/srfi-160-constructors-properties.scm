;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-unfold-property exact? unfold ->list) size)
  "define property to test `@vector-unfold' normative behaviour"
  (if exact?
      (list= =
             (iota size)
             (->list
              (unfold values size 0)))
      (list= =
             (map exact->inexact (iota size))
             (->list
              (unfold (lambda (k _) (values (exact->inexact k) #f))
                      size #f)))))

(define/curried ((define-unfold/count-up-property exact? unfold ->list) size ini)
  "define property to test `@vector-unfold' counting up"
  (if exact?
      (list= =
             (make-range ini (+ ini size))
	     (->list
              (unfold (lambda (_k x) (values x (+ x 1)))
		      size ini)))
      (list= =
             (map exact->inexact (make-range ini (+ ini size)))
             (->list
              (unfold (lambda (_k x) (values (exact->inexact x)
                                             (exact->inexact (+ x 1))))
                      size ini)))))

(define/curried ((define-unfold/count-down-property exact? unfold ->list) size ini)
  "define property to test `@vector-unfold' counting down"
  (let ([down-from (+ 1 ini)]
	[down-to (+ 1 (- ini size))])
    (if exact?
	(list= =
               (reverse (make-range down-to down-from))
	       (->list
                (unfold (lambda (_k x) (values x (- x 1)))
			size ini)))
        (list= =
               (map exact->inexact (reverse (make-range down-to down-from)))
               (->list
                (unfold (lambda (_k x) (values (exact->inexact x)
                                               (exact->inexact (- x 1))))
                        size ini))))))

(define-test-property ((define-persistent-neg-length-property op)
                         proc size ini)
  "Holds true if end is not non-negative"
  (op proc size ini)
  (irritants size)
  (message "length ~a is not a non-negative integer"))

(define-test-property ((define-persistent-non-length-property op)
                         proc size ini)
  "Holds true if end is not numeric"
  (op proc size ini)
  (irritants size)
  (message (format-vector-type "~~a is not a valid length for ~a" type-of)))

(define/curried ((define-copy-property copy rdrp ->list) v start end)
  "define property to test `@vector-copy' normative behaviour"
  (list= = (rdrp (->list v) start end)
	 (->list (copy v start end))))
