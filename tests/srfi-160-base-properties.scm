;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define-test-property ((define-non-list-property from-list) xs)
  "Define invalid `list->@vector' property (non-list)"
  (from-list xs)
  (irritants xs)
  (message "~a is not a proper list"))

(define-test-property ((define-non-elem-property from-list) xs k x)
  "Define invalid `list->@vector' or `@vector' property (bad list element)."
  (let-values ([(pre-vec post-vec) (split-at xs k)])
    (from-list (append pre-vec (cons x post-vec))))
  (irritants x)
  (message (format-vector-type "element ~~a cannot be contained within ~a" type-of)))

(define-test-property ((define-non-vector-property op) xs . args)
  "define invalid `@vector' property"
  (apply op xs args)
  (irritants xs)
  (message (format "~~a is not of type ~a" type-of)))

(define-test-property ((define-non-vector-property/1 op) proc xs . args)
  "define invalid `@vector' property"
  (apply op proc xs args)
  (irritants xs)
  (message (format "~~a is not of type ~a" type-of)))

(define-test-property ((define-repeating-non-elem-property op) rep . rest)
  "define invalid bad repeating property"
  (apply op rep rest)
  (irritants rep)
  (message (format-vector-type "repeating element ~~a cannot be contained within ~a" type-of)))

(define-test-property ((define-repeating-non-elem-property/1 op) pre rep . rest)
  "define invalid bad repeating property"
  (apply op pre rep rest)
  (irritants rep)
  (message (format-vector-type "repeating element ~~a cannot be contained within ~a" type-of)))

#| Properties for initialising (`@vector' or `list->@vector') |#

(define/curried-case (define-init-property from-list)
  "define `list->@vector' or `@vector' property"
  ([(lst) (from-list lst)]
   [(lst extra) (from-list (cons extra lst))]))

#| Properties for `make-@vector' procedures |#

(define/curried ((define-make-property mk) . args)
  "define `make-@vector' property"
  (apply mk args))

(define-test-property ((define-make/neg-width-property mk) width rep)
  "define invalid `make-@vector' property (negative width)"
  (mk width rep)
  (irritants width)
  (message "length ~a is not a non-negative integer"))

(define-test-property ((define-make/non-width-property mk) width rep)
  "define invalid `make-@vector' property (non-numeric width)"
  (mk width rep)
  (irritants width)
  (message (format-vector-type "~~a is not a valid length for ~a" type-of)))

#| Properties for `@vector-length' procedures |#

(define/curried ((define-length-property from-list len) lst)
  "define `@vector-length' property"
  (= (length lst) (len (from-list lst))))

#| Properties for `@vector->list' procedures |#

(define/curried-case (define-to-list-property to-list len)
  "define `@vector->list' property"
   (recur [(vec) (recur vec 0 (len vec))]
          [(vec start) (recur vec start (len vec))]
          [(vec start end)
           (= (length (to-list vec start end))
	      (- end start))]))

(define-test-property ((define-to-list/neg-start-property to-list) vec start)
  "define invalid `@vector->list' property (negative start)"
  (to-list vec start)
  (irritants start)
  (message "start ~a is not a non-negative integer"))

(define-test-property ((define-to-list/neg-end-property to-list) vec start end)
  "define invalid `@vector->list' property (negative end)"
  (to-list vec start end)
  (irritants end)
  (message "end ~a is not a non-negative integer"))

(define-test-property ((define-to-list/bounds-property to-list) vec start end)
  "define invalid `@vector->list' property (start is greater than end)"
  (to-list vec start end)
  (irritants end start)
  (message "end ~a must be greater than or equal to start ~a"))

(define-test-property ((define-to-list/overflow-property to-list) vec start end)
  "define invalid `@vector->list' property (end overflows)"
  (to-list vec start end)
  (irritants end vec)
  (message "end ~a overflows ~a"))

#| Properties common to both `@vector-ref' and `@vector-set!' procedures |#

(define-test-property ((define-sub+upd!/non-index-property op) v k . rest)
  "define invalid index property"
  (apply op v k rest)
  (irritants k)
  (message "index ~a is not a non-negative integer"))

(define-test-property ((define-sub+upd!/bounds-property op len) v k . rest)
  "define overflowing index property"
  (let ([ind (+ k (len v))])
    (apply op v ind rest))
  #| The following irritants may look wrong (actual irritant is index),
     but we're testing for the overflow error when we overflow the end by
     ``k'' elements |#
  (irritants (+ k (len v)) v)
  (message "~a is not a valid index for ~a"))

(define-test-property ((define-sub+upd!/neg-index-property op) v k . rest)
  "define negative index property"
  (apply op v k rest)
  (irritants k v)
  (message "~a is not a valid index for ~a"))

#| Properties for `@vector-ref' procedures only |#

(define/curried ((define-sub-property sub len) v)
  "define `@vector-ref' property"
  (sub v (random (len v))))

#| Properties for `@vector-set!' procedures only |#

(define/curried ((define-upd!-property upd!) v k x)
  "define `@vector-set!' property"
  (upd! v k x))

(define-test-property ((define-upd!/non-elem-property upd!) v k x)
  "define invalid `@vector-set!' property (bad fill value)"
  (upd! v k x)
  (irritants x)
  (message (format-vector-type "element ~~a cannot be contained within ~a" type-of)))

(define-test-property ((define-upd!/non-elem-range-property upd!) v k x)
  "define invalid `@vector-set!' property (bad fill value)"
  (upd! v k x)
  (irritants x)
  (message (format-vector-type "element ~~a is out of range for ~a" type-of)))
