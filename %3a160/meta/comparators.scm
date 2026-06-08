;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-numeric-vector-hash len sub) v)
  "Wrap SRFI 160 `@vector-hash' procedures"
  (let ([width (min 256 (len v))])
    (let loop ([i 0] [acc 0])
      (if (= i width)
	  (abs (floor (real-part (inexact->exact acc))))
	  (loop (+ i 1) (+ acc (sub v i)))))))
