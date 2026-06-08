;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried-case (define-write-numeric-vector)
  "Wrap SRFI 160 `write-@vector' procedures"
  ([(v)      (display v (current-output-port))]
   [(v port) (display v (current-output-port))]))
