;; Copyright © 2020 Amirouche Boubekki
;; SPDX-License-Identifier: MIT
#!r6rs

;; Tests for SRFI 111

(import
  (rnrs)
  (srfi :111 boxes)
  (srfi :64 testing))

(test-begin "boxes")
(test-equal #t (box? (box 42)))
(test-equal 42 (unbox (box 42)))
(test-equal 42 (let ((b (box 0)))
                 (set-box! b 42)
                 (unbox b)))
(test-end "boxes")
