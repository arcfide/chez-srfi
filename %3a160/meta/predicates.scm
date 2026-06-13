;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-numeric-vector-empty? len) v)
  "Wrap SRFI 160 `@vector-empty?' procedures"
  (fxzero? (len v)))

(define/curried ((define-numeric-vector=? every len) v . vs)
  "Wrap SRFI 160 `@vector=' procedures"
  (and (apply all-same-length? len v vs)
       (apply every = v vs)))

(define/curried ((define-numeric-vector<? <? >? sub len) vec1 vec2)
  "Convenience procedure for elementwise ordering, not part of the SRFI 160 specification"
  (let ([len1 (len vec1)]
        [len2 (len vec2)])
    (cond [(<? len1 len2)
           #t]
          [(>? len1 len2)
           #f]
          [else
           (let loop ([k 0])
             (cond [(= k len1)  #f]
                   [(<? (sub vec1 k) (sub vec2 k))  #t]
                   [(>? (sub vec1 k) (sub vec2 k))  #f]
                   [else
                    (loop (+ k 1))]))])))

