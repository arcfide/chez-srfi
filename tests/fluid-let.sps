;; Copyright 2000 Lars T Hansen
;; SPDX-License-Identifier: MIT

#!r6rs

(import (rnrs)
        (srfi :15 fluid-let)
        (srfi :64 testing))

(test-begin "fluid-let")

(let ((results '()))

  (define (display x) (set! results (append results (list x))))

  (define v 1)
  (define again #f)

  (define (test1)
    (display v)
    (fluid-let ((v 2))
      (call-with-current-continuation
       (lambda (k)
         (set! again (lambda ()
                       (set! again #f)
                       (k #t)))))
      (test2)
      (set! v 3))
    (display v)
    (set! v 4)
    (if again (again)))

  (define (test2) (display v))

  (test1)

  (test-equal '(1 2 1 3 4) results))

(test-end)
