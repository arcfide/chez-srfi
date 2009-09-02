;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (srfi :42 eager-comprehensions)
  (export
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until)
  (import (except (rnrs base) error)
    (rnrs io simple)
    (only (rnrs syntax-case) syntax-violation)
    (only (scheme) exact->inexact make-parameter parameterize)
    (prefix (srfi :23 error) ER:)
    (srfi private include))
  
(define (error . args)
  (parameterize ([ER:error-who 
                  "(library (srfi :42 eager-comprehensions))"])
    (apply ER:error args)))

#;(define-syntax nested 
  (lambda (x) (syntax-violation #f "invalid auxiliary syntax" x)))
#;(define-syntax index 
  (lambda (x) (syntax-violation #f "invalid auxiliary syntax" x)))
  
(include/resolve ("srfi" "42") "ec.scm")  

)


