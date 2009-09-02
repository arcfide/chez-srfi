;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (srfi :78 lightweight-testing)
  (export
    check
    check-ec
    check-report
    check-set-mode!
    check-reset!
    check-passed?
    ;; All of (srfi :42 eager-comprehensions):
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
    dispatch-union :generator-proc)
  (import 
    (except (rnrs) error)
    (srfi :78 lightweight-testing compat)
    (srfi :39 parameters)
    (srfi private include)
    (prefix (srfi :23 error) ER:)
    (srfi :42 eager-comprehensions))
  
  (define (error . args)
    (parameterize ([ER:error-who
                    "(library (srfi :78 lightweight-testing))"])
      (apply ER:error args)))
  
  (include/resolve ("srfi" "78") "check.scm")
)
