;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (srfi :27 random-bits)
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals)
  
  (import (except (rnrs) error)
          (rnrs r5rs)
          (srfi :39 parameters)
          (only (srfi :19 time) time-nanosecond current-time)
          (prefix (srfi :23 error) ER:)
          (srfi private include)
          )
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi :27 random-bits))"])
      (apply ER:error args)))
  
   (include/resolve ("srfi" "27") "random.ss")
  )
