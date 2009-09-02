;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (srfi :23 error)
  (export error error-who)
  (import 
    (rename (rnrs base) (error rnrs:error))
    (srfi :39 parameters))
  
  (define error-who (make-parameter #f))
  
  (define (error . args)
    (apply rnrs:error (error-who) args))
)
