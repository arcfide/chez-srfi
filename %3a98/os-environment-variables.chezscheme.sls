#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (srfi :98 os-environment-variables)
  (export get-environment-variables
          (rename (getenv get-environment-variable)))
  (import (only (chezscheme) getenv environ))
    
  (define (get-environment-variables . args)
    (assertion-violation 'get-environment-variables "not implemented")))
