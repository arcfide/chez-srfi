(library (srfi :126 r6rs-hashtables-helpers)
  (export make-weak-eq-hashtable weak-eq-hashtables-supported
          make-weak-eqv-hashtable weak-eqv-hashtables-supported
          make-weak-hashtable weak-hashtables-supported)
  (import (rnrs)
          (only (chezscheme) make-weak-eq-hashtable make-weak-eqv-hashtable make-parameter))
  (define weak-eq-hashtables-supported (make-parameter #t))
  (define weak-eqv-hashtables-supported (make-parameter #t))
  (define (make-weak-hashtable . args)
    (error 'make-weak-eq-hashtable "weak hashtables not supported"))
  (define weak-hashtables-supported (make-parameter #f)))
