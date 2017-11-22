(library (srfi :126 r6rs-hashtables-helpers)
  (export make-weak-eq-hashtable weak-eq-hashtables-supported
          make-weak-eqv-hashtable weak-eqv-hashtables-supported
          make-weak-hashtable weak-hashtables-supported)
  (import (rnrs) (srfi :39))
  (define (make-weak-eq-hashtable . args)
    (error 'make-weak-eq-hashtable "weak eq hashtables not supported"))
  (define weak-eq-hashtables-supported (make-parameter #f))
  (define (make-weak-eqv-hashtable . args)
    (error 'make-weak-eq-hashtable "weak eqv hashtables not supported"))
  (define weak-eqv-hashtables-supported (make-parameter #f))
  (define (make-weak-hashtable . args)
    (error 'make-weak-eq-hashtable "weak hashtables not supported"))
  (define weak-hashtables-supported (make-parameter #f)))
