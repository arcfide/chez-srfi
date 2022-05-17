#!r6rs

(library (srfi :213 impl)
  (export define-property capture-lookup)
  (import (rnrs (6))
          (only (chezscheme) define-property format top-level-bound?))

  ;; The ChezScheme implementation is already doing what is necessary
  ;; when macro transformer returns a procedure, so we just re-use the
  ;; implementation-provided lookup.
  (define (capture-lookup proc)
    proc))
