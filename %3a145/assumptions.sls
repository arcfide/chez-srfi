(library (srfi :145 assumptions)
  (export assume)
  (import (except (rnrs) error) (srfi :0) (srfi :23))

  (define-syntax assume
    (syntax-rules ()
      [(_ expression message ...)
       (cond-expand
         [debug
          (unless expression
            (error "invalid assumption" 'expression message ...))]
         [else (if #f #f)])])))

