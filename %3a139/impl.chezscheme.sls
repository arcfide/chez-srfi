(library (srfi :139 impl)
  (export define-syntax-parameter
          syntax-parameterize)
  (import (chezscheme)
          (srfi :213))

  (define-syntax syntax-parameter? (syntax-rules ()))
  (define-syntax define-syntax-parameter
    (syntax-rules ()
      ((_ name default-transformer)
       (begin
         (define-syntax name default-transformer)
         (define-property name syntax-parameter? #t)))))

  (define-syntax syntax-parameterize
    (lambda (stx)
      (capture-lookup
       (lambda (lookup)
         (syntax-case stx ()
           ((_ ((name new-transformer) ...) body_0 body_1 ...)
            (cond ((find (lambda (n)
                           (not (lookup n #'syntax-parameter?)))
                         #'(name ...))
                   => (lambda (n)
                        (syntax-violation 'syntax-parameterize
                                          "identifier is not bound to a syntax parameter"
                                          stx
                                          n)))
                  (else
                   #'(fluid-let-syntax ((name new-transformer) ...)
                       body_0 body_1 ...))))))))))
