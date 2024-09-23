
(library (srfi :166 columnar)
  (export
   columnar tabular wrapped wrapped/list wrapped/char
   justified from-file line-numbers)
  (import (except (rnrs)
                  error
                  define-record-type)
          (rnrs r5rs)
          (only (srfi :1) count fold)
          (srfi :6)
          (srfi :9)
          (srfi :23 error tricks)
          (srfi :117)
          (srfi :130)
          (only (srfi :152 strings) write-string)
          (srfi :166 base)
          (srfi private include))
  (define-syntax let-optionals*
    (syntax-rules ()
      ((let-optionals* opt-ls () . body)
       (begin . body))
      ((let-optionals* (op . args) vars . body)
       (let ((tmp (op . args)))
         (let-optionals* tmp vars . body)))
      ((let-optionals* tmp ((var default) . rest) . body)
       (let ((var (if (pair? tmp) (car tmp) default))
             (tmp2 (if (pair? tmp) (cdr tmp) '())))
         (let-optionals* tmp2 rest . body)))
      ((let-optionals* tmp tail . body)
       (let ((tail tmp)) . body))))
  (define read-line get-line)
  
  (SRFI-23-error->R6RS
   "(library (srfi :166 columnar))"
   (include/resolve ("srfi" "%3a166") "column.scm")))
