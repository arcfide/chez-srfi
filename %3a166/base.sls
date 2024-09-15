
(library (srfi :166 base)
  (export
   ;; basic
   show displayed written written-shared written-simply
   escaped maybe-escaped
   numeric numeric/comma numeric/si numeric/fitted
   nl fl space-to tab-to nothing each each-in-list
   joined joined/prefix joined/suffix joined/last joined/dot
   joined/range padded padded/right padded/both
   trimmed trimmed/right trimmed/both trimmed/lazy
   fitted fitted/right fitted/both output-default
   ;; computations
   fn with with! forked call-with-output
   ;; state variables
   make-state-variable
   port row col width output writer pad-char ellipsis
   string-width substring/width substring/preserve
   radix precision decimal-sep decimal-align sign-rule
   comma-sep comma-rule word-separator? ambiguous-is-wide?
   pretty-environment
   )
  (import (except (rnrs)
                  error
                  define-record-type
                  string-hash string-ci-hash)
          (rnrs eval)
          (rnrs r5rs)
          (srfi :6)
          (srfi :23 error tricks)
          (srfi :69)
          (srfi :130)
          (only (srfi :152 strings) write-string)
          (rename (srfi :165)
                  (computation-each sequence)
                  (computation-with! with!)
                  (computation-forked forked)
                  (make-computation-environment-variable make-state-variable))
          (srfi private include)
          (srfi :166 show-shared))

  (define (interaction-environment) (environment '(rnrs)))
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

  (SRFI-23-error->R6RS
   "(library (srfi :166 base))"
   (include/resolve ("srfi" "%3a166") "base.scm")
   (include/resolve ("srfi" "%3a166") "write.scm")
   (include/resolve ("srfi" "%3a166") "show.scm")))
