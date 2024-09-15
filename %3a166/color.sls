
(library (srfi :166 color)
  (export
   ;; foreground
   as-red as-blue as-green as-cyan as-yellow
   as-magenta as-white as-black
   as-bold as-italic as-underline
   as-color as-true-color
   ;; background
   on-red on-blue on-green on-cyan on-yellow
   on-magenta on-white on-black
   on-color on-true-color
   )
  (import (except (rnrs) error)
          (srfi :23 error tricks)
          (srfi :130)
          (srfi :166 base)
          (srfi private include))
  (define (exact-integer? n) (and (integer? n) (exact? n)))
  (SRFI-23-error->R6RS
   "(library (srfi :166 color))"
   (include/resolve ("srfi" "%3a166") "color.scm")))
