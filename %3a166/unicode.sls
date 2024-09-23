
(library (srfi :166 unicode)
  (export terminal-aware
          string-terminal-width string-terminal-width/wide
          substring-terminal-width substring-terminal-width/wide
          substring-terminal-preserve
          upcased downcased)
  (import (rnrs)
          (srfi :130)
          (srfi :151)
          (srfi :166 base)
          (srfi private include))
  (include/resolve ("srfi" "%3a166") "width.scm")
  (include/resolve ("srfi" "%3a166") "unicode.scm"))
