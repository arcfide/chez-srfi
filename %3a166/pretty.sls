
(library (srfi :166 pretty)
  (export pretty pretty-shared pretty-simply pretty-with-color)
  (import (except (rnrs)
                  string-hash string-ci-hash)
          (rnrs r5rs)
          (rnrs mutable-pairs)
          (only (srfi :1) length+)
          (srfi :6)
          (srfi :69)
          (srfi :130)
          (srfi :166 base)
          (srfi :166 color)
          (srfi :166 show-shared)
          (srfi private include))
  (include/resolve ("srfi" "%3a166") "pretty.scm"))
