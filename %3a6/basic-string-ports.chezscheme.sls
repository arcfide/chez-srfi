(library (srfi :6 basic-string-ports)
  (export
    (rename (open-string-input-port open-input-string))
    open-output-string
    get-output-string)
  (import (chezscheme))
)
