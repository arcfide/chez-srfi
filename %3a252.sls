(library (srfi :252)
  (export test-property test-property-expect-fail test-property-skip
          test-property-error test-property-error-type
          property-test-runner
          ;; Generator procedures
          boolean-generator bytevector-generator
          char-generator string-generator symbol-generator
          ;; exact number generators
          exact-complex-generator exact-integer-generator
          exact-number-generator exact-rational-generator
          exact-real-generator
          exact-integer-complex-generator
          ;; inexact number generators
          inexact-complex-generator inexact-integer-generator
          inexact-number-generator inexact-rational-generator
          inexact-real-generator
          ;; Unions of numerical generators
          complex-generator integer-generator
          number-generator rational-generator
          real-generator
          ;; Special generators
          list-generator-of pair-generator-of procedure-generator-of
          vector-generator-of)
  (import (srfi :252 property-testing)))
