;; Copyright (C) Marc Nieper-Wißkirchen (2019).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice (including
;; the next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi :165)
  (export make-computation-environment-variable
          make-computation-environment computation-environment-ref
          computation-environment-update
          computation-environment-update! computation-environment-copy
          make-computation computation-each computation-each-in-list
          computation-pure computation-bind computation-sequence
          computation-run computation-ask computation-local
          computation-fn computation-with computation-with!
          computation-forked computation-bind/forked
          default-computation
          define-computation-type)
  (import (except (rnrs)
                  define-record-type)
          (srfi :9)
          (srfi :111)
          (srfi :125)
          (srfi :128)
          (only (srfi :133 vectors) vector-copy)
          (srfi :146)
          (srfi :244)
          (srfi private include))
  (include/resolve ("srfi" "%3a165") "implementation.scm"))
