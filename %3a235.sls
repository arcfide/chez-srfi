;;;
;;; (c) 2022 John Cowan, Arvydas Silanskas.
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice (including the
;;; next paragraph) shall be included in all copies or substantial portions
;;; of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
(library (srfi :235)
  (export constantly
          complement
          swap
          flip
          on-left
          on-right
          conjoin
          disjoin
          each-of
          all-of
          any-of
          on
          left-section
          right-section
          apply-chain
          arguments-drop
          arguments-drop-right
          arguments-take
          arguments-take-right
          group-by

          begin-procedure
          if-procedure
          when-procedure
          unless-procedure
          value-procedure
          case-procedure
          and-procedure
          eager-and-procedure
          or-procedure
          eager-or-procedure
          funcall-procedure
          loop-procedure
          while-procedure
          until-procedure

          always
          never
          boolean)
  (import (srfi :235 combinators)))
