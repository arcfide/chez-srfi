#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi :244 define-values)
  (export define-values)
  (import (rnrs (6)))

  (define-syntax define-values
    (lambda (stx)
      (define who 'define-values)
      (define parse-formals
        (lambda (formals)
          (define output
            (lambda (id*)
              (let f ([id* id*] [i 0])
                (if (null? id*)
                    '()
                    `((,(car id*) ,i)
                      ,@(f (cdr id*) (+ i 1)))))))
          (syntax-case formals ()
            [(id ...)
             (for-all identifier? #'(id ...))
             (output #'(id ...))]
            [(id1 ... . id2)
             (for-all identifier? #'(id1 ... id2))
             (output #'(id1 ... id2))]
            [_
             (syntax-violation who "invalid formals" stx formals)])))
      (syntax-case stx ()
        [(_ () expr)
         #'(define tmp (begin expr #f))]
        [(_ (id) expr)
         (identifier? #'id)
         #'(define id expr)]
        [(_ id expr)
         (identifier? #'id)
         #'(define id (let-values ([tmp expr])
                        tmp))]
        [(_ formals expr)
         (with-syntax ([((id i) ...) (parse-formals #'formals)])
           #'(begin
               (define tmp (let-values ([formals expr])
                             (vector id ...)))
	       (define id (vector-ref tmp i))
	       ...))]
        [_
         (syntax-violation who "invalid syntax" stx)])))

  )

;; Local Variables:
;; mode: scheme
;; End:
