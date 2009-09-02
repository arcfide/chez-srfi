;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chez SRFI-98 Environment Variables
;;; 
;;; Copyright (c) 2009 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.


(library (srfi :98 os-environment-variables)
  (export get-environment-variable get-environment-variables)
  (import
    (rnrs base)
    (rnrs io ports)
    (only (srfi :13) string-index)
    (rename (only (scheme) process getenv let-values trace)
      (getenv get-environment-variable)))

(define get-environment-variables
  (lambda ()
    (let-values ([(in out id) (apply values (process "env"))])
      (let loop ([env '()])
        (if (port-eof? in)
            (reverse env)
            (let-values ([(key val) (env-split (get-line in))])
              (loop (cons (cons key val) env))))))))

(define env-split
  (lambda (line)
    (let ([i (string-index line #\=)])
      (if i 
          (values 
            (substring line 0 i) 
            (substring line (+ i 1) (string-length line)))
          (values line #f)))))

)