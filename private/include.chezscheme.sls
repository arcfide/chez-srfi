;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SRFI include
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

(library (srfi private include)
  (export include/resolve include/resolve-ci include)
  (import (scheme))

(define-syntax include/resolve
  (lambda (x)
    (syntax-case x ()
      [(k (lib ...) file)
       (for-all (lambda (s) (and (string? s) (positive? (string-length s))))
         (syntax->datum #'(lib ... file)))
       (let ([p (format #f "/~{~a~^/~}" (syntax->datum #'(lib ... file)))]
             [sp (library-directories)])
         (let loop ([paths sp])
           (if (null? paths)
               (errorf 'include/resolve
                 "cannot find file ~s in search paths ~s"
                 (substring p 1 (string-length p))
                 sp)
               (let ([full (string-append (car paths) p)])
                 (if (file-exists? full)
                     #`(#,(datum->syntax #'k 'include) #,full)
                     (loop (cdr paths)))))))])))

(define-syntax include/resolve-ci
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (parameterize ([case-sensitive #f])
          (let ([p (open-input-file fn)])
            (let f ([x (read p)])
              (if (eof-object? x)
                  (begin (close-input-port p) '())
                  (cons (datum->syntax k x)
                        (f (read p)))))))))
    (syntax-case x ()
      [(k (lib ...) file)
       (for-all (lambda (s) (and (string? s) (positive? (string-length s))))
         (syntax->datum #'(lib ... file)))
       (let ([p (format #f "/~{~a~^/~}" (syntax->datum #'(lib ... file)))]
             [sp (library-directories)])
         (let loop ([paths sp])
           (if (null? paths)
               (errorf 'include/resolve
                 "cannot find file ~s in search paths ~s"
                 (substring p 1 (string-length p))
                 sp)
               (let ([full (string-append (car paths) p)])
                 (if (file-exists? full)
                     (with-syntax ([(exp ...) (read-file full #'k)])
                       #'(begin exp ...))
                     (loop (cdr paths)))))))])))

)