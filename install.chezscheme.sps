#! /bin/sh
#|
exec /usr/bin/env ${SCHEME:-scheme} --script "$0" "$@"
|#
;; Installs versions of srfi libs where calls to (include/resolve ...) have been inlined with requested scheme code.
;; These inlined libs are written to a separate install directory and compiled by Chez scheme.
;;
;; Inlining this way makes all referenced SRFI code compilable.
;;
;; Use from the top level dir of these srfi libs:
;; ./install.chezscheme.sps <dest-dir>
;;
;; The SRFI library will be installed under <dest-dir>. ie, <dest-dir>/srfi/...
;; <dest-dir> will be created if it does not exist.
;;
;; Written by Akce 2020, released into the public domain.
;; SPDX-License-Identifier: Unlicense
;;
;; (translate-name) copied from link-dirs.chezscheme.sps:
;;; Copyright (c) 2012 Aaron W. Hsu <arcfide@sacrideo.us>
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

(import
  (chezscheme)
  (private install sipp))

;; translate-name copied from link-dirs.chezscheme.sps.
;;; Copyright (c) 2012 Aaron W. Hsu <arcfide@sacrideo.us>
;; See header for full copyright.
(define (translate-name name)
  (let f ([i 0] [j 0])
    (if (fx= i (string-length name))
        (make-string j)
        (let ([c (string-ref name i)])
          (cond
            [(and (char=? c #\%)
                  (let ([next-i (fx+ i 3)])
                    (and (fx<= next-i (string-length name)) next-i))) =>
             (lambda (next-i)
               (let ([translated-name (f next-i (fx+ j 1))])
                 (string-set! translated-name j
                   (integer->char
                     (string->number
                       (substring name (fx+ i 1) next-i) 16)))
                 translated-name))]
            [else
             (let ([translated-name (f (fx+ i 1) (fx+ j 1))])
               (string-set! translated-name j c)
               translated-name)])))))

(define srfi-name?
  (lambda (f)
    (char=? #\% (string-ref f 0))))

(define copy-file
  (lambda (src dest)
    (let ([inp (open-file-input-port src)]
          [outp (open-file-output-port dest)])
      (put-bytevector outp (get-bytevector-all inp))
      (for-each close-port `(,inp ,outp)))))

(define copy-directory
  (lambda (src dest)
    (let ([src-files
            (filter
              file-regular?
              (map
                (lambda (f)
                  (join-path src f))
                (directory-list src)))])
      (unless (file-exists? dest)
        (mkdir dest))
      (mkdir (join-path dest src))
      (for-each
        (lambda (s)
          (copy-file s (join-path dest s)))
        src-files))))

(define join-path
  (lambda parts
    (apply join-string directory-separator-string parts)))

(define get-sub-dirs
  (lambda (dir)
    (define join-dir
      (lambda (subdir)
        (join-path dir subdir)))
    (let ([subdirs (filter file-directory? (map join-dir (directory-list dir)))])
    (apply
      append
      subdirs
      (filter pair? (map get-sub-dirs subdirs))))))

(define collect-library-dirs
  (case-lambda
    [(base-dir)
     (let ([subdirs (filter
                      (lambda (f)
                        (and (file-directory? f) (srfi-name? f)))
                      (directory-list base-dir))])
       (apply
         append
         subdirs
         (filter pair? (map get-sub-dirs subdirs))))]))

(define try-create-dir
  (lambda (dir)
    (unless (file-directory? dir)
      (mkdir dir))))

(define create-dirs
  (lambda (dirs)
    (for-each try-create-dir dirs)))

(define scheme-library-file?
  (lambda (f)
    (let ([ext (path-extension f)])
      (and
        (file-regular? f)
        (not (file-symbolic-link? f))
        (string=? ext "sls")))))

(define scheme-program-file?
  (lambda (f)
    (let ([ext (path-extension f)])
      (and
        (file-regular? f)
        (not (file-symbolic-link? f))
        (string=? ext "sps")))))

;; [proc] directory-list/with-path: list directory contents with leading path.
(define directory-list/with-path
  (lambda (dir)
    (define returner
      (cond
        [(string=? "." dir)
         ;; do not return paths with ./ prefix as this becomes a problem case for the import script that
         ;; 'compile-all' needs to generate.
         values]
        [else
          (lambda (f)
            (join-path dir f))]))
    (map		; list directory contents.
      returner
      (directory-list dir))))

(define collect-library-files
  (lambda (library-dirs)
    (apply		        ; flatten lists.
      append
      (filter		        ; remove empty dirs.
        pair?
        (map		        ; only get real *scheme* files.
          (lambda (d)
            (filter scheme-library-file? (directory-list/with-path d)))
          library-dirs)))))

(define install-file
  (lambda (src dest)
    (with-output-to-file dest
      (lambda ()
        (for-each pretty-print (replace-source src #t))))))

(define install-srfi
  (lambda (src-dir dest-dir)
    (define srfi-dest-dir (join-path dest-dir "srfi"))
    (define join-dest-dir
      (lambda (f)
        (join-path srfi-dest-dir f)))
    (when (file-exists? srfi-dest-dir)
      (error #f "SRFI destination directory exists. Please remove before running again." srfi-dest-dir))
    (let* ([src-dirs (collect-library-dirs src-dir)]
           [src-files (collect-library-files (cons src-dir src-dirs))]
           [prefix-dirs
             (map
               join-dest-dir
               (map translate-name src-dirs))]
           [prefix-files
             (map
               join-dest-dir
               (map translate-name src-files))])
      (create-dirs (apply list dest-dir srfi-dest-dir prefix-dirs))
      (for-each install-file src-files prefix-files)
      (values srfi-dest-dir src-files))))

(define path->srfi-include
  (lambda (p)
    (define /->space
      (lambda (str)
        (list->string
          (map (lambda (c)
                 (case c
                   [(#\/)
                    #\space]
                   [else
                     c]))
          (string->list str)))))
    (define filename->srfi
      (lambda (str)
        (string-append "(srfi " (/->space (translate-name str)) ")")))
    (let* ([fn (path-root p)]
           [ext (path-extension fn)])
      ;; Check for second level extension. These can exist for scheme specific implementations.
      ;; We'll include both generic and Chez versions.
      (cond
        [(string=? "" ext)
         (filename->srfi fn)]
        [(string=? "chezscheme" ext)
         (filename->srfi (path-root fn))]
        [else
          ;; Do not include specific implementations for other schemes.
          #f]))))

;; compile-all generates an import scheme script that compiles all imported libraries in place.
;; Doing it this way lets Chez scheme handle dependancies correctly and compile libs only once.
(define compile-all
  (lambda (dest-dir src-files)
    (let ([fn "./compile-all.chezscheme.sps"])
      (with-output-to-file
        fn
        (lambda ()
          (format #t "#! /bin/sh
#|
exec /usr/bin/env ${SCHEME:-scheme} --compile-imported-libraries --script \"$0\" \"$@\"
|#

;; DO NOT EDIT!!
;; This file was autogenerated by: $ ~a.

(import (chezscheme))
(library-directories \"~a\")

(import
~a
)
" (apply join-string " " (command-line)) dest-dir (apply join-string
              "\n"
              (filter values (map path->srfi-include src-files)))))
        '(replace mode #o755))
      (system fn))))

(define install-private
  (lambda (src-dir dest-dir)
    (copy-directory (join-path src-dir "private") dest-dir)
    ;; create a null (srfi private include) library since those imports haven't been removed.
    (delete-file (join-path dest-dir "private" "include.sls"))
    (call-with-output-file (join-path dest-dir "private" "include.chezscheme.sls")
      (lambda (p)
        (pretty-print '(library (srfi private include)
                         (export)
                         (import (chezscheme)))
                      p)))))

(define install-tests
  (lambda (src-dir dest-dir)
    (let ([src-files (filter scheme-program-file? (directory-list/with-path (join-path src-dir "tests")))]
          [dest-test-dir (join-path dest-dir "tests")])
      (try-create-dir dest-test-dir)
      (for-each
        install-file
        src-files
        (map
          (lambda (src)
            ;; Don't translate-name here. That way tests/test_all.sh works for both installed and linked SRFIs.
            (join-path dest-dir src))
          src-files))
      (copy-file (join-path src-dir "tests" "test_all.sh") (join-path dest-test-dir "test_all.sh")))))

(define main
  (lambda (src-dir dest-dir)
    (let-values ([(srfi-dest-dir lib-files) (install-srfi src-dir dest-dir)])
      (install-private src-dir srfi-dest-dir)
      (install-tests src-dir srfi-dest-dir)
      (library-directories dest-dir)
      (compile-all dest-dir lib-files))))

(cond
  [(null? (cdr (command-line)))
   (format #t "Usage:
  $ ~a <destination-dir>

  Where <destination-dir> is in the Chez scheme library search path, (library-directories).
  The SRFIs will be installed beneath <destination-dir>. ie, <destination-dir>/srfi
  The <destination-dir>/srfi directory must not exist.

" (car (command-line)))
(exit 1)]
   [else
     (main "." (list-ref (command-line) 1))])
;; vi:ft=scheme:
