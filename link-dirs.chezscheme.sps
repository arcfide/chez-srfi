#! /usr/bin/env scheme-script
(import (chezscheme))

;;; Link all of the SRFIs to their normal directories like sane 
;;; people who use Chez Scheme prefer. :-)

(define (file-loop files)
  (cond
    [(null? files) (void)]
    [(should-link? (car files))
     (link-file (car files))
     (file-loop (cdr files))]
    [else (file-loop (cdr files))]))

(define (should-link? file)
  (and (< 3 (string-length file))
       (string=? "%3a" (substring file 0 3))))

(define (link-file file)
  (let ([clean (string-append ":" (substring file 3 (string-length file)))])
	(printf "Linking ~a~n" file)
	(system (format "ln -sf '~a' '~a'" file clean))))

(file-loop (directory-list "."))
