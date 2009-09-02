#! /usr/bin/petite --program
(import (rnrs base)
        (rnrs files)
        (rnrs programs)
        (rnrs lists)
        (rnrs unicode)
        (rnrs control)
        (only (scheme) 
          directory-list 
          file-directory? 
          file-regular?
          file-symbolic-link?
          system
          printf
          cd))

(cd (cadr (command-line)))
(let loop ([files (filter (lambda (f)
                            (char-numeric? (string-ref f 0)))
                    (directory-list "."))])
  (unless (null? files)
    (let ([fname (car files)]
          [nfname (string-append ":" (car files))])
      (unless (file-exists? nfname)
        (printf "Fixing ~a~%" fname)
        (system (string-append "ln -s '" fname "' '" nfname "'")))
      (loop (cdr files)))))

