;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Platform features for SRFI-0
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

(library (srfi private platform-features)
  (export OS-features implementation-features)
  (import (rnrs base) (only (scheme) machine-type))
  
(define OS-features
  (lambda ()
    (case (machine-type)
      [(a6le) '(linux x86_64 posix)] 				; Linux x86_64 non-threaded
      [(ta6le) '(linux x86_64 posix threaded)] 		; Linux x86_64 threaded
      [(i3le) '(linux x86 posix)] 					; Linux x86 non-threaded
      [(ti3le) '(linux x86 posix threaded)] 		; Linux x86 threaded
      [(i3osx) '(darwin x86 posix)] 				; Mac OS X x86 non-threaded
      [(ti3osx) '(darwin x86 posix threaded)] 		; Mac OS X x86 threaded
      [(a6osx) '(darwin x86_64 posix)]				; Mac OS X x86_64 non-threaded
      [(ta6osx) '(darwin x86_64 posix threaded)] 	; Mac OS X x86_64 threaded
      [(ppcosx) '(linux x86_64 posix)] 				; Mac OS X PowerPC non-threaded
      [(tppcosx) '(linux x86_64 posix threaded)] 	; Mac OS X PowerPC threaded
      [(i3nt) '(windows x86_64)] 					; Windows x86 non-threaded
      [(ti3nt) '(windows x86_64 threaded)] 			; Windows x86 threaded
      [(i3ob) '(openbsd x86_64 posix)] 				; OpenBSD x86 non-threaded
      [(ti3ob) '(openbsd x86_64 posix threaded)] 	; OpenBSD x86 threaded
      [(a6ob) '(openbsd x86_64 posix)] 				; OpenBSD x86_64 non-threaded
      [(ta6ob) '(openbsd x86_64 posix threaded)] 	; OpenBSD x86_64 threaded
      [(i3fb) '(freebsd x86 posix)] 				; FreeBSD x86 non-threaded
      [(ti3fb) '(freebsd x86 posix threaded)] 		; FreeBSD x86 threaded
      [(sps2) '(sparc posix)] 						; Sparc 32-bit non-threaded
      [(tsps2) '(sparc posix threaded)] 			; Sparc 32-bit threaded
      [(sp64) '(sparc posix)] 						; Sparc 64-bit non-threaded
      [(tsp64) '(sparc posix threaded)])))			; Sparc 64-bit threaded

(define implementation-features
  (lambda ()
    '(ikarus)))

)