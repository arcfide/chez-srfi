;;; SPDX-FileCopyrightText: 2021 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

;;; Pattern-matching macro for trie values.
;;;
;;; Based on William Byrd's pmatch modification of Oleg Kiselyov's
;;; simple linear pattern-matcher.

;;; Syntax:
;;;
;;; (tmatch exp <clause> ... [<else-clause>])
;;;
;;; <clause>      ::= (<pattern> [<guard>] exp ...)
;;; <else-clause> ::= (else exp ...)
;;; <guard>       ::= (guard boolean-exp ...)
;;; <pattern>     ::= empty
;;;                 | (leaf <pattern> <pattern>)
;;;                 | (branch <pattern> <pattern> <pattern> <pattern>)

;; (define-syntax tmatch
;;   (syntax-rules (else)
;;     ((tmatch exp (e ...) ...)
;;      (tmatch-aux #f exp (e ...) ...))
;;     ((tmatch name exp (e ...) ...)
;;      (tmatch-aux name exp (e ...) ...))))

(define-syntax tmatch
  (syntax-rules (else guard)
    ((tmatch (f x ...) cs ...)
     (let ((v (f x ...)))
       (tmatch v cs ...)))
    ((tmatch v)
     (error "tmatch: no clause matched" v))
    ((tmatch _ (else e0 e1 ...)) (begin e0 e1 ...))
    ((tmatch v (pat (guard g ...) e0 e1 ...) cs ...)
     (let ((fk (lambda () (tmatch v cs ...))))
       (tpat v pat (if (and g ...) (begin e0 e1 ...) (fk)) (fk))))
    ((tmatch v (pat e0 e1 ...) cs ...)
     (let ((fk (lambda () (tmatch v cs ...))))
       (tpat v pat (begin e0 e1 ...) (fk))))))

;; Uses pmatch's `ppat' auxilliary macro, see below.
(define-syntax tpat
  (syntax-rules (empty leaf branch unquote)
    ((tpat v empty kt kf) (if v kf kt))
    ((tpat v (leaf pkey pval) kt kf)
     (if (leaf? v)
         (let ((key (leaf-key v)) (value (leaf-value v)))
           (ppat key pkey (ppat value pval kt kf) kf))
         kf))
    ((tpat v (branch pp pm pl pr) kt kf)
     (if (branch? v)
         (let ((pfx (branch-prefix v))
               (bit (branch-branching-bit v))
               (left (branch-left v))
               (right (branch-right v)))
           (ppat pfx
                 pp
                 (ppat bit pm (ppat left pl (ppat right pr kt kf) kf) kf)
                 kf))
         kf))))

;; Shorthands for a unary function that immediately pattern-matches
;; its trie parameter.
(define-syntax tmatch-lambda
  (syntax-rules ()
    ((tmatch-lambda cs ...)
     (lambda (arg) (tmatch arg cs ...)))))

;;; pmatch, by Oleg Kiselyov, rev. Will Byrd.
;;; The original public-domain code can be found at
;;; http://okmij.org/ftp/Scheme/match-case-simple.scm

;; This is a new version of pmatch (August 8, 2012).
;; It has two important new features:
;; 1.  It allows for a name to be given to the pmatch if an error ensues.
;; 2.  A line from the specification has been removed. (see below).  Without
;; that line removed, it was impossible for a pattern to be (quote ,x),
;; which might be worth having especially when we write an interpreter
;; for Scheme, which includes quote as a language form.

;;; Code written by Oleg Kiselyov
;; (http://pobox.com/~oleg/ftp/)
;;;
;;; Taken from leanTAP.scm
;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS (and R6RS) Scheme system.

; (pmatch exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;        'exp  -- comparison with exp (using equal?)    REMOVED (August 8, 2012)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list

;; We've removed the name parameter for now, since it seems to cause
;; problems for the expander in many Schemes.

;; (define-syntax pmatch
;;   (syntax-rules (else guard)
;;     ((pmatch v (e ...) ...)
;;      (pmatch-aux #f v (e ...) ...))
;;     ((pmatch v name (e ...) ...)
;;      (pmatch-aux name v (e ...) ...))))

(define-syntax pmatch
  (syntax-rules (else guard)
    ((pmatch (rator rand ...) cs ...)
     (let ((v (rator rand ...)))     ; avoid multiple evals
       (pmatch v cs ...)))
    ((pmatch v)  ; no more clauses
     (error "pmatch failed" v))
    ((pmatch _ (else e0 e ...)) (begin e0 e ...))
    ((pmatch v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((pmatch v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (? unquote)
    ((ppat _ ? kt _) kt)  ; the ? wildcard always matches
    ((ppat v () kt kf) (if (null? v) kt kf))
;   ((ppat v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
    ((ppat v (unquote var) kt _) (let ((var v)) kt))
    ((ppat v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
	 (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((ppat v lit kt kf) (if (equal? v (quote lit)) kt kf))))

;;; Shorthands for functions that immediately pattern-match their
;;; parameter(s).

;; One-argument form.
(define-syntax pmatch-lambda
  (syntax-rules ()
    ((pmatch-lambda cs ...)
     (lambda (arg) (pmatch arg cs ...)))))

;; Multi-argument form.
(define-syntax pmatch-lambda*
  (syntax-rules ()
    ((pmatch-lambda* cs ...)
     (lambda args (pmatch args cs ...)))))
