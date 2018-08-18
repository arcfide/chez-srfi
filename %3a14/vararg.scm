; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; Library functionality for writing procedures with variable number of arguments.

; This has the same interface as the OPT-LAMBDA in PLT Scheme's etc.ss
; library.

(define-syntax opt-lambda
  (syntax-rules ()
    ((opt-lambda (?clause1 . ?clauses) ?body1 ?body ...)
     (opt-lambda-aux-1 (?clause1 . ?clauses) () ?body1 ?body ...))
    ((opt-lambda ?id ?body1 ?body ...)
     (lambda ?id ?body1 ?body ...))))

; process the initial vanilla parameters
(define-syntax opt-lambda-aux-1
  (syntax-rules ()
    ((opt-lambda-aux-1 () (?arg ...) ?body ...)
     (lambda (?arg ...) ?body ...))
    ((opt-lambda-aux-1 ((?id ?default) . ?rest) (?arg ...) ?body ...)
     (opt-lambda-aux-2 ((?id ?default) . ?rest)
		       (?arg ... . rest) rest ()
		       ?body ...))
    ((opt-lambda-aux-1 (?id . ?rest) (?arg ...) ?body ...)
     (opt-lambda-aux-1 ?rest (?arg ... ?id) ?body ...))))

; this processes from the optionals on
(define-syntax opt-lambda-aux-2
  (syntax-rules ()
    ((opt-lambda-aux-2 () ?args ?rest-param (?lclause ...) ?body ...)
     (lambda ?args
       (let* (?lclause ...)
	 ?body ...)))
    ;; optimization
    ((opt-lambda-aux-2 ((?id ?default))
		       ?args ?rest-param (?lclause ...) ?body ...)
     (lambda ?args
       (let* (?lclause
	      ...
	      (?id (if (pair? ?rest-param)
		       (car ?rest-param)
		       ?default)))
	 ?body ...)))
    ((opt-lambda-aux-2 ((?id ?default) ?rest ...)
		       ?args ?rest-param (?lclause ...) ?body ...)
     (opt-lambda-aux-2 (?rest ...)
		       ?args
		       new-rest
		       (?lclause ...
				 (?id (if (pair? ?rest-param)
					  (car ?rest-param)
					  ?default))
				 (new-rest (if (pair? ?rest-param)
					       (cdr ?rest-param)
					       '())))
		       ?body ...))
    ;; kludge for dealing with rest parameter
    ((opt-lambda-aux-2 ((?id ?default) . (?rest1 . ?rest))
		       ?args ?rest-param (?lclause ...) ?body ...)
     (opt-lambda-aux-2 (?rest1 . ?rest)
		       ?args
		       new-rest
		       (?lclause ...
				 (?id (if (pair? ?rest-param)
					  (car ?rest-param)
					  ?default))
				 (new-rest (if (pair? ?rest-param)
					       (cdr ?rest-param)
					       '())))
		       ?body ...))
    ((opt-lambda-aux-2 ((?id ?default) . ?rest)
		       ?args ?rest-param (?lclause ...) ?body ...)
     (lambda ?args
       (let* (?lclause
	      ...
	      (?id (if (pair? ?rest-param)
		       (car ?rest-param)
		       ?default))
	      (?rest (if (pair? ?rest-param)
			 (cdr ?rest-param)
			 '())))
	 ?body ...)))))
