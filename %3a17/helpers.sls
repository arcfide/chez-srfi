;; SRFI-17 generic helpers
;;
;; This file contains R6RS compatible wrappers for some of the
;; built-in setters used by the generalized set! syntax.
;;
;; Note: this was never completely fleshed out.
;;
;; Copyright (c) 2018 - 2020 Andrew W. Keep

  (define-syntax define-$set-c...r!
    (lambda (x)
      (define (build-defs-for-level k cnt defs)
        (let ([ls (list "a" "d")])
          (let loop ([i 1] [names ls])
            (if (fx= i (fx- cnt 1))
                (fold-left
                  (lambda (defs name)
                    (fold-left
                      (lambda (defs a)
                        (with-syntax ([base-getter (datum->syntax #'* (string->symbol (string-append "c" name "r")))]
                                      [base-setter (datum->syntax #'* (string->symbol (string-append "set-c" a "r!")))]
                                      [full-setter (datum->syntax k (string->symbol (string-append "$set-c" a name "r!")))])
                          (cons
                            #'(define full-setter
                                (lambda (x v)
                                  (base-setter (base-getter x) v)))
                            defs)))
                      defs ls))
                  defs names)
                (loop (fx+ i 1)
                      (fold-left
                        (lambda (new-names a)
                          (fold-left
                            (lambda (new-names name)
                              (cons (string-append a name) new-names))
                            new-names names))
                        '() ls))))))
      (define (build-defs k s e)
        (do ([i s (fx+ i 1)]
             [defs '() (build-defs-for-level k i defs)])
             ((fx> i e) defs)))
      (syntax-case x ()
        [(k s e)
         (and (and (integer? (datum s)) (exact? (datum s)))
              (and (integer? (datum e)) (exact? (datum s))))
         (with-syntax ([(defs ...) (build-defs #'k (datum s) (datum e))])
           #'(begin defs ...))])))

  (define-$set-c...r! 2 4)

  (define-syntax define-hashtable-set!
    (lambda (x)
      (define (build-def k)
        (lambda (name)
          (with-syntax ([out-name (datum->syntax k (string->symbol (string-append "$" name "hashtable-set!")))]
                        [name (datum->syntax #'* (string->symbol (string-append name "hashtable-set!")))])
            #'(define-syntax out-name
                (syntax-rules ()
                  [(_ ht k dv v) (name ht k v)])))))
      (syntax-case x ()
        [(k name ...) (andmap string? (datum (name ...)))
         (with-syntax ([(defs ...) (map (build-def #'k) (datum (name ...)))])
           #'(begin defs ...))])))

  (define-hashtable-set! "" "eq" "symbol")

  (define $list-set!
    (lambda (ls orig-idx v)
      (let loop ([ls ls] [idx orig-idx])
        (if (fx= idx 0)
            (set-car! ls v)
            (if (null? ls)
                (errorf 'list-ref "~s index out of range" orig-idx)
                (loop (cdr ls) (fx+ idx 1)))))))


