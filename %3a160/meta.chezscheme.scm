;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define like-vowels
  (string->list "faeiohlmnrstvwxyzFAEIOHLMNRSTVWXYZ"))

(define (one-of str)
  (if (and (fx>? (string-length str) 0)
           (memq (string-ref str 0) like-vowels))
      "an"
      "a"))

(define (format-vector-type fmt-str type-of)
  (let ([template (format "~a ~a" (one-of type-of) type-of)])
    (format fmt-str template)))

(define (compare-lengths len comp v . vs)
  "Apply length and run a comparison like min/max, and return the result or #f"
  (let loop ([src vs] [prev (len v)])
    (if (null? src)
	prev
	(let ([res (comp prev (len (car src)))])
	  (and res
	       (loop (cdr src) res))))))

(define (all-same-length? len v . vs)
  (apply compare-lengths
	 len
	 (lambda (prev curr)
	   (and (fx=? prev curr)
		curr))
	 v vs))

(define (total-length len vs)
  (apply compare-lengths len + vs))

(define (vectorised-subscript sub vs i)
  (list-map (lambda (v) (sub v i))
	    vs))

(define (nonnegative-fixnum? x)
  (and (fixnum? x)
       (not (fxnegative? x))))

(define (exact? x)
  (and (number? x) (r6rs:exact? x)))

(define (inexact? x)
  (and (number? x) (r6rs:inexact? x)))

(define (exact-integer? x)
  (and (integer? x)
       (r6rs:exact? x)))

(define (inexact-integer? x)
  (and (integer? x)
       (r6rs:inexact? x)))

(define (magnitude>? x y)
  (> (magnitude x) (magnitude y)))

(define (magnitude<? x y)
  (< (magnitude x) (magnitude y)))

(define (compose f . rest)
  (if (null? rest)
      f
      (let ([g (apply compose rest)])
	(lambda args
	  (call-with-values (lambda () (apply g args)) f)))))

(define identity values)

(define raised-with?
  (case-lambda
   [(ex who)
    (and (who-condition? ex)
         (eq? (condition-who ex) who))]
   [(ex who . who*)
    (and (who-condition? ex)
         (eq? (condition-who ex) (cons who who*)))]))

(define-syntax (guarded-body stx)
  (syntax-case stx ()
    [(_ who (ident ...) body0 ... body*)
     #'(guard
           (ex [(and (who-condition? ex)
                     (memq (condition-who ex) (list (quote ident) ...)))
                (cond [(and (irritants-condition? ex) (message-condition? ex))
                       (apply assertion-violationf who (condition-message ex)
                              (condition-irritants ex))]
                      [(message-condition? ex)
                       (assertion-violationf who (condition-message ex))]
                      [else
                       (raise-continuable ex)])]
               [else
                (raise-continuable ex)])
         body0 ... body*)]))

(define-syntax (reraise stx)
  (syntax-violation #f "invalid use of auxilliary syntax" stx))

(define-syntax (define/who stx)
  (syntax-case stx (reraise)
    [(k (bind-to . args) body0 ... body* (reraise ident ...))
     (identifier? #'bind-to)
     (with-implicit (k who)
                    #'(define bind-to
                        (let ([who (quote bind-to)])
                          (lambda args
                            (guarded-body who (ident ...) body0 ... body*)))))]))

(define-syntax (define/case-who stx)
  (syntax-case stx (reraise)
    [(k bind-to
        [args0 body0 ... body*]
        ...
        [argsN bodyN ... bodyN*]
        (reraise ident ...))
     (identifier? #'bind-to)
     (with-implicit (k who)
                    #'(define bind-to
                        (let ([who (quote bind-to)])
                          (case-lambda
                           [args0 (guarded-body who (ident ...) body0 ... body*)]
                           ...
                           [argsN (guarded-body who (ident ...) bodyN ... bodyN*)]))))]))

(define-syntax (assert/who stx)
  (syntax-case stx ()
    [(_ who expression message)
     #'(unless expression
	 (assertion-violationf who message))]
    [(_ who expression message irritant ...)
     #'(unless expression
         (assertion-violationf who message
                               irritant ...))]))

(define assert-start-nat
  (case-lambda
   [(who start)
    (assert/who who
                (nonnegative-fixnum? start) "start ~a is not a non-negative fixnum"
                start)]
   [(who start prefix)
    (assert/who who
                (nonnegative-fixnum? start)
                (format "~a start ~~a is not a non-negative fixnum" prefix) start)]))

(define assert-end-nat
  (case-lambda
   [(who end)
    (assert/who who (nonnegative-fixnum? end) "end ~a is not a non-negative fixnum" end)]
   [(who end prefix)
    (assert/who who
                (nonnegative-fixnum? end)
                (format "~a end ~~a is not a non-negative fixnum" prefix) end)]))

(define assert-start<=end
  (case-lambda
   [(who start end)
    (assert/who who (fx<=? start end) "end ~a must be greater than or equal to start ~a" end start)]
   [(who start end anno)
    (assert/who who
                (fx<=? start end)
                (format "~a end ~~a must be greater than or equal to ~a start ~~a" anno anno)
                end start)]))

(define (assert-bounds who end width v)
  (assert/who who (fx<=? end width) "end ~a overflows ~a"
              end v))

(define assert-index-nat
  (case-lambda
   [(who k)
    (assert/who who
                (nonnegative-fixnum? k)
                "index ~a is not a non-negative fixnum"
                k)]
   [(who k prefix)
    (assert/who who
                (nonnegative-fixnum? k)
                (format "~a index ~~a is not a non-negative fixnum" prefix) k)]))

(define assert-index-bounds
  (lambda (who k size v)
    (assert/who who (fx>=? size k) "index ~a overflows ~a"
                k v)))

(define (sub-append-triple who lst)
  (if (and (list? lst) (fx=? (length lst) 3))
      (apply values lst)
      (assert/who who #f
                  "expected triple of vector, start and end"
                  #;lst)))

(define-syntax (define-meta-all stx)
  (syntax-case stx ()
    [(_ variant *real?*
	*repr?* *elem?*
	*type-of* *gen-elem*
	*from-args*
	*make*
	*from-list*
	*to-list*
	*length*
	*subscript*
	*update!*)
     (letrec* ([make-emit-ident
                (lambda (xs)
                  (case-lambda
                   [() (emit-ident #f #f #f #f)]
                   [(suffix) (emit-ident #f #f '- suffix)]
                   [(prefix delim-pre delim-post suffix)
                    (let-values ([(prefix delim-pre delim-post suffix)
		                  (apply values (list-map (lambda (sym)
					                    (if (not sym)
                                                                ""
                                                                (symbol->string sym)))
				                          (list prefix delim-pre delim-post suffix)))])
	              (datum->syntax xs
	                             (string->symbol
                                      (format "~a~a~a~a~a"
                                              prefix delim-pre (syntax->datum xs)
                                              delim-post suffix))))]))]
               [emit-ident
                (make-emit-ident #'variant)])
       (with-syntax
	   (#| mutators
               NB., these need to be defined first as core operations used throughout SRFI 160 |#
	    [(swap! unfold! unfold-right! fill! reverse! copy! reverse-copy!)
	     (list-map emit-ident '(swap! unfold! unfold-right! fill! reverse! copy! reverse-copy!))]
	    #| constructors |#
	    [(unfold unfold-right copy reverse-copy
		     append sub-append concatenate)
	     (list-map emit-ident '(unfold unfold-right copy reverse-copy
		    	                   append append-subvectors concatenate))]
	    #| iteration |#
	    [(take take-right drop drop-right segment
	           fold fold-right map map! for-each count cumulate)
	     (list-map emit-ident '(take take-right drop drop-right segment
			                 fold fold-right map map! for-each count cumulate))]
	    #| searching |#
	    [(take-while take-while-right drop-while drop-while-right
		         index index-right skip skip-right any every partition filter remove)
	     (list-map emit-ident '(take-while take-while-right drop-while drop-while-right
			                       index index-right skip skip-right any every partition filter remove))]
	    #| predicates |#
	    [(empty? =? #;<?)
             (list (emit-ident 'empty?) (emit-ident #f #f #f '=) #;(emit-ident #f #f #f '<))]
            [(comp>? comp<?)
             (if (syntax->datum #'*real?*)
                 (list #'> #'<)
                 (list #'magnitude>? #'magnitude<?))]
	    #| conversion |#
	    [(reverse-to-list reverse-from-list to-vector from-vector)
	     (list (apply emit-ident '(reverse - -> list))
                   (apply emit-ident '(reverse-list -> #f #f))
                   (apply emit-ident '(#f #f -> vector))
                   (apply emit-ident '(vector -> #f #f)))]
	    #| comparators |#
	    [(hash comparator) (list-map emit-ident '(hash comparator))]
	    #| generators |#
	    [make-generator
             (apply emit-ident '(make - - generator))]
            #| output |#
            [write-port
             (apply emit-ident '(write- #f #f #f))])
         ;;
	 #'(begin
	     #| mutators |#

             (define/who (swap! vec i j)
               (let ([i@v (*subscript* vec i)]
	             [j@v (*subscript* vec j)])
                 (*update!* vec i j@v)
                 (*update!* vec j i@v))
               (reraise *subscript* *update!*))

             (define/who (unfold! proc vec start end ini)
               (assert-start-nat  who start)
               (assert-end-nat    who end)
               (assert-start<=end who start end)
               (assert-bounds     who end (*length* vec) vec)
               (let loop ([i start] [seed ini])
                 (when (fx<? i end)
                   (let-values ([(x next) (proc i seed)])
	             (*update!* vec i x)
	             (loop (fx+ i 1) next))))
               (reraise *length* *update!*))

             (define/who (unfold-right! proc vec start end ini)
               (assert-start-nat  who start)
               (assert-end-nat    who end)
               (assert-start<=end who start end)
               (assert-bounds     who end (*length* vec) vec)
               (let loop ([i (fx- end 1)] [seed ini])
                 (when (fx>=? i start)
                   (let-values ([(x next) (proc i seed)])
	             (*update!* vec i x)
	             (loop (fx- i 1) next))))
               (reraise *length* *update!*))

             (define/case-who fill!
               [(vec rep)       (fill! vec rep 0 (*length* vec))]
               [(vec rep start) (fill! vec rep start (*length* vec))]
               [(vec rep start end)
                (assert/who who
                            (*elem?* rep)
                            (format-vector-type "repeating element ~~a cannot be contained within ~a" *type-of*)
                            rep)
                (unfold!
                 (lambda _ (values rep rep))
                 vec start end #f)]
               (reraise *length* unfold!))

             (define/case-who reverse!
               [(vec)       (reverse! vec 0 (*length* vec))]
               [(vec start) (reverse! vec start (*length* vec))]
               [(vec start end)
                (assert-start-nat  who start)
                (assert-end-nat    who end)
                (assert-start<=end who start end)
                (assert-bounds     who end (*length* vec) vec)
                (let loop ([i start] [j (fx- end 1)])
                  (when (fx<? i j)
	            (swap! vec i j)
	            (loop (fx+ i 1) (fx- j 1))))]
               (reraise *length*))

             (define/case-who copy!
               [(tgt tgt-start src)           (copy! tgt tgt-start src 0 (*length* src))]
               [(tgt tgt-start src src-start) (copy! tgt tgt-start src src-start (*length* src))]
               [(tgt tgt-start src src-start src-end)
                (assert-start-nat  who tgt-start "target")
                (assert-start-nat  who src-start "source")
                (assert-end-nat    who src-end "source")
                (assert-start<=end who src-start src-end "source")
                (let ([src-length (*length* src)]
	              [tgt-length (*length* tgt)])
                  (assert/who who
                              (fx<=? src-end src-length)
                              "source end ~a exceeds length of source ~a"
                              src-end src)
                  (assert/who who
                              (fx<=? tgt-start tgt-length)
                              "target start ~a exceeds length of target ~a"
                              tgt-start tgt)
                  (let ([adjusted-tgt-end (fx+ tgt-start (fx- src-end src-start))]
	                [unfold-with (lambda (tgt-i src-i)
			               (values (*subscript* src src-i) (fx+ src-i 1)))])
                    (assert/who who
                                (fx<=? adjusted-tgt-end tgt-length)
                                "slice source overruns target by ~a elements"
                                (fx- adjusted-tgt-end tgt-length))
	            (unfold! unfold-with
                             tgt
                             tgt-start
                             adjusted-tgt-end
                             src-start)))]
               (reraise *length* unfold!))

             (define/case-who reverse-copy!
               [(tgt tgt-start src)           (reverse-copy! tgt tgt-start src 0 (*length* src))]
               [(tgt tgt-start src src-start) (reverse-copy! tgt tgt-start src src-start (*length* src))]
               [(tgt tgt-start src src-start src-end)
                (assert-start-nat  who tgt-start "target")
                (assert-start-nat  who src-start "source")
                (assert-end-nat    who src-end "source")
                (assert-start<=end who src-start src-end "source")
                (let ([src-length (*length* src)]
	              [tgt-length (*length* tgt)])
                  (assert/who who
                              (fx<=? src-end src-length)
                              "source end ~a exceeds length of source ~a"
                              src-end src)
                  (assert/who who
                              (fx<=? tgt-start tgt-length)
                              "target start ~a exceeds length of target ~a"
                              tgt-start tgt)
                  (let ([adjusted-tgt-end (fx+ tgt-start (fx- src-end src-start))]
	                [unfold-with (lambda (tgt-i src-i)
			               (values (*subscript* src src-i) (fx+ src-i 1)))])
                    (assert/who who
                                (fx<=? adjusted-tgt-end tgt-length)
                                "slice source overruns target by ~a elements"
                                (fx- adjusted-tgt-end tgt-length))
	            (unfold-right! unfold-with
                                   tgt
                                   tgt-start
                                   adjusted-tgt-end
                                   src-start)))]
               (reraise *length* unfold!))


             #| constructors |#

             (define/who (unfold proc size ini)
               (let ([slots (*make* size)])
                 (unfold! proc slots 0 size ini)
                 slots)
               (reraise *make* unfold!))

             (define/who (unfold-right proc size ini)
               (let ([slots (*make* size)])
                 (unfold-right! proc slots 0 size ini)
                 slots)
               (reraise *make* unfold-right!))

             (define/case-who copy
               [(vec)       (copy vec 0 (*length* vec))]
               [(vec start) (copy vec start (*length* vec))]
               [(vec start end)
                (assert-start-nat  who start)
                (assert-end-nat    who end)
                (assert-start<=end who start end)
                (assert-bounds     who end (*length* vec) vec)
                (let ([slots (*make* (fx- end start))])
	          (copy! slots 0 vec start end)
	          slots)]
               (reraise *length* *make* *copy!*))

             (define/case-who reverse-copy
               [(vec)       (reverse-copy vec 0 (*length* vec))]
               [(vec start) (reverse-copy vec start (*length* vec))]
               [(vec start end)
                (assert-start-nat  who start)
                (assert-end-nat    who end)
                (assert-start<=end who start end)
                (assert-bounds     who end (*length* vec) vec)
                (let ([slots (*make* (fx- end start))])
	          (reverse-copy! slots 0 vec start end)
	          slots)]
               (reraise *length* *make* *copy!*))

             (define/who (append v . vs)
               (if (and (null? vs) (*length* v))
                   v
                   (let* ([vecs (cons v vs)]
	                  [lens (list-map *length* vecs)]
	                  [slots (*make* (apply fx+ lens))])
	             (fold-left
	              (lambda (last-extent curr-width curr-vec)
	                (copy! slots last-extent curr-vec)
	                (fx+ last-extent curr-width))
	              0
	              lens vecs)
	             slots))
               (reraise *length* *make* copy!))

             (define/who (sub-append v . vs)
               (if (null? vs)
                   (let*-values ([(vv start end) (sub-append-triple who v)]
                                 [(slots)        (*make* (fx- end start))])
	             (copy! slots 0 vv start end)
	             slots)
                   (let*-values ([(vecs lens starts ends total-len)
		                  (let loop
			              ([to-process (cons v vs)]
			               [vecs '()] [lens '()] [starts '()] [ends '()]
			               [tally 0])
		                    (if (null? to-process)
			                (values vecs lens starts ends tally)
                                        (let*-values ([(vv start end) (sub-append-triple who (car to-process))]
                                                      [(width) (fx- end start)])
				          (loop (cdr to-process)
				                (cons vv vecs)
				                (cons width lens)
				                (cons start starts)
				                (cons end ends)
				                (fx+ width tally)))))]
		                 [(slots)
                                  (*make* total-len)])
	             (fold-left (lambda (l v start end last-extent)
		                  (copy! slots last-extent v start end)
		                  (fx+ last-extent l))
		                0
		                lens vecs starts ends)
	             slots))
               (reraise *make* copy!))

             (define/who (concatenate vecs)
               (let ([v (*make* (total-length *length* vecs))])
                 (let loop ([vecs vecs] [at 0])
                   (unless (null? vecs)
                     (let ([vec (car vecs)])
                       (copy! v at vec 0 (*length* vec))
                       (loop (cdr vecs) (fx+ at (*length* vec)))))
                   v))
               (reraise *length* *make* copy!))

             #| iteration |#

             (define/who (take v k)
               (assert-index-nat who k)
               (assert-index-bounds who k (*length* v) v)
               (copy v 0 k)
               (reraise *length*))

             (define/who (take-right v k)
               (let ([size (*length* v)])
                 (assert-index-nat who k)
                 (assert-index-bounds who k size v)
                 (copy v (fx- size k) size))
               (reraise *length*))

             (define/who (drop v k)
               (let ([size (*length* v)])
                 (assert-index-nat who k)
                 (assert-index-bounds who k size v)
                 (copy v k size))
               (reraise *length*))

             (define/who (drop-right v k)
               (let ([size (*length* v)])
                 (assert-index-nat who k)
                 (assert-index-bounds who k size v)
                 (copy v 0 (fx- size k)))
               (reraise *length*))

             (define/who (segment v n)
               (assert/who who
                           (nonnegative-fixnum? n)
                           "segment ~a is not a non-negative fixnum" n)
               (let loop ([acc '()] [i 0] [remain (*length* v)])
                 (if (fx<=? remain 0)
	             (reverse acc)
	             (let ([size (fxmin n remain)])
                       (loop (cons (copy v i (fx+ i size)) acc)
		             (fx+ i size)
		             (fx- remain size)))))
               (reraise *length*))

             (define/who (fold proc id v . vs)
               (let* ([vecs (cons v vs)]
	              [width (apply compare-lengths *length* fxmin vecs)])
                 (let loop ([i 0] [acc id])
                   (if (fx=? i width)
	               acc
	               (let* ([indices (list-map (lambda (v) (*subscript* v i)) vecs)]
		              [processed (apply proc acc indices)])
	                 (loop (fx+ i 1) processed)))))
               (reraise *length* *subscript*))

             (define/who (fold-right proc id v . vs)
               (let* ([vecs (cons v vs)]
	              [width (apply compare-lengths *length* fxmin vecs)])
                 (let loop ([i (fx- width 1)] [acc id])
                   (if (fx<? i 0)
	               acc
	               (let* ([indices (list-map (lambda (v) (*subscript* v i)) vecs)]
		              [processed (apply proc acc indices)])
	                 (loop (fx- i 1) processed)))))
               (reraise *length* *subscript*))

             (define/who (map proc v . vs)
               (let ([slots (copy v)])
	         (apply map! proc slots vs)
	         slots)
               (reraise copy map!))

             (define/who (map! proc v . vs)
               (let* ([vecs (cons v vs)]
	              [width (apply compare-lengths *length* fxmin vecs)])
	         (unfold! (lambda (i _)
		            (let ([xs (list-map (lambda (v) (*subscript* v i)) vecs)])
		              (values (apply proc xs) #f)))
		          v 0 width #f))
               (reraise *length* *subscript* unfold!))

             (define/who (for-each proc v . vs)
               (define (fold-with _ v . vs) (apply proc v vs))
               (apply fold fold-with #f v vs)
               (reraise fold))

             (define/who (count pred? v . vs)
               (define (fold-with tally x . xs)
                 (if (apply pred? x xs)
	             (fx+ tally 1)
	             tally))
               (apply fold fold-with 0 v vs)
               (reraise fold))

             (define/who (cumulate proc ini v)
               (define (unfold-with i prev)
                 (let ([res (proc prev (*subscript* v i))])
                   (values res res)))
               (unfold unfold-with (*length* v) ini)
               (reraise *length* *subscript* unfold))

             ;; #| searching |#
             (define/who (take-while pred? v)
               (let* ([idx (skip pred? v)]
	              [idx* (if idx idx (*length* v))])
                 (copy v 0 idx*))
               (reraise *length* skip))

             (define/who (take-while-right pred? v)
               (let* ([idx (skip-right pred? v)]
	              [idx* (if idx (fx+ idx 1) 0)])
                 (copy v idx* (*length* v)))
               (reraise skip-right copy))

             (define/who (drop-while pred? v)
               (let* ([width (*length* v)]
	              [idx (skip pred? v)]
	              [idx* (if idx idx width)])
                 (copy v idx* width))
               (reraise *length* skip copy))

             (define/who (drop-while-right pred? v)
               (let* ([idx (skip-right pred? v)]
	              [idx* (if idx idx -1)])
                 (copy v 0 (fx+ idx* 1)))
               (reraise skip-right copy))

             (define/case-who index
               [(pred? v)
                (let loop ([i 0])
	          (cond [(fx=? i (*length* v)) #f]
	                [(pred? (*subscript* v i)) i]
	                [else (loop (fx+ i 1))]))]
               [(pred? v . vs)
                (let* ([vecs (cons v vs)]
	               [width (apply fxmin (list-map *length* vecs))])
                  (let loop ([i 0])
	            (cond [(fx=? i width) #f]
	                  [(apply pred? (vectorised-subscript *subscript* vecs i)) i]
	                  [else (loop (fx+ i 1))])))]
               (reraise *length* *subscript*))

             (define/case-who index-right
               [(pred? v)
                (let ([width (*length* v)])
                  (let loop ([i (fx- width 1)])
	            (cond [(fxnegative? i) #f]
	                  [(pred? (*subscript* v i)) i]
	                  [else (loop (fx- i 1))])))]
               [(pred? v . vs)
                (let* ([vecs (cons v vs)]
	               [width (apply fxmin (list-map *length* vecs))])
                  (let loop ([i (fx- width 1)])
	            (cond [(fxnegative? i) #f]
	                  [(apply pred? (vectorised-subscript *subscript* vecs i)) i]
	                  [else (loop (fx- i 1))])))]
               (reraise *length* *subscript*))

             (define/case-who skip
               [(pred? v)
                (index (compose not pred?) v)]
               [(pred? v . vs)
                (apply index
                       (lambda vs (not (apply pred? vs)))
                       v vs)]
               (reraise index))

             (define/case-who skip-right
               [(pred? v)
                (index-right (compose not pred?) v)]
               [(pred? v . vs)
                (apply index-right
                       (lambda vs (not (apply pred? vs)))
                       v vs)]
               (reraise index-right))

             (define/case-who any
               [(pred? v)
                (or (empty? v)
	            (call/cc
	             (lambda (break)
	               (fold (lambda (_ c . cs)
		               (let ([res (pred? c)])
		                 (if res (break res) #f)))
		             #f v))))]
               [(pred? v . vs)
                (or (empty? v)
	            (call/cc
	             (lambda (break)
	               (apply fold
		              (lambda (_ c . cs)
		                (let ([res (apply pred? c cs)])
		                  (if res (break res) #f)))
		              #f v vs))))]
               (reraise empty? fold))

             (define/case-who every
               [(pred? v)
                (or (empty? v)
	            (call/cc
	             (lambda (break)
	               (fold (lambda (_ c . cs)
		               (let ([res (pred? c)])
		                 (or res (break #f))))
		             #t v))))]
               [(pred? v . vs)
                (or (empty? v)
	            (call/cc
	             (lambda (break)
	               (apply fold
		              (lambda (_ c . cs)
		                (let ([res (apply pred? c cs)])
		                  (or res (break #f))))
		              #t v vs))))]
               (reraise empty? fold))

             (define/who (partition pred? v)
               (let ([cnt (count pred? v)]
	             [slots (*make* (*length* v))])
                 (let loop ([i 0] [yes 0] [no cnt])
                   (if (fx=? i (*length* v))
	               (values slots cnt)
	               (let ([elem (*subscript* v i)])
	                 (if (pred? elem)
		             (begin (*update!* slots yes elem)
		                    (loop (fx+ i 1) (fx+ yes 1) no))
		             (begin (*update!* slots no elem)
		                    (loop (fx+ i 1) yes (fx+ no 1))))))))
               (reraise count *length*))

             (define/who (filter pred? v)
               (let* ([cnt (count pred? v)]
	              [slots (*make* cnt)])
                 (let loop ([src-index 0] [tgt-index 0])
                   (if (fx=? tgt-index cnt)
	               slots
	               (let ([elem (*subscript* v src-index)])
	                 (if (pred? elem)
		             (begin (*update!* slots tgt-index elem)
		                    (loop (fx+ src-index 1) (fx+ tgt-index 1)))
		             (loop (fx+ src-index 1) tgt-index))))))
               (reraise count *make* *length*))

             (define/who (remove pred? v)
               (filter (compose not pred?) v)
               (reraise filter))

             #| predicates |#

             (define/who (empty? v)
               (fxzero? (*length* v))
               (reraise *length*))

             (define/who (=? v . vs)
               (and (apply all-same-length? *length* v vs)
                    (apply every fx=? v vs))
               (reraise *length* every))

             (define (<? vec1 vec2)
               "Convenience procedure for elementwise ordering, not part of the SRFI 160 specification"
               (let ([len1 (*length* vec1)]
                     [len2 (*length* vec2)])
                 (cond [(fx<? len1 len2) #t]
                       [(fx>? len1 len2) #f]
                       [else
                        (let loop ([k 0])
                          (cond [(fx=? k len1) #f]
                                [(<? (*subscript* vec1 k) (*subscript* vec2 k))  #t]
                                [(>? (*subscript* vec1 k) (*subscript* vec2 k))  #f]
                                [else (loop (fx+ k 1))]))])))

	     #| conversion |#

             (define/case-who reverse-to-list
               [(v) (reverse-to-list v 0 (*length* v))]
               [(v start) (reverse-to-list v start (*length* v))]
               [(v start end)
                (assert-start-nat  who start)
                (assert-end-nat    who end)
                (assert-start<=end who start end)
                (assert-bounds     who end (*length* v) v)
	        (let loop ([i start] [acc '()])
	          (if (fx=? i end)
	              acc
	              (loop (fx+ i 1)
		            (cons (*subscript* v i) acc))))]
               (reraise *length* *subscript*))

             (define/who (reverse-from-list xs)
               (if (list? xs)
                   (let* ([width (length xs)]
	                  [slots (*make* width)])
                     (let loop ([k (fx- width 1)] [acc xs])
                       (if (fxnegative? k)
	                   slots
	                   (begin (*update!* slots k (car acc))
		                  (loop (fx- k 1) (cdr acc)))))
	             slots)
                   (assertion-violationf (quote who) "~a is not a proper list" xs))
               (reraise *length* *update!*))

             (define/case-who to-vector
               [(v)       (to-vector v 0 (*length* v))]
               [(v start) (to-vector v start (*length* v))]
               [(v start end)
                (assert-start-nat  who start)
                (assert-end-nat    who end)
                (assert-start<=end who start end)
                (let* ([source-size (*length* v)]
                       [target-size (fx- end start)]
                       [slots (make-vector target-size)])
                  (assert-bounds who end source-size v)
                  (let loop ([k start] [tgt 0])
                    (if (fx=? k end)
                        slots
                        (begin
                          (vector-set! slots tgt (*subscript* v k))
                          (loop (fx+ k 1) (fx+ tgt 1))))))]
               (reraise *length*))

             (define/case-who from-vector
               [(v)       (from-vector v 0 (vector-length v))]
               [(v start) (from-vector v start (vector-length v))]
               [(v start end)
                (assert-start-nat  who start)
                (assert-end-nat    who end)
                (assert-start<=end who start end)
                (let* ([source-size (vector-length v)]
                       [target-size (fx- end start)]
                       [slots (*make* target-size)])
                  (assert-bounds who end source-size v)
                  (let loop ([k start] [tgt 0])
                    (if (fx=? k end)
                        slots
                        (begin
                          (*subscript* slots tgt (vector-ref v k))
                          (loop (fx+ k 1) (fx+ tgt 1))))))]
               (reraise vector-length))

	     #| comparators |#
             (define/who (hash v)
               (let ([width (fxmin 256 (*length* v))])
                 (let loop ([k 0] [acc 0])
                   (if (fx=? k width)
	               (abs (floor (real-part (inexact->exact acc))))
	               (loop (fx+ k 1) (fx+ acc (*subscript* v k))))))
               (reraise *length*))

             (define comparator (make-comparator *repr?* =? <? hash))

	     #| generators |#
             (define/case-who make-generator
               [(v)       (make-generator v 0 (*length* v))]
               [(v start) (make-generator v start (*length* v))]
               [(v start end)
                (assert-start-nat who start)
                (assert-end-nat who end)
                (assert/who who
                            (fx<? start end)
                            "source end (~a) must be greater than or equal to source start (~a), both less than length"
                            end start)
                (make-coroutine-generator
                 (lambda (yield)
	           (let loop ([k start])
                     (when (fx<? k end)
                       (yield (*subscript* v k))
                       (loop (fx+ k 1))))))]
               (reraise *length*))

             #| output
                N.B. this definition isn't portable as it relies on the lexical syntax
                being printed for the record types originating in the SRFI 160 base library. |#
             (define/case-who write-port
               [(v)      (display v (current-output-port))]
               [(v port) (display v (current-output-port))]
               (reraise))

               ;;;
             )))])); define-syntax

