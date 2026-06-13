;; SPDX-FileCopyrightText: 2018 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
(define/curried ((define-numeric-vector-unfold unfold! mk) proc size ini)
  "wrap SRFI 160 `@vector-unfold' and `@vector-unfold-right' procedures"
  (let ([slots (mk size)])
    (unfold! proc slots 0 size ini)
    slots))

;; unfold-right should use the exact same skeleton

(define/curried-case (define-numeric-vector-copy copy! mk len)
  "wrap SRFI 160 `@vector-copy' and `@vector-reverse-copy' procedures"
  (recur [(vec) (recur vec 0 (len vec))]
         [(vec start) (recur vec start (len vec))]
         [(vec start end)
          (assert-start-nat who start)
          (assert-end-nat who end)
          (assert-start<=end who start end)
          (assert-bounds who end (len vec) vec)
	  (let ([slots (mk (- end start))])
	    (copy! slots 0 vec start end)
	    slots)]))

;; reverse-copy! type procedures can already be defined, so should use the same skeleton

(define/curried ((define-numeric-vector-append copy! mk len) v . vs)
  "wrap SRFI 160 `@vector-append' procedures"
  (if (and (null? vs) (len v))
      v
      (let* ([vecs (cons v vs)]
	     [lens (map len vecs)]
	     [slots (mk (apply + lens))])
	(fold-left
	 (lambda (last-extent curr-width curr-vec)
	   ;; (@vector-copy! @to at @from [start [end]])
	   (copy! slots last-extent curr-vec)
	   (+ last-extent curr-width))
	 0
	 lens vecs)
	slots)))

(define/curried ((define-numeric-vector-sub-append copy! mk len) v . vs)
  "(@vector-append-subvectors [@vec start end] ...) -> @vector [SRFI 133]
   Concatenates the result of applying @vector-copy to each triplet of @vec, start, end
   arguments, but may be implemented more efficiently."
  (if (null? vs)
      (let*-values ([(vv start end)
                     (sub-append-triple who v)]
                    [(slots)
                     (mk (- end start))])
	  (copy! slots 0 vv start end)
	  slots)
      (let*-values ([(vecs lens starts ends total-len)
		     (let loop
			 ([to-process (cons v vs)]
			  [vecs '()] [lens '()] [starts '()] [ends '()]
			  [tally 0])
		       (if (null? to-process)
			   (values vecs lens starts ends tally)
                           (let*-values ([(vv start end)
                                          (sub-append-triple (car to-process))]
                                         [(width)
                                          (- end start)])
				(loop (cdr to-process)
				      (cons vv vecs)
				      (cons width lens)
				      (cons start starts)
				      (cons end ends)
				      (+ width tally)))))]
		    [(slots)
                     (mk total-len)])
	(fold-left (lambda (l v start end last-extent)
		     (copy! slots last-extent v start end)
		     (+ last-extent l))
		   0
		   lens vecs starts ends)
	slots)))

(define/curried ((define-numeric-vector-concatenate copy! mk len) vecs)
  "wrap SRFI 160 `@vector-concatenate' procedures"
  (let ([v (mk (total-length len vecs))])
    (let loop ([vecs vecs] [at 0])
      (unless (null? vecs)
        (let ([vec (car vecs)])
          (copy! v at vec 0 (len vec))
          (loop (cdr vecs) (+ at (len vec)))))
      v)))
