;;; SPDX-FileCopyrightText: 2021 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

;;;; Utility

(define (plist-fold proc nil ps)
  (let loop ((b nil) (ps ps))
    (pmatch ps
      (() b)
      ((,k ,v . ,ps*)
       (loop (proc k v b) ps*))
      (else (error 'plist-fold "invalid plist")))))

(define (first-arg _k x _y) x)
(define (second-arg _k _x y) y)

(define (constantly x)
  (lambda (_) x))

;;;; Type

(define-record-type <fxmapping>
  (raw-fxmapping trie)
  fxmapping?
  (trie fxmapping-trie))

;;;; Constructors

(define (fxmapping . args)
  (raw-fxmapping
    (plist-fold (lambda (k v trie) (trie-adjoin trie k v))
                the-empty-trie
                args)))

(define (pair-or-null? x)
  (or (pair? x) (null? x)))

(define (alist->fxmapping/combinator comb as)
  (assume (procedure? comb))
  (assume (pair-or-null? as))
  (raw-fxmapping
    (fold (lambda (p trie)
            (assume (pair? p) "alist->fxmapping/combinator: not a pair")
            (trie-insert/combine trie (car p) (cdr p) comb))
          the-empty-trie
          as)))

(define (alist->fxmapping as)
  (alist->fxmapping/combinator second-arg as))

(define fxmapping-unfold
  (case-lambda
    ((stop? mapper successor seed)                ; fast path
     (assume (procedure? stop?))
     (assume (procedure? mapper))
     (assume (procedure? successor))
     (let lp ((trie the-empty-trie) (seed seed))
       (if (stop? seed)
           (raw-fxmapping trie)
           (let-values (((k v) (mapper seed)))
             (assume (valid-integer? k))
             (lp (trie-adjoin trie k v) (successor seed))))))
    ((stop? mapper successor . seeds)             ; variadic path
     (assume (procedure? stop?))
     (assume (procedure? mapper))
     (assume (procedure? successor))
     (assume (pair? seeds))
     (let lp ((trie the-empty-trie) (seeds seeds))
       (if (apply stop? seeds)
           (raw-fxmapping trie)
           (let-values (((k v) (apply mapper seeds))
                        (seeds* (apply successor seeds)))
             (assume (valid-integer? k))
             (lp (trie-adjoin trie k v) seeds*)))))))

(define fxmapping-accumulate
  (case-lambda
    ((proc seed)                                ; fast path
     (assume (procedure? proc))
     (call-with-current-continuation
      (lambda (k)
        (let lp ((trie the-empty-trie) (seed seed))
          (let-values (((k v seed*)
                        (proc (lambda xs (apply k (raw-fxmapping trie) xs))
                              seed)))
            (lp (trie-adjoin trie k v) seed*))))))
    ((proc . seeds)                             ; variadic path
     (assume (procedure? proc))
     (assume (pair? seeds))
     (call-with-current-continuation
      (lambda (k)
        (let lp ((trie the-empty-trie) (seeds seeds))
          (let-values (((k v . seeds*)
                        (apply proc
                               (lambda xs (apply k (raw-fxmapping trie) xs))
                               seeds)))
            (lp (trie-adjoin trie k v) seeds*))))))))

;;;; Predicates

(define (fxmapping-contains? fxmap n)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? n))
  (trie-contains? (fxmapping-trie fxmap) n))

(define (fxmapping-empty? fxmap)
  (assume (fxmapping? fxmap))
  (eqv? (fxmapping-trie fxmap) the-empty-trie))

(define (fxmapping-disjoint? fxmap1 fxmap2)
  (assume (fxmapping? fxmap1))
  (assume (fxmapping? fxmap2))
  (trie-disjoint? (fxmapping-trie fxmap1) (fxmapping-trie fxmap2)))

;;;; Accessors

(define fxmapping-ref
  (case-lambda
    ((fxmap key)
     (fxmapping-ref fxmap
                    key
                    (lambda () (error 'fxmapping-ref
                                      "key not found"
                                      key
                                      fxmap))
                    values))
    ((fxmap key failure)
     (fxmapping-ref fxmap key failure values))
    ((fxmap key failure success)
     (assume (fxmapping? fxmap))
     (assume (valid-integer? key))
     (assume (procedure? failure))
     (assume (procedure? success))
     (trie-assoc (fxmapping-trie fxmap) key failure success))))

(define (fxmapping-ref/default fxmap key default)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? key))
  (trie-assoc/default (fxmapping-trie fxmap) key default))

(define (fxmapping-min fxmap)
  (assume (not (fxmapping-empty? fxmap)))
  (trie-min (fxmapping-trie fxmap)))

(define (fxmapping-max fxmap)
  (assume (not (fxmapping-empty? fxmap)))
  (trie-max (fxmapping-trie fxmap)))

;;;; Updaters

(define fxmapping-adjoin/combinator
  (case-lambda
    ((fxmap combine key value)      ; one-assoc fast path
     (raw-fxmapping
      (trie-insert/combine (fxmapping-trie fxmap) key value combine)))
    ((fxmap combine . ps)
     (raw-fxmapping
      (plist-fold (lambda (k v t)
                    (trie-insert/combine t k v combine))
                  (fxmapping-trie fxmap)
                  ps)))))

;; Preserve existing associations for keys.
(define fxmapping-adjoin
  (case-lambda
    ((fxmap key value)              ; one-assoc fast path
     (raw-fxmapping
      (trie-adjoin (fxmapping-trie fxmap) key value)))
    ((fxmap . ps)
     (raw-fxmapping
      (plist-fold (lambda (k v t) (trie-adjoin t k v))
                  (fxmapping-trie fxmap)
                  ps)))))

;; Replace existing associations for keys.
(define fxmapping-set
  (case-lambda
    ((fxmap key value)      ; one-assoc fast path
     (raw-fxmapping
      (trie-insert (fxmapping-trie fxmap) key value)))
    ((fxmap . ps)
     (raw-fxmapping
      (plist-fold (lambda (k v t) (trie-insert t k v))
                  (fxmapping-trie fxmap)
                  ps)))))

(define (fxmapping-adjust fxmap key proc)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? key))
  (assume (procedure? proc))
  (raw-fxmapping (trie-adjust (fxmapping-trie fxmap) key proc)))

(define fxmapping-delete
  (case-lambda
    ((fxmap key)      ; fast path
     (assume (fxmapping? fxmap))
     (assume (valid-integer? key))
     (raw-fxmapping (trie-delete (fxmapping-trie fxmap) key)))
    ((fxmap . keys)
     (fxmapping-delete-all fxmap keys))))

(define (fxmapping-delete-all fxmap keys)
  (assume (or (pair? keys) (null? keys)))
  (let ((key-map (fxmapping-trie
                  (fxmapping-unfold null?
                                    (lambda (ks) (values (car ks) #t))
                                    cdr
                                    keys))))
    (fxmapping-remove (lambda (k _) (trie-contains? key-map k))
                      fxmap)))

(define fxmapping-update
  (case-lambda
    ((fxmap key success)
     (fxmapping-update fxmap
                       key
                       success
                       (lambda ()
                         (error 'fxmapping-update "key not found" key fxmap))))
    ((fxmap key success failure)
     (assume (fxmapping? fxmap))
     (assume (valid-integer? key))
     (assume (procedure? success))
     (assume (procedure? failure))
     (trie-update (fxmapping-trie fxmap) key success failure raw-fxmapping))))

(define (fxmapping-alter fxmap key failure success)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? key))
  (assume (procedure? failure))
  (assume (procedure? success))
  (trie-alter (fxmapping-trie fxmap) key failure success raw-fxmapping))

(define (fxmapping-delete-min fxmap)
  (fxmapping-update-min fxmap
                        (lambda (_k _v _rep delete)
                          (delete))))

(define (fxmapping-update-min fxmap success)
  (assume (fxmapping? fxmap))
  (assume (not (fxmapping-empty? fxmap)))
  (assume (procedure? success))
  (trie-update-min (fxmapping-trie fxmap) success raw-fxmapping))

(define (fxmapping-pop-min fxmap)
  (assume (fxmapping? fxmap))
  (assume (not (fxmapping-empty? fxmap)))
  (let-values (((k v trie) (trie-pop-min (fxmapping-trie fxmap))))
    (values k v (raw-fxmapping trie))))

(define (fxmapping-delete-max fxmap)
  (fxmapping-update-max fxmap
                        (lambda (_k _v _rep delete)
                          (delete))))

(define (fxmapping-update-max fxmap success)
  (assume (fxmapping? fxmap))
  (assume (not (fxmapping-empty? fxmap)))
  (assume (procedure? success))
  (trie-update-max (fxmapping-trie fxmap) success raw-fxmapping))

(define (fxmapping-pop-max fxmap)
  (assume (fxmapping? fxmap))
  (assume (not (fxmapping-empty? fxmap)))
  (let-values (((k v trie) (trie-pop-max (fxmapping-trie fxmap))))
    (values k v (raw-fxmapping trie))))

;;;; The whole fxmapping

(define (fxmapping-size fxmap)
  (assume (fxmapping? fxmap))
  (trie-size (fxmapping-trie fxmap)))

(define fxmapping-find
  (case-lambda
    ((pred fxmap failure)
     (fxmapping-find pred fxmap failure values))
    ((pred fxmap failure success)
     (assume (procedure? pred))
     (assume (fxmapping? fxmap))
     (assume (procedure? failure))
     (assume (procedure? success))
     (trie-find pred (fxmapping-trie fxmap) failure success))))

(define (fxmapping-count pred fxmap)
  (assume (procedure? pred))
  (fxmapping-fold (lambda (k v acc)
                    (if (pred k v) (+ 1 acc) acc))
                  0
                  fxmap))

(define (fxmapping-any? pred fxmap)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (fxmapping-fold (lambda (k v _)
                       (and (pred k v) (return #t)))
                     #f
                     fxmap))))

(define (fxmapping-every? pred fxmap)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (fxmapping-fold (lambda (k v _)
                       (or (pred k v) (return #f)))
                     #t
                     fxmap))))

;;;; Mapping and folding

;; Map proc over the assocs. of fxmap, inserting result values under
;; the same key.
;; This is *not* the same as SRFI 146's mapping-map.
(define (fxmapping-map proc fxmap)
  (assume (procedure? proc))
  (assume (fxmapping? fxmap))
  (raw-fxmapping (trie-map proc (fxmapping-trie fxmap))))

(define (unspecified)
  (if #f #f))

(define (fxmapping-for-each proc fxmap)
  (assume (procedure? proc))
  (fxmapping-fold (lambda (k v _)
                    (proc k v)
                    (unspecified))
                  (unspecified)
                  fxmap))

(define (fxmapping-fold proc nil fxmap)
  (assume (procedure? proc))
  (assume (fxmapping? fxmap))
  (let ((trie (fxmapping-trie fxmap)))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (negative? m))
       (trie-fold-left proc (trie-fold-left proc nil r) l))
      ((branch ? ? ,l ,r)
       (trie-fold-left proc (trie-fold-left proc nil l) r))
      (else (trie-fold-left proc nil trie)))))

(define (fxmapping-fold-right proc nil fxmap)
  (assume (procedure? proc))
  (assume (fxmapping? fxmap))
  (let ((trie (fxmapping-trie fxmap)))
    (tmatch trie
      ((branch ? ,m ,l ,r) (guard (negative? m))
       (trie-fold-right proc (trie-fold-right proc nil l) r))
      ((branch ? ? ,l ,r)
       (trie-fold-right proc (trie-fold-right proc nil r) l))
      (else (trie-fold-right proc nil trie)))))

(define (fxmapping-map->list proc fxmap)
  (assume (procedure? proc))
  (fxmapping-fold-right (lambda (k v us)
                          (cons (proc k v) us))
                        '()
                        fxmap))

(define (fxmapping-filter pred fxmap)
  (assume (procedure? pred))
  (assume (fxmapping? fxmap))
  (raw-fxmapping (trie-filter pred (fxmapping-trie fxmap))))

(define (fxmapping-remove pred fxmap)
  (fxmapping-filter (lambda (k v) (not (pred k v))) fxmap))

(define (fxmapping-partition pred fxmap)
  (assume (procedure? pred))
  (assume (fxmapping? fxmap))
  (let-values (((tin tout)
                (trie-partition pred (fxmapping-trie fxmap))))
    (values (raw-fxmapping tin) (raw-fxmapping tout))))

;;;; Conversion

(define (fxmapping->alist fxmap)
  (fxmapping-fold-right (lambda (k v as) (cons (cons k v) as))
                        '()
                        fxmap))

(define (fxmapping->decreasing-alist fxmap)
  (fxmapping-fold (lambda (k v as) (cons (cons k v) as))
                  '()
                  fxmap))

(define (fxmapping-keys fxmap)
  (fxmapping-fold-right (lambda (k _ ks) (cons k ks)) '() fxmap))

(define (fxmapping-values fxmap)
  (fxmapping-fold-right (lambda (_ v vs) (cons v vs)) '() fxmap))

(define (fxmapping->generator fxmap)
  (assume (fxmapping? fxmap))
  (make-coroutine-generator
   (lambda (yield)
     (fxmapping-fold (lambda (k v _) (yield (cons k v)))
                     #f
                     fxmap))))

(define (fxmapping->decreasing-generator fxmap)
  (assume (fxmapping? fxmap))
  (make-coroutine-generator
   (lambda (yield)
     (fxmapping-fold-right (lambda (k v _) (yield (cons k v)))
                           #f
                           fxmap))))

;;;; Comparison

(define (fxmapping=? comp fxmap1 fxmap2 . imaps)
  (assume (comparator? comp))
  (assume (fxmapping? fxmap1))
  (let ((fxmap-eq1 (lambda (fxmap)
                     (assume (fxmapping? fxmap))
                     (or (eqv? fxmap1 fxmap)
                         (trie=? comp
                                 (fxmapping-trie fxmap1)
                                 (fxmapping-trie fxmap))))))
    (and (fxmap-eq1 fxmap2)
         (or (null? imaps)
             (every fxmap-eq1 imaps)))))

(define (fxmapping<? comp fxmap1 fxmap2 . imaps)
  (assume (comparator? comp))
  (assume (fxmapping? fxmap1))
  (assume (fxmapping? fxmap2))
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (trie-proper-subset? comp t1 t2)
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

(define (fxmapping>? comp fxmap1 fxmap2 . imaps)
  (assume (comparator? comp))
  (assume (fxmapping? fxmap1))
  (assume (fxmapping? fxmap2))
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (trie-proper-subset? comp t2 t1)
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

(define (fxmapping<=? comp fxmap1 fxmap2 . imaps)
  (assume (comparator? comp))
  (assume (fxmapping? fxmap1))
  (assume (fxmapping? fxmap2))
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (memv (trie-subset-compare comp t1 t2) '(less equal))
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

(define (fxmapping>=? comp fxmap1 fxmap2 . imaps)
  (assume (comparator? comp))
  (assume (fxmapping? fxmap1))
  (assume (fxmapping? fxmap2))
  (let lp ((t1 (fxmapping-trie fxmap1))
           (t2 (fxmapping-trie fxmap2))
           (imaps imaps))
    (and (memv (trie-subset-compare comp t2 t1) '(less equal))
         (pmatch imaps
           (() #t)
           ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))

;;;; Set theory operations

(define (fxmapping-union . args)
  (apply fxmapping-union/combinator first-arg args))

(define (fxmapping-intersection . args)
  (apply fxmapping-intersection/combinator first-arg args))

(define fxmapping-difference
  (case-lambda
    ((fxmap1 fxmap2)
     (assume (fxmapping? fxmap1))
     (assume (fxmapping? fxmap2))
     (raw-fxmapping
      (trie-difference (fxmapping-trie fxmap1)
                       (fxmapping-trie fxmap2))))
    ((fxmap . rest)
     (assume (fxmapping? fxmap))
     (assume (pair? rest))
     (raw-fxmapping
      (trie-difference (fxmapping-trie fxmap)
                       (fxmapping-trie
                        (apply fxmapping-union rest)))))))

(define (fxmapping-xor fxmap1 fxmap2)
  (assume (fxmapping? fxmap1))
  (assume (fxmapping? fxmap2))
  (raw-fxmapping
   (trie-xor (fxmapping-trie fxmap1) (fxmapping-trie fxmap2))))

(define (fxmapping-union/combinator proc fxmap . rest)
  (assume (procedure? proc))
  (assume (fxmapping? fxmap))
  (assume (pair? rest))
  (raw-fxmapping
   (fold (lambda (im t)
           (assume (fxmapping? im))
           (trie-merge proc t (fxmapping-trie im)))
         (fxmapping-trie fxmap)
         rest)))

(define (fxmapping-intersection/combinator proc fxmap . rest)
  (assume (procedure? proc))
  (assume (fxmapping? fxmap))
  (assume (pair? rest))
  (raw-fxmapping
   (fold (lambda (im t)
           (assume (fxmapping? im))
           (trie-intersection proc t (fxmapping-trie im)))
         (fxmapping-trie fxmap)
         rest)))

;;;; Subsets

(define (fxsubmapping= fxmap key)
  (fxmapping-ref fxmap
                 key
                 fxmapping
                 (lambda (v) (fxmapping key v))))

(define (fxmapping-open-interval fxmap low high)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #f #f)))

(define (fxmapping-closed-interval fxmap low high)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #t #t)))

(define (fxmapping-open-closed-interval fxmap low high)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #f #t)))

(define (fxmapping-closed-open-interval fxmap low high)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (fx>=? high low))
  (raw-fxmapping
   (subtrie-interval (fxmapping-trie fxmap) low high #t #f)))

(define (fxsubmapping< fxmap key)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? key))
  (raw-fxmapping (subtrie< (fxmapping-trie fxmap) key #f)))

(define (fxsubmapping<= fxmap key)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? key))
  (raw-fxmapping (subtrie< (fxmapping-trie fxmap) key #t)))

(define (fxsubmapping> fxmap key)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? key))
  (raw-fxmapping (subtrie> (fxmapping-trie fxmap) key #f)))

(define (fxsubmapping>= fxmap key)
  (assume (fxmapping? fxmap))
  (assume (valid-integer? key))
  (raw-fxmapping (subtrie> (fxmapping-trie fxmap) key #t)))

(define (fxmapping-split fxmap k)
  (assume (fxmapping? fxmap))
  (assume (integer? k))
  (let-values (((trie-low trie-high)
                (trie-split (fxmapping-trie fxmap) k)))
    (values (raw-fxmapping trie-low) (raw-fxmapping trie-high))))

;;;; fxmappings as relations

(define (fxmapping-relation-map proc fxmap)
  (assume (procedure? proc))
  (assume (fxmapping? fxmap))
  (raw-fxmapping (trie-relation-map proc (fxmapping-trie fxmap))))
