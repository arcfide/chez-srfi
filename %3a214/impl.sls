#!r6rs

(library (srfi :214 impl)
  (export
   ;; Constructors
   make-flexvector flexvector
   flexvector-unfold flexvector-unfold-right
   flexvector-copy flexvector-reverse-copy
   flexvector-append flexvector-concatenate flexvector-append-subvectors

   ;; Predicates
   flexvector? flexvector-empty? flexvector=?

   ;; Selectors
   flexvector-ref flexvector-front flexvector-back flexvector-length

   ;; Mutators
   flexvector-add! flexvector-add-front! flexvector-add-back!
   flexvector-remove! flexvector-remove-front! flexvector-remove-back!
   flexvector-add-all! flexvector-remove-range! flexvector-clear!
   flexvector-set! flexvector-swap!
   flexvector-fill! flexvector-reverse!
   flexvector-copy! flexvector-reverse-copy!
   flexvector-append!

   ;; Iteration
   flexvector-fold flexvector-fold-right
   flexvector-map flexvector-map! flexvector-map/index flexvector-map/index!
   flexvector-append-map flexvector-append-map/index
   flexvector-filter flexvector-filter! flexvector-filter/index flexvector-filter/index!
   flexvector-for-each flexvector-for-each/index
   flexvector-count flexvector-cumulate

   ;; Searching
   flexvector-index flexvector-index-right
   flexvector-skip flexvector-skip-right
   flexvector-binary-search
   flexvector-any flexvector-every flexvector-partition

   ;; Conversion
   flexvector->vector flexvector->list flexvector->string
   vector->flexvector list->flexvector string->flexvector
   reverse-flexvector->list reverse-list->flexvector
   generator->flexvector flexvector->generator)
  (import (except (rnrs) vector-fill!)
          (srfi :26)
          (srfi :214 parameters)
          (except (srfi :133 vectors) vector->list list->vector)
          (srfi :158))

  ;;; Utilities

  ;; Checks if number is non-negative
  (define (nonnegative? num)
    (and (number? num) (>= num 0)))

  ;; Returns negated predicate.
  (define (negate pred?)
    (lambda args
      (not (apply pred? args))))

  ;; Asserts that index is valid for the given flexvector.
  (define (assert-index-validity fv . ids)
    (let ([len (flexvector-len fv)])
      (for-each (lambda (i)
                  (assert (and (>= i 0) (< i len))))
                ids)))

  ;; Collects groups of `num` length in lst into a new list.
  (define (group lst num)
    (define (split l n)
      (let loop ([acc (list)]
                 [ls l]
                 [count 0])
        (cond [(>= count n) (values (reverse acc) ls)]
              [(null? ls) (error group "Not enough elements for a group" lst num)]
              [else (loop (cons (car ls) acc)
                          (cdr ls)
                          (+ count 1))])))
    (assert (positive? num))
    (let loop ([acc (list)]
               [source lst])
      (cond [(null? source) (reverse acc)]
            [else (let-values ([(grp rest) (split source num)])
                    (loop (cons grp acc) rest))])))

  ;; Flexvector is represented as backing storage and number of used slots within.
  (define-record-type (%flexvector %make-flexvector flexvector?)
    (nongenerative flexvector-vtlmh5fxbj)
    (fields (mutable vec flexvector-vec flexvector-vec-set!)
            (mutable len flexvector-len flexvector-len-set!)))

  ;; Convenience wrapper to deconstruct flexvectors into parts.
  (define-syntax with-flexvectors
    (syntax-rules ()
      [(_ ([(vec len) fv] rest ...) body ...)
       (let* ([fv* fv]
              [vec (flexvector-vec fv*)]
              [len (flexvector-len fv*)])
         (with-flexvectors (rest ...) body ...))]
      [(_ () body ...)
       (let ()
         body ...)]))

  ;; Calls body with start and end clamped to dimensions of the flexvector.
  (define-syntax with-clamped-range
    (syntax-rules ()
      [(_ (fv start end) body ...)
       (let ([start (max 0 start)]
             [end (min end (flexvector-len fv))])
         (assert (<= start end))
         (let ()
           body ...))]))

  ;; Convenience definition facility for procedures that accept [start, [end]] optional arguments.
  (define-syntax define-with-range
    (syntax-rules (of)
      [(_ (name fv ... start (end of what)) body ...)
       (define name
         (case-lambda
           [(fv ...)
            (name fv ... 0)]
           [(fv ... start)
            (name fv ... start (flexvector-len what))]
           [(fv ... start end)
            (with-clamped-range (what start end)
              body ...)]))]))

  ;; Returns shortest length of given vectors.
  (define (shortest-length vecs)
    (apply min (map flexvector-len vecs)))

  ;; Shortcut macro for over-vectors iterations.
  (define-syntax with-shortest-length
    (syntax-rules ()
      [(_ [(vecs shortest-len) vecs-expr] body ...)
       (let* ([vecs vecs-expr]
              [shortest-len (shortest-length vecs)])
         body ...)]))

  ;; Returns flexvector capacity.
  (define (flexvector-cap fv)
    (vector-length (flexvector-vec fv)))

  ;; Makes sure that the flexvector has enough capacity
  (define (ensure-capacity! fv required-cap)
    (let ([current-cap (flexvector-cap fv)])
      (unless (>= current-cap required-cap)
        (let* ([vec (flexvector-vec fv)]
               [new-cap ((flexvector-capacity-estimator) current-cap required-cap)];  (exact (expt 2 (+ (floor (log required-cap 2)) 1)))]
               [new-vec (make-vector new-cap)])
          (vector-copy! new-vec 0 vec 0 (vector-length vec))
          (flexvector-vec-set! fv new-vec)))))

  ;; Makes sure capacity requested for flexvector creation is no less than
  ;; allowed minimal one.
  (define (ensure-valid-capacity requested-size)
    (assert (and (exact? requested-size) (>= requested-size 0)))
    (max requested-size (flexvector-min-capacity)))

  ;; Constructs flexvector with given size and fill.
  (define make-flexvector
    (case-lambda
      [(size)
       (assert (nonnegative? size))
       (%make-flexvector (make-vector (ensure-valid-capacity size)) size)]
      [(size fill)
       (assert (nonnegative? size))
       (%make-flexvector (make-vector (ensure-valid-capacity size) fill) size)]))

  ;; Constructs flexvector from given elements.
  (define (flexvector . els)
    (cond [(null? els) (make-flexvector 0)]
          [else (list->flexvector els)]))

  ;; Generates flexvector by applying func to seeds generated by gen until pred returns true.
  (define (flexvector-unfold pred func gen . seeds)
    (let ([fv (flexvector)])
      (do [(seeds seeds (let-values [(seeds (apply gen seeds))]
                          seeds))]
          [(apply pred seeds) fv]
        (flexvector-add-back! fv (apply func seeds)))))

  ;; Generates flexvector like flexvector-unfold does but fill the resulting vector starting
  ;; from the right end.
  (define (flexvector-unfold-right pred func gen . seeds)
    (let ([fv (apply flexvector-unfold pred func gen seeds)])
      (flexvector-reverse! fv)
      fv))

  ;; Makes a copy of the flexvector.
  (define-with-range (flexvector-copy fv start (end of fv))
    (with-flexvectors ([(vec len) fv])
      (let ([result (make-flexvector (- end start))])
        (vector-copy! (flexvector-vec result) 0 vec start end)
        result)))

  ;; Makes a reverse copy of the flexvector.
  (define (flexvector-reverse-copy . args)
    (let ([copy (apply flexvector-copy args)])
      (flexvector-reverse! copy)
      copy))

  ;; Appends flexvectors together.
  (define (flexvector-append fv . rest)
    (flexvector-concatenate (cons fv rest)))

  ;; Concatenates given list of flexvectors together.
  (define (flexvector-concatenate fvs)
    (let* ([total-len (fold-left + 0 (map flexvector-len fvs))]
           [result (make-flexvector total-len)])
      (let loop ([vecs fvs]
                 [offset 0])
        (cond [(null? vecs) result]
              [else
               (let ([current (car vecs)])
                 (flexvector-copy! result offset current)
                 (loop (cdr vecs) (+ offset (flexvector-len current))))]))))

  ;; Appends sequence of subvectors specified by vector + offset + count.
  (define (flexvector-append-subvectors . args)
    (let* ([triplets (group args 3)]
           [subvecs (map (lambda (triplet)
                           (apply flexvector-copy triplet))
                         triplets)])
      (flexvector-concatenate subvecs)))

  ;; Is the flexvector empty?
  (define (flexvector-empty? fv)
    (zero? (flexvector-len fv)))

  ;; Compares flexvectors. They should have same lengths and their
  ;; elements should be equal according to elt=?.
  (define (flexvector=? elt=? . fvs)
    (define (combine result . es)
      (and result (apply elt=? es)))
    (let ([lens (map flexvector-len fvs)])
      (or (null? fvs) (null? (cdr fvs))
          (and (apply = lens)
               (apply flexvector-fold combine #t fvs)))))

  ;; References element at given index in the flexvector.
  (define (flexvector-ref fv index)
    (with-flexvectors ([(vec len) fv])
      (assert-index-validity fv index)
      (vector-ref vec index)))

  ;; References the first element of an (assumed) non empty flexvector.
  (define (flexvector-front fv)
    (flexvector-ref fv 0))

  ;; References the last element of an (assumed) non empty flexvector.
  (define (flexvector-back fv)
    (let ([len (flexvector-len fv)])
      (flexvector-ref fv (- len 1))))

  ;; Returns the length of the flexvector.
  (define flexvector-length flexvector-len)

  ;; Adds elements els at position i shifting existing elements to the right.
  (define (flexvector-add! fv i . els)
    (flexvector-add-all! fv i els))

  ;; Adds elements els at the beginning of the flexvector.
  (define (flexvector-add-front! fv . els)
    (flexvector-add-all! fv 0 els))

  ;; Adds elements els at the end of the flexvector.
  (define (flexvector-add-back! fv . els)
    (flexvector-add-all! fv (flexvector-len fv) els))

  ;; Adds elements to the flexvector starting at index i.
  (define (flexvector-add-all! fv i els)
    (let* ([count (length els)]
           [len (flexvector-len fv)]
           [total-len (+ len count)])
      (ensure-capacity! fv total-len)
      (let ([vec (flexvector-vec fv)])
        (vector-copy! vec (+ i count) vec i len)
        (let loop ([ls els]
                   [offset 0])
          (cond [(null? ls)
                 (flexvector-len-set! fv total-len)
                 fv]
                [else (let ([el (car ls)])
                        (vector-set! vec (+ i offset) el)
                        (loop (cdr ls) (+ offset 1)))])))))

  ;; Appends flexvector to the first one.
  (define (flexvector-append! fv . fvs)
    (let* ([len (flexvector-len fv)]
           [total-len (fold-left + len (map flexvector-len fvs))])
      (ensure-capacity! fv total-len)
      (let ([vec (flexvector-vec fv)])
        (let loop ([fvs fvs]
                   [offset len])
          (cond [(null? fvs)
                 (flexvector-len-set! fv total-len)
                 fv]
                [else (with-flexvectors ([(current-vec current-len) (car fvs)])
                        (vector-copy! vec offset current-vec 0 current-len)
                        (loop (cdr fvs) (+ offset current-len)))])))))

  ;; Removes element at position i shifting remaining elements accordingly.
  (define (flexvector-remove! fv i)
    (let ([value (flexvector-ref fv i)])
      (flexvector-remove-range! fv i (+ i 1))
      value))

  ;; Removes the first element from the flexvector.
  (define (flexvector-remove-front! fv)
    (flexvector-remove! fv 0))

  ;; Removes element from the back of flexvector.
  (define (flexvector-remove-back! fv)
    (flexvector-remove! fv (- (flexvector-len fv) 1)))

  ;; Removes a range of elements from the vector.
  (define flexvector-remove-range!
    (case-lambda
      [(fv start)
       (flexvector-remove-range! fv start (flexvector-len fv))]
      [(fv start end)
       (with-flexvectors ([(vec len) fv])
         (let* ([start (max start 0)]
                [end (min len end)]
                [move-count (- len end)]
                [len-delta (- end start)])
           (assert (<= start end))
           (unless (zero? move-count)
             (vector-copy! vec start vec end (+ end move-count)))
           (flexvector-len-set! fv (- len len-delta))
           fv))]))

  ;; Clears the flexvector.
  (define (flexvector-clear! fv)
    (flexvector-len-set! fv 0)
    fv)

  ;; Sets the value at index i to el.
  (define (flexvector-set! fv i el)
    (assert-index-validity fv i)
    (with-flexvectors ([(vec len) fv])
      (let* ([val (vector-ref vec i)])
        (vector-set! vec i el)
        val)))

  ;; Swaps elements of flexvector.
  (define (flexvector-swap! fv i j)
    (assert-index-validity fv i j)
    (with-flexvectors ([(vec len) fv])
      (vector-swap! vec i j)))

  ;; Fills elements between start and end of the flexvector with given fill value.
  (define-with-range (flexvector-fill! fv fill start (end of fv))
    (with-flexvectors ([(vec len) fv])
      (let ([start (max 0 start)]
            [end (min end len)])
        (vector-fill! vec fill start end)
        fv)))

  ;; Reverses the flexvector in place.
  (define-with-range (flexvector-reverse! fv start (end of fv))
    (let* ([len (- end start)]
           [vec (flexvector-vec fv)]
           [half (floor (/ len 2))])
      (do [(index 0 (+ index 1))]
          [(>= index half) fv]
        (let ([jndex (- len index 1)])
          (vector-swap! vec (+ start index) (+ start jndex))))))

  ;; Copies section of flexvector `from` defined by `[start, end]` to flexvector `to` starting at index `at`.
  (define-with-range (flexvector-copy! to at from start (end of from))
    (let* ([len (flexvector-len to)]
           [delta (- end start)]
           [new-len (+ len delta)])
      (assert (and (>= at 0) (<= at len)))
      (ensure-capacity! to new-len)
      (with-flexvectors ([(to-vec to-len) to]
                         [(from-vec from-len) from])
        (vector-copy! to-vec at from-vec start end)
        (flexvector-len-set! to (max len (+ at delta)))
        to)))

  ;; Copies section of flexvector `from` defined by `[start, end]` in reverse order to flexvector `to` starting at index `at`.
  (define-with-range (flexvector-reverse-copy! to at from start (end of from))
    (flexvector-copy! to at from start end)
    (flexvector-reverse! to at (+ at (- end start))))

  ;; Implementation of fold to reuse in left and right folds.
  (define (flexvector-fold-impl index-proc reductor initial fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let loop ([index 0]
                 [state initial])
        (cond [(>= index shortest-len) state]
              [else (loop (+ index 1)
                          (apply reductor state (map (cut flexvector-ref <> (index-proc index shortest-len)) vecs)))]))))

  ;; Folds flexvectors over initial value with reductor
  (define (flexvector-fold reductor initial fv . rest)
    (define (left-to-right index len)
      index)
    (apply flexvector-fold-impl left-to-right reductor initial fv rest))

  ;; Folds flexvectors over initial value with reductor from right to left.
  (define (flexvector-fold-right reductor initial fv . rest)
    (define (right-to-left index len)
      (- len index 1))
    (apply flexvector-fold-impl right-to-left reductor initial fv rest))

  ;; Helper map building procedure.
  (define (flexvector-map/index-into! dst proc fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let loop ([index 0])
        (cond [(>= index shortest-len) dst]
              [else (let ([value (apply proc index (map (cut flexvector-ref <> index) vecs))])
                      (flexvector-set! dst index value)
                      (loop (+ index 1)))]))))

  ;; Maps results of application of proc to index and elements of fv + rest back to fv.
  (define (flexvector-map/index! proc fv . rest)
    (apply flexvector-map/index-into! fv proc fv rest))

  ;; Maps results of application of proc to elements of fv + rest back to fv.
  (define (flexvector-map! proc fv . rest)
    (define (ignore-index . args)
      (apply proc (cdr args)))
    (apply flexvector-map/index! ignore-index fv rest))

  ;; Constructs a new flexvector from applications of proc to indices and elements of fv and rest.
  (define (flexvector-map/index proc fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let ([result (make-flexvector shortest-len)])
        (apply flexvector-map/index-into! result proc fv rest))))

  ;; Maps results of application of proc to elements of fv + rest into fresh flexvector.
  (define (flexvector-map proc fv . rest)
    (define (ignore-index . args)
      (apply proc (cdr args)))
    (apply flexvector-map/index ignore-index fv rest))

  ;; Appends results (which should be flexvectors) of application of proc to flexvectors (using index).
  (define (flexvector-append-map/index proc fv . rest)
    (flexvector-fold flexvector-append!
                     (flexvector)
                     (apply flexvector-map/index proc fv rest)))

  ;; Appends results (which should be flexvectors) of application of proc to flexvectors.
  (define (flexvector-append-map proc fv . rest)
    (flexvector-fold flexvector-append!
                     (flexvector)
                     (apply flexvector-map proc fv rest)))

  ;; Destructively updates fv to retain only those elements for which pred? returns true.
  ;; pred? is given an index as first argument.
  (define (flexvector-filter/index! pred? fv)
    (with-flexvectors ([(vec len) fv])
      (let loop ([check-index 0]
                 [fill-index 0])
        (cond [(>= check-index len)
               (flexvector-len-set! fv fill-index)
               fv]
              [(pred? check-index (flexvector-ref fv check-index))
               (flexvector-set! fv fill-index (flexvector-ref fv check-index))
               (loop (+ check-index 1) (+ fill-index 1))]
              [else (loop (+ check-index 1) fill-index)]))))

  ;; Destructively updates fv to retain only those elements for which pred? returns true.
  (define (flexvector-filter! pred? fv)
    (define (ignore-index index value)
      (pred? value))
    (flexvector-filter/index! ignore-index fv))

  ;; Create a new vector that has only those elements for which pred? returns true.
  ;; pred? is given an index as first argument.
  (define (flexvector-filter/index pred? fv)
    (flexvector-filter/index! pred? (flexvector-copy fv)))

  ;; Create a new vector that has only those elements for which pred? returns true.
  (define (flexvector-filter pred? fv)
    (flexvector-filter! pred? (flexvector-copy fv)))

  ;; Applies procedure proc to each element of vectors (stopping at shortest one).
  ;; proc is given an index as first argument.
  (define (flexvector-for-each/index proc fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (do [(index 0 (+ index 1))]
          [(>= index shortest-len)]
        (apply proc index (map (cut flexvector-ref <> index) vecs)))))

  ;; Applies procedure proc to each element of vectors (stopping at shortest one).
  (define (flexvector-for-each proc fv . rest)
    (define (ignore-index . args)
      (apply proc (cdr args)))
    (apply flexvector-for-each/index ignore-index fv rest))

  ;; Counts elements for which pred? returns true.
  (define (flexvector-count pred? fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let loop ([index 0]
                 [count 0])
        (cond [(>= index shortest-len) count]
              [(apply pred? (map (cut flexvector-ref <> index) vecs))
               (loop (+ index 1) (+ count 1))]
              [else (loop (+ index 1) count)]))))

  ;; Constructs new vector using following rule - result[i] = (reductor result[i-1] fv[i])
  ;; where result[-1] = initial;
  (define (flexvector-cumulate reductor initial fv)
    (with-flexvectors ([(vec len) fv])
      (let* ([result (make-flexvector len)]
             [new-vec (flexvector-vec result)])
        (let loop ([index 0]
                   [state initial])
          (cond [(>= index len) result]
                [else (let ([new-state (reductor state (vector-ref vec index))])
                        (vector-set! new-vec index new-state)
                        (loop (+ index 1) new-state))])))))

  ;; Finds first index that matches predicate.
  (define (flexvector-index pred? fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let loop ([index 0])
        (cond [(>= index shortest-len) #f]
              [(apply pred? (map (cut flexvector-ref <> index) vecs)) index]
              [else (loop (+ index 1))]))))

  ;; Finds last index that matches predicate.
  (define (flexvector-index-right pred? fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let loop ([index 0])
        (cond [(>= index shortest-len) #f]
              [(apply pred? (map (cut flexvector-ref <> (- shortest-len index 1)) vecs)) (- shortest-len index 1)]
              [else (loop (+ index 1))]))))

  ;; Skips elements matching predicate returning first index of non-matching element.
  (define (flexvector-skip pred? fv . rest)
    (apply flexvector-index (negate pred?) fv rest))

  ;; Skips elements matching predicate returning first index of non-matching element moving from right to left.
  (define (flexvector-skip-right pred? fv . rest)
    (apply flexvector-index-right (negate pred?) fv rest))

  ;; Performs binary search of a value using given direction computation function.
  (define-with-range (flexvector-binary-search fv value comp start (end of fv))
    (let ([vec (flexvector-vec fv)])
      (vector-binary-search vec value comp start end)))

  ;; Searches first element for which pred? returns generalized true value and returns that value. If no such value found returns #f.
  (define (flexvector-any pred? fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let loop ([index 0])
        (cond [(>= index shortest-len) #f]
              [(apply pred? (map (cut flexvector-ref <> index) vecs))]
              [else (loop (+ index 1))]))))

  ;; Checks if all elements match given predicate and returns last result from it if they do. Otherwise returns #f.
  (define (flexvector-every pred? fv . rest)
    (with-shortest-length [(vecs shortest-len) (cons fv rest)]
      (let loop ([index 0]
                 [last-value #t])
        (cond [(>= index shortest-len) last-value]
              [(apply pred? (map (cut flexvector-ref <> index) vecs)) => (lambda (val)
                                                                       (loop (+ index 1) val))]
              [else #f]))))

  ;; Partitions the flexvector into flexvectors of elements that match predicate and those that do not match.
  (define (flexvector-partition pred? fv)
    (let ([matching (flexvector)]
          [non-matching (flexvector)])
      (flexvector-for-each (lambda (el)
                             (flexvector-add-back! (if (pred? el) matching non-matching) el))
                           fv)
      (values matching non-matching)))

  ;;; Conversion

  ;; Creates a vector from given flexvector.
  (define-with-range (flexvector->vector fv start (end of fv))
    (let ([vec (flexvector-vec fv)])
      (vector-copy vec start end)))

  ;; Creates a flexvector from a given vector.
  (define vector->flexvector
    (case-lambda
      [(vec)
       (vector->flexvector vec 0)]
      [(vec start)
       (vector->flexvector vec start (vector-length vec))]
      [(vec start end)
       (let* ([len (- end start)]
              [result (make-flexvector len)]
              [fvec (flexvector-vec result)])
         (vector-copy! fvec 0 vec start end)
         result)]))

  ;; Common implementation for flexvector->list and reverse-flexvector->list
  (define (flexvector->list-impl finalizer fv start end)
    (with-flexvectors ([(vec len) fv])
      (let loop ([acc (list)]
                 [index 0])
        (cond [(>= index len) (finalizer acc)]
              [else (loop (cons (vector-ref vec index) acc)
                          (+ index 1))]))))

  ;; Turns flexvector into list.
  (define-with-range (flexvector->list fv start (end of fv))
    (flexvector->list-impl reverse fv start end))

  ;; Turns reversed flexvector into list.
  (define-with-range (reverse-flexvector->list fv start (end of fv))
    (define (identity l) l)
    (flexvector->list-impl identity fv start end))

  ;; Conversion helper from list to flexvector.
  (define (list->flexvector-impl lst index-func)
    (let* ([len (length lst)]
           [fv (make-flexvector len)])
      (let loop ([lst lst]
                 [index 0])
        (cond [(null? lst) fv]
              [else (flexvector-set! fv (index-func len index) (car lst))
                    (loop (cdr lst) (+ index 1))]))))

  ;; Converts list into flexvector.
  (define (list->flexvector lst)
    (list->flexvector-impl lst (lambda (len index) index)))

  ;; Converts reversed list into flexvector.
  (define (reverse-list->flexvector lst)
    (list->flexvector-impl lst (lambda (len index) (- len index 1))))

  ;; Converts flexvector of chars to string.
  (define-with-range (flexvector->string fv start (end of fv))
    (let-values ([(out fin) (open-string-output-port)])
      (with-flexvectors ([(vec len) fv])
        (let loop ([index start])
          (cond [(>= index end) (fin)]
                [else (put-char out (vector-ref vec index))
                      (loop (+ index 1))])))))

  ;; Converts string into a flexvector.
  (define string->flexvector
    (case-lambda
      [(str)
       (string->flexvector str 0)]
      [(str start)
       (string->flexvector str start (string-length str))]
      [(str start end)
       (let ([in (open-string-input-port (substring str start end))]
             [fv (flexvector)])
         (let loop ([ch (get-char in)])
           (cond [(eof-object? ch) fv]
                 [else (flexvector-add-back! fv ch)
                       (loop (get-char in))])))]))

  ;; Converts flexvector into generator.
  (define (flexvector->generator fv)
    (with-flexvectors ([(vec len) fv])
      (let ([index 0])
        (lambda ()
          (cond [(>= index len) (eof-object)]
                [else
                 (let ([value (vector-ref vec index)])
                   (set! index (+ index 1))
                   value)])))))

  ;; Converts generator into a flexvector.
  (define (generator->flexvector gen)
    (let ([fv (flexvector)])
      (generator-fold (lambda (v r) (flexvector-add-back! r v))
                      fv
                      gen))))
