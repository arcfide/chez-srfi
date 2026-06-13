;; SRFI-4 r6rs library entry
;;
;; Copyright (c) 2018 - 2020 Andrew W. Keep
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;;
;; ------------------------------------------------------------------------
;;
;; Modifications for SRFI 160, including complex vectors, and improved
;; error-handling, D. Guthrie, Glasgow, 2025-2026.
;;
(library (srfi :160 base)
  (export s8? s8vector? make-s8vector s8vector s8vector-length
          s8vector-ref s8vector-set! s8vector->list list->s8vector

          s16? s16vector? make-s16vector s16vector s16vector-length
          s16vector-ref s16vector-set! s16vector->list list->s16vector

          s32? s32vector? make-s32vector s32vector s32vector-length
          s32vector-ref s32vector-set! s32vector->list list->s32vector

          s64? s64vector? make-s64vector s64vector s64vector-length
          s64vector-ref s64vector-set! s64vector->list list->s64vector

          u8? u8vector? make-u8vector u8vector u8vector-length u8vector-ref
          u8vector-set! u8vector->list list->u8vector

          u16? u16vector? make-u16vector u16vector u16vector-length
          u16vector-ref u16vector-set! u16vector->list list->u16vector

          u32? u32vector? make-u32vector u32vector u32vector-length
          u32vector-ref u32vector-set! u32vector->list list->u32vector

          u64? u64vector? make-u64vector u64vector u64vector-length
          u64vector-ref u64vector-set! u64vector->list list->u64vector

          f32? f32vector? make-f32vector f32vector f32vector-length
          f32vector-ref f32vector-set! f32vector->list list->f32vector

          f64? f64vector? make-f64vector f64vector f64vector-length
          f64vector-ref f64vector-set! f64vector->list list->f64vector

          c64? c64vector? make-c64vector c64vector c64vector-length
          c64vector-ref c64vector-set! c64vector->list list->c64vector

          c128? c128vector? make-c128vector c128vector c128vector-length
          c128vector-ref c128vector-set! c128vector->list list->c128vector)
  (import (only (chezscheme)
                format
                record-writer type-descriptor
                assertion-violationf)
          (rename (rnrs (6))
                  (exact? r6rs:exact?)
                  (inexact? r6rs:inexact?))
          (only (rnrs r5rs (6)) exact->inexact inexact->exact)
          (only (srfi :160 meta utils)
                format-vector-type
                nonnegative-integer? exact-integer? inexact?)
          (only (srfi :160 meta curried) raised-with?))

  (define (u8? n)  (and (exact-integer? n) (<= 0 n 255)))
  (define (s8? n)  (and (exact-integer? n) (<= -128 n 127)))
  (define (u16? n) (and (exact-integer? n) (<= 0 n 65535)))
  (define (s16? n) (and (exact-integer? n) (<= -32768 n 32767)))
  (define (u32? n) (and (exact-integer? n) (<= 0 n 4294967295)))
  (define (s32? n) (and (exact-integer? n) (<= -2147483648 n 2147483647)))
  (define (u64? n) (and (exact-integer? n) (<= 0 n 18446744073709551615)))
  (define (s64? n) (and (exact-integer? n) (<= -9223372036854775808 n 9223372036854775807)))
  (define (f32? n) (and (inexact? n) (real? n)))
  (define (f64? n) (f32? n))
  (define (c64? n) (inexact? n))
  (define (c128? n) (inexact? n))

  (define-syntax define-integer-vector
    (lambda (x)
      (define format-id
        (lambda (tid fmt . args)
          (datum->syntax tid
            (string->symbol
              (apply format fmt args)))))
      (syntax-case x ()
        [(k signed? bit-size)
         (and (boolean? (syntax->datum #'signed?))
              (let ([bit-size (syntax->datum #'bit-size)])
                (and (integer? bit-size) (r6rs:exact? bit-size))))
         (let ([signed? (syntax->datum #'signed?)]
               [bit-size (syntax->datum #'bit-size)])
           (let ([lexical-template
                  (format "~a~s~~a" (if signed? #\s #\u) bit-size)])
             (let ([base-name (format lexical-template "vector")])
               (with-syntax ([lexical-form (datum->syntax #'lexical-form base-name)]
                             [name (format-id #'k "$~a" base-name)]
                             [bv-accessor (format-id #'k "$~a-bv" base-name)]
                             [maker (format-id #'k "make-~a" base-name)]
                             [pred (format-id #'k "~a?" base-name)]
                             [litmaker (format-id #'k "~a" base-name)]
                             [len (format-id #'k "~a-length" base-name)]
                             [ref (format-id #'k "~a-ref" base-name)]
                             [set (format-id #'k "~a-set!" base-name)]
                             [->list (format-id #'k "~a->list" base-name)]
                             [list-> (format-id #'k "list->~a" base-name)]
                             [bytes (fxdiv bit-size 8)]
                             [min (if signed? (- (expt 2 (- bit-size 1))) 0)]
                             [max (if signed?
                                      (- (expt 2 (- bit-size 1)) 1)
                                      (- (expt 2 bit-size) 1))]
                             [(bytevector-ref bytevector-set!)
                              (let ([signed-char (if signed? #\s #\u)])
                                (if (fx=? bit-size 8)
                                    (list
                                     (format-id #'k "bytevector-~a~s-ref" signed-char bit-size)
                                     (format-id #'k "bytevector-~a~s-set!" signed-char bit-size))
                                    (list
                                     (format-id #'k "bytevector-~a~s-native-ref" signed-char bit-size)
                                     (format-id #'k "bytevector-~a~s-native-set!" signed-char bit-size))))])
                 #'(begin
                     (define-record-type (name maker pred)
                       (nongenerative)
                       (sealed #t)
                       (opaque #t)
                       (fields (immutable bv bv-accessor))
                       (protocol
                        (lambda (new)
                          (case-lambda
                           [(size)
                            (guard (ex [(raised-with? ex 'fx*)
                                        (assertion-violationf (quote maker)
                                                              (format-vector-type "~~a is not a valid length for ~a" lexical-form) size)]
                                       [(raised-with? ex 'make-bytevector)
                                        (assertion-violationf (quote maker)
                                                              "length ~a is not a non-negative integer" size)]
                                       [else
                                        (raise-continuable ex)])
                              (new
                               (make-bytevector (fx* size bytes))))]
                           [(size value)
                            (if (and (exact-integer? value)
                                     (<= min value max))
                                (guard (ex [(raised-with? ex 'fx*)
                                            (assertion-violationf (quote maker)
                                              (format-vector-type "~~a is not a valid length for ~a" lexical-form) size)]
                                           [(raised-with? ex 'make-bytevector)
                                            (assertion-violationf (quote maker)
                                              "length ~a is not a non-negative integer" size)]
                                           [else
                                            (raise-continuable ex)])
                                   (let ([bv-size (fx* size bytes)])
                                     (let ([bv (make-bytevector bv-size)])
                                       (do ([i 0 (fx+ i bytes)])
                                           ((fx=? i bv-size) (new bv))
                                         (bytevector-set! bv i value)))))
                                (assertion-violationf (quote maker)
                                  (format-vector-type "repeating element ~~a cannot be contained within ~a" lexical-form)
                                  value))]))))
                     (define len
                       (lambda (v)
                         (guard (ex [(raised-with? ex (quote bv-accessor))
                                     (assertion-violationf (quote len) (format "~~a is not of type ~a" lexical-form) v)]
                                    [else
                                     (raise-continuable ex)])
                                (fxdiv (bytevector-length (bv-accessor v)) bytes))))

                     (define ref
                       (lambda (v i)
                         (guard (ex [(raised-with? ex (quote fx*))
                                     (assertion-violationf (quote ref) "index ~a is not a non-negative integer" i)]
                                    [(raised-with? ex (quote bv-accessor))
                                     (assertion-violationf (quote ref) (format "~~a is not of type ~a" lexical-form) v)]
                                    [(raised-with? ex (quote bytevector-ref))
                                     (assertion-violationf (quote ref) "~a is not a valid index for ~a" i v)]
                                    [else
                                     (raise-continuable ex)])
                                (bytevector-ref (bv-accessor v) (fx* i bytes)))))
                     (define set
                       (lambda (v i value)
                         (if (exact-integer? value)
                             (if (<= min value max)
                                 (guard (ex [(raised-with? ex (quote fx*))
                                             (assertion-violationf (quote set) "index ~a is not a non-negative integer" i)]
                                            [(raised-with? ex (quote bv-accessor))
                                             (assertion-violationf (quote set) (format "~~a is not of type ~a" lexical-form) v)]
                                            [(raised-with? ex (quote bytevector-set!))
                                             (assertion-violationf (quote set) "~a is not a valid index for ~a" i v)]
                                            [else
                                             (raise-continuable ex)])
                                        (bytevector-set! (bv-accessor v) (fx* i bytes) value))
                                 (assertion-violationf (quote set)
                                   (format-vector-type "element ~~a is out of range for ~a" lexical-form)
                                   value))
                             (assertion-violationf (quote set)
                               (format-vector-type "element ~~a cannot be contained within ~a" lexical-form)
                               value))))
                     (define ->list
                       (case-lambda
                        [(v)
                         (guard (ex [(raised-with? ex (quote bv-accessor))
                                     (assertion-violationf (quote ->list) (format "~~a is not of type ~a" lexical-form) v)]
                                    [else
                                     (raise-continuable ex)])
                           (let ([bv (bv-accessor v)])
                             (let loop ([n (bytevector-length bv)] [acc '()])
                               (if (fx=? n 0)
                                   acc
                                   (let ([n (fx- n bytes)])
                                     (loop n (cons (bytevector-ref bv n) acc)))))))]
                        [(v start)
                         (guard (ex [(raised-with? ex (quote len))
                                     (assertion-violationf (quote ->list)
                                       (format "~~a is not of type ~a" 'lexical-form) v)]
                                    [else
                                     (raise-continuable ex)])
                           (->list v start (len v)))]
                        [(v start end)
                         (cond [(not (nonnegative-integer? start))
                                (assertion-violationf (quote ->list)
                                  "start ~a is not a non-negative integer" start)]
                               [(not (nonnegative-integer? end))
                                (assertion-violationf (quote ->list)
                                  "end ~a is not a non-negative integer" end)]
                               [(> start end)
                                (assertion-violationf (quote ->list)
                                  "end ~a must be greater than or equal to start ~a" end start)]
                               [(> end (len v))
                                (assertion-violationf (quote ->list)
                                  "end ~a overflows ~a" end v)]
                               [else
                                (let ([bv (bv-accessor v)]
                                      [start (fx* start bytes)])
                                  (let loop ([n (fx* end bytes)] [acc '()])
                                    (if (fx=? n start)
                                        acc
                                        (let ([n (fx- n bytes)])
                                          (loop n (cons (bytevector-ref bv n) acc))))))])]))
                     (define $list->
                       (lambda (who ls)
                         (let f ([ls ls] [bv-bytes 0])
                           (if (null? ls)
                               (maker (fxdiv bv-bytes bytes))
                               (let ([v (f (cdr ls) (fx+ bv-bytes bytes))] [val (car ls)])
                                 (if (and (exact-integer? val)
                                          (<= min val max))
                                     (begin
                                       (bytevector-set! (bv-accessor v) bv-bytes val)
                                       v)
                                     (assertion-violationf who
                                       (format-vector-type "element ~~a cannot be contained within ~a" lexical-form)
                                       val)))))))
                     (define list->
                       (lambda (ls)
                         (guard (ex [(raised-with? ex (quote car))
                                     (assertion-violationf (quote list->) "~a is not a proper list" ls)]
                                    [else (raise-continuable ex)])
                           ($list-> (quote list->) ls))))
                     (define litmaker
                       (lambda args
                         ($list-> (quote litmaker) args)))
                     ;;
                     )))))])))

  (define-integer-vector #t 8)
  (define-integer-vector #t 16)
  (define-integer-vector #t 32)
  (define-integer-vector #t 64)

  (define-integer-vector #f 8)
  (define-integer-vector #f 16)
  (define-integer-vector #f 32)
  (define-integer-vector #f 64)

  (define-syntax define-float-vector
    (lambda (x)
      (define format-id
        (lambda (tid fmt . args)
          (datum->syntax tid
            (string->symbol
              (apply format fmt args)))))
      (syntax-case x ()
        [(k bit-size)
         (let ([bit-size (syntax->datum #'bit-size)])
           (and (integer? bit-size) (r6rs:exact? bit-size)))
         (let ([bit-size (syntax->datum #'bit-size)])
           (let ([base-name (format "f~svector" bit-size)])
             (with-syntax ([lexical-form (datum->syntax #'lexical-form base-name)]
                           [name (format-id #'k "$~a" base-name)]
                           [bv-accessor (format-id #'k "$~a-bv" base-name)]
                           [maker (format-id #'k "make-~a" base-name)]
                           [pred (format-id #'k "~a?" base-name)]
                           [litmaker (format-id #'k "~a" base-name)]
                           [len (format-id #'k "~a-length" base-name)]
                           [ref (format-id #'k "~a-ref" base-name)]
                           [set (format-id #'k "~a-set!" base-name)]
                           [->list (format-id #'k "~a->list" base-name)]
                           [list-> (format-id #'k "list->~a" base-name)]
                           [bytes (fxdiv bit-size 8)]
                           [(bytevector-ref bytevector-set!)
                            (case bit-size
                              [(32)
                               (list #'bytevector-ieee-single-native-ref
                                     #'bytevector-ieee-single-native-set!)]
                              [(64)
                               (list #'bytevector-ieee-double-native-ref
                                     #'bytevector-ieee-double-native-set!)])])
               #'(begin
                   (define-record-type (name maker pred)
                     (nongenerative)
                     (sealed #t)
                     (opaque #t)
                     (fields (immutable bv bv-accessor))
                     (protocol
                       (lambda (new)
                         (case-lambda
                          [(size)
                           (guard (ex [(raised-with? ex 'fx*)
                                       (assertion-violationf (quote maker)
                                         (format-vector-type "~~a is not a valid length for ~a" lexical-form) size)]
                                      [(raised-with? ex 'make-bytevector)
                                       (assertion-violationf (quote maker)
                                         "length ~a is not a non-negative integer" size)]
                                      [else
                                       (raise-continuable ex)])
                             (new
                              (make-bytevector (fx* size bytes))))]
                           [(size value)
                            (if (flonum? value)
                                (guard (ex [(raised-with? ex 'fx*)
                                            (assertion-violationf (quote maker)
                                              (format-vector-type "~~a is not a valid length for ~a" lexical-form) size)]
                                           [(raised-with? ex 'make-bytevector)
                                            (assertion-violationf (quote maker)
                                              "length ~a is not a non-negative integer" size)]
                                           [else
                                            (raise-continuable ex)])
                                   (let ([bv-size (fx* size bytes)])
                                     (let ([bv (make-bytevector bv-size)])
                                       (do ([i 0 (fx+ i bytes)])
                                           ((fx=? i bv-size) (new bv))
                                         (bytevector-set! bv i value)))))
                                (assertion-violationf (quote maker)
                                  (format-vector-type "repeating element ~~a cannot be contained within ~a" lexical-form)
                                  value))]))))
                   (define len
                     (lambda (v)
                       (guard (ex [(raised-with? ex (quote bv-accessor))
                                   (assertion-violationf (quote len) (format "~~a is not of type ~a" lexical-form) v)]
                                  [else
                                   (raise-continuable ex)])
                         (fxdiv (bytevector-length (bv-accessor v)) bytes))))
                   (define ref
                     (lambda (v i)
                       (guard (ex [(raised-with? ex (quote fx*))
                                   (assertion-violationf (quote ref) "index ~a is not a non-negative integer" i)]
                                  [(raised-with? ex (quote bv-accessor))
                                   (assertion-violationf (quote ref) (format "~~a is not of type ~a" lexical-form) v)]
                                  [(raised-with? ex (quote bytevector-ref))
                                   (assertion-violationf (quote ref) "~a is not a valid index for ~a" i v)]
                                  [else
                                   (raise-continuable ex)])
                         (bytevector-ref (bv-accessor v) (fx* i bytes)))))
                   (define set
                     (lambda (v i value)
                       (if (flonum? value)
                           (guard (ex [(raised-with? ex (quote fx*))
                                       (assertion-violationf (quote set) "index ~a is not a non-negative integer" i)]
                                      [(raised-with? ex (quote bv-accessor))
                                       (assertion-violationf (quote set) (format "~~a is not of type ~a" lexical-form) v)]
                                      [(raised-with? ex (quote bytevector-set!))
                                       (assertion-violationf (quote set) "~a is not a valid index for ~a" i v)]
                                      [else
                                       (raise-continuable ex)])
                              (bytevector-set! (bv-accessor v) (fx* i bytes) value))
                           (assertion-violationf (quote set)
                             (format-vector-type "element ~~a cannot be contained within ~a" lexical-form)
                             value))))
                   (define ->list
                     (case-lambda
                      [(v)
                       (guard (ex [(raised-with? ex (quote bv-accessor))
                                   (assertion-violationf (quote ->list) (format "~~a is not of type ~a" lexical-form) v)]
                                  [else
                                   (raise-continuable ex)])
                         (let ([bv (bv-accessor v)])
                           (let loop ([n (bytevector-length bv)] [ls '()])
                             (if (fx=? n 0)
                                 ls
                                 (let ([n (fx- n bytes)])
                                   (loop n (cons (bytevector-ref bv n) ls)))))))]
                      [(v start)
                       (guard (ex [(raised-with? ex (quote len))
                                     (assertion-violationf (quote ->list)
                                       (format "~~a is not of type ~a" 'lexical-form) v)]
                                    [else
                                     (raise-continuable ex)])
                         (->list v start (len v)))]
                      [(v start end)
                       (cond [(not (nonnegative-integer? start))
                              (assertion-violationf (quote ->list)
                                "start ~a is not a non-negative integer" start)]
                             [(not (nonnegative-integer? end))
                              (assertion-violationf (quote ->list)
                                "end ~a is not a non-negative integer" end)]
                             [(> start end)
                              (assertion-violationf (quote ->list)
                                "end ~a must be greater than or equal to start ~a" end start)]
                             [(> end (len v))
                              (assertion-violationf (quote ->list)
                                "end ~a overflows ~a" end v)]
                             [else
                              (let ([bv (bv-accessor v)])
                                (let loop ([n (bytevector-length bv)] [ls '()])
                                  (if (fx=? n 0)
                                      ls
                                      (let ([n (fx- n bytes)])
                                        (loop n (cons (bytevector-ref bv n) ls))))))])]))
                   (define $list->
                     (lambda (who ls)
                       (let f ([ls ls] [bv-bytes 0])
                         (if (null? ls)
                             (maker (fxdiv bv-bytes bytes))
                             (let ([v (f (cdr ls) (fx+ bv-bytes bytes))] [val (car ls)])
                               (if (flonum? val)
                                   (begin
                                     (bytevector-set! (bv-accessor v) bv-bytes val)
                                     v)
                                   (assertion-violationf who
                                     (format-vector-type "element ~~a cannot be contained within ~a" lexical-form)
                                     val)))))))
                   (define list->
                     (lambda (ls)
                       (guard (ex [(raised-with? ex (quote car))
                                   (assertion-violationf (quote list->) "~a is not a proper list" ls)]
                                  [else (raise-continuable ex)])
                         ($list-> (quote list->) ls))))
                   (define litmaker
                     (lambda args
                       ($list-> (quote litmaker) args)))))))])))

  (define-float-vector 32)
  (define-float-vector 64)

  (define-syntax define-complex-vector
    (lambda (x)
      (define format-id
        (lambda (tid fmt . args)
          (datum->syntax tid
            (string->symbol
              (apply format fmt args)))))
      (syntax-case x ()
        [(k bit-size)
         (let ([bit-size (syntax->datum #'bit-size)])
           (or (fx=? bit-size 64)
               (fx=? bit-size 128)))
         (let ([bit-size (syntax->datum #'bit-size)])
           (let ([base-name (format "c~svector" bit-size)])
             (with-syntax ([lexical-form (datum->syntax #'lexical-form base-name)]
                           [name (format-id #'k "$~a" base-name)]
                           [maker (format-id #'k "make-~a" base-name)]
                           [pred (format-id #'k "~a?" base-name)]
                           [litmaker (format-id #'k "~a" base-name)]
                           [len (format-id #'k "~a-length" base-name)]
                           [ref (format-id #'k "~a-ref" base-name)]
                           [set (format-id #'k "~a-set!" base-name)]
                           [->list (format-id #'k "~a->list" base-name)]
                           [list-> (format-id #'k "list->~a" base-name)]
                           [internal-repr (format-id #'k "$~a-raw-vector" base-name)]
                           [(make-internal-repr internal-repr-ref internal-repr-set! internal-repr-length)
                            (case bit-size
                              [(64)
                               (list #'make-f32vector #'f32vector-ref #'f32vector-set! #'f32vector-length)]
                              [(128)
                               (list #'make-f64vector #'f64vector-ref #'f64vector-set! #'f64vector-length)])])
               #'(begin
                   (define-record-type (name maker pred)
                     (nongenerative)
                     (sealed #t)
                     (opaque #t)
                     (fields (immutable raw-vector))
                     (protocol
                      (lambda (new)
                        (case-lambda
                         [(size)
                          (cond [(nonnegative-integer? size)
                                 (new (make-internal-repr (fx* 2 size)))]
                                [(integer? size)
                                 (assertion-violationf (quote maker)
                                  "length ~a is not a non-negative integer" size)]
                                [else
                                 (assertion-violationf (quote maker)
                                  (format-vector-type "~~a is not a valid length for ~a" lexical-form) size)])]
                         [(size value)
                          (cond [(not (inexact? value))
                                 (assertion-violationf (quote maker)
                                   (format-vector-type "repeating element ~~a cannot be contained within ~a" lexical-form)
                                   value)]
                                 [(nonnegative-integer? size)
                                  (let ([slots (maker size)])
                                   (let loop ([k 0])
                                     (if (= k size)
                                         slots
                                         (begin
                                           (set slots k value)
                                           (loop (+ k 1))))))]
                                [(integer? size)
                                 (assertion-violationf (quote maker)
                                   "length ~a is not a non-negative integer" size)]
                                [else
                                 (assertion-violationf (quote maker)
                                  (format-vector-type "~~a is not a valid length for ~a" lexical-form) size)])]))))
                   (define len
                     (lambda (v)
                       (guard (ex [(raised-with? ex (quote internal-repr))
                                   (assertion-violationf (quote len)
                                     (format "~~a is not of type ~a" lexical-form) v)]
                                  [else
                                   (raise-continuable ex)])
                          (fxdiv (internal-repr-length (internal-repr v))
                                 2))))
                   (define set
                     (lambda (v i value)
                       (guard (ex [(raised-with? ex (quote internal-repr))
                                   (assertion-violationf (quote set)
                                     (format "~~a is not of type ~a" lexical-form)
                                     v)]
                                  [(raised-with? ex (quote internal-repr-set!))
                                   (assertion-violationf (quote set)
                                         "~a is not a valid index for ~a"
                                         i v)]
                                  [(raised-with? ex (quote fx*))
                                   (assertion-violationf (quote set)
                                     "index ~a is not a non-negative integer" i)]
                                  [else
                                   (raise-continuable ex)])
                          (let ([raw (internal-repr v)]
                                [j (fx* i 2)])
                            (cond [(and (inexact? value) (inexact? (imag-part value)))
                                   (internal-repr-set! raw j       (real-part value))
                                   (internal-repr-set! raw (+ j 1) (imag-part value))]
                                  [(inexact? value)
                                   (internal-repr-set! raw j       (real-part value))
                                   (internal-repr-set! raw (+ j 1) (exact->inexact (imag-part value)))]
                                  [else
                                   (assertion-violationf (quote set)
                                     (format-vector-type "element ~~a cannot be contained within ~a" lexical-form)
                                     value)])))))
                   (define ref
                     (lambda (v i)
                       (guard (ex [(raised-with? ex (quote internal-repr))
                                   (assertion-violationf (quote ref)
                                     (format "~~a is not of type ~a" lexical-form)
                                     v)]
                                  [(and (raised-with? ex (quote internal-repr-ref))
                                        (message-condition? ex)
                                        (irritants-condition? ex))
                                   (assertion-violationf (quote ref)
                                     "~a is not a valid index for ~a"
                                     i v)]
                                  [(raised-with? ex (quote fx*))
                                   (assertion-violationf (quote ref)
                                     "index ~a is not a non-negative integer" i)]
                                  [else
                                   (raise-continuable ex)])
                         (let ([raw (internal-repr v)]
                               [j (fx* i 2)])
                           (make-rectangular (internal-repr-ref raw j)
                                             (internal-repr-ref raw (+ j 1)))))))
                   (define ->list
                     (case-lambda
                      [(v)
                       (guard (ex [(raised-with? ex (quote len))
                                   (assertion-violationf (quote ->list)
                                     (format "~~a is not of type ~a" lexical-form) v)]
                                  [else
                                   (raise-continuable ex)])
                         (let ([size (len v)])
                           (let loop ([k 0])
                             (if (fx=? k size)
                                 '()
                                 (cons (ref v k) (loop (fx+ k 1)))))))]
                      [(v start)
                       (guard (ex [(raised-with? ex (quote len))
                                     (assertion-violationf (quote ->list)
                                       (format "~~a is not of type ~a" 'lexical-form) v)]
                                    [else
                                     (raise-continuable ex)])
                         (->list v start (len v)))]
                      [(v start end)
                       (guard (ex [(raised-with? ex (quote len))
                                   (assertion-violationf (quote ->list)
                                     (format "~~a is not of type ~a" lexical-form) v)]
                                  [else
                                   (raise-continuable ex)])
                         (cond [(not (nonnegative-integer? start))
                                (assertion-violationf (quote ->list)
                                  "start ~a is not a non-negative integer" start)]
                               [(not (nonnegative-integer? end))
                                (assertion-violationf (quote ->list)
                                  "end ~a is not a non-negative integer" end)]
                               [(> start end)
                                (assertion-violationf (quote ->list)
                                  "end ~a must be greater than or equal to start ~a" end start)]
                               [(> end (len v))
                                (assertion-violationf (quote ->list)
                                  "end ~a overflows ~a" end v)]
                             [else
                              (let ([size (len v)])
                              (let loop ([k start])
                                (if (fx=? k end)
                                    '()
                                    (cons (ref v k) (loop (fx+ k 1))))))]))]))
                   (define $list->
                     (lambda (who lst)
                       (guard (ex [(raised-with? ex (quote set))
                                   (apply assertion-violationf
                                          who
                                          (condition-message ex)
                                          (condition-irritants ex))]
                                  [else (raise-continuable ex)])
                         (let* ([size (length lst)]
                                [slots (maker size)])
                           (let loop ([k 0] [lst lst])
                             (if (fx=? k size)
                                 slots
                                 (let ([val (car lst)])
                                   (set slots k (car lst))
                                   (loop (fx+ k 1) (cdr lst)))))))))
                   (define list->
                     (lambda (ls)
                       (guard (ex [(raised-with? ex (quote length))
                                   (assertion-violationf (quote list->) "~a is not a proper list" ls)]
                                  [else (raise-continuable ex)])
                         ($list-> (quote list->) ls))))
                   (define litmaker
                     (lambda args
                       ($list-> (quote litmaker) args)))
                   ))))])))

  (define-complex-vector 64)
  (define-complex-vector 128)

  (define-syntax (writer-with stx)
    (syntax-case stx ()
      [(k atom)
       (let ([prefix (symbol->string (syntax->datum #'atom))])
         (let ([descriptor/str (format "$~avector" prefix)]
               [->list/str (format "~avector->list" prefix)]
               [lexical-form (format "#~a~~a" prefix)])
           (with-syntax ([descriptor
                          (datum->syntax #'k (string->symbol descriptor/str))]
                         [->list
                          (datum->syntax #'k (string->symbol ->list/str))])
           #`(record-writer (type-descriptor descriptor)
               (lambda (r p _wr)
                 (display (format #,lexical-form (->list r)) p))))))]))

  (writer-with u8)
  (writer-with u16)
  (writer-with u32)
  (writer-with u64)
  (writer-with s8)
  (writer-with s16)
  (writer-with s32)
  (writer-with s64)
  (writer-with f32)
  (writer-with f64)
  (writer-with c64)
  (writer-with c128)

  ;;;
  ); library
