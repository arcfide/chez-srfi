#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (srfi :19 time compat)
  (export
    time-resolution
    current-time 
    time-nanosecond 
    time-second 
    timezone-offset
    cumulative-thread-time
    cumulative-process-time
    cumulative-gc-time)
  (import
    (rnrs base)
    (ironscheme datetime)
    (ironscheme process)
    (only (srfi :19 time not-implemented) cumulative-gc-time))
  
  (define time-resolution 100)
  
  (define base (datetime->local (make-utc-datetime 1970 1 1)))
  
  (define (current-time) (now))
  ; since 1970, but fractional (100ns per tick in .NET)
  (define (time-nanosecond t) (mod (* 100 (ticks (difference t base))) #e1e9))
  (define (time-second t) (exact (truncate (total-seconds (difference t base)))))
  (define timezone-offset (exact (truncate (total-seconds (difference (now) (datetime->utc (now)))))))
  ;; todo: check if parameters correct
  (define (cumulative-thread-time) 
    (datetime-add base (process-user-cpu-time (get-current-process))))
  (define (cumulative-process-time)
    (datetime-add base (process-total-cpu-time (get-current-process))))
)
