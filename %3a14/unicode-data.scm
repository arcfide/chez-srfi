; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber
; Copyright (c) 2005-2006 by Basis Technology Corporation. 

; Parse UnicodeData.txt and various other files from the Unicode
; consortium, and generate character classification and conversion
; tables from it.

(define (string-split string at)
  (let ((count (string-length string)))
    (let loop ((index 0)
	       (rev-result '()))
      (cond
       ((>= index count)
	(reverse (cons "" rev-result)))
       ((string-index string at index)
	=> (lambda (found)
	     (loop (+ 1 found)
		   (cons (substring string index found)
			 rev-result))))
       (else
	(reverse (cons (substring string index count)
		       rev-result)))))))
	  

(define (split-unicode-data-record line)
  (string-split line #\;))

(define (maybe-code-point text default)
  (if (zero? (string-length text))
      default
      (string->number text 16)))

(define-record-type code-point-info :code-point-info
  (make-code-point-info code-point
			name
			general-category
			combining-class
			bidirectional-category-id
			canonical-decomposition
			compatibility-decomposition
			decimal-digit-value
			digit-value
			numeric-value
			mirrored?
			unicode-1.0-name
			iso-10646-comment
			uppercase-code-point
			lowercase-code-point
			titlecase-code-point)
  code-point-info?
  ;; number
  (code-point code-point-info-code-point)
  ;; string
  (name code-point-info-name)
  ;; :GENERAL-CATEGORY
  (general-category code-point-info-general-category)
  ;; number
  (combining-class code-point-info-combining-class)
  ;; symbol
  (bidirectional-category-id code-point-info-bidirectional-category-id)
  ;; #f or list
  (canonical-decomposition code-point-info-canonical-decomposition)
  (compatibility-decomposition code-point-info-compatibility-decomposition)
  ;; number
  (decimal-digit-value code-point-info-decimal-digit-value)
  ;; number
  (digit-value code-point-info-digit-value)
  ;; number
  (numeric-value code-point-info-numeric-value)
  ;; boolean
  (mirrored? code-point-info-mirrored?)
  ;; string
  (unicode-1.0-name code-point-info-unicode-1.0-name)
  ;; string
  (iso-10646-comment code-point-info-iso-10646-comment)
  ;; number
  (uppercase-code-point code-point-info-uppercase-code-point)
  ;; number
  (lowercase-code-point code-point-info-lowercase-code-point)
  ;; number
  (titlecase-code-point code-point-info-titlecase-code-point))

(define-record-discloser :code-point-info
  (lambda (r)
    (list 'code-point-info
	  (code-point-info-code-point r)
	  (code-point-info-name r)
	  (code-point-info-general-category r)
	  (code-point-info-combining-class r)
	  (code-point-info-bidirectional-category-id r)
	  (code-point-info-canonical-decomposition r)
	  (code-point-info-compatibility-decomposition r)
	  (code-point-info-decimal-digit-value r)
	  (code-point-info-digit-value r)
	  (code-point-info-numeric-value r)
	  (code-point-info-mirrored? r)
	  (code-point-info-unicode-1.0-name r)
	  (code-point-info-iso-10646-comment r)
	  (code-point-info-uppercase-code-point r)
	  (code-point-info-lowercase-code-point r)
	  (code-point-info-titlecase-code-point r))))

(define (unicode-data-record->info line)
  (destructure (((code-point-hex
		  name
		  general-category-id
		  combining-class-id
		  bidirectional-category-text
		  decomposition-text
		  decimal-digit-value-text
		  digit-value-text
		  numeric-value-text
		  mirrored-y/n
		  unicode-1.0-name
		  iso-10646-comment
		  uppercase-code-point-hex
		  lowercase-code-point-hex
		  titlecase-code-point-hex)
		 (split-unicode-data-record line)))
    (let ((code-point (maybe-code-point code-point-hex #f)))
      (let ((uppercase-code-point (maybe-code-point uppercase-code-point-hex code-point))
	    (lowercase-code-point (maybe-code-point lowercase-code-point-hex code-point))
	    (titlecase-code-point (maybe-code-point titlecase-code-point-hex code-point))
	    (decomposition (parse-decomposition decomposition-text)))
	     
	(make-code-point-info code-point
			      name
			      (id->general-category general-category-id)
			      (string->number combining-class-id)
			      (string->symbol bidirectional-category-text)
			      (and (and (pair? decomposition) (number? (car decomposition)))
				   decomposition)
			      (and (and (pair? decomposition) (symbol? (car decomposition)))
				   (cdr decomposition))
			      (string->number decimal-digit-value-text)
			      (string->number digit-value-text)
			      (string->number numeric-value-text)
			      (string=? mirrored-y/n "Y")
			      unicode-1.0-name
			      iso-10646-comment
			      uppercase-code-point
			      lowercase-code-point
			      titlecase-code-point)))))

;; return #f or a list, which contains the scalar values of the decompositon
;; for compatibility decompositions, the tag is prepended as a symbol
(define (parse-decomposition d)
  (cond
   ((zero? (string-length d))
    #f)
   ((char=? #\< (string-ref d 0))
    (let ((after (string-index d #\space)))
      (cons (string->symbol (substring d 0 after))
	    (call-with-values
		(lambda ()
		  (parse-scalar-values d after))
	      (lambda (l i) l)))))
   (else
    (call-with-values
	(lambda ()
	  (parse-scalar-values d 0))
      (lambda (l i) l)))))

; for EXPANDED-CODE-POINT-INFO-SOURCE
(define (code-point-info-with-code-point+name info code-point name)
  (make-code-point-info code-point
			name
			(code-point-info-general-category info)
			(code-point-info-combining-class info)
			(code-point-info-bidirectional-category-id info)
			(code-point-info-canonical-decomposition info)
			(code-point-info-compatibility-decomposition info)
			(code-point-info-decimal-digit-value info)
			(code-point-info-digit-value info)
			(code-point-info-numeric-value info)
			(code-point-info-mirrored? info)
			(code-point-info-unicode-1.0-name info)
			(code-point-info-iso-10646-comment info)
			code-point code-point code-point)) ; kludge

; expand the code-point ranges that look like this:
; 3400;<CJK Ideograph Extension A, First>;Lo;0;L;;;;;N;;;;;
; 4DB5;<CJK Ideograph Extension A, Last>;Lo;0;L;;;;;N;;;;;

; returns a thunk that returns the infos from consecutive calls,
; then #f

(define (expanded-code-point-info-source infos)
  (let ((first-info #f)
	(code-point #f)
	(last-code-point #f)
	(name-base #f))
    (lambda ()
      (let again ()
	(cond
	 (first-info
	  (if (<= code-point last-code-point)
	      (begin
		(set! code-point (+ 1 code-point))
		(code-point-info-with-code-point+name
		 first-info
		 (- code-point 1)
		 name-base)) ; kludge for speed; should be:
	                     ; (string-append name-base (number->string code-point 16))
	      (begin
		(set! first-info #f)
		(again))))
	 ((null? infos)
	  #f)
	 (else
	  (let* ((info (car infos))
		 (name (code-point-info-name info)))
	    (cond
	     ((and (string-prefix? "<" name)
		   (string-suffix? ", First>" name))
	      (set! first-info info)
	      (set! code-point (code-point-info-code-point info))
	      (set! last-code-point (code-point-info-code-point (cadr infos)))
	      (set! name-base (string-append
			       (substring name
					  1 ; (string-length "<")
					  (- (string-length name)
					     8 ; (string-length ", First>")
					     ))
			       "-<code point>")) ; kludge, see above
	      (set! infos (cddr infos))
	      (again))
	     (else
	      (set! infos (cdr infos))
	      info)))))))))

(define (for-each-expanded-code-point-info proc infos)
  (let ((source (expanded-code-point-info-source infos)))
    (let loop ()
      (let ((info (source)))
	(if info
	    (begin
	      (proc info)
	      (loop)))))))
	    
(define (read-line port)
  (let loop ((l '()))
    (let ((c (read-char port)))
      (if (eof-object? c)
          c
          (if (char=? c #\newline)
              (list->string (reverse l))
              (loop (cons c l)))))))

(define (parse-unicode-data filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((rev-infos '()))
	(let ((thing (read-line port)))
	  (if (eof-object? thing)
	      (reverse rev-infos)
	      (loop (cons (unicode-data-record->info thing) rev-infos))))))))

; Mapping the relevant info (general category + case mappings) into a
; compact array

(define (mapping-offsets infos accessor)
  (let loop ((infos infos)
	     (offsets '()))
    (if (null? infos)
	(list->vector offsets)
	(let* ((info (car infos))
	       (code-point (code-point-info-code-point info))
	       (other (accessor info))
	       (offset (- other code-point)))
	  (if (member offset offsets)
	      (loop (cdr infos) offsets)
	      (loop (cdr infos) (cons offset offsets)))))))

(define (vector-index vector value)
  (let ((count (vector-length vector)))
    (let loop ((i 0))
      (cond
       ((>= i count) #f)
       ((equal? value (vector-ref vector i)) i)
       (else (loop (+ 1 i)))))))

(define (code-point-info->case+general-category-encoding
	 info
	 specialcasing?
	 special-lowercase-table special-uppercase-table
	 uppercase-offsets lowercase-offsets titlecase-offsets
	 uppercase-index-width lowercase-index-width titlecase-index-width)
  (let ((code-point (code-point-info-code-point info)))
    (let ((uppercase-index (vector-index uppercase-offsets
					 (- (code-point-info-uppercase-code-point info)
					    code-point)))
	  (lowercase-index (vector-index lowercase-offsets
					 (- (code-point-info-lowercase-code-point info)
					    code-point)))
	  (titlecase-index (vector-index titlecase-offsets
					 (- (code-point-info-titlecase-code-point info)
					    code-point)))
	  (uppercase? (or (eq? (general-category uppercase-letter)
			       (code-point-info-general-category info))
			  (table-ref special-uppercase-table code-point)))
	  (lowercase? (or (eq? (general-category lowercase-letter)
			       (code-point-info-general-category info))
			  (table-ref special-lowercase-table code-point))))

      (bitwise-ior
       (arithmetic-shift
	(bitwise-ior
	 (arithmetic-shift (bitwise-ior
			    (arithmetic-shift
			     (bitwise-ior
			      (arithmetic-shift
			       (bitwise-ior (if specialcasing? 4 0)
					    (if uppercase? 2 0)
					    (if lowercase? 1 0))
			       uppercase-index-width)
			      uppercase-index)
			     lowercase-index-width)
			    lowercase-index)
			   titlecase-index-width)
	 titlecase-index)
	*general-category-bits*)
       (general-category-index (code-point-info-general-category info))))))

(define (code-point-encoding-uppercase? encoding
					uppercase-index-width lowercase-index-width titlecase-index-width)
  (not
   (zero?
    (bitwise-and 1
		 (arithmetic-shift encoding
				   (- (+ 1
					 uppercase-index-width
					 lowercase-index-width
					 titlecase-index-width
					 *general-category-bits*)))))))

(define (code-point-encoding-lowercase? encoding
					uppercase-index-width lowercase-index-width titlecase-index-width)
  (not
   (zero?
    (bitwise-and 1
		 (arithmetic-shift encoding
				   (- (+ uppercase-index-width
					 lowercase-index-width
					 titlecase-index-width
					 *general-category-bits*)))))))

(define (lookup-by-offset-index code-point offset-index offsets)
  (+ code-point (vector-ref offsets offset-index)))

(define (code-point-encoding-uppercase-code-point code-point encoding
						  uppercase-offsets
						  uppercase-index-width lowercase-index-width titlecase-index-width)
  (lookup-by-offset-index
   code-point
   (bitwise-and (- (arithmetic-shift 1 uppercase-index-width) 1)
		(arithmetic-shift encoding
				  (- (+ lowercase-index-width titlecase-index-width *general-category-bits*))))
   uppercase-offsets))

(define (code-point-encoding-lowercase-code-point code-point encoding
						  lowercase-offsets
						  uppercase-index-width lowercase-index-width titlecase-index-width)
  (lookup-by-offset-index
   code-point
   (bitwise-and (- (arithmetic-shift 1 lowercase-index-width) 1)
		(arithmetic-shift encoding
				  (- (+ titlecase-index-width *general-category-bits*))))
   lowercase-offsets))

(define (code-point-encoding-titlecase-code-point code-point encoding
						  titlecase-offsets
						  uppercase-index-width lowercase-index-width titlecase-index-width)
  (lookup-by-offset-index
   code-point
   (bitwise-and (- (arithmetic-shift 1 titlecase-index-width) 1)
		(arithmetic-shift encoding (- *general-category-bits*)))
   titlecase-offsets))

(define *code-point-encoding-general-category-mask*
  (- (arithmetic-shift 1 *general-category-bits*) 1))

(define (code-point-encoding-general-category encoding)
  (vector-ref general-categories
	      (bitwise-and encoding *code-point-encoding-general-category-mask*)))
						  

(define (max-code-point infos)
  (let loop ((max 0) (infos infos))
    (cond
     ((null? infos) max)
     ((> (code-point-info-code-point (car infos))
	 max)
      (loop (code-point-info-code-point (car infos)) (cdr infos)))
     (else (loop max (cdr infos))))))

; returns a THUNK that will return for each code-point in sequence
; (PROC <code-point>) or DEFAULT if there's no info.

; assumes INFOS are sorted

(define (make-consecutive-info-source source make-default proc)
  (let ((next-info #f)
	(last-code-point -1))
    (lambda ()

      (define (upto info)
	(if (< last-code-point (code-point-info-code-point info))
	    (begin
	      (set! next-info info)
	      (proc (make-default last-code-point)))
	    (begin
	      (set! next-info #f)
	      ;; scalar values only
	      (if (eq? (code-point-info-general-category info)
		       (general-category surrogate))
		  (proc (make-default last-code-point))
		  (proc info)))))

      (set! last-code-point (+ 1 last-code-point))

      (cond
       ((or next-info (source)) => upto)
       (else #f)))))

; Dealing with PropList.txt

(define (parse-proplist-for-upper/lowercase filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((uppercase (make-integer-table)) (lowercase (make-integer-table)))
	(let loop ()
	  (let ((thing (read-line port)))
	    (if (eof-object? thing)
		(values uppercase lowercase)
		(call-with-values
		    (lambda ()
		      (extract-upper/lowercase thing))
		  (lambda (uppers lowers)
		    (for-each (lambda (u)
				(table-set! uppercase u #t))
			      uppers)
		    (for-each (lambda (l)
				(table-set! lowercase l #t))
			      lowers)
		    (loop))))))))))

(define (extract-upper/lowercase line)
  (cond
   ((string-prefix? "#" line)
    (values '() '()))
   ((string-contains line "Other_Uppercase")
    (values (proplist-line-range line)
	    '()))
   ((string-contains line "Other_Lowercase")
    (values '()
	    (proplist-line-range line)))
   (else
    (values '() '()))))

(define (proplist-line-range line)
  (let* ((i1 (string-skip line char-set:hex-digit))
	 (first (string->number (substring line 0 i1) 16)))
    (if (char=? #\. (string-ref line i1))
	(let* ((i2 (string-skip line #\. i1))
	       (i3 (string-skip line char-set:hex-digit i2))
	       (last (string->number (substring line i2 i3) 16)))
	  (let loop ((last last) (range '()))
	    (if (= last first)
		(cons last range)
		(loop (- last 1) (cons last range)))))
	(list first))))

; assumes START points to whitespace or the first digit
; returns list of scalar values + position after sequence
; (possibly after trailing semicolon)
(define (parse-scalar-values s start)
  (let ((size (string-length s)))
    (let loop ((start start) (rev-values '()))
      (let ((i1 (string-skip s char-set:whitespace start)))
	(cond
	 ((not i1)
	  (values (reverse rev-values) (+ start 1)))
	 ((char=? #\; (string-ref s i1))
	  (values (reverse rev-values) (+ i1 1)))
	 (else
	  (let* ((i2 (or (string-skip s char-set:hex-digit i1)
			 size))
		 (n (string->number (substring s i1 i2) 16)))
	    (loop i2 (cons n rev-values)))))))))

(define-record-type specialcasing :specialcasing
  (make-specialcasing scalar-value
		      lowercase titlecase uppercase foldcase
		      final-sigma?)
  specialcasing?
  (scalar-value specialcasing-scalar-value)
  (lowercase specialcasing-lowercase)
  (titlecase specialcasing-titlecase)
  (uppercase specialcasing-uppercase)
  ;; This will actually come from CaseFolding.txt
  (foldcase specialcasing-foldcase set-specialcasing-foldcase!)
  (final-sigma? specialcasing-final-sigma?))

(define (parse-specialcasing-line line)
  (let* ((i1 (string-skip line char-set:hex-digit 0))
	 (n (string->number (substring line 0 i1) 16)))
    (call-with-values
	(lambda () (parse-scalar-values line (+ 1 i1)))
      (lambda (lowercase i2)
	(call-with-values
	    (lambda () (parse-scalar-values line i2))
	  (lambda (titlecase i3)
	    (call-with-values
		(lambda () (parse-scalar-values line i3))
	      (lambda (uppercase i4)
		(let ((i5 (or (string-index line #\; (+ 1 i4))
			      (string-index line #\# (+ 1 i4))
			      (string-length line))))
		  (let ((conditions (string-trim-both (substring line i4 i5))))
		    (if (or (string=? "" conditions)
			    (string=? "Final_Sigma" conditions))
			(make-specialcasing n
					    lowercase titlecase uppercase #f
					    (string=? conditions "Final_Sigma"))
			#f)))))))))))

(define (parse-specialcasing filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((specialcasings '()))
	(let ((thing (read-line port)))
	  (if (eof-object? thing)
	      specialcasings
	      (cond
	       ((and (not (string=? "" thing))
		     (not (char=? #\# (string-ref thing 0)))
		     (parse-specialcasing-line thing))
		=> (lambda (sc)
		     (loop (cons sc specialcasings))))
	       (else (loop specialcasings)))))))))

; we only extract the common and full case foldings
(define (parse-casefolding-line line)
  (let* ((i1 (string-skip line char-set:hex-digit 0))
	 (n (string->number (substring line 0 i1) 16))
	 (i2 (string-skip line char-set:whitespace (+ 1 i1)))
	 (status (string-ref line i2)))
    (call-with-values
	(lambda ()
	  (parse-scalar-values line (+ 2 i2)))
      (lambda (scalar-values i)
	(cond
	 ((or (char=? status #\C)
	      (char=? status #\F))
	  (cons n (cons status scalar-values)))
	 ((> (length scalar-values) 1)
	  (error "multi-character common case-folding mapping"))
	 (else #f))))))

(define (parse-casefolding filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((casefoldings '()))
	(let ((thing (read-line port)))
	  (cond
	   ((eof-object? thing) casefoldings)
	   ((and (not (string=? "" thing))
		 (not (char=? #\# (string-ref thing 0)))
		 (parse-casefolding-line thing))
	    => (lambda (folding)
		 (loop (cons folding casefoldings))))
	   (else (loop casefoldings))))))))

(define (merge-specialcasings+casefoldings! specialcasings casefoldings)
  (for-each
   (lambda (casefolding)
     (let ((sv (car casefolding))
	   (status (cadr casefolding))
	   (folding (cddr casefolding)))
       (cond
	((find (lambda (specialcasing)
		 (= (specialcasing-scalar-value specialcasing) sv))
	       specialcasings)
	 => (lambda (specialcasing)
	      (set-specialcasing-foldcase! specialcasing folding)))
	((char=? status #\F) ; the others will be covered by UnicodeData.txt
	 (let ((sv-list (list sv)))
	   (set! specialcasings
		 (cons
		  (make-specialcasing sv
				      sv-list sv-list sv-list
				      folding
				      #f)
		  specialcasings)))))))
   casefoldings)
  specialcasings)

(define (parse-specialcasing+casefolding specialcasing-filename casefolding-filename)
  (let ((specialcasings (parse-specialcasing specialcasing-filename))
	(casefoldings (parse-casefolding casefolding-filename)))
    (merge-specialcasings+casefoldings! specialcasings casefoldings)))

(define (list-prefix? l1 l2)
  (let loop ((l1 l1) (l2 l2))
    (cond
     ((null? l1) #t)
     ((null? l2) #f)
     ((equal? (car l1) (car l2))
      (loop (cdr l1) (cdr l2)))
     (else #f))))

; We return two lists: a list of :SPECIALCASING records where the
; xxxCASE fields are replaced by (offset . length) pairs into the
; second list, which contains all the case mappings jumbled together.

(define (specialcasing-encoding specialcasings)
  (let ((casings '()))
    
    (define (add-casing! l)
      (let loop ((rest casings)
		 (index 0))
	(cond
	 ((null? rest)
	  (set! casings (append casings l))
	  index)
	 ((list-prefix? l rest)
	  index)
	 (else
	  (loop (cdr rest) (+ 1 index))))))

    (define (transform-specialcasing s)
      (let ((lowercase (cons (add-casing! (specialcasing-lowercase s))
			     (length  (specialcasing-lowercase s))))
	    (titlecase (cons (add-casing! (specialcasing-titlecase s))
			     (length  (specialcasing-titlecase s))))
	    (uppercase (cons (add-casing! (specialcasing-uppercase s))
			     (length  (specialcasing-uppercase s))))
	    (foldcase (cons (add-casing! (specialcasing-foldcase s))
			    (length (specialcasing-foldcase s)))))
	(make-specialcasing (specialcasing-scalar-value s)
			    lowercase titlecase uppercase foldcase
			    (specialcasing-final-sigma? s))))
    
    (let ((transformed
	   (map transform-specialcasing specialcasings)))
      (values transformed
	      casings))))

(define (specialcasing-encoding-ref casings offset size)
  (let loop ((i 0) (r '()))
    (if (>= i size)
	(reverse r)
	(loop (+ 1 i)
	      (cons (vector-ref casings (+ offset i))
		    r)))))

; for testing
(define (check-specialcasing-encodings specialcasings)
  (call-with-values
      (lambda () (specialcasing-encoding specialcasings))
    (lambda (encodings casings)
      (let ((casings (list->vector casings)))
	(for-each
	 (lambda (specialcasing encoding)
	 
	   (define (check select)
	     (let ((pair (select encoding))
		   (reference (select specialcasing)))
	       (if (not
		    (equal? reference
			    (specialcasing-encoding-ref casings 
							(car pair) (cdr pair))))
		   (error "encoding failure" encoding
			  reference (specialcasing-encoding-ref casings
								(car pair) (cdr pair))))))

	   (check specialcasing-lowercase)
	   (check specialcasing-uppercase)
	   (check specialcasing-titlecase)
	   (check specialcasing-foldcase))
	 specialcasings encodings)))))

(define (specialcasings->table specialcasings)
  (let ((table (make-integer-table)))
    (for-each (lambda (s)
		(table-set! table (specialcasing-scalar-value s)
			    s))
	      specialcasings)
    table))

(define (make-scalar-value-case+general-category-encoding-tables
	 infos 
	 special-lowercase-table special-uppercase-table
	 specialcasings)

  (let ((uppercase-offsets (mapping-offsets infos code-point-info-uppercase-code-point))
	(lowercase-offsets (mapping-offsets infos code-point-info-lowercase-code-point))
	(titlecase-offsets (mapping-offsets infos code-point-info-titlecase-code-point)))

    (let ((uppercase-index-width (bits-necessary (vector-length uppercase-offsets)))
	  (lowercase-index-width (bits-necessary (vector-length lowercase-offsets)))
	  (titlecase-index-width (bits-necessary (vector-length titlecase-offsets)))

	  (specialcasings-table (specialcasings->table specialcasings))
	  
	  (block-size (expt 2 *block-bits*)))

      (call-with-values
	  (lambda ()
	    (compute-compact-table
	     (make-consecutive-info-source
	      (expanded-code-point-info-source infos)
	      (lambda (code-point)
		(make-code-point-info code-point
				      "<unassigned>"
				      (general-category unassigned)
				      #f #f #f #f #f #f #f #f #f #f
				      code-point code-point code-point))
	      (lambda (info)
		(code-point-info->case+general-category-encoding
		 info
		 (table-ref specialcasings-table
			    (code-point-info-code-point info))
		 special-lowercase-table special-uppercase-table
		 uppercase-offsets lowercase-offsets titlecase-offsets
		 uppercase-index-width lowercase-index-width titlecase-index-width)))
	     block-size))
	(lambda (indices encodings)
	  (values indices encodings
		  uppercase-offsets lowercase-offsets titlecase-offsets))))))

; saves a couple of kilobyes, but probably not worthwhile

(define (write-vector-code/rll name vector port)
  (write `(define ,name (make-vector ,(vector-length vector)))
	 port)
  (newline port)
  (let loop ((values (vector->list vector))
	     (index 0))
    (cond
     ((null? values))
     ((or (null? (cdr values))
	  (not (equal? (car values) (cadr values))))
      (write `(vector-set! ,name ,index ,(car values))
	     port)
      (newline port)
      (loop (cdr values) (+ 1 index)))
     (else
      (let ((value (car values)))
	(let inner-loop ((values values)
			 (last-index index))
	  (cond
	   ((or (null? values)
	       (not (equal? (car values) value)))
	    (write
	     `(do ((i ,index (+ 1 i)))
		  ((>= i ,last-index))
		(vector-set! ,name i ,value))
	     port)
	    (newline port)
	    (loop values last-index))
	   (else
	    (inner-loop (cdr values) (+ 1 last-index))))))))))

(define (create-unicode-tables unicode-data-filename
			       proplist-filename
			       specialcasing-filename
			       casefolding-filename
			       composition-exclusions-filename
			       category-output-file
			       syntax-info-output-file
			       normalization-output-file
			       srfi-14-base-output-file)
  (let ((infos (parse-unicode-data unicode-data-filename))
	(specialcasings (parse-specialcasing+casefolding specialcasing-filename
							 casefolding-filename)))
    (call-with-values
	(lambda ()
	  (parse-proplist-for-upper/lowercase proplist-filename))
      (lambda (special-uppercase-table special-lowercase-table)
	(call-with-output-file category-output-file
	  (lambda (port)
	    (display "; Automatically generated by WRITE-UNICODE-CATEGORY-TABLES; do not edit."
		     port)
	    (newline port)
	    (newline port)
	    (write-unicode-category-tables infos
					   special-uppercase-table special-lowercase-table 
					   specialcasings
					   port)
	    (write-specialcasings-tables specialcasings port)))

	(call-with-output-file syntax-info-output-file
	  (lambda (port)
	    (display "; Automatically generated by WRITE-UNICODE-CATEGORY-TABLES; do not edit."
		     port)
	    (newline port)
	    (newline port)
	    (write-syntax-info infos port)
	    (newline port)))

	(write-srfi-14-base-char-sets infos srfi-14-base-output-file)
	(call-with-output-file normalization-output-file
	  (lambda (port)
	    (display "; Automatically generated by WRITE-UNICODE-CATEGORY-TABLES; do not edit."
		     port)
	    (newline port)
	    (newline port)
	    (write-normalization-tables
	     infos
	     (parse-composition-exclusions composition-exclusions-filename)
	     port)))))))
    
(define *block-bits* 8)			; better than 9, at least

(define (write-unicode-category-tables infos 
				       special-uppercase-table special-lowercase-table
				       specialcasings
				       port)
  (call-with-values
      (lambda ()
	(make-scalar-value-case+general-category-encoding-tables
	 infos
	 special-lowercase-table special-uppercase-table
	 specialcasings))
    (lambda (indices
	     encodings
	     uppercase-offsets lowercase-offsets titlecase-offsets)

      (write `(define *encoding-table-block-bits* ,*block-bits*)
	     port)
      (newline port)
      (newline port)

      (write `(define *uppercase-index-width*
		,(bits-necessary (vector-length uppercase-offsets)))
	     port)
      (newline port)
      (write `(define *lowercase-index-width*
		,(bits-necessary (vector-length lowercase-offsets)))
	     port)
      (newline port)
      (write `(define *titlecase-index-width*
		,(bits-necessary (vector-length titlecase-offsets)))
	     port)
      (newline port)
      (newline port)

      (write `(define *scalar-value-info-indices* ',indices)
	     port)
      (newline port)
      (write `(define *scalar-value-info-encodings* ',encodings)
	     port)
      (newline port)
      (newline port)

      (write `(define *uppercase-offsets* ',uppercase-offsets)
	     port)
      (newline port)
      (write `(define *lowercase-offsets* ',lowercase-offsets)
	     port)
      (newline port)
      (write `(define *titlecase-offsets* ',titlecase-offsets)
	     port)
      (newline port)
      (newline port))))

(define (write-specialcasings-tables specialcasings port)
  (call-with-values
      (lambda () (specialcasing-encoding specialcasings))
    (lambda (encodings casings)

      ;; we write it out here to avoid introducing yet another file
      ;; into the UNICODE-CHAR-MAPS package
      (write
       '(define-record-type specialcasing :specialcasing
	  (make-specialcasing scalar-value
			      lowercase-start lowercase-length
			      titlecase-start titlecase-length
			      uppercase-start uppercase-length
			      foldcase-start foldcase-length
			      final-sigma?)
	  specialcasing?
	  (scalar-value specialcasing-scalar-value)
	  (lowercase-start specialcasing-lowercase-start)
	  (lowercase-length specialcasing-lowercase-length)
	  (titlecase-start specialcasing-titlecase-start)
	  (titlecase-length specialcasing-titlecase-length)
	  (uppercase-start specialcasing-uppercase-start)
	  (uppercase-length specialcasing-uppercase-length)
	  (foldcase-start specialcasing-foldcase-start)
	  (foldcase-length specialcasing-foldcase-length)
	  (final-sigma? specialcasing-final-sigma?))
       port)
      (newline port)
      (newline port)

      (write `(define *specialcasing-table* (make-integer-table)) port)
      (newline port)
      (newline port)
      
      (for-each
       (lambda (c)
	 (write
	  `(table-set! *specialcasing-table*
		       ,(specialcasing-scalar-value c)
		       (make-specialcasing
			,(specialcasing-scalar-value c)
			,(car (specialcasing-lowercase c))
			,(cdr (specialcasing-lowercase c))
			,(car (specialcasing-titlecase c))
			,(cdr (specialcasing-titlecase c))
			,(car (specialcasing-uppercase c))
			,(cdr (specialcasing-uppercase c))
			,(car (specialcasing-foldcase c))
			,(cdr (specialcasing-foldcase c))
			,(specialcasing-final-sigma? c)))
	  port)
	 (newline port))
       encodings)

      (newline port)

      (write `(define *specialcasings* (list->string (map scalar-value->char ',casings))) port)
      (newline port)
      (newline port))))

;; Read syntax

(define (write-syntax-info infos port)
  (write `(define *non-symbol-constituents-above-127*
	    ',(list->vector (non-symbol-constituents-above-127 infos)))
	 port)
  (newline port)
  (newline port)
  (write `(define *whitespaces*
	    ',(list->vector (whitespaces infos)))
	   port)
  (newline port))
  
(define *symbol-constituent-general-categories*
  (list (general-category uppercase-letter)
	(general-category lowercase-letter)
	(general-category titlecase-letter)
	(general-category modified-letter)
	(general-category other-letter)
	(general-category non-spacing-mark)
	(general-category combining-spacing-mark)
	(general-category enclosing-mark)
	(general-category decimal-digit-number)
	(general-category letter-number)
	(general-category other-number)
	(general-category dash-punctuation)
	(general-category connector-punctuation)
	(general-category other-punctuation)
	(general-category currency-symbol)
	(general-category mathematical-symbol)
	(general-category modifier-symbol)
	(general-category other-symbol)
	(general-category private-use-character)))

(define (symbol-constituent-above-127? info)
  (memq (code-point-info-general-category info)
	*symbol-constituent-general-categories*))

(define (non-symbol-constituents-above-127 infos)
  (let ((reverse-non-constituents '()))
    (for-each-expanded-code-point-info
     (lambda (info)
       (let ((cp (code-point-info-code-point info)))
	 (if (and (> cp 127)
		  (not (eq? (general-category surrogate)
			    (code-point-info-general-category info)))
		  (not (symbol-constituent-above-127? info)))
	     (set! reverse-non-constituents
		   (cons cp reverse-non-constituents)))))
     infos)
    (reverse reverse-non-constituents)))

(define (whitespaces infos)
  (let ((reverse-whitespaces '()))
    (for-each-expanded-code-point-info
     (lambda (info)
       (if (eq? (general-category-primary-category
		 (code-point-info-general-category info))
		(primary-category separator))
	   (set! reverse-whitespaces
		 (cons (code-point-info-code-point info)
		       reverse-whitespaces))))
     infos)
    (sort-list (append '(#x009 #x00a #x00b #x00c #x00d #x085)
		       reverse-whitespaces)
	       <)))

(define (write-srfi-14-base-char-sets infos output-file)
  (call-with-output-file output-file
    (lambda (port)

      (display "; Automatically generated by WRITE-SRFI-14-BASE-CHAR-SETS; do not edit."
	       port)
      (newline port)
      (newline port)

      (let-syntax
	  ((general-category-predicate
	    (syntax-rules ()
	      ((general-category-predicate ?name)
	       (lambda (info)
		 (eq? (code-point-info-general-category info)
		      (general-category ?name))))))
	   (primary-category-predicate
	    (syntax-rules ()
	      ((primary-category-predicate ?name)
	       (lambda (info)
		 (eq? (general-category-primary-category
		       (code-point-info-general-category info))
		      (primary-category ?name)))))))

	(write-srfi-14-base-char-set-definition
	 'char-set:lower-case
	 srfi-14-lower-case?
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:upper-case
	 srfi-14-upper-case?
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:title-case
	 (general-category-predicate titlecase-letter)
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:letter
	 (primary-category-predicate letter)
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:digit
	 (general-category-predicate decimal-digit-number)
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:mark
	 (primary-category-predicate mark)
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:separator
	 (primary-category-predicate separator)
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:punctuation
	 (primary-category-predicate punctuation)
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:symbol
	 (primary-category-predicate symbol)
	 infos port)
	(write-srfi-14-base-char-set-definition
	 'char-set:space-separator
	 (general-category-predicate space-separator)
	 infos port)))))

; SRFI 14 has funny notions of lower case and upper case

(define (srfi-14-lower-case? info)
  (let ((cp (code-point-info-code-point info)))
    (and (not (and (>= cp #x2000)
		   (<= cp #x2fff)))
	 (= cp (code-point-info-lowercase-code-point info))
	 (or (not (= cp (code-point-info-uppercase-code-point info)))
	     (string-contains (code-point-info-name info)
			      "SMALL LETTER")
	     (string-contains (code-point-info-name info)
			      "SMALL LIGATURE")))))

(define (srfi-14-upper-case? info)
  (let ((cp (code-point-info-code-point info)))
    (and (not (and (>= cp #x2000)
		   (<= cp #x2fff)))
	 (= cp (code-point-info-uppercase-code-point info))
	 (or (not (= cp (code-point-info-lowercase-code-point info)))
	     (string-contains (code-point-info-name info)
			      "CAPITAL LETTER")
	     (string-contains (code-point-info-name info)
			      "CAPITAL LIGATURE")))))

(define (write-srfi-14-base-char-set-definition name pred infos port)
  (write (srfi-14-base-char-set-definition name pred infos)
	 port)
  (newline port))

(define (ranges->range-vector ranges)
  (let* ((range-count (length ranges))
	 (range-vector (make-vector (* 2 (length ranges)))))

    (let loop ((i 0) (ranges ranges))
      (if (< i range-count)
	  (begin
	    (vector-set! range-vector (* 2 i) (caar ranges))
	    (vector-set! range-vector (+ 1 (* 2 i)) (cdar ranges))
	    (loop (+ 1 i) (cdr ranges)))))
    range-vector))

(define (srfi-14-base-char-set-definition name pred infos)
  (let ((accumulator (make-ranges-accumulator pred)))
    (for-each-expanded-code-point-info accumulator infos)
    `(define ,name
       (range-vector->char-set
	',(ranges->range-vector (accumulator 'ranges))))))

(define (make-ranges-accumulator pred)
  (let ((rev-ranges '())
	(current-left #f)
	(current-right #f))
    ;; assumes the characters arrive with ascending scalar values
    (lambda (message)
      (cond
       ((not (code-point-info? message))
	(if current-left
	    (reverse (cons (cons current-left current-right)
			   rev-ranges))
	    (reverse rev-ranges)))
       ((pred message)
	(let ((scalar-value (code-point-info-code-point message)))
	  (cond
	   ((not current-left)
	    (set! current-left scalar-value)
	    (set! current-right (+ 1 scalar-value)))
	   ((= scalar-value current-right)
	    (set! current-right (+ 1 current-right)))
	   (else
	    (set! rev-ranges
		  (cons (cons current-left current-right)
			rev-ranges))
	    (set! current-left scalar-value)
	    (set! current-right (+ 1 scalar-value))))))))))


(define (write-normalization-tables infos excluded port)
  (call-with-values
      (lambda ()
	(make-normalization-encoding-tables infos))
    (lambda (indices encodings)
      (write `(define *normalization-info-block-bits* ,*block-bits*)
	     port)
      (newline port)
      (write `(define *normalization-info-indices* ',indices)
	     port)
      (newline port)
      (write `(define *normalization-info-encodings* ',encodings)
	     port)
      (newline port)))

  (newline port)

  (let ((canonical-pairs (canonical-decomposition-pairs infos)))
    (write `(define *canonical-decomposition-scalar-values*
	      ',(list->vector (map car canonical-pairs)))
	   port)
    (newline port)
    (write `(define *canonical-decompositions*
	      ',(list->vector (map cdr canonical-pairs)))
	   port)
    (newline port))

  (newline port)
      
  (call-with-values
      (lambda ()
	(compatibility-decomposition-tables infos))
    (lambda (decompositions scalar-values indices)
      (write `(define *compatibility-decompositions* ',decompositions)
	     port)
      (newline port)
      (write `(define *compatibility-scalar-values* ',scalar-values)
	     port)
      (newline port)
      (write `(define *compatibility-indices* ',indices)
	     port)
      (newline port)))

  (newline port)

  (let ((composition-pairs (composition-pairs infos excluded)))
    (write `(define *composition-scalar-values*
	      ',(list->vector (map car composition-pairs)))
	   port)
    (newline port)
    (write `(define *composition-encodings*
	      ',(list->vector (map cdr composition-pairs)))
	   port)
    (newline port)))

(define (parse-composition-exclusions filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((exclusions '()))
	(let ((thing (read-line port)))
	  (cond
	   ((eof-object? thing) exclusions)
	   ((and (not (string=? "" thing))
		 (not (char=? #\# (string-ref thing 0))))
	    (let ((end (or (string-skip thing char-set:hex-digit)
			   (string-length thing))))
	      (loop
	       (cons (string->number (substring thing 0 end) 16)
		     exclusions))))
	   (else (loop exclusions))))))))

(define (make-normalization-encoding-tables infos)
  (compute-compact-table
   (make-consecutive-info-source
    (expanded-code-point-info-source infos)
    (lambda (code-point)
      (make-code-point-info code-point
			    "<unassigned>"
			    (general-category unassigned)
			    0 #f #f #f #f #f #f #f #f #f
			    code-point code-point code-point))
    (lambda (info)
      (bitwise-ior (code-point-info-combining-class info) ; 0..240
		   (if (code-point-info-canonical-decomposition info)
		       #x100
		       0)
		   (if (code-point-info-compatibility-decomposition info)
		       #x200
		       0))))
   (expt 2 *block-bits*)))

(define (encode-canonical-decomposition l)
  (cond
   ((null? (cdr l))
    (if (> (car l) #xffff)
	l
	(car l)))
   (else
    (let ((a (car l))
	  (b (cadr l)))
      (if (or (> a #xffff)
	      (> b #xffff))
	  (cons a b)
	  (bitwise-ior (arithmetic-shift b 16) a))))))

;; generate an alist that maps scalar values to decomposition encodings
(define (canonical-decomposition-pairs infos)
  (let ((pairs '()))
    (for-each-expanded-code-point-info
     (lambda (info)
       (cond
	((code-point-info-canonical-decomposition info)
	 => (lambda (d)
	      (set! pairs
		    (cons
		     (cons (code-point-info-code-point info)
			   (encode-canonical-decomposition d))
		     pairs))))))
     infos)
    (reverse pairs)))

(define (compatibility-decomposition-tables infos)
  (let ((reverse-decomps '())
	(decomp-index 0)
	(rev-infos '()))
    (for-each-expanded-code-point-info
     (lambda (info)
       (cond
	((code-point-info-compatibility-decomposition info)
	 => (lambda (d)
	      (let ((size (length d)))
		(set! reverse-decomps
		      (append (reverse d) reverse-decomps))
		(set! rev-infos
		      (cons (cons (code-point-info-code-point info)
				  decomp-index)
			    rev-infos))
		(set! decomp-index (+ decomp-index size)))))))
     infos)
    (let ((decomps (list->vector (reverse reverse-decomps))))
      (values decomps
	      (list->vector (map car (reverse rev-infos)))
	      (list->vector 
	       (map cdr (reverse (cons (cons #f (vector-length decomps)) rev-infos))))))))

(define (composition-pairs infos excluded)
  (let ((pairs '()))
    (for-each-expanded-code-point-info
     (lambda (info)
       (cond
	((code-point-info-canonical-decomposition info)
	 => (lambda (d)
	      (if (and (pair? (cdr d)) ; not a singleton
		       (not (member (code-point-info-code-point info) excluded))
		       (code-point-info-combining-class
			(find-code-point-info (car d) infos))) ; possibly expensive
		  (set! pairs
			(cons (cons (code-point-info-code-point info)
				    (encode-composition d))
			      pairs)))))))
     infos)

    (sort-list pairs
	       (lambda (p1 p2)
		 (< (cdr p1) (cdr p2))))))

(define (encode-composition l)
  (if (or (> (car l) #xffff)
	  (> (cadr l) #xffff))
      (error "non-BMP composition"))
  (bitwise-ior (arithmetic-shift (cadr l) 16)
	       (car l)))

; for debugging

(define (test-code-point-case+general-category-encoding-tables
	 infos special-uppercase-table special-lowercase-table
	 specialcasings
	 indices encodings
	 uppercase-offsets lowercase-offsets titlecase-offsets)

  (let ((lower-mask (- (arithmetic-shift 1 *block-bits*) 1))
	(uppercase-index-width (bits-necessary (vector-length uppercase-offsets)))
	(lowercase-index-width (bits-necessary (vector-length lowercase-offsets)))
	(titlecase-index-width (bits-necessary (vector-length titlecase-offsets))))

    (for-each-expanded-code-point-info
     (lambda (info)
       (let* ((code-point (code-point-info-code-point info))
	      (base-index (vector-ref indices
				      (arithmetic-shift code-point (- *block-bits*))))
	      (index (+ base-index (bitwise-and code-point lower-mask)))
	      (encoding (vector-ref encodings index)))

	 (if (not (eq? (code-point-info-general-category info)
		       (general-category surrogate)))
	     (begin

	       (if (not (eq? (code-point-info-general-category info)
			     (code-point-encoding-general-category encoding)))
		   (error "general category mismatch"
			  info
			  (code-point-encoding-general-category encoding)))

	       (let ((uppercase-code-point
		      (code-point-encoding-uppercase-code-point
		       code-point encoding
		       uppercase-offsets
		       uppercase-index-width lowercase-index-width titlecase-index-width))
		     (lowercase-code-point
		      (code-point-encoding-lowercase-code-point
		       code-point encoding
		       lowercase-offsets
		       uppercase-index-width lowercase-index-width titlecase-index-width))
		     (titlecase-code-point
		      (code-point-encoding-titlecase-code-point
		       code-point encoding
		       titlecase-offsets
		       uppercase-index-width lowercase-index-width titlecase-index-width))
		     (uppercase?
		      (code-point-encoding-uppercase?
		       encoding
		       uppercase-index-width lowercase-index-width titlecase-index-width))
		     (lowercase?
		      (code-point-encoding-lowercase?
		       encoding
		       uppercase-index-width lowercase-index-width titlecase-index-width)))
		      

		 (if (not (= (code-point-info-uppercase-code-point info)
			     uppercase-code-point))
		     (error "uppercase mismatch" info uppercase-code-point))

		 (if (not (= (code-point-info-lowercase-code-point info)
			     lowercase-code-point))
		     (error "lowercase mismatch" info lowercase-code-point))
	   
		 (if (not (= (code-point-info-titlecase-code-point info)
			     titlecase-code-point))
		     (error "titlecase mismatch" info titlecase-code-point))

		 (if (not (eq? (or (table-ref special-uppercase-table code-point)
				   (eq? (code-point-info-general-category info)
					(general-category uppercase-letter)))
			       uppercase?))
		     (error "uppercase? mismatch" info code-point))
		 (if (not (eq? (or (table-ref special-lowercase-table code-point)
				   (eq? (code-point-info-general-category info)
					(general-category lowercase-letter)))
			       lowercase?))
		     (error "lowercase? mismatch" info code-point))
		 )))))
     infos)))

(define (check-unicode-tables unicode-data-filename
			      proplist-filename
			      specialcasing-filename)
  (let ((infos (parse-unicode-data unicode-data-filename))
	(specialcasings (parse-specialcasing specialcasing-filename)))
    (call-with-values
	(lambda ()
	  (parse-proplist-for-upper/lowercase proplist-filename))
      (lambda (special-uppercase-table special-lowercase-table)
	(call-with-values
	    (lambda ()
	      (make-scalar-value-case+general-category-encoding-tables
	       infos
	       special-lowercase-table special-uppercase-table
	       specialcasings))
	  (lambda (indices
		   encodings
		   uppercase-offsets lowercase-offsets titlecase-offsets)
	    (test-code-point-case+general-category-encoding-tables
	     infos special-uppercase-table special-lowercase-table
	     specialcasings
	     indices encodings
	     uppercase-offsets lowercase-offsets titlecase-offsets)))))))

(define (find-code-point-info code-point infos)
  (call-with-current-continuation
   (lambda (return)
     (for-each-expanded-code-point-info
      (lambda (info)
	(if (= code-point (code-point-info-code-point info))
	    (return info)))
      infos))))
