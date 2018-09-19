; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber, Olin Shivers

; SRFI 14 test suite

;; adapted from Olin's test suite
(define (vowel? c)
  (member c '(#\a #\e #\i #\o #\u)))

(define-test-suite srfi-14-tests)

(define-test-case char-set? srfi-14-tests
  (check (not (char-set? 5)))
  (check (char-set? (char-set #\a #\e #\i #\o #\u))))

(define-test-case char-set= srfi-14-tests
  (check (char-set=))
  (check (char-set= (char-set)))
  (check (string->char-set "ioeauaiii")
	 (=> char-set=)
	 (char-set #\a #\e #\i #\o #\u))
  (check (not (char-set= (string->char-set "ioeauaiii")
			 (char-set #\e #\i #\o #\u)))))

(define-test-case char-set<= srfi-14-tests
  (check (char-set<=))
  (check (char-set<= (char-set)))
  (check (char-set<= (char-set #\a #\e #\i #\o #\u)
		     (string->char-set "ioeauaiii")))
  (check (char-set<= (char-set #\e #\i #\o #\u)
		     (string->char-set "ioeauaiii"))))

(define-test-case char-set-hash srfi-14-tests
  (check-that (char-set-hash char-set:graphic 100)
	      (all-of (is (lambda (x) (>= x 0)))
		      (is (lambda (x) (<= x 99))))))

(define-test-case char-set-fold srfi-14-tests
  (check (char-set-fold (lambda (c i) (+ i 1)) 0
			(char-set #\e #\i #\o #\u #\e #\e))
	 => 4))

; The following test is ASCII/Latin-1 only, and fails with Unicode
; (char-set= (string->char-set "eiaou2468013579999")
;            (char-set-unfold null? car cdr '(#\a #\e #\i #\o #\u #\u #\u)
;                             char-set:digit))

(define-test-case char-set-unfold srfi-14-tests
  (check-that (char-set-unfold null? car cdr '(#\a #\e #\i #\o #\u)
			  (string->char-set "0123456789"))
	      (is char-set=
		  (string->char-set "eiaou246801357999"))))

(define-test-case char-set-unfold! srfi-14-tests
  (check-that (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
				(string->char-set "0123456789"))
	      (opposite (is char-set= 
			    (string->char-set "eiaou246801357")))))

(define-test-case char-set-for-each srfi-14-tests
  (let ((cs (string->char-set "0123456789")))
    (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
		       (string->char-set "02468000"))
    (check-that cs (is char-set= (string->char-set "97531"))))

  (let ((cs (string->char-set "0123456789")))
    (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
		       (string->char-set "02468"))
    (check-that cs (opposite (is char-set= (string->char-set "7531"))))))

(define-test-case char-set-map srfi-14-tests
  (check-that (char-set-map char-upcase (string->char-set "aeiou"))
	      (is char-set=
		  (string->char-set "IOUAEEEE")))
  (check-that (char-set-map char-upcase (string->char-set "aeiou"))
	      (opposite (is char-set= 
			    (string->char-set "OUAEEEE")))))

(define-test-case char-set-copy srfi-14-tests
  (check-that (char-set-copy (string->char-set "aeiou"))
	      (is char-set= (string->char-set "aeiou"))))

(define-test-case char-set srfi-14-tests
  (check-that (char-set #\x #\y) (is char-set= (string->char-set "xy")))
  (check-that (char-set #\x #\y #\z) (opposite (is char-set= (string->char-set "xy")))))

(define-test-case list->char-set srfi-14-tests
  (check-that (list->char-set '(#\x #\y)) (is char-set= (string->char-set "xy")))
  (check-that (list->char-set '(#\x #\y)) (opposite (is char-set= (string->char-set "axy"))))
  (check-that (list->char-set '(#\x #\y) (string->char-set "12345"))
	      (is char-set= (string->char-set "xy12345")))
  (check-that (list->char-set '(#\x #\y) (string->char-set "12345"))
	      (opposite (is char-set= (string->char-set "y12345")))))

(define-test-case list->char-set! srfi-14-tests
  (check-that (list->char-set! '(#\x #\y) (string->char-set "12345"))
	      (is char-set= (string->char-set "xy12345")))
  (check-that (list->char-set! '(#\x #\y) (string->char-set "12345"))
	      (opposite (is char-set= (string->char-set "y12345")))))

(define-test-case char-set-filter srfi-14-tests
  (check-that (char-set-filter vowel? char-set:ascii (string->char-set "12345"))
	      (is char-set= (string->char-set "aeiou12345")))
  (check-that (char-set-filter vowel? char-set:ascii (string->char-set "12345"))
	      (opposite (is char-set= (string->char-set "aeou12345")))))

(define-test-case char-set-filter! srfi-14-tests
  (check-that (char-set-filter! vowel? char-set:ascii (string->char-set "12345"))
	      (is char-set= (string->char-set "aeiou12345")))
  (check-that (char-set-filter! vowel? char-set:ascii (string->char-set "12345"))
	      (opposite (is char-set= (string->char-set "aeou12345")))))

(define-test-case ucs-range->char-set srfi-14-tests
  (check-that (ucs-range->char-set 97 103 #t (string->char-set "12345"))
	      (is char-set= (string->char-set "abcdef12345")))
  (check-that (ucs-range->char-set 97 103 #t (string->char-set "12345"))
	      (opposite (is char-set= (string->char-set "abcef12345")))))

(define-test-case ucs-range_>char-set! srfi-14-tests
  (check-that (ucs-range->char-set! 97 103 #t (string->char-set "12345"))
	      (is char-set= (string->char-set "abcdef12345")))
  (check-that (ucs-range->char-set! 97 103 #t (string->char-set "12345"))
	      (opposite (is char-set= (string->char-set "abcef12345")))))

(define-test-case x->char-set srfi-14-tests
  (check-that (x->char-set #\x) (is char-set= (x->char-set "x")))
  (check-that (x->char-set #\x) (is char-set= (x->char-set (char-set #\x))))
  (check-that (x->char-set "y")
	      (opposite (is char-set= (x->char-set #\x)))))

(define-test-case char-set-size srfi-14-tests
  (check (char-set-size (char-set-intersection char-set:ascii char-set:digit))
	 => 10))

(define-test-case char-set-count srfi-14-tests
  (check (char-set-count vowel? char-set:ascii)
	 => 5))

(define-test-case char-set->list srfi-14-tests
  (check (char-set->list (char-set #\x)) => '(#\x))
  (check-that (char-set->list (char-set #\x)) (opposite (is '(#\X)))))

(define-test-case char-set->string srfi-14-tests
  (check (char-set->string (char-set #\x)) => "x")
  (check-that (char-set->string (char-set #\x)) (opposite (is "X" ))))

(define-test-case char-set-contains? srfi-14-tests
  (check (char-set-contains? (x->char-set "xyz") #\x))
  (check (not (char-set-contains? (x->char-set "xyz") #\a))))

(define-test-case char-set-every srfi-14-tests
  (check (char-set-every char-lower-case? (x->char-set "abcd")))
  (check-that (char-set-every char-lower-case? (x->char-set "abcD")) (is-false)))

(define-test-case char-set-any srfi-14-tests
  (check (char-set-any char-lower-case? (x->char-set "abcd")))
  (check-that (char-set-any char-lower-case? (x->char-set "ABCD")) (is-false)))

(define-test-case cursors srfi-14-tests
  (check-that
   (let ((cs (x->char-set "abcd")))
     (let lp ((cur (char-set-cursor cs)) (ans '()))
       (if (end-of-char-set? cur) (list->char-set ans)
	   (lp (char-set-cursor-next cs cur)
	       (cons (char-upcase (char-set-ref cs cur)) ans)))))
   (is char-set=
       (x->char-set "ABCD"))))

(define-test-case char-set-adjoin srfi-14-tests
  (check-that (char-set-adjoin (x->char-set "123") #\x #\a)
	      (is char-set= (x->char-set "123xa")))
  (check-that (x->char-set "123x")
	      (opposite (is char-set= (char-set-adjoin (x->char-set "123") #\x #\a)))))

(define-test-case char-set-adjoin! srfi-14-tests
  (check-that (char-set-adjoin! (x->char-set "123") #\x #\a)
	      (is char-set= (x->char-set "123xa")))
  (check-that (x->char-set "123x")
	      (opposite (is char-set= (char-set-adjoin! (x->char-set "123") #\x #\a)))))

(define-test-case char-set-delete srfi-14-tests
  (check-that (char-set-delete (x->char-set "123") #\2 #\a #\2)
	      (is char-set= (x->char-set "13")))
  (check-that (char-set-delete (x->char-set "123") #\2 #\a #\2)
	      (opposite (is char-set= (x->char-set "13a")))))

(define-test-case char-set-delete! srfi-14-tests
  (check-that (char-set-delete! (x->char-set "123") #\2 #\a #\2)
	      (is char-set= (x->char-set "13")))
  (check-that (char-set-delete! (x->char-set "123") #\2 #\a #\2)
	      (opposite (is char-set= (x->char-set "13a")))))

(define-test-case char-set-intersection srfi-14-tests
  (check-that
   (char-set-intersection char-set:hex-digit (char-set-complement char-set:digit))
   (is char-set=
       (x->char-set "abcdefABCDEF"))))

(define-test-case char-set-intersection! srfi-14-tests
  (check-that
   (char-set-intersection! (char-set-complement! (x->char-set "0123456789"))
			   char-set:hex-digit)
   (is char-set=
       (x->char-set "abcdefABCDEF"))))

(define-test-case char-set-union srfi-14-tests
  (check-that
   (char-set-union char-set:hex-digit
		   (x->char-set "abcdefghijkl"))
   (is char-set=
       (x->char-set "abcdefABCDEFghijkl0123456789"))))

(define-test-case char-set-union! srfi-14-tests
  (check-that
   (char-set-union! (x->char-set "abcdefghijkl")
		    char-set:hex-digit)
   (is char-set=
       (x->char-set "abcdefABCDEFghijkl0123456789"))))

(define-test-case char-set-difference srfi-14-tests
  (check-that
   (char-set-difference (x->char-set "abcdefghijklmn")
			char-set:hex-digit)
   (is char-set=
       (x->char-set "ghijklmn"))))

(define-test-case char-set-difference! srfi-14-tests
  (check-that
   (char-set-difference! (x->char-set "abcdefghijklmn")
			 char-set:hex-digit)
   (is char-set=
       (x->char-set "ghijklmn"))))

(define-test-case char-set-xor srfi-14-tests
  (check-that
   (char-set-xor (x->char-set "0123456789")
		 char-set:hex-digit)
   (is char-set=
       (x->char-set "abcdefABCDEF"))))

(define-test-case char-set-xor! srfi-14-tests char-set=
  (check-that (char-set-xor! (x->char-set "0123456789")
			     char-set:hex-digit)
	      (is char-set= (x->char-set "abcdefABCDEF"))))

(define-test-case char-set-diff+intersection srfi-14-tests
  (call-with-values (lambda ()
		      (char-set-diff+intersection char-set:hex-digit
						  char-set:letter))
    (lambda (d i)
      (check-that d (is char-set= (x->char-set "0123456789")))
      (check-that i (is char-set= (x->char-set "abcdefABCDEF")))))
  (call-with-values (lambda ()
		      (char-set-diff+intersection (char-set-union char-set:letter
								  char-set:digit)
						  char-set:letter))
    (lambda (d i)
      (check-that d (is char-set= char-set:digit))
      (check-that i (is char-set= char-set:letter)))))

(define-test-case char-set-diff+intersection! srfi-14-tests
  (call-with-values (lambda ()
		      (char-set-diff+intersection! (char-set-copy char-set:hex-digit)
						   (char-set-copy char-set:letter)))
    (lambda (d i)
      (check-that d (is char-set= (x->char-set "0123456789")))
      (check-that i (is char-set= (x->char-set "abcdefABCDEF")))))
  (call-with-values (lambda ()
		      (char-set-diff+intersection! (char-set-union char-set:letter
								   char-set:digit)
						   (char-set-copy char-set:letter)))
    (lambda (d i)
      (check-that d (is char-set= char-set:digit))
      (check-that i (is char-set= char-set:letter)))))

; The following stuff was adapted from the suite Matthew Flatt wrote
; for PLT Scheme

(define-test-case char-set:lower-case srfi-14-tests
  (check (char-set-contains? char-set:lower-case #\a))
  (check (not (char-set-contains? char-set:lower-case #\A)))
  (check (char-set-contains? char-set:lower-case (scalar-value->char #x00E0)))
  (check (not (char-set-contains? char-set:lower-case (scalar-value->char #x00C2))))
  (check (char-set-contains? char-set:lower-case (scalar-value->char #x00B5))))

(define-test-case char-set:upper-case srfi-14-tests
  (check (char-set-contains? char-set:upper-case #\A))
  (check (not (char-set-contains? char-set:upper-case #\a)))
  (check (char-set-contains? char-set:upper-case (scalar-value->char #x00C2)))
  (check (not (char-set-contains? char-set:upper-case (scalar-value->char #x00E0)))))

(define-test-case char-set:title-case srfi-14-tests
  (check (char-set-contains? char-set:title-case (scalar-value->char #x01C5)))
  (check (char-set-contains? char-set:title-case (scalar-value->char #x1FA8)))
  (check (not (char-set-contains? char-set:title-case #\a)))
  (check (not (char-set-contains? char-set:title-case #\A))))

(define-test-case char-set:letter srfi-14-tests
  (check (char-set-contains? char-set:letter #\a))
  (check (char-set-contains? char-set:letter #\A))
  (check (not (char-set-contains? char-set:letter #\1)))
  (check (char-set-contains? char-set:letter (scalar-value->char #x00AA)))
  (check (char-set-contains? char-set:letter (scalar-value->char #x00BA))))

(define-test-case char-set:lower-case/2 srfi-14-tests
  (check (not (char-set-every (lambda (c) (char-set-contains? char-set:lower-case c)) char-set:letter)))
  (check (char-set-any (lambda (c) (char-set-contains? char-set:lower-case c)) char-set:letter)))

(define-test-case char-set:upper-case/2 srfi-14-tests
  (check (not (char-set-every (lambda (c) (char-set-contains? char-set:upper-case c)) char-set:letter)))
  (check (char-set-any (lambda (c) (char-set-contains? char-set:upper-case c)) char-set:letter)))

;; Not true?
;; (test #t char-set<= char-set:letter (char-set-union char-set:lower-case char-set:upper-case char-set:title-case))

(define-test-case char-set:digit srfi-14-tests
  (check (char-set-contains? char-set:digit #\1))
  (check (not (char-set-contains? char-set:digit #\a))))

(define-test-case char-set:hex-digit srfi-14-tests
  (check (char-set-contains? char-set:hex-digit #\1))
  (check (char-set-contains? char-set:hex-digit #\a))
  (check (char-set-contains? char-set:hex-digit #\A))
  (check (not (char-set-contains? char-set:hex-digit #\g))))

(define-test-case char-set:letter+digit srfi-14-tests equal?
  (check (char-set-contains? char-set:letter+digit #\1))
  (check (char-set-contains? char-set:letter+digit #\a))
  (check (char-set-contains? char-set:letter+digit #\z))
  (check (char-set-contains? char-set:letter+digit #\A))
  (check (char-set-contains? char-set:letter+digit #\Z)))

(define-test-case char-set:letter/size srfi-14-tests 
  (check (char-set-size char-set:letter) => 92496))

(define-test-case char-set:letter/2 srfi-14-tests
  (check-that (char-set-union char-set:letter char-set:digit) 
	      (is char-set=
		  char-set:letter+digit))
  (check (not (char-set-every (lambda (c) (char-set-contains? char-set:letter c)) char-set:letter+digit)))
  (check (not (char-set-every (lambda (c) (char-set-contains? char-set:digit c)) char-set:letter+digit)))
  (check (char-set-any (lambda (c) (char-set-contains? char-set:letter c)) char-set:letter+digit)))

(define-test-case char-set:letter+digit/2 srfi-14-tests
  (check (char-set-every (lambda (c) (char-set-contains? char-set:letter+digit c)) char-set:letter))
  (check (char-set-every (lambda (c) (char-set-contains? char-set:letter+digit c)) char-set:digit)))

(define char-set:latin-1 (ucs-range->char-set 0 256))

(define-test-case char-set:latin-1 srfi-14-tests 
  (check-that
   (char-set-intersection (char-set-union char-set:letter char-set:digit char-set:punctuation char-set:symbol)
			  char-set:latin-1)
   (is char-set=
       (char-set-intersection char-set:graphic char-set:latin-1))))

(define-test-case char-set:printing srfi-14-tests
  (check-that (char-set-union char-set:graphic char-set:whitespace)
	      (is char-set= char-set:printing)))

(define-test-case char-set:whitespace srfi-14-tests
  (check (char-set-contains? char-set:whitespace (scalar-value->char #x0009)))
  (check (char-set-contains? char-set:whitespace (scalar-value->char #x000D)))
  (check (not (char-set-contains? char-set:whitespace #\a))))

(define-test-case char-set:iso-control srfi-14-tests
  (check-that (char-set-union (ucs-range->char-set #x0000 #x0020)
			      (ucs-range->char-set #x007F #x00A0)) 
	      (is char-set=
		  char-set:iso-control)))

(define-test-case char-set:punctuation srfi-14-tests 
  (check (char-set-contains? char-set:punctuation #\!))
  (check (char-set-contains? char-set:punctuation (scalar-value->char #x00A1)))
  (check (not (char-set-contains? char-set:punctuation #\a))))

(define-test-case char-set:symbol srfi-14-tests
  (check (char-set-contains? char-set:symbol #\$))
  (check (char-set-contains? char-set:symbol (scalar-value->char #x00A2)))
  (check (not (char-set-contains? char-set:symbol #\a))))

(define-test-case char-set:blank srfi-14-tests
  (check (char-set-contains?  char-set:blank #\space))
  (check (char-set-contains?  char-set:blank (scalar-value->char #x3000)))
  (check (not (char-set-contains?  char-set:blank #\a))))

;; General procedures ----------------------------------------

(define-test-case char-set=/2 srfi-14-tests
  (check (char-set= char-set:letter char-set:letter char-set:letter))
  (check (not (char-set= char-set:letter char-set:digit)))
  (check (not (char-set= char-set:letter char-set:letter char-set:digit)))
  (check (not (char-set= char-set:letter char-set:digit char-set:letter))))

(define-test-case char-set<=/2 srfi-14-tests
  (check (char-set<= char-set:graphic char-set:printing))
  (check (not (char-set<= char-set:printing char-set:graphic)))
  (check (char-set<= char-set:graphic char-set:printing char-set:full))
  (check (not (char-set<= char-set:graphic char-set:full char-set:printing))))

(define-test-case char-set-hash/2 srfi-14-tests
  (check (char-set-hash char-set:graphic)
	 =>
	 (char-set-hash char-set:graphic)))

;; Iterating over character sets ----------------------------------------

;; The number 290 comes from "grep Nd UnicodeData.txt | wc -l"
(define-test-case char-set-size/2 srfi-14-tests
  (check (char-set-size char-set:digit)
	 => 290))

(define-test-case cursors/2 srfi-14-tests
  (check-that (list->char-set
	       (let loop ((c (char-set-cursor char-set:digit)) (l '()))
		 (if (end-of-char-set? c)
		     l
		     (loop (char-set-cursor-next char-set:digit c)
			   (cons (char-set-ref char-set:digit c)
				 l)))))
	      (is char-set= char-set:digit)))

(define (add1 x) (+ 1 x))

(define-test-case char-set-unfold/2 srfi-14-tests
  (check-that
   (char-set-unfold (lambda (x) (= x 20)) scalar-value->char add1 10)
   (is char-set= (ucs-range->char-set 10 20)))
  (check-that
   (char-set-unfold (lambda (x) (= x 20)) scalar-value->char add1 10 (char-set (scalar-value->char #x14)))
   (is char-set=
       (ucs-range->char-set 10 21))))

(define-test-case char-set-unfold!/2 srfi-14-tests
  (check-that
   (char-set-unfold! (lambda (x) (= x 20)) scalar-value->char add1 10
		     (char-set-copy char-set:empty))
   (is char-set= (ucs-range->char-set 10 20))))

(define-test-case char-set-for-each/2 srfi-14-tests
  (check-that
   (let ((cs char-set:empty))
     (char-set-for-each 
      (lambda (c) 
	(set! cs (char-set-adjoin cs c)))
      char-set:digit)
     cs)
   (is char-set= char-set:digit)))

(define-test-case char-set-map/2 srfi-14-tests equal?
  (check-that (char-set-map
	       (lambda (c) c)
	       char-set:digit)
	      (is char-set= char-set:digit))
  (check-that (char-set-map
	       (lambda (c) c)
	       char-set:digit)
	      (is char-set= char-set:digit))
  (check-that (char-set-union
	       (char-set-map
		(lambda (c) c)
		char-set:digit)
	       (char-set #\A))
	      (is char-set= (char-set-adjoin char-set:digit #\A))))

;; Creating character sets ----------------------------------------

(define-test-case char-set-copy/2 srfi-14-tests
  (check-that (char-set-copy char-set:digit)
	      (is char-set= char-set:digit)))

(define-test-case abc srfi-14-tests
  (let ((abc (char-set #\a #\b #\c)))
    (check-that (char-set #\c #\a #\b)
		(is char-set=
		    abc))
    (check-that (string->char-set "cba") (is char-set= abc))
    (check-that (string->char-set! "cba" (char-set-copy char-set:empty)) (is char-set= abc))
    (check-that (string->char-set "cb" (char-set #\a)) (is char-set= abc))
    (check-that (char-set-filter (lambda (c) (char=? c #\b)) abc) (is char-set= (char-set #\b)))
    (check-that (char-set-filter (lambda (c) (char=? c #\b)) abc char-set:empty) (is char-set= (char-set #\b)))
    (check-that (char-set-filter! (lambda (c) (char=? c #\b)) (char-set-copy abc) (char-set-copy char-set:empty))
		(is char-set= (char-set #\b)))

    (check-that (x->char-set "abc") (is char-set= abc))
    (check-that (x->char-set abc) (is char-set= abc))
    (check-that (x->char-set #\a) (is char-set= (char-set #\a)))))

(define-test-case ucs-range->char/2 srfi-14-tests
  (check-that
   (char-set-union (ucs-range->char-set 0 #xD800)
		   (ucs-range->char-set #xE000 #x20000))
   (is char-set= (ucs-range->char-set 0 #x20000)))
  (check-that
   (ucs-range->char-set 0 #xD800)
   (is char-set= (ucs-range->char-set 0 #xD801)))
  (check-that
   (ucs-range->char-set 0 #xD800)
   (is char-set= (ucs-range->char-set 0 #xDFFF)))
  (check-that
   char-set:empty
   (is char-set= (ucs-range->char-set #xD800 #xD810)))
  (check-that
   char-set:empty
   (is char-set= (ucs-range->char-set #xD810 #xE000)))
  (check-that
   (ucs-range->char-set #xD810 #xE001)
   (is char-set= (ucs-range->char-set #xE000 #xE001)))
  (check-that
   (char-set (scalar-value->char #xD7FF) (scalar-value->char #xE000))
   (is char-set= (ucs-range->char-set #xD7FF #xE001))))

;; Querying character sets ------------------------------

(define-test-case char-set-count/2 srfi-14-tests
  (check
   (char-set-count (lambda (x)
		     (and (char<=? #\0 x)
			  (char<=? x #\2)))
		   char-set:digit)
   => 3))

(define-test-case list->char-set/2 srfi-14-tests
  (check-that (list->char-set (char-set->list char-set:digit))
	      (is char-set= char-set:digit))
  (check-that (list->char-set (char-set->list char-set:digit) char-set:empty)
	      (is char-set= char-set:digit))
  (check-that (list->char-set! (char-set->list char-set:digit) (char-set-copy char-set:empty))
	      (is char-set= char-set:digit))
  (check-that (string->char-set (char-set->string char-set:digit))
	      (is char-set= char-set:digit)))


