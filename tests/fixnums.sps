;; Copyright © 2017 John Cowan
;; Copyright © 2020 Amirouche Boubekki
;; SPDX-License-Identifier: MIT
#!r6rs

;; Tests for SRFI 5

(import
 (except (rnrs)
         fixnum? fx=? fx<? fx>? fx<=? fx>=? fxzero? fxpositive? fxnegative? fxodd?
         fxeven? fxmax fxmin

         fx+ fx- fx*

         fx+/carry fx-/carry fx*/carry

         fxnot fxand fxior fxxor fxarithmetic-shift fxarithmetic-shift-left
         fxarithmetic-shift-right fxbit-count fxlength fxif fxbit-set? fxcopy-bit
         fxbit-field)

 (srfi :143 fixnums)
 (srfi :64 testing))

(test-begin "fixnums")

(define check-00
  (test-equal "fixnum-000" #t (fixnum? 32767)))

(define check-01
  (test-equal "fixnum-001" #f (fixnum? 1.1)))

(define check-02
  (test-equal "fixnum-002" #t (fx=? 1 1 1)))

(define check-03
  (test-equal "fixnum-003" #f (fx=? 1 2 2)))

(define check-04
  (test-equal "fixnum-004" #f (fx=? 1 1 2)))

(define check-05
  (test-equal "fixnum-005" #f (fx=? 1 2 3)))

(define check-06
  (test-equal "fixnum-006" #t (fx<? 1 2 3)))

(define check-07
  (test-equal "fixnum-007" #f (fx<? 1 1 2)))

(define check-08
  (test-equal "fixnum-008" #t (fx>? 3 2 1)))

(define check-09
  (test-equal "fixnum-009" #f (fx>? 2 1 1)))

(define check-10
  (test-equal "fixnum-010" #t (fx<=? 1 1 2)))

(define check-11
  (test-equal "fixnum-011" #f (fx<=? 1 2 1)))

(define check-12
  (test-equal "fixnum-012" #t (fx>=? 2 1 1)))

(define check-13
  (test-equal "fixnum-013" #f (fx>=? 1 2 1)))

(define check-14
  (test-equal "fixnum-014" '(#t #f) (list (fx<=? 1 1 2) (fx<=? 2 1 3))))

(define check-15
  (test-equal "fixnum-015" #t (fxzero? 0)))

(define check-16
  (test-equal "fixnum-016" #f (fxzero? 1)))

(define check-17
  (test-equal "fixnum-017" #f (fxpositive? 0)))

(define check-18
  (test-equal "fixnum-018" #t (fxpositive? 1)))

(define check-19
  (test-equal "fixnum-019" #f (fxpositive? -1)))

(define check-20
  (test-equal "fixnum-020" #f (fxnegative? 0)))

(define check-21
  (test-equal "fixnum-021" #f (fxnegative? 1)))

(define check-22
  (test-equal "fixnum-022" #t (fxnegative? -1)))

(define check-23
  (test-equal "fixnum-023" #f (fxodd? 0)))

(define check-24
  (test-equal "fixnum-024" #t (fxodd? 1)))

(define check-25
  (test-equal "fixnum-025" #t (fxodd? -1)))

(define check-26
  (test-equal "fixnum-026" #f (fxodd? 102)))

(define check-27
  (test-equal "fixnum-027" #t (fxeven? 0)))

(define check-28
  (test-equal "fixnum-028" #f (fxeven? 1)))

(define check-29
  (test-equal "fixnum-029" #t (fxeven? -2)))

(define check-30
  (test-equal "fixnum-030" #t (fxeven? 102)))

(define check-31
  (test-equal "fixnum-031" 4 (fxmax 3 4)))

(define check-32
  (test-equal "fixnum-032" 5 (fxmax 3 5 4)))

(define check-33
  (test-equal "fixnum-033" 3 (fxmin 3 4)))

(define check-34
  (test-equal "fixnum-034" 3 (fxmin 3 5 4)))

(define check-35
  (test-equal "fixnum-035" 7 (fx+ 3 4)))

(define check-36
  (test-equal "fixnum-036" 12 (fx* 4 3)))

(define check-37
  (test-equal "fixnum-037" -1 (fx- 3 4)))

(define check-38
  (test-equal "fixnum-038" -3 (fxneg 3)))

(define check-39
  (test-equal "fixnum-039" 7 (fxabs -7)))

(define check-40
  (test-equal "fixnum-040" 7 (fxabs 7)))

(define check-41
  (test-equal "fixnum-041" 1764 (fxsquare 42)))

(define check-42
  (test-equal "fixnum-042" 4 (fxsquare 2)))

(define check-43
  (test-equal "fixnum-043" 2 (fxquotient 5 2)))

(define check-44
  (test-equal "fixnum-044" -2 (fxquotient -5 2)))

(define check-45
  (test-equal "fixnum-045" -2 (fxquotient 5 -2)))

(define check-46
  (test-equal "fixnum-046" 2 (fxquotient -5 -2)))

(define check-47
  (test-equal "fixnum-047" 1 (fxremainder 13 4)))

(define check-48
  (test-equal "fixnum-048" -1 (fxremainder -13 4)))

(define check-49
  (test-equal "fixnum-049" 1 (fxremainder 13 -4)))

(define check-50
  (test-equal "fixnum-050" -1 (fxremainder -13 -4)))

(define check-51
  (test-equal "fixnum-051" 35 (let*-values (((root rem) (fxsqrt 32)))
              (* root rem))))
(define check-52
  (test-equal "fixnum-052" -1 (fxnot 0)))

(define check-53
  (test-equal "fixnum-053"  0 (fxand #b0 #b1)))

(define check-54
  (test-equal "fixnum-054"  6 (fxand 14 6)))

(define check-55
  (test-equal "fixnum-055"  14 (fxior 10 12)))

(define check-56
  (test-equal "fixnum-056"  6 (fxxor 10 12)))

(define check-57
  (test-equal "fixnum-057" 0 (fxnot -1)))

(define check-58
  (test-equal "fixnum-058" 9 (fxif 3 1 8)))

(define check-59
  (test-equal "fixnum-059" 0 (fxif 3 8 1)))

(define check-60
  (test-equal "fixnum-060" 2 (fxbit-count 12)))

(define check-61
  (test-equal "fixnum-061" 0 (fxlength 0)))

(define check-62
  (test-equal "fixnum-062" 8 (fxlength 128)))

(define check-63
  (test-equal "fixnum-063" 8 (fxlength 255)))

(define check-64
  (test-equal "fixnum-064" 9 (fxlength 256)))

(define check-65
  (test-equal "fixnum-065" -1 (fxfirst-set-bit 0)))

(define check-66
  (test-equal "fixnum-066" 0 (fxfirst-set-bit 1)))

(define check-67
  (test-equal "fixnum-067" 0 (fxfirst-set-bit 3)))

(define check-68
  (test-equal "fixnum-068" 2 (fxfirst-set-bit 4)))

(define check-69
  (test-equal "fixnum-069" 1 (fxfirst-set-bit 6)))

(define check-70
  (test-equal "fixnum-070" 0 (fxfirst-set-bit -1)))

(define check-71
  (test-equal "fixnum-071" 1 (fxfirst-set-bit -2)))

(define check-72
  (test-equal "fixnum-072" 0 (fxfirst-set-bit -3)))

(define check-73
  (test-equal "fixnum-073" 2 (fxfirst-set-bit -4)))

(define check-74
  (test-equal "fixnum-074" #t (fxbit-set? 0 1)))

(define check-75
  (test-equal "fixnum-075" #f (fxbit-set? 1 1)))

(define check-76
  (test-equal "fixnum-076" #f (fxbit-set? 1 8)))

(define check-77
  (test-equal "fixnum-077" #t (fxbit-set? 10000 -1)))

(define check-78
  (test-equal "fixnum-078" #t (fxbit-set? 1000 -1)))

(define check-79
  (test-equal "fixnum-079" 0 (fxcopy-bit 0 0 #f)))

(define check-80
  (test-equal "fixnum-080" -1 (fxcopy-bit 0 -1 #t)))

(define check-81
  (test-equal "fixnum-081" 1 (fxcopy-bit 0 0 #t)))

(define check-82
  (test-equal "fixnum-082" #x106 (fxcopy-bit 8 6 #t)))

(define check-83
  (test-equal "fixnum-083" 6 (fxcopy-bit 8 6 #f)))

(define check-84
  (test-equal "fixnum-084" -2 (fxcopy-bit 0 -1 #f)))

(define check-85
  (test-equal "fixnum-085" 0 (fxbit-field 6 0 1)))

(define check-86
  (test-equal "fixnum-086" 3 (fxbit-field 6 1 3)))

(define check-87
  (test-equal "fixnum-087" 2 (fxarithmetic-shift 1 1)))

(define check-88
  (test-equal "fixnum-088" 0 (fxarithmetic-shift 1 -1)))

(define check-89
  (test-equal "fixnum-089" #b110  (fxbit-field-rotate #b110 1 1 2)))

(define check-90
  (test-equal "fixnum-090" #b1010 (fxbit-field-rotate #b110 1 2 4)))

(define check-91
  (test-equal "fixnum-091" #b1011 (fxbit-field-rotate #b0111 -1 1 4)))

(define check-92
  (test-equal "fixnum-092" #b110 (fxbit-field-rotate #b110 0 0 10)))

(define check-93
  (test-equal "fixnum-093" 6 (fxbit-field-reverse 6 1 3)))

(define check-94
  (test-equal "fixnum-094" 12 (fxbit-field-reverse 6 1 4)))

(define check-95
  (test-equal "fixnum-095" -11 (fxnot 10)))

(define check-96
  (test-equal "fixnum-096" 36 (fxnot -37)))

(define check-97
  (test-equal "fixnum-097" 11 (fxior 3  10)))

(define check-98
  (test-equal "fixnum-098" 10 (fxand 11 26)))

(define check-99
  (test-equal "fixnum-099" 9 (fxxor 3 10)))

(define check-100
  (test-equal "fixnum-100" 4 (fxand 37 12)))

(define check-101
  (test-equal "fixnum-101" 32 (fxarithmetic-shift 8 2)))

(define check-102
  (test-equal "fixnum-102" 4 (fxarithmetic-shift 4 0)))

(define check-103
  (test-equal "fixnum-103" 4 (fxarithmetic-shift 8 -1)))

(define check-104
  (test-equal "fixnum-104" 0 (fxlength  0)))

(define check-105
  (test-equal "fixnum-105" 1 (fxlength  1)))

(define check-106
  (test-equal "fixnum-106" 0 (fxlength -1)))

(define check-107
  (test-equal "fixnum-107" 3 (fxlength  7)))

(define check-108
  (test-equal "fixnum-108" 3 (fxlength -7)))

(define check-109
  (test-equal "fixnum-109" 4 (fxlength  8)))

(define check-110
  (test-equal "fixnum-110" 3 (fxlength -8)))

(define check-111
  ;; TODO: investigate
  (test-equal "fixnum-111" #t (fxbit-set? 3 10)))

(define check-112
  ;; TODO: investigate
  (test-equal "fixnum-112" #t (fxbit-set? 2 6)))

(define check-113
  (test-equal "fixnum-113" #f (fxbit-set? 0 6)))

(define check-114
  (test-equal "fixnum-114" #b100 (fxcopy-bit 2 0 #t)))

(define check-115
  (test-equal "fixnum-115" #b1011 (fxcopy-bit 2 #b1111 #f)))

(define check-116
  (test-equal "fixnum-116" 1 (fxfirst-set-bit 2)))

(define check-117
  (test-equal "fixnum-117" 3 (fxfirst-set-bit 40)))

(define check-118
  (test-equal "fixnum-118" 2 (fxfirst-set-bit -28)))

(define check-119
  (test-equal "fixnum-119" 1 (fxand #b1 #b1)))

(define check-120
  (test-equal "fixnum-120" 0 (fxand #b1 #b10)))

(define check-121
  (test-equal "fixnum-121" #b10 (fxand #b11 #b10)))

(define check-122
  (test-equal "fixnum-122" #b101 (fxand #b101 #b111)))

(define check-123
  (test-equal "fixnum-123" #b111 (fxand -1 #b111)))

(define check-124
  (test-equal "fixnum-124" #b110 (fxand -2 #b111)))

(define check-125
  (test-equal "fixnum-125" 1 (fxarithmetic-shift 1 0)))

(define check-126
  (test-equal "fixnum-126" 4 (fxarithmetic-shift 1 2)))

(define check-127
  (test-equal "fixnum-127" 8 (fxarithmetic-shift 1 3)))

(define check-128
  (test-equal "fixnum-128" 16 (fxarithmetic-shift 1 4)))

(define check-129
  (test-equal "fixnum-129" -1 (fxarithmetic-shift -1 0)))

(define check-130
  (test-equal "fixnum-130" -2 (fxarithmetic-shift -1 1)))

(define check-131
  (test-equal "fixnum-131" -4 (fxarithmetic-shift -1 2)))

(define check-132
  (test-equal "fixnum-132" -8 (fxarithmetic-shift -1 3)))

(define check-133
  (test-equal "fixnum-133" -16 (fxarithmetic-shift -1 4)))

(define check-134
  (test-equal "fixnum-134" #b1010 (fxbit-field #b1101101010 0 4)))

(define check-135
  (test-equal "fixnum-135" #b101101 (fxbit-field #b1101101010 3 9)))

(define check-136
  (test-equal "fixnum-136" #b10110 (fxbit-field #b1101101010 4 9)))

(define check-137
  (test-equal "fixnum-137" #b110110 (fxbit-field #b1101101010 4 10)))

(define check-138
  (test-equal "fixnum-138" 3 (fxif 1 1 2)))

(define check-139
  (test-equal "fixnum-139" #b00110011 (fxif #b00111100 #b11110000 #b00001111)))

(define check-140
  (test-equal "fixnum-140" #b1 (fxcopy-bit 0 0 #t)))

(test-end "fixnums")
