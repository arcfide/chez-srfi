(library (srfi :171)
  (export rcons reverse-rcons rcount rany revery
          list-transduce vector-transduce string-transduce bytevector-u8-transduce port-transduce generator-transduce
          compose
          tmap tfilter tremove treplace tfilter-map tdrop tdrop-while ttake ttake-while
          tconcatenate tappend-map tdelete-neighbor-dupes tdelete-duplicates tflatten
          tsegment tpartition tinterpose tlog tenumerate)
  (import (srfi :171 transducers)))
