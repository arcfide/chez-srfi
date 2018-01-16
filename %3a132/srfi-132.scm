(module srfi-132 ()
  (import scheme)
  (import (only chicken include error))
  (export list-sorted? vector-sorted? list-merge vector-merge list-sort vector-sort
          list-stable-sort vector-stable-sort list-merge! vector-merge! list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median vector-find-median!)
  (include "delndups.scm")
  (include "lmsort.scm")
  (include "sortp.scm")
  (include "vector-util.scm")
  (include "vhsort.scm")
  (include "visort.scm")
  (include "vmsort.scm")
  (include "vqsort2.scm")
  (include "median.scm")
  (include "sort.scm")  ; must be last
)
