(library (srfi :125 hashtables)
  (export
    make-hash-table hash-table hash-table-unfold alist->hash-table

    hash-table? hash-table-contains? hash-table-empty? hash-table=?
    hash-table-mutable?

    hash-table-ref hash-table-ref/default 

    hash-table-set! hash-table-delete! hash-table-intern! hash-table-update!
    hash-table-update!/default hash-table-pop! hash-table-clear! 

    hash-table-size hash-table-keys hash-table-values hash-table-entries
    hash-table-find hash-table-count

    hash-table-map hash-table-for-each hash-table-map! hash-table-map->list
    hash-table-fold hash-table-prune!

    hash-table-copy hash-table-empty-copy hash-table->alist 

    hash-table-union! hash-table-intersection! hash-table-difference!
    hash-table-xor!

    ;; The following procedures are deprecated by SRFI 125:

    (rename
      (hash deprecated:hash)
      (string-hash deprecated:string-hash)
      (string-ci-hash deprecated:string-ci-hash)
      (hash-by-identity deprecated:hash-by-identity)
      (hash-table-equivalence-function
        deprecated:hash-table-equivalence-function)
      (hash-table-hash-function deprecated:hash-table-hash-function)
      (hash-table-exists? deprecated:hash-table-exists?)
      (hash-table-walk deprecated:hash-table-walk)
      (hash-table-merge! deprecated:hash-table-merge!)))
  (import (except (rnrs) make-hashtable hashtable-clear! hashtable-copy
            hashtable-ref hashtable-update! make-eq-hashtable
            make-eqv-hashtable)
          (srfi private include)
          (rename (srfi :126)
                  (hashtable-merge! hash-table-merge!)
                  (hashtable-walk hash-table-walk)
                  (hashtable-hash-function hash-table-hash-function)
                  (hashtable-equivalence-function hash-table-equivalence-function))
          (except (srfi :128) hash-salt string-hash string-ci-hash symbol-hash))


  (define (hash-table-exists? ht key) (hashtable-contains? ht key))
  (define hash-by-identity 
    (case-lambda
      [(obj) (default-hash obj)]
      [(obj ig) (default-hash obj)]))

  (define hash
    (case-lambda
      [(obj) (default-hash obj)]
      [(obj ig) (default-hash obj)]))

  (include/resolve ("srfi" "%3a125") "125.body.scm"))

