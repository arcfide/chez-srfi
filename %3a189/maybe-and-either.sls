(library (srfi :189 maybe-and-either)
  (export maybe? either? just nothing left right nothing? just? maybe= left?
          right? either= either-swap

          maybe-ref maybe-ref/default either-ref either-ref/default

          maybe-join maybe-bind maybe-compose either-join either-bind
          either-compose

          maybe-length maybe-filter maybe-remove either-length
          either-filter either-remove
          maybe-sequence either-sequence

          maybe->either either->maybe list->just list->right maybe->list
          either->list maybe->truth either->truth truth->maybe maybe->values
          maybe->two-values values->maybe either->values values values->either
          two-values->maybe maybe-for-each either-for-each maybe->generation
          generation->maybe list->left list->maybe list->either
          maybe->list-truth either->list-truth list-truth->maybe
          list-truth->either truth->either
          either->generation generation->either
          exception->either either-guard

          maybe-map maybe-fold maybe-unfold either-map either-fold
          either-unfold

          tri-not tri=? tri-and tri-or tri-merge

          maybe-and maybe-or maybe-let* either-and either-or either-let*
          maybe-let*-values either-let*-values
          maybe-if)

  (import (except (rnrs) define-record-type)
          (only (srfi :1 lists) list-copy list= every)
          (srfi :9 records)
          (srfi :145 assumptions)
          (srfi private include))

  (include/resolve ("srfi" "%3a189") "srfi-189-impl.scm"))
