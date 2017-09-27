;;;; Library for R7RS

(define-library (srfi-152)

  (import (scheme base))
  (import (scheme char))
  (import (scheme cxr))
  (import (scheme case-lambda))

  ;; Don't export R7RS procedures
  #;(no-export string? make-string string
               string->vector string->list list->string vector->string
               string-length string-ref substring string-copy
               string=? string<? string>? string<=? string>=?
               string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
               string-upcase string-downcase string-foldcase
               string-append string-map string-for-each
               read-string write-string
               string-set! string-fill! string-copy!)

  ;; Remaining exports, grouped as in the SRFI
  (export string-null? string-every string-any)
  (export string-tabulate string-unfold string-unfold-right)
  (export reverse-list->string)
  (export string-take string-drop string-take-right string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both)
  (export string-replace)
  (export string-prefix-length string-suffix-length
          string-prefix? string-suffix?)
  (export string-index string-index-right string-skip string-skip-right
          string-contains string-contains-right
          string-take-while string-take-while-right
          string-drop-while string-drop-while-right
          string-break string-span)
  (export string-append string-concatenate string-concatenate-reverse
          string-join)
  (export string-fold string-fold-right string-count
          string-filter string-remove)
  (export string-replicate string-segment string-split)


  (include "macros.scm")
  (include "portable.scm")
)
