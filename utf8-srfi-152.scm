;;;; Module for Chicken's native 8-bit strings

(module utf8-srfi-152 ()

  ;; R5RS+ and utf8 procedures must not be imported, as we redefine them
  (import (except scheme
     string-length string-ref string-set! make-string string substring
     string-copy string->list list->string string-fill!))

  (import (only chicken include error use))

  ;; Cherry-pick utf8 procedures and re-export them
  (use (only utf8
    string-length string-ref string-set! make-string string substring list->string))
  (export string-length string-ref string-set! make-string string substring list->string)

  ;; Don't export R5RS procedures
  #;(no-export string? make-string list->string
               string-length string-ref substring
              string=? string<? string>? string<=? string>=?
              string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
              string-set!)

  ;; Export R5RS+ procedures
  (export string->list string-copy string-fill!)

  ;; Bytevector procedures aren't available on Chicken
  #;(no-export string->utf8 string->utf16 string->utf16be string->utf16le
               utf8->string utf16->string utf16be->string utf16le->string)

  ;; Normalization procedures aren't available on Chicken
  #;(no-export string-normalize-nfc string-normalize-nfkc
               string-normalize-nfd string-normalize-nfkd)

  ;; Import case-mapping functions from utf-8-casemap and re-export them
  (export string-upcase string-downcase string-foldcase)
  (use (rename (only utf8-case-map utf8-string-upcase utf8-string-downcase)
                  (utf8-string-upcase string-upcase)
                  (utf8-string-downcase string-downcase)))
  (define (string-foldcase str) (error "string-foldcase not supported"))

  ;; Export R7RS procedures (defined in r7rs-shim file and chicken module)
  (import (only utf8 read-string))
  (export string->vector vector->string string-map string-for-each
          read-string write-string string-copy! write-string)

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
          string-take-while string-drop-while string-break string-span)
  (export string-append string-concatenate string-concatenate-reverse
          string-join)
  (export string-fold string-fold-right string-count
          string-filter string-remove)
  (export string-replicate string-segment string-split)


  (include "macros.scm")
  (include "portable.scm")
  (include "r7rs-shim.scm")
)
