;;;; Module for Chicken's native 8-bit strings

(module srfi-152 ()

  ;; R5RS+ procedures must not be imported, as we redefine them
  (import (except scheme string->list string-copy string-fill!))

  (import (only chicken include error use case-lambda
                        open-input-string open-output-string get-output-string))

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

  ;; Simple case-mapping functions
  (export string-upcase string-downcase string-foldcase)
  (define (string-upcase str) (string-map char-upcase str))
  (define (string-downcase str) (string-map char-downcase str))
  (define (string-foldcase str) (string-map char-downcase str))  ; good enough for ASCII work

  ;; Export R7RS procedures (defined in r7rs-shim file and chicken module)
  (import (only extras read-string))
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
  (include "r7rs-shim.scm")
)
