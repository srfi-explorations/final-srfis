;;;; Library for R7RS

(define-library (srfi-152)

  (import (scheme base))
  (import (scheme char))
  (import (scheme cxr))

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

  ;; Export bytevector procedures if R6RS library available
  (cond-expand
    ((library (rnrs bytevectors))
     (export string->utf8 string->utf16 string->utf16be string->utf16le
               utf8->string utf16->string utf16be->string utf16le->string)
     (import (only (rnrs bytevectors) string->utf8 string->utf16
                                      utf8->string utf16->string))
     (begin
       (define (string->utf16be string) (string->utf16 'big))
       (define (string->utf16le string) (string->utf16 'little))
       (define (utf16be->string bytevector) (utf16->string bytevector 'big))
       (define (utf16le->string bytevector) (utf16->string bytevector 'little))))
    (else (begin)))

  ;; Export normalization procedures if R6RS library available
  (cond-expand
    ((library (rnrs bytevectors))
     (export string-normalize-nfc string-normalize-nfkc
             string-normalize-nfd string-normalize-nfkd)
     (import (only (rnrs unicode)
             string-normalize-nfc string-normalize-nfkc
             string-normalize-nfd string-normalize-nfkd))
    (else (begin))))

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
)
