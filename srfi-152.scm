(module srfi-152 ()

  (import (except scheme string->list string-copy string-fill!
                         string-length string-ref string-set!
                         make-string string substring list->string
                         write-char read-char display))

  (import (only chicken include error use))

  (use (only utf8 string-length string-ref string-set!
                  make-string string substring list->string
                  write-char read-char display))

  (use (rename (only utf8-case-map utf8-string-upcase utf8-string-downcase)
       (utf8-string-upcase string-upcase)
       (utf8-string-downcase string-downcase)))

  (export string-null? string-every string-any)
  (export string-tabulate string-unfold string-unfold-right)
  (export string->vector string->list vector->string
          reverse-list->string)
  #;(export string->utf8 string->utf16 string->utf16be string->utf16le
          utf8->string utf16->string utf16be->string utf16le->string)
  (export string-copy
          string-take string-drop string-take-right string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both)
  (export string-replace)
  (export string-prefix-length string-suffix-length
          string-prefix? string-suffix?)
  (export string-index string-index-right string-skip string-skip-right
          string-contains string-contains-right
          string-take-while string-drop-while string-break string-span)
  (export string-upcase string-downcase string-foldcase)
  (export string-append string-concatenate string-concatenate-reverse
          string-join)
  (export string-fold string-fold-right
          string-map string-for-each
          string-count
          string-filter string-remove)
  (export string-replicate string-split)
  #;(export string-normalize-nfc string-normalize-nfkc
          string-normalize-nfd string-normalize-nfkd)
  (export string-fill! string-copy!)

  (define string-foldcase string-downcase)   ; FIXME

  (include "macros.scm")
  (include "portable.scm")
  (include "r7rs-shim.scm")
)
