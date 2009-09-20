#!r6rs
;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; Ikarusisms: relies serializability of records, implicit phases.

(library (srfi :101 random-access-lists syntax)
  (export (rename (ra:quote quote)))
  (import (rnrs)
          (srfi :101 random-access-lists syntax ra-datum))

  (define-syntax ra:quote
    (lambda (stx)
      (syntax-case stx ()
        ((_ x)
         (with-syntax ((v (datum->syntax (syntax stx)
                            (datum->ra-datum (syntax->datum (syntax x))))))
           (syntax (quote v))))))))
