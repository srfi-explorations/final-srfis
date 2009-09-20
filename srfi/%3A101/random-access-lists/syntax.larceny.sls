#!r6rs
;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; Non portable assumption: single instantiation of libraries.

(library (srfi :101 random-access-lists syntax)
  (export (rename (ra:quote quote)))
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) expand)
          (for (srfi :101 random-access-lists syntax fetch) run expand)
          (for (srfi :101 random-access-lists syntax ra-datum) expand))

  (define-syntax ra:quote
    (lambda (stx)
      (syntax-case stx ()
        ((_ x)
         (let* ((y (syntax->datum (syntax x)))
                (label (new-label-for y)))
           (quasisyntax
            (fetch-labelled (unsyntax label)))))))))
