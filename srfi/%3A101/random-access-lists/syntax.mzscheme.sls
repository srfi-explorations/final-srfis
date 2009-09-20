#!r6rs
;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; PLTisms: begin-lifted.

(library (srfi :101 random-access-lists syntax)
  (export (rename (ra:quote quote)))
  (import (rnrs base)
          (srfi :101 random-access-lists syntax ra-datum)
          (only (mzlib etc) begin-lifted))

  (define-syntax ra:quote
    (syntax-rules ()
      ((ra:quote x)
       (begin-lifted (datum->ra-datum (quote x)))))))
      