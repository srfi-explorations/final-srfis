#!r6rs
;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

(library (srfi :101 random-access-lists syntax ra-datum)
  (export datum->ra-datum)
  (import (rnrs base)
          (rnrs bytevectors)
          (prefix (srfi :101 random-access-lists procedures) ra:))

  (define (datum->ra-datum d)
    (cond ((boolean? d) d)
          ((number? d) d)
          ((char? d) d)
          ((string? d) d)
          ((symbol? d) d)          
          ((bytevector? d) d)
          ((null? d) d)
          ((vector? d) 
           (vector-map datum->ra-datum d))
          ((pair? d)
           (ra:cons (datum->ra-datum (car d))
                    (datum->ra-datum (cdr d))))
          (else
           (error "Unkown datum" d)))))
