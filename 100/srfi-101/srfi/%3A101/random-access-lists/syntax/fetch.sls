#!r6rs
;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; This code is derived from code written by William D Clinger.
;; http://lists.ccs.neu.edu/pipermail/larceny-users/2009-September/000735.html

(library (srfi :101 random-access-lists syntax fetch)
  (export new-label-for fetch-labelled) 
  (import (rnrs base)
          (rnrs hashtables)
          (srfi :101 random-access-lists syntax ra-datum))

  (define label-counter 0)                     ; largest label in use
  (define label-table                          ; maps labels to things
    (make-hashtable (lambda (x) x) =))

  (define (new-label-for y)
    (set! label-counter (+ 1 label-counter))
    (hashtable-set! label-table label-counter (datum->ra-datum y))
    label-counter)

  (define (fetch-labelled label)
    (hashtable-ref label-table label 'this-never-matters))

  ) ; (srfi :101 random-access-lists syntax fetch)
