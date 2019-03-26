#! /usr/bin/env chibi-scheme

(import (scheme base)
        (scheme write))

(define min 0)
(define max 165)

(define (srfi-bucket srfi-number)
  (let ((tens (truncate-quotient srfi-number 10)))
    (let-values (((hundreds tens) (truncate/ tens 10)))
      (apply string-append (map number->string (list hundreds tens 0))))))

(define (enumerate zeroth every)
  (let loop ((i min))
    (when (<= i max)
      (when (= 0 (truncate-remainder i 10))
        (zeroth i))
      (every i)
      (loop (+ i 1)))))

(define (buckets)
  (enumerate (lambda (i)
               (display (srfi-bucket i))
               (newline))
             (lambda (i) #f)))

(define (srfis)
  (enumerate (lambda (i)
               (newline))
             (lambda (i)
               (display (srfi-bucket i))
               (display "   ")
               (display "srfi-")
               (display i)
               (newline))))

(define (main)
  (buckets)
  (newline)
  (display "----------")
  (newline)
  (srfis))

(main)
