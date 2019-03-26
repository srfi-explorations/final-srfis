#! /usr/bin/env chibi-scheme

;; Usage: scripts/add-srfi-subtree.scm 1 12 123 | sh -x

(import (scheme base)
        (scheme write)
        (scheme process-context))

(define github-organization "scheme-requests-for-implementation")

(define (display-command arg . args)
  (display arg)
  (if (null? args)
      (newline)
      (begin (display " ")
             (apply display-command args))))

(define (srfi-bucket srfi-number)
  (let ((tens (truncate-quotient srfi-number 10)))
    (let-values (((hundreds tens) (truncate/ tens 10)))
      (apply string-append (map number->string (list hundreds tens 0))))))

(define (display-srfi-subtree-add-commands srfi-number)
  (let* ((bucket (srfi-bucket srfi-number))
         (srfi-n (string-append "srfi-" (number->string srfi-number)))
         (remote (string-append "git@github.com:" github-organization "/"
                                srfi-n ".git"))
         (prefix (string-append bucket "/" srfi-n)))
    (display-command "git" "remote" "add" srfi-n remote)
    (display-command "git" "subtree" "add" (string-append "--prefix=" prefix)
                     srfi-n "master")
    (newline)))

(define (main arguments)
  (let ((srfi-numbers (map string->number (cdr arguments))))
    (for-each display-srfi-subtree-add-commands srfi-numbers)))

(main (command-line))
