; CONFIDENCE TEST FOR IMPLEMENTATION OF "CURRY"-SRFI
; ==================================================
;
; Sebastian.Egner@philips.com, 14-Feb-2002.
;
; This file checks a few assertions about the implementation.
; If you run it and no error message is issued, the implementation
; is correct on the cases that have are tested.
;
; compliance:
;   Scheme R5RS with
;     SRFI-23: error
;
; loading this file into Scheme 48 0.57:
;   ,open srfi-23
;   ,load check-curry.scm

; (check expr)
;    evals expr and issues an error if it is not #t.

(define (check expr)
  (if (not (eq? (eval expr (interaction-environment)) #t))
      (error "check failed" expr)))

; (check-all)
;    runs several tests on curry and reports.

(define (check-all)
  (for-each 
   check
   '((equal? ((curry list)) '())
     (equal? ((curry list <...>)) '())
     (equal? ((curry list 1)) '(1))
     (equal? ((curry list <>) 1) '(1))
     (equal? ((curry list <...>) 1) '(1))
     (equal? ((curry list 1 2)) '(1 2))
     (equal? ((curry list 1 <>) 2) '(1 2))
     (equal? ((curry list 1 <...>) 2) '(1 2))
     (equal? ((curry list 1 <...>) 2 3 4) '(1 2 3 4))
     (equal? ((curry list 1 <> 3 <>) 2 4) '(1 2 3 4))
     (equal? ((curry list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)))))

; run the checks when loading
(check-all)
(display "passed")
(newline)