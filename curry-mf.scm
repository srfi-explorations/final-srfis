; MACRO-FREE IMPLEMENTATION OF "CURRY"-SRFI
; =========================================
;
; Sebastian.Egner@philips.com, 14-Feb-2002
;
; This implementation is based on the code and idea by Stephan Houben.
; Refer to the SRFI discussion archive for the original posting with 
; subject line "Implementing it as a procedure".
;
; compliance:
;   Scheme R5RS (macros are not used) with
;     SRFI-23: error
;
; loading this file into Scheme 48 0.57:
;   ,open srfi-23
;   ,load curry-mf.scm

(define <>    '<>)    ; marker for slot
(define <...> '<...>) ; marker for rest-slot

(define (curry proc . consts-or-slots)
  
  (define has-rest-slot
    (memq <...> consts-or-slots))
  
  (define arity
    (apply + (map (lambda (x) (if (eq? x <>) 1 0)) consts-or-slots)))
  
  (define (substitute consts-or-slots args)
    (cond
     ((null? consts-or-slots)
      '())
     ((eq? (car consts-or-slots) <>)
      (cons (car args) (substitute (cdr consts-or-slots) (cdr args))))
     ((eq? (car consts-or-slots) <...>)
      args)
     (else
      (cons (car consts-or-slots) (substitute (cdr consts-or-slots) args)))))
  
   ; check syntax
  (if (not (procedure? proc))
      (error "non-procedure in operator positition" proc))
  (if (and has-rest-slot (not (null? (cdr has-rest-slot))))
      (error "<const-or-slot> after <...>" (cdr has-rest-slot)))
    
   ; create procedure
  (if has-rest-slot
      (lambda args
	(if (>= (length args) arity)
	    (apply proc (substitute consts-or-slots args))
	    (error "too few arguments" args)))
      (lambda args
	(if (= (length args) arity)
	    (apply proc (substitute consts-or-slots args))
	    (error "wrong number of arguments" args)))))
