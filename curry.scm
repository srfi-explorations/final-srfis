; REFERENCE IMPLEMENTATION FOR "CURRY"-SRFI
; =========================================
;
; Sebastian.Egner@philips.com, 14-Feb-2002.
;
; The code to handle the variable argument case was originally
; proposed by Michael Sperber and has been adapted to the new
; syntax of the macro using an explicit rest-slot symbol.
;
; compliance:
;   Scheme R5RS (including macros).
;
; loading this file into Scheme 48 0.57:
;   ,load curry.scm

(define-syntax curry
  (syntax-rules (<> <...>)

    ; construct variable or fixed arity procedure
    ((curry "loop" (params ...) proc (args ...) <...>)
     (lambda (params ... . rest-slot) (apply proc args ... rest-slot)))
    ((curry "loop" (params ...) proc (args ...))
     (lambda (params ...) (proc args ...)))
				
    ; loop over <const-or-slot>*
    ((curry "loop" (param ...) proc (arg ...) <> . consts-or-slots)
     (curry "loop" (param ... slot) proc (arg ... slot) . consts-or-slots))
    ((curry "loop" (param ...) proc (arg ...) const . consts-or-slots)
     (curry "loop" (param ...) proc (arg ... const) . consts-or-slots))

    ; specified syntax
    ((curry proc . consts-or-slots)
     (curry "loop" () proc () . consts-or-slots))))

