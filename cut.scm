; REFERENCE IMPLEMENTATION FOR SRFI-26 "CUT"
; ==========================================
;
; Sebastian.Egner@philips.com, 27-Feb-2002.
;
; The code to handle the variable argument case was originally
; proposed by Michael Sperber and has been adapted to the new
; syntax of the macro using an explicit rest-slot symbol.
;
; compliance:
;   Scheme R5RS (including macros).
;
; loading this file into Scheme 48 0.57:
;   ,load cut.scm
;
; history of this file:
;   SE,  6-Feb-2002: initial version as 'curry' with ". <>" notation
;   SE, 14-Feb-2002: revised for <...>
;   SE, 27-Feb-2002: revised for 'cut'

(define-syntax cut
  (syntax-rules (<> <...>)

    ; construct variable or fixed arity procedure
    ((cut "loop" (params ...) proc (args ...) <...>)
     (lambda (params ... . rest-slot) (apply proc args ... rest-slot)))
    ((cut "loop" (params ...) proc (args ...))
     (lambda (params ...) (proc args ...)))
				
    ; loop over <const-or-slot>*
    ((cut "loop" (param ...) proc (arg ...) <> . consts-or-slots)
     (cut "loop" (param ... slot) proc (arg ... slot) . consts-or-slots))
    ((cut "loop" (param ...) proc (arg ...) const . consts-or-slots)
     (cut "loop" (param ...) proc (arg ... const) . consts-or-slots))

    ; specified syntax
    ((cut proc . consts-or-slots)
     (cut "loop" () proc () . consts-or-slots))))
