; REFERENCE IMPLEMENTATION FOR SRFI-26 "CUT"
; ==========================================
;
; Sebastian.Egner@philips.com, 4-Jun-2002.
;
; The code to handle the variable argument case was originally
; proposed by Michael Sperber and has been adapted to the new
; syntax of the macro using an explicit rest-slot symbol. The
; code to evaluate the non-slots for cute has been proposed by
; Dale Jordan. The code to allow a slot for the procedure position
; and to process the macro using an internal macro is based on 
; a suggestion by Al Petrofsky.
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
;   SE, 03-Jun-2002: revised for proc-slot, cute
;   SE, 04-Jun-2002: rewritten with internal transformer (no "loop" pattern)

; (srfi-26-internal mode params args . cs)
;   transformer used internally
;     mode   : either "cut" or "cute"
;     params : list of parameters of the specialized procedure
;     args   : procedure being specialized, followed by its arguments
;     cs     : consts-or-slots, the qualifiers of the macro

(define-syntax srfi-26-internal
  (syntax-rules (<> <...>)

    ; construct variable- or fixed-arity procedure
    ((srfi-26-internal mode (param ...) (proc arg ...) <...>)
     (lambda (param ... . rest-slot) (apply proc arg ... rest-slot)))
    ((srfi-26-internal mode (param ...) (proc arg ...))
     (lambda (param ...) (proc arg ...)))

    ; loop over cs
    ((srfi-26-internal mode (param ...) (proc-arg ...) <> . cs)
     (srfi-26-internal mode (param ... slot) (proc-arg ... slot) . cs))
    ((srfi-26-internal "cute" (param ...) (proc-arg ...) (c-op . c-args) . cs)
     (let ((const (c-op . c-args)))
       (srfi-26-internal "cute" (param ...) (proc-arg ... const) . cs)))
    ((srfi-26-internal mode (param ...) (proc-arg ...) const . cs)
     (srfi-26-internal mode (param ...) (proc-arg ... const) . cs))))

; exported syntax

(define-syntax cut
  (syntax-rules (<> <...>)
    ((cut . consts-or-slots)
     (srfi-26-internal "cut" () () . consts-or-slots))))

(define-syntax cute
  (syntax-rules (<> <...>)
    ((cute . consts-or-slots)
     (srfi-26-internal "cute" () () . consts-or-slots))))
