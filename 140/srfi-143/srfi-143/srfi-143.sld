;;;; R7RS module for SRFI 143

(define-library (srfi-143)

  (import (rename (scheme base)
                  (exact-integer-sqrt fxsqrt)))

  (export fx-width fx-greatest fx-least)
  (export fixnum? fx=? fx<? fx>? fx<=? fx>=?
          fxzero? fxpositive? fxnegative?
          fxodd? fxeven? fxmax fxmin)
  (export fx+ fx- fxneg fx* fxquotient fxremainder
          fxabs fxsquare fxsqrt)
  (export fx+/carry fx-/carry fx*/carry)
  (export fxnot fxand fxior fxxor fxarithmetic-shift
          fxarithmetic-shift-left fxarithmetic-shift-right
          fxbit-count fxlength fxif fxbit-set? fxcopy-bit
          fxfirst-set-bit fxbit-field
          fxbit-field-rotate fxbit-field-reverse)

  ;; Provide Chicken emulation
  (include "rubber-chicken.scm")

  ;; Provide core bitwise functions
  (cond-expand
    (chibi
      (include-shared "srfi/142/bit")
      (begin
        (define (fxnot i) (- -1 i))
  
        (define (make-nary proc2 default)
          (lambda args
            (if (null? args)
                default
                (let lp ((i (car args)) (ls (cdr args)))
                  (if (null? ls)
                      i
                      (lp (proc2 i (car ls)) (cdr ls)))))))
  
        (define fxand  (make-nary bit-and  -1))
        (define fxior  (make-nary bit-ior   0))
        (define fxxor  (make-nary bit-xor   0))
        (define fxlength integer-length)
        (define fxbit-count bit-count)
        (define fxarithmetic-shift-left arithmetic-shift)
        (define (fxarithmetic-shift-right i count)
          (fxarithmetic-shift-left i (- count)))))

    (gauche
      (import (only (gauche base)
                    integer-length))
      (import (rename (only (gauche base)
                            lognot logand logior logxor ash)
                      (lognot fxnot)
                      (logand fxand)
                      (logior fxior)
                      (logxor fxxor)
                      (ash arithmetic-shift-left)))
      (begin
        (define (arithmetic-shift-right i count)
          (arithmetic-shift-left i (- count)))))

    (else (include "fxcore.scm")))


  ;; Stable part of the implementation
  (include "carries.scm")
  (include "srfi-143-impl.scm")
)
