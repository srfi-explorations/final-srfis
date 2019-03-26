;;;; R6RS module for SRFI 143

(library (srfi-143)
  (export fx-width fx-greatest fx-least
          fixnum? fx=? fx<? fx>? fx<=? fx>=?
          fxzero? fxpositive? fxnegative?
          fxodd? fxeven? fxmax fxmin
          fx+ fx- fxneg fx* fxquotient fxremainder
          fxabs fxsquare fxsqrt
          fx+/carry fx-/carry fx*/carry
          fxnot fxand fxior fxxor fxarithmetic-shift
          fxarithmetic-shift-left fxarithmetic-shift-right
          fxbit-count fxlength fxif fxbit-set? fxcopy-bit
          fxfirst-set-bit fxbit-field
          fxbit-field-rotate fxbit-field-reverse)

  (import (rename (rnrs base)
                  (exact-integer-sqrt fxsqrt))
          (rename (except (rnrs arithmetic fixnums) fxcopy-bit)
            (fxfirst-bit-set fxfirst-set-bit)
            (fxbit-count r6rs:fxbit-count)
            (fxbit-set? r6rs:fxbit-set?)
            (fxreverse-bit-field fxbit-field-reverse))
          (rename (only (rnrs r5rs) quotient remainder)
                  (quotient fxquotient)
                  (remainder fxremainder)))

  ;; Constants not in R6RS
  (define fx-width (fixnum-width))
  (define fx-greatest (greatest-fixnum))
  (define fx-least (least-fixnum))

  ;; Procedures not in R6RS
  (define (fxneg i) (fx- i))

  (define (fxabs i)
    (if (fxnegative? i) (- i) i))

  (define (fxsquare i)
    (fx* i i))

  ;; Incompatible semantics
  (define (fxbit-count i)
    (if (fx>=? i 0)
      (r6rs:fxbit-count i)
      (r6rs:fxbit-count (fxnot i))))

  ;; R6RS fxcopy-bit is only loosely related: we don't use it
  (define (fxcopy-bit index to bool)
    (if bool
        (fxior to (fxarithmetic-shift-left 1 index))
        (fxand to (fxnot (fxarithmetic-shift-left 1 index)))))

  ;; Imcompatible argument orderings
  (define (fxbit-set? index i)
    (r6rs:fxbit-set? i index))

  (define (fxbit-field-rotate i count start end)
    (if (fxnegative? count)
      (fxrotate-bit-field i start end (+ count (- end start)))
      (fxrotate-bit-field i start end count)))
)
