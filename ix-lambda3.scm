;;; array ix-lambda3

(define (array:ixr ns ks)
  (do ((k (car ns) (+ k (* (car ns) (car ks))))
       (ns (cdr ns) (cdr ns))
       (ks ks (cdr ks)))
    ((null? ns)
     k)))

(define (array:ixs ns r kz) (array:ixr ns kz))

(define (array:shape-ixr) '(0 2 1))
(define (array:empty-ixr) '(-1 0 0))
