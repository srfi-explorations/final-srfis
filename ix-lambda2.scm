;;; array ix-lambda2

(define (array:ixr f ks) (apply f ks))

(define (array:ixs f r kz)
  (case r
    ((0) (apply (lambda (_) (f)) kz))
    ((1) (apply (lambda (k1 _) (f k1)) kz))
    ((2) (apply (lambda (k1 k2 _) (f k1 k2)) kz))
    ((3) (apply (lambda (k1 k2 k3 _) (f k1 k2 k3)) kz))
    (else (apply f kz))))

(define (array:shape-ixr) (lambda (r k) (+ k r r)))
(define (array:empty-ixr) (lambda (r k) -1))
