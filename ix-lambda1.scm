;;; array index1

(define (array:ixr f ks) (apply f ks))

(define (array:ixs f r kz) (apply f kz))

(define (array:shape-ixr) (lambda (r k . _) (+ k r r)))
(define (array:empty-ixr) (lambda (r k . _) -1))
