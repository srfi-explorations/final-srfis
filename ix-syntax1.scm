;;; array ix-syntax1

(define-syntax array:ixr (syntax-rules () ((array:ixr f ks) (apply f ks))))

(define-syntax array:ixs (syntax-rules () ((array:ixs f r kz) (apply f kz))))

(define-syntax array:shape-ixr
  (syntax-rules () ((array:shape-ixr) (lambda (r k . _) (+ k r r)))))

(define-syntax array:empty-ixr
  (syntax-rules () ((array:empty-ixr) (lambda (r k . _) -1))))
