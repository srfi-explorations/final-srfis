;;; array ix-syntax3

(define-syntax array:ixr
  (syntax-rules
   ()
   ((array:ixr ns0 ks0)
    (let ((ns ns0)
	  (ks ks0))
      (do ((k (car ns) (+ k (* (car ns) (car ks))))
	   (ns (cdr ns) (cdr ns))
	   (ks ks (cdr ks)))
	  ((null? ns)
	   k))))))

(define-syntax array:ixs
  (syntax-rules
   ()
   ((array:ixs ns r kz) (array:ixr ns kz))))

(define-syntax array:shape-ixr
  (syntax-rules
   ()
   ((array:shape-ixr) '(0 2 1))))

(define-syntax array:empty-ixr
  (syntax-rules
   ()
   ((array:empty-ixr) '(-1 0 0))))

