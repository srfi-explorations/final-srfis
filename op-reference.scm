(begin
  (define (array:opt-make) (list 'type3 "array:" 0))
  (define (array:optimize f r empty?)
    (if empty?
      (cons -1 (do ((k 0 (+ k 1)) (t '() (cons 0 t))) ((= k r) t)))
      (case r
        (else
         (let ((v (do ((k 0 (+ k 1)) (v '() (cons 0 v))) ((= k r) v))))
           (let ((n0 (apply f v)))
             (array:n n0 (array:coefficients f n0 v v))))))))
  (define (array:coefficients f n0 vs vp)
    (case vp
      ((()) '())
      (else
       (set-car! vp 1)
       (let ((n (- (apply f vs) n0)))
         (set-car! vp 0)
         (cons n (array:coefficients f n0 vs (cdr vp)))))))
  (define (array:n n0 ns) (cons n0 ns)))
