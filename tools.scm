;;; array tools
;;; 2001 Jussi Piitulainen

;;; (build-array shape proc)
;;; returns a newly allocated array of the given shape with initial
;;; contents at each index whatever proc returns given the indices.

(define (build-array shape bob)
  (let* ((arr (make-array shape))
         (dim (array-dimensions arr)))
    (case dim
      ((0) (array-set! arr (bob)))
      (else
       (let ((last-box (cons '* '*))
             (value-box (cons '* '())))
         (do ((first-box last-box (cons '* first-box))
              (bak (- dim 1) (- bak 1)))
           ((= bak 0)
            (let wok ((d 0) (box-d first-box))
              (if (< d dim)
                  (let ((bd (array-ref shape d 0))
                        (ed (array-ref shape d 1)))
                    (do ((k bd (+ k 1)))
                      ((= k ed))
                      (set-car! box-d k) ; Is this gross or not?
                      (wok (+ d 1) (cdr box-d))))
                  (begin
                    (set-cdr! last-box '())
                    (set-car! value-box (apply bob first-box))
                    (set-cdr! last-box value-box)
                    (apply array-set! arr first-box)))))))))
    arr))

;;; This compares elements with equal? so elements better not be arrays.

(define (array-equal? a b)
  (let ((an (array-dimensions a))
        (bn (array-dimensions b)))
    (and (= an bn)
         (let ((as (array-shape a))
               (bs (array-shape b)))
           (and (do ((k 0 (+ k 1))
                     (true #t (and true
                                   (= (array-ref as k 0)
                                      (array-ref bs k 0))
                                   (= (array-ref as k 1)
                                      (array-ref bs k 1)))))
                  ((= k an) true))
                (do ((ks '() (cons '* ks))
                     (dn an (- dn 1)))
                  ((= dn 0)
                   (let wok ((d 0) (box-d ks))
                     (if (< d an)
                         (let ((bd (array-ref as d 0))
                               (ed (array-ref as d 1)))
                           (do ((k bd (+ k 1))
                                (true #t (and true 
                                              (wok (+ d 1) (cdr box-d)))))
                             ((= k ed) true)
                             (set-car! box-d k)))
                         (equal? (apply array-ref a ks)
                                 (apply array-ref b ks)))))))))))

;;; This is matrix multiplication for (transpose array . permutation).

(define (times a b)
  (or (and (= (array-dimensions a) 2)
           (= (array-dimensions b) 2))
      (error "times: arrays are not matrices"))
  (let ((as (array-shape a))
        (bs (array-shape b)))
    (let ((r0 (array-ref as 0 0))
          (rn (array-ref as 0 1))
          (t0 (array-ref as 1 0))
          (tn (array-ref as 1 1))
          (u0 (array-ref bs 0 0))
          (un (array-ref bs 0 1)) 
          (k0 (array-ref bs 1 0))
          (kn (array-ref bs 1 1)))
      (or (= (- tn t0) (- un u0))
          (error "times: matrices are not compatible"))
      (let ((ab (make-array (shape r0 rn k0 kn))))
        (do ((r r0 (+ r 1)))
          ((= r rn))
          (do ((k k0 (+ k 1)))
            ((= k kn))
            (do ((t t0 (+ t 1))
                 (u u0 (+ u 1))
                 (s 0 (+ s (* (array-ref a r t)
                              (array-ref b u k)))))
              ((and (= t tn)
                    (= u un))
               (array-set! ab r k s)))))
        ab))))

; This is a generalized transpose. It can permute the dimensions any which 
; way. The permutation is provided by a permutation matrix: a square matrix
; of zeros and ones, with exactly one one in each row and column, or a
; permutation of the rows of an identity matrix; the size of the matrix
; must match the number of dimensions of the array.
;
; The default permutation is [ 0 1 | 1 0 ] of course, but any permutation
; array can be specified, and the shape array of the original array is then
; multiplied with it, and index column vectors of the new array with its
; inverse, from left, to permute the rows appropriately.

; Note that zero-dimensional arrays are a bit delicate again. The
; permutation must be null and must not be interpreted as missing.

(define (transpose a . p0)
  (let* ((d (array-dimensions a))
         (p (if (and (= d 2)
                     (null? p0))
                (array (shape 0 d
                              0 d)
                       0 1
                       1 0)
                (apply array
                       (shape 0 d
                              0 d)
                       p0)))
         (q (share-array p       ;inverse of p is transpose of p
                         (array-shape p)
                         (lambda (r k)
                           (values k r)))))
    (share-array
     a
     (times p (array-shape a))
     (lambda ks0
       (let ((ks1 (times
                   q
                   (apply array
                          (shape 0 d
                                 0 1)
                          ks0))))
         (do ((j d (- j 1))
              (ks2 '()
                   (cons (array-ref ks1
                                    (- j 1)
                                    0)
                         ks2)))
           ((= j 0)
            (apply values ks2))))))))
