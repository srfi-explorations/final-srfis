;;; array as-vector
;;; 1997 - 2001 Jussi Piitulainen

;;; --- Implementation dependencies ---

;;; Records are not standard in Scheme. An array representation consists of
;;; four fields: underlying vector, index mapping, shape, mutability flag.
;;; The vector representation here adds a type tag. Note that array:shape
;;; is not just an accessor.

(define array:*tag* (list 'array))

(define (array:make vec idx shp mut)
   (vector vec idx shp mut array:*tag*))

(define (array:vector array)
   (vector-ref array 0))

(define (array:index array)
   (vector-ref array 1))

;;; `Array:shape' reaches [ 0 2 | 0 2 ] in three iterations.

(define array:shape
   (let ((fixed-point (array:make '#(0 2 0 2) (array:shape-ixr) #f #f)))
      (lambda (array)
	 (or (vector-ref array 2) fixed-point))))

(define (array:mutable? array)
  (vector-ref array 3))

(define (array:array? obj)
   (and (vector? obj)
	(= (vector-length obj) 5)
	(eq? (vector-ref obj 4) array:*tag*)))
