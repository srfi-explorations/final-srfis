;;; array as-srfi-9-record
;;; 2001 Jussi Piitulainen

(define-record-type array:srfi-9-record-type-descriptor
                    (array:make vec ind shp mut)
                    array:array?
                    (vec array:vector)
                    (ind array:index)
                    (shp array:srfi-9-shape)
                    (mut array:mutable?))

(define (array:shape array)
  (let ((fixed-point (array:make '#(0 2 0 2) (array:shape-ixr) #f #f)))
    (lambda (array)
      (or (array:srfi-9-shape array)
          fixed-point))))
