;;; arrays as-plt-struct

(define-values (array:make array:array?
                           array:vector
                           array:index
                           array:shape
                           array:mutable?)
  (call-with-values
   (lambda () (struct array (vec idx shp mut)))
   (lambda (_ make array? vec set-vec idx set-idx shp set-shp mut set-mut)
     (values make
             array?
             vec
             idx
             (let ((fixed-point (make '#(0 2 0 2) (array:shape-ixr) #f #f)))
               (lambda (array)
                 (or (shp array)
                     fixed-point)))
             mut))))
