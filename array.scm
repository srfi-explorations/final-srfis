;;; array
;;; 1997 - 2001 Jussi Piitulainen

;;; --- Intro ---

;;; This interface to arrays is based on Alan Bawden's array.scm of
;;; 1993 (earlier version in the Internet Repository and another
;;; version in SLIB). This is a complete rewrite, to be consistent
;;; with the rest of Scheme and to make arrays independent of lists.

;;; This implementation uses the same names since the names are right.

;;; (array? obj)
;;; (make-array shape [obj])             changed arguments
;;; (shape bound ...)                    new
;;; (array shape obj ...)                new
;;; (array-dimensions array)             changed name
;;; (array-shape array)                  shape is an array
;;; (array-ref array k ...) 
;;; (array-set! array k ... obj)         changed argument order
;;; (share-array array shape proc)       changed arguments

;;; All other variables in this file have names in "array:".

;;; Should there be a way to make arrays with initial values mapped
;;; from indices. Sure. The current "initial object" is lame.
;;;
;;; Should there be a way to make immutable arrays?
;;;
;;; What should array-dimensions really be called?

;;; --- Representation type dependencies ---

;;; The mapping from array indices to the index to the underlying vector
;;; is whatever array:optimize returns. The file "opt" provides three
;;; representations:
;;; 
;;; type1) mapping is a procedure that allows optional arguments
;;; type2) mapping is a procedure that takes exactly the indices
;;; type3) mapping is a list of a constant term and coefficients
;;;
;;; Choose one in "opt" to make the optimizer. Then choose the matching
;;; implementation of ixr and ixs here (for array-ref and array-set!).
;;; The idea behing type2 is that a fixed number of arguments is faster.
;;; Unfortunately, that requires the dispatch in ixs to get rid of the
;;; extra argument, so the good idea turns out rather messy in the end.
;;;
;;; These should be made macros to inline them. Or have a good compiler
;;; and plant the package as a module.

;;; 1. Pick an optimizer.
;;; 2. Pick matching index representation.
;;; 3. Pick a record implementation; as-vector is generic; syntax inlines.
;;; 3. This file is otherwise portable.

;;; --- Portable R5RS (R4RS and multiple values) ---

;;; (array? obj)
;;; returns #t if `obj' is an array and #t or #f otherwise.

(define (array? obj)
   (array:array? obj))

;;; (make-array shape)
;;; (make-array shape obj)
;;; makes array of `shape' with each cell containing `obj' initially.

(define (make-array shape . rest)
  (or (array:good-shape? shape)
      (error "make-array: shape is not a shape"))
  (let ((size (array:size shape)))
    (array:make
     (if (pair? rest)
         (apply (lambda (o) (make-vector size o)) rest)
         (make-vector size))
     (array:optimize
      (array:make-index shape)
      (array:dimensions shape)
      (zero? size))
     shape
     #t)))

;;; (shape bound ...)
;;; makes a shape. Bounds must be an even number of exact, pairwise
;;; non-decreasing integers. Note that any such array can be a shape.

(define (shape . bounds)
  (let ((v (list->vector bounds)))
    (or (even? (vector-length v))
        (error (string-append "shape: uneven number of bounds: "
                              (array:list->string bounds))))
    (let ((shp (array:make
                v
                (if (pair? bounds) (array:shape-ixr) (array:empty-ixr))
                (array:make
                 `#(0 ,(quotient (vector-length v) 2) 0 2)
                 (array:shape-ixr)
                 #f
                 #f)
                #f)))
      (or (array:good-shape? shp)
          (error (string-append "shape: bounds are not pairwise "
                                "non-decreasing exact integers: "
                                (array:list->string bounds))))
      shp)))

;;; (array shape obj ...)
;;; is analogous to `vector'.

(define (array shape . elts)
  (or (array:good-shape? shape)
      (error (string-append "array: shape " (array:to-string shape)
                            " is not a shape")))
  (let ((size (array:size shape)))
    (let ((vector (list->vector elts)))
      (or (= (vector-length vector) size)
          (error (string-append "array: an array of shape "
                                (array:shape-vector->string
                                 (array:vector shape))
                                " has "
                                (number->string size)
                                " elements but got "
                                (number->string (vector-length vector))
                                " values: "
                                (array:list->string elts))))
      (array:make
       vector
       (array:optimize
        (array:make-index shape)
        (array:dimensions shape)
        (zero? size))
       shape
       #t))))

;;; (array-dimensions array)
;;; returns the number of dimensions of `array'.

(define (array-dimensions array)
   (array:dimensions (array:shape array)))

(define (array:dimensions shape)
   (vector-ref (array:vector (array:shape shape)) 1))

;;; (array-shape array)
;;; returns the shape of `array' as a dimensions times two, zero-based
;;; array.  It is an error to modify a shape.

(define (array-shape array)
   (array:shape array))

;;; (array-ref array k ...)
;;; returns the contents of the element of `array' at `k ...'. The
;;; error check seems expensive.

(define (array-ref array . ks)
  (or (array:good-indices? ks (array:vector (array:shape array)))
      (error (array:not-in "array-ref"
                           ks
                           (array:vector (array:shape array)))))
  (vector-ref (array:vector array) (array:ixr (array:index array) ks)))

;;; (array-set! array k ... obj)
;;; replaces the contents of the element of `array' at `k ...' with
;;; `obj'. The error check seems expensive.

(define (array-set! array . spec)
  (or (array:mutable? array)
      (error "array-set!: array is not mutable"))
  (or (array:good-indices/value? spec (array:vector (array:shape array)))
      (error (array:not-in "array-set!"
                           (reverse (cdr (reverse spec)))
                           (array:vector (array:shape array)))))
   (vector-set! (array:vector array)
                (array:ixs (array:index array)
			   (array:dimensions (array:shape array))
			   spec)
		(do ((_ (car spec) (car r))
                     (r (cdr spec) (cdr r)))
                  ((null? r) _))))

;;; (share-array array shape proc)
;;; makes an array that shares elements of `array' at shape `shape'.
;;; The arguments to `proc' are indices of the result.  The values of
;;; `proc' are indices of `array'.

;;; Todo: in the error message, should recognise the mapping and show it.

(define (share-array array subshape f)
  (or (array:good-shape? subshape)
      (error (string-append "share-array: shape "
                            (array:thing->string subshape)
                            " is not a shape")))
  (let ((subsize (array:size subshape)))
    (or (array:good-share? subshape subsize f (array:shape array))
        (error (string-append "share-array: subshape "
                              (array:shape-vector->string
                               (array:vector subshape))
                              " does not map into supershape "
                              (array:shape-vector->string
                               (array:vector (array:shape array)))
                              " under mapping")))
    (let ((g (array:index array)))
      (array:make
       (array:vector array)
       (array:optimize
        (lambda ks
          (call-with-values
           (lambda () (apply f ks))
           (lambda ks (array:ixr g ks))))
        (array:dimensions subshape)
        (zero? subsize))
       subshape
       (array:mutable? array)))))


;;; --- Internals ---

;;; (array:size shape)
;;; returns the number of elements in arrays of shape `shape'.

(define (array:size shape)
   (let ((shv (array:vector shape)))
      (let ((shp (vector-length shv)))
	 (do   ((k 0 (+ k 2))
		(s 1 (* s (- (vector-ref shv (+ k 1))
			     (vector-ref shv k)))))
	       ((= k shp) s)))))

;;; (array:make-index shape)
;;; returns an index function for arrays of shape `shape'. This is a
;;; runtime composition of several variable arity procedures, to be
;;; passed to array:optimize for recognition as a linear function of
;;; as many variables as there are dimensions in arrays of this shape.

(define (array:make-index shape)
   (let ((shv (array:vector shape)))
      (do   ((f (lambda () 0)
		(lambda (k . ks)
		   (+ (* s (- k (vector-ref shv (- j 2))))
		      (apply f ks))))
	     (s 1 (* s (- (vector-ref shv (- j 1))
			  (vector-ref shv (- j 2)))))
	     (j (vector-length shv) (- j 2)))
	    ((= j 0) f))))


;;; --- Error checking ---

;;; (array:good-shape? shape)
;;; returns true if `shape' is an array of the right shape and its
;;; elements are exact integers that pairwise bound intervals `[lo..hi)´.

(define (array:good-shape? shape)
  (and (array:array? shape)
       (let ((u (array:vector (array:shape shape))))
         (and (= (vector-length u) 4)
              (= (vector-ref u 0) 0)
              (= (vector-ref u 2) 0)
              (= (vector-ref u 3) 2)))
       (let ((v (array:vector shape)))
         (let ((p (vector-length v)))
           (do   ((k 0 (+ k 2))
                  (true #t (let ((lo (vector-ref v k))
                                 (hi (vector-ref v (+ k 1))))
                             (and true
                                  (integer? lo)
                                  (exact? lo)
                                  (integer? hi)
                                  (exact? hi)
                                  (<= lo hi)))))
             ((= k p) true))))))

;;; (array:good-share? subv subsize mapping superv)
;;; returns true if the extreme indices in the subshape vector map
;;; into the bounds in the supershape vector.

;;; If some interval in `subv' is empty, then `subv' is empty and its
;;; image under `f' is empty and it is trivially alright.  One must
;;; not call `f', though.

(define (array:good-share? subshape subsize f supershape)
  (or (zero? subsize)
      (letrec
          ((sub (array:vector subshape))
           (super (array:vector supershape))
           (ck (lambda (k ks)
		 (if (zero? k)
                     (call-with-values
                      (lambda () (apply f ks))
                      (lambda qs (array:good-indices? qs super)))
                     (and (ck (- k 2)
                              (cons (vector-ref sub (- k 2)) ks))
                          (ck (- k 2)
                              (cons (- (vector-ref sub (- k 1)) 1) ks)))))))
        (ck (vector-length sub) '()))))

;;; (array:good-indices? indices shape-vector)

(define (array:good-indices? ks shv)
   (let ((d2 (vector-length shv)))
      (do   ((kp ks (cdr kp))
	     (k 0 (+ k 2))
             (true #t (and true (array:good-index? kp shv k))))
        ((= k d2)
         (and true (null? kp))))))

;;; (array:good-indices/value? indices/value shape-vector)

(define (array:good-indices/value? ksv shv)
   (let ((d2 (vector-length shv)))
     (do   ((kp ksv (cdr kp))
            (k 0 (+ k 2))
            (true #t (and true (array:good-index? kp shv k))))
       ((= k d2)
        (and true (pair? kp) (null? (cdr kp)))))))

;;; (array:good-index? indices-tail shape-vector dimension)
;;; returns true if first index is valid for dimension index.

(define (array:good-index? kp shv k)
   (and (pair? kp)
        (let ((w (car kp)))
          (and (integer? w)
               (exact? w)
               (<= (vector-ref shv k) w (- (vector-ref shv (+ k 1)) 1))))))

(define (array:not-in who ks shv)
  (let ((index (array:list->string ks))
        (bounds (array:shape-vector->string shv)))
    (array:error (string-append who
                                ": index " index
                                " not in bounds " bounds))))

(define (array:list->string ks)
  (do ((index "" (string-append index (array:thing->string (car ks)) " "))
       (ks ks (cdr ks)))
    ((null? ks) index)))

(define (array:shape-vector->string shv)
  (do ((bounds "" (string-append bounds
                                 "["
                                 (number->string (vector-ref shv t))
                                 ".."
                                 (number->string (vector-ref shv (+ t 1)))
                                 ")"
                                 " "))
       (t 0 (+ t 2)))
    ((= t (vector-length shv)) bounds)))

(define (array:thing->string thing)
  (cond
    ((number? thing) (number->string thing))
    ((symbol? thing) (string-append "#<symbol>" (symbol->string thing)))
    ((char? thing) "#<char>")
    ((string? thing) "#<string>")
    ((list? thing) (string-append "#" (number->string (length thing))
                                  "<list>"))
                                  
    ((pair? thing) "#<pair>")
    ((array? thing) "#<array>")
    ((vector? thing) (string-append "#" (number->string
                                         (vector-length thing))
                                    "<vector>"))
    ((procedure? thing) "#<procedure>")
    (else
     (case thing
       ((()) "()")
       ((#t) "#t")
       ((#f) "#f")
       (else
        "#<whatsit>")))))
