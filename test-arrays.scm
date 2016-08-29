;;(declare (standard-bindings)(extended-bindings)(block)(not safe) (fixnum))
(declare (inlining-limit 0))
(define tests 10000)

(define-macro (test expr value)
  `(let* (;(ignore (pretty-print ',expr))
	  (result (call-with-current-continuation
		   (lambda (c)
		     (with-exception-catcher
		      (lambda (args)
			(cond ((error-exception? args)
			       (c (error-exception-message args)))
			      ;; I don't expect any of these, but it sure makes debugging easier
			      ((unbound-global-exception? args)
			       (unbound-global-exception-variable args))
			      (else
			       "piffle")))
		      
		      (lambda ()
			,expr))))))
     (if (not (equal? result ,value))
	 (pp (list ',expr" => " result ", not " ,value)))))

(define-macro (test-multiple-values expr vals)
  `(call-with-values
       (lambda () ,expr)
     (lambda args
       (if (not (equal? args ,vals))
	   (pp (list ',expr  " => " args ", not " ,vals #\newline))))))

(define-macro (do-times  var n . body)
  ;; not so useful when the debugger can't report line numbers inside macros.
  `(do ((,var 0 (+ ,var 1)))
       ((= ,var ,n))
     ,@body))

(define (random a #!optional b)
  (if b
      (+ a (random-integer (- b a)))
      (random-integer a)))

(include "generic-arrays.scm")

(declare (generic))

(pp "Interval error tests")

(test (make-interval 1 '#(3 4))
      "make-interval: lower-bounds must be a vector: ")

(test (make-interval '#(1 1)  3)
      "make-interval: upper-bounds must be a vector: ")

(test (make-interval '#(1 1)  '#(3))
      "make-interval: lower-bounds and upper-bounds must be the same length: ")

(test (make-interval '#()  '#())
      "make-interval: lower-bounds and upper-bounds must be nonempty vectors: ")

(test (make-interval '#(1.)  '#(1))
      "make-interval: All lower-bounds must be exact integers: ")

(test (make-interval '#(1 #f)  '#(1 2))
      "make-interval: All lower-bounds must be exact integers: ")

(test (make-interval '#(1)  '#(1.))
      "make-interval: All upper-bounds must be exact integers: ")

(test (make-interval '#(1 1)  '#(1 #f))
      "make-interval: All upper-bounds must be exact integers: ")

(test (make-interval '#(1)  '#(1))
      "make-interval: Each lower-bound must be less than the associated upper-bound: ")

(test (make-interval '#(1 2 3)  '#(4 2 6))
      "make-interval: Each lower-bound must be less than the associated upper-bound: ")

(pp "interval result tests")

(test (make-interval '#(11111)  '#(11112))
      (make-interval '#(11111) '#(11112)))

(test (make-interval '#(1 2 3)  '#(4 5 6))
      (make-interval '#(1 2 3) '#(4 5 6)))

(pp "interval? result tests")

(test (interval? #t)
      #f)

(test (interval? (make-interval '#(1 2 3) '#(4 5 6)))
      #t)


(pp "interval-dimension error tests")

(test (interval-dimension 1)
      "interval-dimension: argument is not an interval: ")

(pp "interval-dimension result tests")

(test (interval-dimension (make-interval '#(1 2 3) '#(4 5 6)))
      3)

(pp "interval-lower-bound error tests")

(test (interval-lower-bound 1 0)
      "interval-lower-bound: argument is not an interval: ")

(test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-lower-bound: argument is not an exact integer: ")

(test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 1.)
      "interval-lower-bound: argument is not an exact integer: ")

(test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) -1)
      "interval-lower-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 3)
      "interval-lower-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 4)
      "interval-lower-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(pp "interval-upper-bound error tests")

(test (interval-upper-bound 1 0)
      "interval-upper-bound: argument is not an interval: ")

(test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-upper-bound: argument is not an exact integer: ")

(test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 1.)
      "interval-upper-bound: argument is not an exact integer: ")

(test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) -1)
      "interval-upper-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 3)
      "interval-upper-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 4)
      "interval-upper-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(pp "interval-lower-bounds->list error tests")

(test (interval-lower-bounds->list 1)
      "interval-lower-bounds->list: argument is not an interval: ")

(pp "interval-upper-bounds->list error tests")

(test (interval-upper-bounds->list #f)
      "interval-upper-bounds->list: argument is not an interval: ")

(pp "interval-lower-bound, interval-upper-bound, interval-lower-bounds->list, and interval-upper-bounds->list result tests")



(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (let ((interval (make-interval (list->vector lower)
				   (list->vector upper)))
	  (offset (random (length lower))))
      (test (interval-lower-bound interval offset)
	    (list-ref lower offset))
      (test (interval-upper-bound interval offset)
	    (list-ref upper offset))
      (test (interval-lower-bounds->list interval)
	    lower)
      (test (interval-upper-bounds->list interval)
	    upper))))

(pp "interval-lower-bounds->vector error tests")

(test (interval-lower-bounds->vector 1)
      "interval-lower-bounds->vector: argument is not an interval: ")

(pp "interval-upper-bounds-> error tests")

(test (interval-upper-bounds->vector #f)
      "interval-upper-bounds->vector: argument is not an interval: ")

(pp "interval-lower-bound, interval-upper-bound, interval-lower-bounds->vector, and interval-upper-bounds->vector result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (let ((interval (make-interval (list->vector lower)
				   (list->vector upper)))
	  (offset (random (length lower))))
      (test (interval-lower-bound interval offset)
	    (list-ref lower offset))
      (test (interval-upper-bound interval offset)
	    (list-ref upper offset))
      (test (interval-lower-bounds->vector interval)
	    (list->vector lower))
      (test (interval-upper-bounds->vector interval)
	    (list->vector upper)))))

(pp "interval-curry error tests")

(test (interval-curry 1 1)
      "interval-curry: argument is not an interval: ")

(test (interval-curry (make-interval '#(0) '#(1)) #t)
      "interval-curry: the dimension of the interval is not greater than 1: " )


(test (interval-curry (make-interval '#(0 0) '#(1 1)) 1/2)
      "interval-curry: argument is not an exact integer: ")

(test (interval-curry (make-interval '#(0 0) '#(1 1)) 1.)
      "interval-curry: argument is not an exact integer: ")

(test (interval-curry (make-interval '#(0 0) '#(1 1)) 0)
      "interval-curry: argument is not between 0 and (interval-dimension interval) (exclusive): ")

(test (interval-curry (make-interval '#(0 0) '#(1 1)) 2)
      "interval-curry: argument is not between 0 and (interval-dimension interval) (exclusive): ")

(pp "interval-curry result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 3 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower))
	 (left-dimension (random 1 (- (length lower) 1))))
    (test-multiple-values
     (interval-curry (make-interval (list->vector lower)
				    (list->vector upper))
		     left-dimension)
     (list (make-interval (list->vector (reverse (list-tail (reverse lower) (- (length lower) left-dimension))))
			  (list->vector (reverse (list-tail (reverse upper) (- (length upper) left-dimension)))))
	   (make-interval (list->vector (list-tail lower left-dimension))
			  (list->vector (list-tail upper left-dimension)))))))


(pp "interval-contains-multi-index? error tests")



(pp "interval-volume error tests")

(test (interval-volume #f)
      "interval-volume: argument is not an interval: ")

(pp "interval-volume result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (test (interval-volume (make-interval (list->vector lower)
					  (list->vector upper)))
	  (apply * (map - upper lower)))))

(pp "interval= error tests")

(test (interval= #f (make-interval '#(1 2 3) '#(4 5 6)))
      "interval=: Not all arguments are intervals: ")

(test (interval= (make-interval '#(1 2 3) '#(4 5 6)) #f)
      "interval=: Not all arguments are intervals: ")

(pp "interval= result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 1 6)))))
	 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
	 (lower2 (map (lambda (x) (random 2)) lower1))
	 (upper2 (map (lambda (x) (+ 1 (random 1 3) x)) lower2)))
    (test (interval= (make-interval (list->vector lower1)
				    (list->vector upper1))
		     (make-interval (list->vector lower2)
				    (list->vector upper2)))
	  (and (equal? lower1 lower2)                              ;; the probability of this happening is about 1/16
	       (equal? upper1 upper2)))))

(pp "interval-subset? error tests")

(test (interval-subset? #f (make-interval '#(1 2 3) '#(4 5 6)))
      "interval-subset?: Not all arguments are intervals: ")

(test (interval-subset? (make-interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-subset?: Not all arguments are intervals: ")

(pp "interval-subset? result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 1 6)))))
	 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
	 (lower2 (map (lambda (x) (random 2)) lower1))
	 (upper2 (map (lambda (x) (+ (random 1 3) x)) lower2)))
    (test (interval-subset? (make-interval (list->vector lower1)
					   (list->vector upper1))
			    (make-interval (list->vector lower2)
					   (list->vector upper2)))
	  (and (##every >= lower1 lower2)
	       (##every <= upper1 upper2)))))

(pp "interval-contains-multi-index?  error tests")

(test (interval-contains-multi-index? 1 1)
      "interval-contains-multi-index?: argument is not an interval: ")

(test (interval-contains-multi-index? (make-interval '#(1 2 3) '#(4 5 6)) 1)
      "interval-contains-multi-index?: dimension of interval does not match number of arguments: ")

(test (interval-contains-multi-index? (make-interval '#(1 2 3) '#(4 5 6)) 1 1/2 0.1)
      "interval-contains-multi-index?: at least one multi-index component is not an exact integer: ")

(pp "interval-contains-multi-index?  result tests")

(let ((interval   (make-interval '#(1 2 3) '#(4 5 6)))
      (interval-2 (make-interval '#(10 11 12) '#(13 14 15))))
  (if (not (interval-reduce list
			    (lambda (result x)
			      (and result (apply interval-contains-multi-index? interval x)))
			    #t
			    interval))
      (error "these should all be true"))
  (if (not (interval-reduce list
			    (lambda (result x)
			      (and result (not (apply interval-contains-multi-index? interval x))))
			    #t
			    interval-2))
      (error "these should all be false")))

(pp "interval-for-each error tests")

(test (interval-for-each (lambda (x) x) 1)
      "interval-for-each: Argument is not a interval: ")

(test (interval-for-each 1 (make-interval '#(3) '#(4)))
      "interval-for-each: Argument is not a procedure: ")

(define (iota a b)
  (if (= a b)
      '()
      (cons a (iota (+ a 1) b))))

(define (all-elements lower upper)
  (if (null? (cdr lower))
      (map list (iota (car lower) (car upper)))
      (apply append (map (lambda (x)
			   (map (lambda (y)
				  (cons x y))
				(all-elements (cdr lower) (cdr upper))))
			 (iota (car lower) (car upper))))))

(pp "interval-for-each result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10))
		     (vector->list (make-vector (random 1 7)))))
	 (upper (map (lambda (x) (+ (random 1 4) x))
		     lower)))
    (let ((result '()))

      (define (f . args)
	(set! result (cons args result)))

      (test (let ()
	      (interval-for-each f
				 (make-interval (list->vector lower)
						(list->vector upper)))
	      result)
	    (reverse (all-elements lower upper))))))

;;; can't come up with a reasonable test for interval-for-each

(pp "interval-reduce error tests")

(test (interval-reduce #t #t #t #t)
      "interval-reduce: Argument is not a interval: ")

(test (interval-reduce #f #f #f (make-interval '#(2 3) '#(4 5)))
      "interval-reduce: Argument is not a procedure: ")

(test (interval-reduce (lambda (x) x) #f #f (make-interval '#(2 3) '#(4 5)))
      "interval-reduce: Operator is not a procedure: ")


(pp "interval-reduce result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10))
		     (vector->list (make-vector (random 1 7)))))
	 (upper (map (lambda (x) (+ (random 1 4) x))
		     lower))
	 (result (reverse (all-elements lower upper))))
    
    (define (f . args)
      args)
    
    (test (interval-reduce list
			   (lambda (x y) (cons y x))
			   '()
			   (make-interval (list->vector lower)
					  (list->vector upper)))
	  result)
    (test (interval-reduce list
			   (lambda (x y) (cons y x))
			   '()
			   (make-interval (list->vector lower)
					  (list->vector upper))
			   #f)
	  result)))

(pp "interval-dilate error tests")

(let ((interval (make-interval '#(0 0) '#(100 100))))
  (test (interval-dilate interval 'a '#(-10 10))
	"interval-dilate: second argument must be a vector: ")
  (test (interval-dilate 'a '#(10 10) '#(-10 -10))
	"interval-dilate: first argument is not an interval: ")
  (test (interval-dilate interval '#(10 10) 'a)
	"interval-dilate: third argument must be a vector: ")
  (test (interval-dilate interval '#(10) '#(-10 -10))
	"interval-dilate: The second and third arguments must have the same length as the dimension of the first argument: ")
  (test (interval-dilate interval '#(10 10) '#( -10 a))
	"interval-dilate: The second and third arguments must contain only exact integers: ")
  (test (interval-dilate interval '#(10 10) '#( -10))
	"interval-dilate: The second and third arguments must have the same length as the dimension of the first argument: ")
  (test (interval-dilate interval '#(100 100) '#(-100 -100))
	"interval-dilate: the resulting interval is empty: "))



;;; define random-interval, random-multi-index

(define (random-multi-index interval)
  (apply values
	 (apply map
		random
		(map (lambda (bounds)
		       (bounds interval))
		     (list interval-lower-bounds->list
			   interval-upper-bounds->list)))))


(define (random-interval #!optional (min 1) (max 11) )
  ;; a random interval with min <= dimension < max
  ;; positive and negative lower bounds
  (let* ((lower
	  (map (lambda (x)
		 (random -10 10))
	       (vector->list (make-vector (random min max)))))
	 (upper
	  (map (lambda (x)
		 (+ (random 1 11) x))
	       lower)))
    (make-interval (list->vector lower)
		   (list->vector upper))))



(pp "array error tests")

(test (make-array 1 values)
      "make-array: domain is not an interval: ")

(test (make-array (make-interval '#(3) '#(4)) 1)
      "make-array: getter is not a procedure: ")

(pp "array result tests")

(let ((getter (lambda args 1.)))
  (test (make-array (make-interval '#(3) '#(4)) getter)
	(make-##array-base (make-interval '#(3) '#(4))
			   getter
			   #f
			   #f
			   #f
			   #f
			   #f)))

(pp "array-domain and array-getter error tests")

(test (array-domain #f)
      "array-domain: object is not an array: ")

(test (array-getter #f)
      "array-getter: object is not an array: ")

(pp "array?, array-domain, and array-getter result tests")

(let* ((getter (lambda args 1.))
       (array    (make-array (make-interval '#(3) '#(4)) getter)))
  (test (array? #f)
	#f)
  (test (array? array)
	#t)
  (test (array-domain array)
	(make-interval '#(3) '#(4)))
  (test (array-getter array)
	getter))


(pp "mutable-array result tests")

(let ((result #f))
  (let ((getter (lambda (i) result))
	(setter   (lambda (v i) (set! result v)))
	(domain   (make-interval '#(3) '#(4))))
    (test (make-array domain
		      getter
		      setter)
	  (make-##array-base domain
			     getter
			     setter
			     #f
			     #f
			     #f
			     #f))))

(pp "array-setter error tests")

(test (array-setter #f)
      "array-setter: object is not an mutable array: ")

(pp "mutable-array? and array-setter result tests")

(let ((result (cons #f #f)))
  (let ((getter (lambda (i) (car result)))
	(setter   (lambda (v i) (set-car! result v)))
	(domain   (make-interval '#(3) '#(4))))
    (let ((array (make-array domain
			     getter
			     setter)))
      (test (array? array)
	    #t)
      (test (mutable-array? array)
	    #t)
      (test (mutable-array? 1)
	    #f)
      (test (array-setter array)
	    setter)
      (test (array-getter array)
	    getter)
      (test (array-domain array)
	    domain))))

(define (myindexer= indexer1 indexer2 interval)
  (interval-reduce (lambda args
		     (= (apply indexer1 args)
			(apply indexer2 args)))
		   (lambda (x y) (and x y))
		   #t
		   interval))


(define (my-indexer base lower-bounds increments)
  (lambda indices
    (apply + base (map * increments (map - indices lower-bounds)))))



(pp "new-indexer result tests")

(define (random-sign)
  (- 1 (* 2 (random 2))))

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower-bounds
	  (map (lambda (x) (random 2))
	       (vector->list (make-vector (random 1 7)))))
	 (upper-bounds
	  (map (lambda (x) (+ x (random 1 3)))
	       lower-bounds))
	 (new-domain
	  (make-interval (list->vector lower-bounds)
			 (list->vector upper-bounds)))
	 (new-domain-dimension
	  (interval-dimension new-domain))
	 (old-domain-dimension
	  (random 1 7))
	 (base
	  (random 100))
	 (coefficients
	  (map (lambda (x) (* (random-sign)
			      (random 20)))
	       (iota 0 old-domain-dimension)))
	 (old-indexer
	  (lambda args
	    (apply + base (map * args coefficients))))
	 (new-domain->old-domain-coefficients
	  (map (lambda (x)
		 (map (lambda (x) (* (random-sign) (random 10)))
		      (iota 0 new-domain-dimension)))
	       (iota 0 old-domain-dimension)))
	 (new-domain->old-domain
	  (lambda args
	    (apply values (map (lambda (row)
				 (apply + (map * row args)))
			       new-domain->old-domain-coefficients)))))
    (if (not (and (myindexer= (lambda args
				(call-with-values
				    (lambda () (apply new-domain->old-domain args))
				  old-indexer))
			      (##compose-indexers old-indexer new-domain  new-domain->old-domain)
			      new-domain)))
	(pp (list new-domain
		  old-domain-dimension
		  base
		  coefficients
		  new-domain->old-domain-coefficients)))))

(define (myarray= array1 array2)
  (and (interval= (array-domain array1)
		  (array-domain array2))
       (let ((getter1 (array-getter array1))
	     (getter2 (array-getter array2)))
	 (interval-reduce
	  (lambda indices
	    (equal? (apply getter1 indices)
		    (apply getter2 indices)))
	  (lambda (x y)
	    (and x y))
	  #t
	  (array-domain array1)))))

(pp "array body, indexer, storage-class, and safe? error tests")

(let ((a (make-array (make-interval '#(0 0) '#(1 1)) ;; not valid
		     values
		     values)))
  (test (array-body a)
	"array-body: argument is not a specialized array: ")
  (test (array-indexer a)
	"array-indexer: argument is not a specialized array: ")
  (test (array-storage-class a)
	"array-storage-class: argument is not a specialized array: ")
  (test (array-safe? a)
	"array-safe?: argument is not a specialized array: "))

(pp "specialized-array error tests")

(test (specialized-array  'a)
      "specialized-array: The first argument is not an interval: ")

(test (specialized-array (make-interval '#(0) '#(10)) 'a)
      "specialized-array: The second argument is not a storage-class: ")

(test (specialized-array (make-interval '#(0) '#(10)) generic-storage-class 'a)
      "specialized-array: The third argument is not a boolean: ")






(pp "array->specialized-array error tests")

(test (array->specialized-array #f generic-storage-class)
      "array->specialized-array: Argument is not an array: ")

(test (array->specialized-array (make-array (make-interval '#(1) '#(2))
					    list)
				#f)
      "array->specialized-array: result-storage-class is not a storage-class: ")

(test (array->specialized-array (make-array (make-interval '#(1) '#(2))
					    list)
				generic-storage-class
				'a)
      "array->specialized-array: safe? is not a boolean: ")

;; We gotta make sure than the error checks work in all dimensions ...

(test (array->specialized-array (make-array (make-interval '#(1) '#(2))
					    list)
				u16-storage-class)
      "array->specialized-array: not all elements of the array can be manipulated by the storage class: ")

(test (array->specialized-array (make-array (make-interval '#(1 1) '#(2 2))
					    list)
				u16-storage-class)
      "array->specialized-array: not all elements of the array can be manipulated by the storage class: ")

(test (array->specialized-array (make-array (make-interval '#(1 1 1) '#(2 2 2))
					    list)
				u16-storage-class)
      "array->specialized-array: not all elements of the array can be manipulated by the storage class: ")

(test (array->specialized-array (make-array (make-interval '#(1 1 1 1) '#(2 2 2 2))
					    list)
				u16-storage-class)
      "array->specialized-array: not all elements of the array can be manipulated by the storage class: ")

(test (array->specialized-array (make-array (make-interval '#(1 1 1 1 1) '#(2 2 2 2 2))
					    list)
				u16-storage-class)
      "array->specialized-array: not all elements of the array can be manipulated by the storage class: ")

(test (specialized-array-default-safe? 'a)
      "specialized-array-default-safe?: The argument is not a boolean: ")


(pp "array->specialized-array result tests")

(specialized-array-default-safe? #t)

(pp "Safe tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower-bounds
	  (map (lambda (x) (random 4))
	       (vector->list (make-vector (random 1 7)))))
	 (upper-bounds
	  (map (lambda (x) (+ x (random 1 5)))
	       lower-bounds))
	 (domain
	  (make-interval (list->vector lower-bounds)
			 (list->vector upper-bounds)))
	 (array1
	  (let ((alist '()))
	    (make-array
	     domain
	     (lambda indices
	       (cond ((assoc indices alist)
		      => cdr)
		     (else
		      indices)))
	     (lambda (value . indices)
	       (cond ((assoc indices alist)
		      =>(lambda (entry)
			  (set-cdr! entry value)))
		     (else
		      (set! alist (cons (cons indices value)
					alist))))))))
	 (array2
	  (array->specialized-array array1 generic-storage-class))
	 (setter1
	  (array-setter array1))
	 (setter2
	  (array-setter array2)))
    (do ((j 0 (+ j 1)))
	((= j 25))
      (let ((v (random 1000))
	    (indices (map random lower-bounds upper-bounds)))
	(apply setter1 v indices)
	(apply setter2 v indices)))
    (or (myarray= array1 array2) (pp "test1"))
    (or (myarray= (array->specialized-array        array1 generic-storage-class ) array2) (pp "test3"))
    ))

(specialized-array-default-safe? #f)

(pp "Unsafe tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower-bounds
	  (map (lambda (x) (random 4))
	       (vector->list (make-vector (random 1 7)))))
	 (upper-bounds
	  (map (lambda (x) (+ x (random 1 5)))
	       lower-bounds))
	 (domain
	  (make-interval (list->vector lower-bounds)
			 (list->vector upper-bounds)))
	 (array1
	  (let ((alist '()))
	    (make-array
	     domain
	     (lambda indices
	       (cond ((assoc indices alist)
		      => cdr)
		     (else
		      indices)))
	     (lambda (value . indices)
	       (cond ((assoc indices alist)
		      =>(lambda (entry)
			  (set-cdr! entry value)))
		     (else
		      (set! alist (cons (cons indices value)
					alist))))))))
	 (array2
	  (array->specialized-array array1 generic-storage-class ))
	 (setter1
	  (array-setter array1))
	 (setter2
	  (array-setter array2)))
    (do ((j 0 (+ j 1)))
	((= j 25))
      (let ((v (random 1000))
	    (indices (map random lower-bounds upper-bounds)))
	(apply setter1 v indices)
	(apply setter2 v indices)))
    (or (myarray= array1 array2) (pp "test1"))
    (or (myarray= (array->specialized-array        array1 generic-storage-class ) array2) (pp "test3"))
    ))

(pp "array-map error tests")

(test (array-map 1 #f)
      "array-map: Argument is not a procedure: ")

(test (array-map list 1 (make-array (make-interval '#(3) '#(4))
				    list))
      "array-map: Not all arguments are arrays: ")

(test (array-map list (make-array (make-interval '#(3) '#(4))
				  list) 1)
      "array-map: Not all arguments are arrays: ")

(test (array-map list
		 (make-array (make-interval '#(3) '#(4))
			     list)
		 (make-array (make-interval '#(3 4) '#(4 5))
			     list))
      "array-map: Not all arrays have the same domain: ")


(pp "array-reduce error tests")

(test (array-reduce 1 1 1)
      "array-reduce: operator is not a procedure: ")

(test (array-reduce list 1 1)
      "array-reduce: argument is not an array: ")


(pp "array-for-each error tests")

(test (array-for-each 1 #f)
      "array-for-each: Argument is not a procedure: ")

(test (array-for-each list 1 (make-array (make-interval '#(3) '#(4))
					 list))
      "array-for-each: Not all arguments are arrays: ")

(test (array-for-each list (make-array (make-interval '#(3) '#(4))
				       list) 1)
      "array-for-each: Not all arguments are arrays: ")

(test (array-for-each list
		      (make-array (make-interval '#(3) '#(4))
				  list)
		      (make-array (make-interval '#(3 4) '#(4 5))
				  list))
      "array-for-each: Not all arrays have the same domain: ")


(pp "array-map, array-reduce, and array-for-each result tests")

(specialized-array-default-safe? #t)

(let ((array-builders (vector (list u1-storage-class      (lambda indices (random 0 (expt 2 1))))
			      (list u8-storage-class      (lambda indices (random 0 (expt 2 8))))
			      (list u16-storage-class     (lambda indices (random 0 (expt 2 16))))
			      (list u32-storage-class     (lambda indices (random 0 (expt 2 32))))
			      (list u64-storage-class     (lambda indices (random 0 (expt 2 64))))
			      (list s8-storage-class      (lambda indices (random (- (expt 2 7))  (expt 2 7))))
			      (list s16-storage-class     (lambda indices (random (- (expt 2 15)) (expt 2 15))))
			      (list s32-storage-class     (lambda indices (random (- (expt 2 31)) (expt 2 31))))
			      (list s64-storage-class     (lambda indices (random (- (expt 2 63)) (expt 2 63))))
			      (list f32-storage-class     (lambda indices (random-real)))
			      (list f64-storage-class     (lambda indices (random-real)))
			      (list c64-storage-class     (lambda indices (make-rectangular (random-real) (random-real))))
			      (list c128-storage-class    (lambda indices (make-rectangular (random-real) (random-real))))
			      (list generic-storage-class (lambda indices indices)))))
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((lower-bounds
	    (map (lambda (x) (random 4))
		 (vector->list (make-vector (random 1 10)))))
	   (upper-bounds
	    (map (lambda (x) (+ x (random 1 4)))
		 lower-bounds))
	   (array-length
	    (lambda (a)
	      (let ((upper-bounds (interval-upper-bounds->list (array-domain a)))
		    (lower-bounds (interval-lower-bounds->list (array-domain a))))
		(apply * (map - upper-bounds lower-bounds)))))
	   (domain
	    (make-interval (list->vector lower-bounds)
			   (list->vector upper-bounds)))
	   (arrays
	    (map (lambda (ignore)
		   (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
		     (array->specialized-array (make-array domain
							   (cadr array-builder))
					       (car array-builder)
					       )))
		 (iota 0 (random 1 7))))
	   (result-array-1
	    (apply array-map
		   list
		   arrays))
	   (result-array-2
	    (array->specialized-array
	     (apply array-map
		    list
		    arrays)))
	   (getters
	    (map array-getter arrays))
	   (result-array-3
	    (make-array domain
			(lambda indices
			  (map (lambda (g) (apply g indices)) getters)))))
      (if (not (and (myarray= result-array-1 result-array-2)
		    (myarray= result-array-2 result-array-3)
		    (equal? (vector->list (array-body result-array-2))
			    (reverse (array-reduce (lambda (x y) (cons y x))
						   '()
						   result-array-2)))
		    (equal? (vector->list (array-body result-array-2))
			    (reverse (let ((result '()))
				       (array-for-each (lambda (f)
							 (set! result (cons f result)))
						       result-array-2)
				       result)))
		    (equal?  (map array-length arrays)
			     (map (lambda (array)
				    ((storage-class-length (array-storage-class array)) (array-body array)))
				  arrays))))
	  (pp "Arghh"))
      )))

(specialized-array-default-safe? #f)

(let ((array-builders (vector (list u1-storage-class      (lambda indices (random (expt 2 1))))
			      (list u8-storage-class      (lambda indices (random (expt 2 8))))
			      (list u16-storage-class     (lambda indices (random (expt 2 16))))
			      (list u32-storage-class     (lambda indices (random (expt 2 32))))
			      (list u64-storage-class     (lambda indices (random (expt 2 64))))
			      (list s8-storage-class      (lambda indices (random (- (expt 2 7))  (expt 2 7))))
			      (list s16-storage-class     (lambda indices (random (- (expt 2 15)) (expt 2 15))))
			      (list s32-storage-class     (lambda indices (random (- (expt 2 31)) (expt 2 31))))
			      (list s64-storage-class     (lambda indices (random (- (expt 2 63)) (expt 2 63))))
			      (list f32-storage-class     (lambda indices (random-real)))
			      (list f64-storage-class     (lambda indices (random-real)))
                              (list c64-storage-class     (lambda indices (make-rectangular (random-real) (random-real))))
                              (list c128-storage-class    (lambda indices (make-rectangular (random-real) (random-real))))
			      (list generic-storage-class (lambda indices indices)))))
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((lower-bounds
	    (map (lambda (x) (random 4))
		 (vector->list (make-vector (random 1 10)))))
	   (upper-bounds
	    (map (lambda (x) (+ x (random 1 4)))
		 lower-bounds))
	   (domain
	    (make-interval (list->vector lower-bounds)
			   (list->vector upper-bounds)))
	   (arrays
	    (map (lambda (ignore)
		   (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
		     (array->specialized-array (make-array domain
							   (cadr array-builder))
					       (car array-builder)
					       )))
		 (iota 0 (random 1 7))))
	   (result-array-1
	    (apply array-map
		   list
		   arrays))
	   (result-array-2
	    (array->specialized-array
	     (apply array-map
		    list
		    arrays)))
	   (getters
	    (map array-getter arrays))
	   (result-array-3
	    (make-array domain
			(lambda indices
			  (map (lambda (g) (apply g indices)) getters)))))
      (if (not (and (myarray= result-array-1 result-array-2)
		    (myarray= result-array-2 result-array-3)
		    (equal? (vector->list (array-body result-array-2))
			    (reverse (array-reduce (lambda (x y) (cons y x))
						   '()
						   result-array-2)))
		    (equal? (vector->list (array-body result-array-2))
			    (reverse (let ((result '()))
				       (array-for-each (lambda (f)
							 (set! result (cons f result)))
						       result-array-2)
				       result)))))
	  (pp "Arghh")))))

(pp "Some array-curry tests.")

(test (array-curry 'a 1)
      "array-curry: The first argument is not an array: ")

(test (array-curry (make-array (make-interval '#(0) '#(1)) list)  'a)
      "array-curry: The second argument is not an exact integer: ")

(test (array-curry (make-array (make-interval '#(0 0) '#(1 1)) list)  0)
      "array-curry: The second argument is not between 0 and (interval-dimension (array-domain array)) (exclusive): ")

(test (array-curry (make-array (make-interval '#(0 0) '#(1 1)) list)  2)
      "array-curry: The second argument is not between 0 and (interval-dimension (array-domain array)) (exclusive): ")


(let ((array-builders (vector (list u1-storage-class      (lambda indices (random (expt 2 1))))
			      (list u8-storage-class      (lambda indices (random (expt 2 8))))
			      (list u16-storage-class     (lambda indices (random (expt 2 16))))
			      (list u32-storage-class     (lambda indices (random (expt 2 32))))
			      (list u64-storage-class     (lambda indices (random (expt 2 64))))
			      (list s8-storage-class      (lambda indices (random (- (expt 2 7))  (expt 2 7))))
			      (list s16-storage-class     (lambda indices (random (- (expt 2 15)) (expt 2 15))))
			      (list s32-storage-class     (lambda indices (random (- (expt 2 31)) (expt 2 31))))
			      (list s64-storage-class     (lambda indices (random (- (expt 2 63)) (expt 2 63))))
			      (list f32-storage-class     (lambda indices (random-real)))
			      (list f64-storage-class     (lambda indices (random-real)))
                              (list c64-storage-class     (lambda indices (make-rectangular (random-real) (random-real))))
                              (list c128-storage-class    (lambda indices (make-rectangular (random-real) (random-real))))
			      (list generic-storage-class (lambda indices indices)))))
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((lower-bounds
	    (map (lambda (x) (random 4))
		 (vector->list (make-vector (random 2 7)))))
	   (upper-bounds
	    (map (lambda (x) (+ x (random 1 5)))
		 lower-bounds))
	   (domain
	    (make-interval (list->vector lower-bounds)
			   (list->vector upper-bounds)))
	   (array-builder
	    (vector-ref array-builders (random (vector-length array-builders))))
	   (random-array-element
	    (cadr array-builder))
	   (storage-class
	    (car array-builder))
	   (Array
	    (array->specialized-array (make-array domain
						  random-array-element)
				      storage-class))
	   (copied-array
	    (array->specialized-array Array
				      storage-class))
	   (outer-dimension
	    (random 1 (interval-dimension domain)))
	   (domains
	    (call-with-values (lambda ()(interval-curry domain outer-dimension)) list))
	   (outer-domain
	    (car domains))
	   (inner-domain
	    (cadr domains))
	   (immutable-curry
	    (array-curry (make-array (array-domain Array)
				     (array-getter Array))
			 outer-dimension))
	   (mutable-curry
	    (array-curry (make-array (array-domain Array)
				     (array-getter Array)
				     (array-setter Array))
			 outer-dimension))
	   (specialized-curry
	    (array-curry Array outer-dimension))
	   (immutable-curry-from-definition
	    (call-with-values
		(lambda () (interval-curry (array-domain Array) outer-dimension))
	      (lambda (outer-interval inner-interval)
		(make-array outer-interval
			    (lambda outer-multi-index
			      (make-array inner-interval
					  (lambda inner-multi-index
					    (apply (array-getter Array) (append outer-multi-index inner-multi-index)))))))))
	   (mutable-curry-from-definition
	    (call-with-values
		(lambda () (interval-curry (array-domain Array) outer-dimension))
	      (lambda (outer-interval inner-interval)
		(make-array outer-interval
			    (lambda outer-multi-index
			      (make-array inner-interval
					  (lambda inner-multi-index
					    (apply (array-getter Array) (append outer-multi-index inner-multi-index)))
					  (lambda (v . inner-multi-index)
					    (apply (array-setter Array) v (append outer-multi-index inner-multi-index)))))))))
	   (specialized-curry-from-definition
	    (call-with-values
		(lambda () (interval-curry (array-domain Array) outer-dimension))
	      (lambda (outer-interval inner-interval)
		(make-array outer-interval
			    (lambda outer-multi-index
			      (specialized-array-share Array
						       inner-interval
						       (lambda inner-multi-index
							 (apply values (append outer-multi-index inner-multi-index))))))))))
      ;; mutate the curried array
      (for-each (lambda (curried-array)
		  (let ((outer-getter
			 (array-getter curried-array)))
		    (do ((i 0 (+ i 1)))
			((= i 50))  ;; used to be tests, not 50, but 50 will do fine
		      (call-with-values
			  (lambda ()
			    (random-multi-index outer-domain))
			(lambda outer-multi-index
			  (let ((inner-setter
				 (array-setter (apply outer-getter outer-multi-index))))
			    (call-with-values
				(lambda ()
				  (random-multi-index inner-domain))
			      (lambda inner-multi-index
				(let ((new-element
				       (random-array-element)))
				  (apply inner-setter new-element inner-multi-index)
				  ;; mutate the copied array without currying
				  (apply (array-setter copied-array) new-element (append outer-multi-index inner-multi-index)))))))))))
		(list mutable-curry
		      specialized-curry
		      mutable-curry-from-definition
		      specialized-curry-from-definition
		      ))
      
      (and (or (myarray= Array copied-array) (error "Arggh"))
	   (or (array-every? array? immutable-curry) (error "Arggh"))
	   (or (array-every? (lambda (a) (not (mutable-array? a))) immutable-curry) (error "Arggh"))
	   (or (array-every? mutable-array? mutable-curry) (error "Arggh"))
	   (or (array-every? (lambda (a) (not (specialized-array? a))) mutable-curry) (error "Arggh"))
	   (or (array-every? specialized-array? specialized-curry) (error "Arggh"))
	   (or (array-every? (lambda (xy) (apply myarray= xy))
			     (array-map list immutable-curry immutable-curry-from-definition))
	       (error "Arggh"))
	   (or (array-every? (lambda (xy) (apply myarray= xy))
			     (array-map list mutable-curry mutable-curry-from-definition))
	       (error "Arggh"))
	   (or (array-every? (lambda (xy) (apply myarray= xy))
			     (array-map list specialized-curry specialized-curry-from-definition))
	       (error "Arggh"))))))

(pp "array-every? error tests")

(test (array-every? 'a 'a)
      "array-every?: The first argument is not a procedure: ")

(test (array-every? (lambda args #t) 'a)
      "array-every?: The second argument is not an array: ")



(pp "specialized-array-share error tests")

(test (specialized-array-share 1 1 1)
      "specialized-array-share: array is not a specialized-array: ")

(test (specialized-array-share (specialized-array (make-interval '#(1) '#(2)))
			       1 1)
      "specialized-array-share: new-domain is not an interval: ")

(test (specialized-array-share (specialized-array (make-interval '#(1) '#(2)))
			       (make-interval '#(0) '#(1))
			       1)
      "specialized-array-share: new-domain->old-domain is not a procedure: ")

(test (specialized-array-share (specialized-array (make-interval '#(1) '#(2)))
			       (make-interval '#(0) '#(1))
			       (lambda args #t)
			       'a)
      "specialized-array-share: safe? is not a boolean: ")

(test (myarray= (list->specialized-array (reverse (iota 0 10))
					 (make-interval '#(0) '#(10)))
		(specialized-array-share (list->specialized-array (iota 0 10)
								  (make-interval '#(0) '#(10)))
					 (make-interval '#(0) '#(10))
					 (lambda (i)
					   (- 9 i))))
      #t)


(pp "specialized-array-share result tests")

(define (random-permutation n)
  (let ((result (make-vector n)))
    ;; fill it
    (do ((i 0 (fx+ i 1)))
	((fx= i n))
      (vector-set! result i i))
    ;; permute it
    (do ((i 0 (fx+ i 1)))
	((fx= i n) result)
      (let* ((index (random i n))
	     (temp (vector-ref result index)))
	(vector-set! result index (vector-ref result i))
	(vector-set! result i temp)))))

(define (vector-permute v permutation)
  (let* ((n (vector-length v))
	 (result (make-vector n)))
    (do ((i 0 (+ i 1)))
	((= i n) result)
      (vector-set! result i (vector-ref v (vector-ref permutation i))))))

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((n (random 1 11))
	 (permutation (random-permutation n))
	 (input-vec (list->vector (f64vector->list (random-f64vector n)))))
    (test (vector-permute input-vec permutation)
	  (##vector-permute input-vec permutation))
    (test (list->vector (##vector-permute->list input-vec permutation))
	  (vector-permute input-vec permutation))))



(specialized-array-default-safe? #t)

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((axes (iota 0 (random 1 5)))
	 (lower-bounds (list->vector (map (lambda (x) (random -10 10)) axes)))
	 (upper-bounds (list->vector (map (lambda (l) (+ l (random 1 4))) (vector->list lower-bounds))))
	 (a (array->specialized-array (make-array (make-interval lower-bounds
								 upper-bounds)
						  list)
				      generic-storage-class
				      ))
	 (new-axis-order (vector-permute (list->vector axes) (random-permutation (length axes))))
	 (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
    (let ((b (make-array (make-interval (vector-permute lower-bounds new-axis-order)
					(vector-permute upper-bounds new-axis-order))
			 (lambda multi-index
			   (apply (array-getter a)
				  (let* ((n (vector-length new-axis-order))
					 (multi-index-vector (list->vector multi-index))
					 (result (make-vector n)))
				    (do ((i 0 (+ i 1)))
					((= i n) (vector->list result))
				      (vector-set! result (vector-ref new-axis-order i)
						   (if (vector-ref reverse-order? (vector-ref new-axis-order i))
						       (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
							  (- (vector-ref upper-bounds (vector-ref new-axis-order i))
							     (vector-ref multi-index-vector i)
							     1))
						       (vector-ref multi-index-vector i)))))))))
	  (c (specialized-array-share a
				      (make-interval (vector-permute lower-bounds new-axis-order)
						     (vector-permute upper-bounds new-axis-order))
				      (lambda multi-index
					(apply values
					       (let* ((n (vector-length new-axis-order))
						      (multi-index-vector (list->vector multi-index))
						      (result (make-vector n)))
						 (do ((i 0 (+ i 1)))
						     ((= i n) (vector->list result))
						   (vector-set! result (vector-ref new-axis-order i)
								(if (vector-ref reverse-order? (vector-ref new-axis-order i))
								    (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
								       (- (vector-ref upper-bounds (vector-ref new-axis-order i))
									  (vector-ref multi-index-vector i)
									  1))
								    (vector-ref multi-index-vector i))))))))))
      (if (not (myarray= b c))
	  (pp (list "piffle"
		    a b c))))))

(specialized-array-default-safe? #f)

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((axes (iota 0 (random 1 5)))
	 (lower-bounds (list->vector (map (lambda (x) (random -10 10)) axes)))
	 (upper-bounds (list->vector (map (lambda (l) (+ l (random 1 4))) (vector->list lower-bounds))))
	 (a (array->specialized-array (make-array (make-interval lower-bounds
								 upper-bounds)
						  list)
				      generic-storage-class
				      ))
	 (new-axis-order (vector-permute (list->vector axes) (random-permutation (length axes))))
	 (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
    (let ((b (make-array (make-interval (vector-permute lower-bounds new-axis-order)
					(vector-permute upper-bounds new-axis-order))
			 (lambda multi-index
			   (apply (array-getter a)
				  (let* ((n (vector-length new-axis-order))
					 (multi-index-vector (list->vector multi-index))
					 (result (make-vector n)))
				    (do ((i 0 (+ i 1)))
					((= i n) (vector->list result))
				      (vector-set! result (vector-ref new-axis-order i)
						   (if (vector-ref reverse-order? (vector-ref new-axis-order i))
						       (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
							  (- (vector-ref upper-bounds (vector-ref new-axis-order i))
							     (vector-ref multi-index-vector i)
							     1))
						       (vector-ref multi-index-vector i)))))))))
	  (c (specialized-array-share a
				      (make-interval (vector-permute lower-bounds new-axis-order)
						     (vector-permute upper-bounds new-axis-order))
				      (lambda multi-index
					(apply values
					       (let* ((n (vector-length new-axis-order))
						      (multi-index-vector (list->vector multi-index))
						      (result (make-vector n)))
						 (do ((i 0 (+ i 1)))
						     ((= i n) (vector->list result))
						   (vector-set! result (vector-ref new-axis-order i)
								(if (vector-ref reverse-order? (vector-ref new-axis-order i))
								    (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
								       (- (vector-ref upper-bounds (vector-ref new-axis-order i))
									  (vector-ref multi-index-vector i)
									  1))
								    (vector-ref multi-index-vector i))))))))))
      (if (not (myarray= b c))
	  (pp (list "piffle"
		    a b c))))))


(pp "interval and array translation tests")

(let ((int (make-interval '#(0 0) '#(10 10)))
      (translation '#(10 -2)))
  (test (interval-translate 'a 10)
	"interval-translate: The first argument is not an interval: ")
  (test (interval-translate int 10)
	"interval-translate: The second argument is not a vector of exact integers: ")
  (test (interval-translate int '#(a b))
	"interval-translate: The second argument is not a vector of exact integers: ")
  (test (interval-translate int '#(1. 2.))
	"interval-translate: The second argument is not a vector of exact integers: ")
  (test (interval-translate int '#(1))
	"interval-translate: The dimension of the first argument (an interval) does not equal the length of the second (a vector): ")
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((int (random-interval))
	   (lower-bounds (interval-lower-bounds->vector int))
	   (upper-bounds (interval-upper-bounds->vector int))
	   (translation (list->vector (map (lambda (x)
					     (random -10 10))
					   (iota 0 (vector-length lower-bounds))))))
      (interval= (interval-translate int translation)
		 (make-interval (##vector-map + lower-bounds translation)
				(##vector-map + upper-bounds translation))))))

(let* ((specialized-array (array->specialized-array (make-array (make-interval '#(0 0) '#(10 12))
								list)))
       (mutable-array (let ((temp (array->specialized-array specialized-array)))
			(make-array (array-domain temp)
				    (array-getter temp)
				    (array-setter temp))))
       (immutable-array (make-array (array-domain mutable-array)
				    (array-getter mutable-array)))
       (translation '#(10 -2)))
  
  (define (my-array-translate Array translation)
    (let* ((array-copy (array->specialized-array Array))
	   (getter (array-getter array-copy))
	   (setter (array-setter array-copy)))
      (make-array (interval-translate (array-domain Array)
				      translation)
		  (lambda args
		    (apply getter
			   (map - args (vector->list translation))))
		  (lambda (v . args)
		    (apply setter
			   v
			   (map - args (vector->list translation)))))))
  
  (test (array-translate 'a 1)
	"array-translate: the first argument is not an array: ")
  (test (array-translate immutable-array '#(1.))
	"array-translate: the second argument is not a vector of exact integers: ")
  (test (array-translate immutable-array '#(0 2 3))
	"array-translate: the dimension of the first argument (an array) does not equal the dimension of the second argument (a vector): ")
  (let ((specialized-result (array-translate specialized-array translation)))
    (test (specialized-array? specialized-result)
	  #t))
  (let ((mutable-result (array-translate mutable-array translation)))
    (test (and (mutable-array? mutable-array)
	       (not (specialized-array? mutable-array))
	       (mutable-array? mutable-result)
	       (not (specialized-array? mutable-result)))
	  #t))
  (let ((immutable-result (array-translate immutable-array translation)))
    (test (and (array? immutable-array)
	       (not (mutable-array? immutable-array))
	       (array? immutable-result)
	       (not (mutable-array? immutable-result)))
	  #t))

  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((domain (random-interval 1 6))
	   (Array (let ((temp (make-array domain list)))
		    (case (random-integer 3)
		      ((0) temp)
		      ((1) (array->specialized-array temp))
		      ((2) (let ((temp (array->specialized-array temp)))
			     (make-array (array-domain temp)
					 (array-getter temp)
					 (array-setter temp)))))))
	   (translation (list->vector (map (lambda (x) (random -10 10)) (vector->list (##interval-lower-bounds domain))))))
      ;;(pp (list domain translation (interval-volume domain)))
      (let ((translated-array       (array-translate Array translation))
	    (my-translated-array (my-array-translate Array translation)))
	(if (mutable-array? Array)
	    (let ((translated-domain (interval-translate domain translation)))
	      (do ((j 0 (+ j 1)))
		  ((= j 50))
		(call-with-values
		    (lambda ()
		      (random-multi-index translated-domain))
		  (lambda multi-index
		    (let ((value (random-integer 10000)))
		      (apply (array-setter translated-array) value multi-index)
		      (apply (array-setter my-translated-array) value multi-index)))))))
	(test (myarray= (array-translate Array translation)
			(my-array-translate Array translation))
	      #t)))))

(let* ((specialized (specialized-array (make-interval '#(0 0 0 0 0) '#(1 1 1 1 1))))
       (mutable (make-array (array-domain specialized)
			    (array-getter specialized)
			    (array-setter specialized)))
       (A (array-translate  mutable '#(0 0 0 0 0))))
  
  (test ((array-getter A) 0 0)
	"The number of indices does not equal the array dimension: ")
  
  (test ((array-setter A) 'a 0 0)
	"The number of indices does not equal the array dimension: "))


(pp "interval and array permutation tests")

(let ((int (make-interval '#(0 0) '#(10 10)))
      (permutation '#(1 0)))
  (test (interval-permute 'a 10)
	"interval-permute: The first argument is not an interval: ")
  (test (interval-permute int 10)
	"interval-permute: The second argument is not a permutation: ")
  (test (interval-permute int '#(a b))
	"interval-permute: The second argument is not a permutation: ")
  (test (interval-permute int '#(1. 2.))
	"interval-permute: The second argument is not a permutation: ")
  (test (interval-permute int '#(10 -2))
	"interval-permute: The second argument is not a permutation: ")
  (test (interval-permute int '#(0))
	"interval-permute: The dimension of the first argument (an interval) does not equal the length of the second (a permutation): ")
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((int (random-interval))
	   (lower-bounds (interval-lower-bounds->vector int))
	   (upper-bounds (interval-upper-bounds->vector int))
	   (permutation (random-permutation (vector-length lower-bounds))))
      (interval= (interval-permute int permutation)
		 (make-interval (vector-permute lower-bounds permutation)
				(vector-permute upper-bounds permutation))))))

(let* ((specialized-array (array->specialized-array (make-array (make-interval '#(0 0) '#(10 12))
								list)))
       (mutable-array (let ((temp (array->specialized-array specialized-array)))
			(make-array (array-domain temp)
				    (array-getter temp)
				    (array-setter temp))))
       (immutable-array (make-array (array-domain mutable-array)
				    (array-getter mutable-array)))
       (permutation '#(1 0)))
  
  (test (array-permute 'a 1)
	"array-permute: the first argument is not an array: ")
  (test (array-permute immutable-array '#(1.))
	"array-permute: the second argument is not a permutation: ")
  (test (array-permute immutable-array '#(2))
	"array-permute: the second argument is not a permutation: ")
  (test (array-permute immutable-array '#(0 1 2))
	"array-permute: the dimension of the first argument (an array) does not equal the dimension of the second argument (a permutation): ")
  (let ((specialized-result (array-permute specialized-array permutation)))
    (test (specialized-array? specialized-result)
	  #t))
  (let ((mutable-result (array-permute mutable-array permutation)))
    (test (and (mutable-array? mutable-array)
	       (not (specialized-array? mutable-array))
	       (mutable-array? mutable-result)
	       (not (specialized-array? mutable-result)))
	  #t))
  (let ((immutable-result (array-permute immutable-array permutation)))
    (test (and (array? immutable-array)
	       (not (mutable-array? immutable-array))
	       (array? immutable-result)
	       (not (mutable-array? immutable-result)))
	  #t))
  
  (specialized-array-default-safe? #t)

  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((domain (random-interval 1 6))
	   (Array (let ((temp (make-array domain list)))
		    (case (random-integer 3)
		      ((0) temp)
		      ((1) (array->specialized-array temp))
		      ((2) (let ((temp (array->specialized-array temp)))
			     (make-array (array-domain temp)
					 (array-getter temp)
					 (array-setter temp)))))))
	   (permutation (random-permutation (interval-dimension domain))))
      
      (define (my-array-permute Array permutation)
	(let* ((array-copy (array->specialized-array Array))
	       (getter (array-getter array-copy))
	       (setter (array-setter array-copy))
	       (permutation-inverse (##permutation-invert permutation)))
	  (make-array (interval-permute (array-domain Array)
					permutation)
		      (lambda args
			(apply getter
			       (vector->list (vector-permute (list->vector args) permutation-inverse))))
		      (lambda (v . args)
			(apply setter
			       v
			       (vector->list (vector-permute (list->vector args) permutation-inverse)))))))
      
      ;; (pp (list domain permutation (interval-volume domain)))
      (let ((permuted-array       (array-permute Array permutation))
	    (my-permuted-array (my-array-permute Array permutation)))
	(let ((permuted-domain (interval-permute domain permutation)))
	  (do ((j 0 (+ j 1)))
	      ((= j 50))
	    (call-with-values
		(lambda ()
		  (random-multi-index permuted-domain))
	      (lambda multi-index
		(test (apply (array-getter permuted-array)    multi-index)
		      (apply (array-getter my-permuted-array) multi-index))))))
	(if (mutable-array? Array)
	    (let ((permuted-domain (interval-permute domain permutation)))
	      (do ((j 0 (+ j 1)))
		  ((= j 50))
		(call-with-values
		    (lambda ()
		      (random-multi-index permuted-domain))
		  (lambda multi-index
		    (let ((value (random-integer 10000)))
		      (apply (array-setter permuted-array) value multi-index)
		      (apply (array-setter my-permuted-array) value multi-index)))))))
	(test (myarray= permuted-array
			my-permuted-array)
	      #t))))

  (specialized-array-default-safe? #f)

  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((domain (random-interval 1 6))
	   (Array (let ((temp (make-array domain list)))
		    (case (random-integer 3)
		      ((0) temp)
		      ((1) (array->specialized-array temp))
		      ((2) (let ((temp (array->specialized-array temp)))
			     (make-array (array-domain temp)
					 (array-getter temp)
					 (array-setter temp)))))))
	   (permutation (random-permutation (interval-dimension domain))))
      
      (define (my-array-permute Array permutation)
	(let* ((array-copy (array->specialized-array Array))
	       (getter (array-getter array-copy))
	       (setter (array-setter array-copy))
	       (permutation-inverse (##permutation-invert permutation)))
	  (make-array (interval-permute (array-domain Array)
					permutation)
		      (lambda args
			(apply getter
			       (vector->list (vector-permute (list->vector args) permutation-inverse))))
		      (lambda (v . args)
			(apply setter
			       v
			       (vector->list (vector-permute (list->vector args) permutation-inverse)))))))
      
      ;; (pp (list domain permutation (interval-volume domain)))
      (let ((permuted-array       (array-permute Array permutation))
	    (my-permuted-array (my-array-permute Array permutation)))
	(let ((permuted-domain (interval-permute domain permutation)))
	  (do ((j 0 (+ j 1)))
	      ((= j 50))
	    (call-with-values
		(lambda ()
		  (random-multi-index permuted-domain))
	      (lambda multi-index
		(test (apply (array-getter permuted-array)    multi-index)
		      (apply (array-getter my-permuted-array) multi-index))))))
	(if (mutable-array? Array)
	    (let ((permuted-domain (interval-permute domain permutation)))
	      (do ((j 0 (+ j 1)))
		  ((= j 50))
		(call-with-values
		    (lambda ()
		      (random-multi-index permuted-domain))
		  (lambda multi-index
		    (let ((value (random-integer 10000)))
		      (apply (array-setter permuted-array) value multi-index)
		      (apply (array-setter my-permuted-array) value multi-index)))))))
	(test (myarray= permuted-array
			my-permuted-array)
	      #t))))
  )

(pp "interval-intersect? tests")

(let ((a (make-interval '#(0 0) '#(10 10)))
      (b (make-interval '#(0) '#(10)))
      (c (make-interval '#(10 10) '#(20 20))))
  (test (interval-intersect? 'a)
	"interval-intersect?: The argument is not an interval: ")
  (test (interval-intersect?  a 'a)
	"interval-intersect?: Not all arguments are intervals: ")
  (test (interval-intersect? a b)
	"interval-intersect?: Not all arguments have the same dimension: "))


(define (my-interval-intersect? . args)
  
  (define (fold-left operator           ;; called with (operator result-so-far (car list))
		     initial-value
		     list)
    (if (null? list)
	initial-value
	(fold-left operator
		   (operator initial-value (car list))
		   (cdr list))))
  
  
  (let ((new-uppers (let ((uppers (map interval-upper-bounds->vector args)))
		      (fold-left (lambda (arg result)
				   (##vector-map min arg result))
				 (car uppers)
				 uppers)))
	(new-lowers (let ((lowers (map interval-lower-bounds->vector args)))
		      (fold-left (lambda (arg result)
				   (##vector-map max arg result))
				 (car lowers)
				 lowers))))
    ;; (pp (list args new-lowers new-uppers (vector-every < new-lowers new-uppers)))
    (and (##vector-every < new-lowers new-uppers)
	 (make-interval new-lowers new-uppers))))


(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((dimension (random 1 6))
	 (number-of-intervals (random 1 4))
	 (intervals (map (lambda (x)
			   (random-interval dimension (+ dimension 1)))
			 (iota 0 number-of-intervals))))
    ;; (pp (list intervals (apply my-interval-intersect? intervals)))
    (test (apply my-interval-intersect? intervals)
	  (apply interval-intersect? intervals))))

(pp "test array-extract")

(let* ((domain (make-interval '#(0 0) '#(10 10)))
       (array (make-array domain
			  list))
       (specialized-array (array->specialized-array array))
       (mutable-array (let ((copy (array->specialized-array specialized-array)))
			(make-array domain
				    (array-getter copy)
				    (array-setter copy)))))
  (test (array-extract 'a 10)
	"array-extract: The first argument is not an array: ")
  (test (array-extract array 10)
	"array-extract: The second argument is not an interval: ")
  (test (array-extract array (make-interval '#(1 1) '#(11 11)))
	"array-extract: The second argument (an interval) is not a subset of the domain of the first argument (an array): ")
  (let* ((sub-interval (make-interval '#(0 0) '#(5 5)))
	 (specialized-subarray (array-extract specialized-array sub-interval))
	 (mutable-subarray     (array-extract mutable-array sub-interval)))
    (test (myarray= (array-extract array sub-interval)
		    (make-array sub-interval list))
	  #t)
    (do ((i 0 (fx+ i 1)))
	((fx= i 100))
      (call-with-values
	  (lambda ()
	    (random-multi-index domain))
	(lambda args
	  (let ((v (random-real)))
	    (apply (array-setter specialized-subarray)
		   v
		   args)
	    (apply (array-setter mutable-subarray)
		   v
		   args)))))
    (test (myarray= specialized-subarray
		    mutable-subarray)
	  #t)))

(pp "Miscellaneous error tests")

(test (make-array (make-interval '#(0 0) '#(10 10))
		  list
		  'a)
      "make-array: setter is not a procedure: ")

(test (array-dimension 'a)
      "array-dimension: argument is not an array: ")

(test (array-safe? (array->specialized-array (make-array (make-interval '#(0 0) '#(10 10)) list) generic-storage-class #t))
      #t)


(test (array-safe? (array->specialized-array (make-array (make-interval '#(0 0) '#(10 10)) list) generic-storage-class #f))
      #f)

(let ((array-builders (vector (list u1-storage-class      (lambda indices (random (expt 2 1))) '(a -1))
			      (list u8-storage-class      (lambda indices (random (expt 2 8))) '(a -1))
			      (list u16-storage-class     (lambda indices (random (expt 2 16))) '(a -1))
			      (list u32-storage-class     (lambda indices (random (expt 2 32))) '(a -1))
			      (list u64-storage-class     (lambda indices (random (expt 2 64))) '(a -1))
			      (list s8-storage-class      (lambda indices (random (- (expt 2 7))  (expt 2 7))) `(a ,(expt 2 8)))
			      (list s16-storage-class     (lambda indices (random (- (expt 2 15)) (expt 2 15))) `(a ,(expt 2 16)))
			      (list s32-storage-class     (lambda indices (random (- (expt 2 31)) (expt 2 31))) `(a ,(expt 2 32)))
			      (list s64-storage-class     (lambda indices (random (- (expt 2 63)) (expt 2 63))) `(a ,(expt 2 64)))
			      (list f32-storage-class     (lambda indices (random-real)) `(a 1))
			      (list f64-storage-class     (lambda indices (random-real)) `(a 1))
                              (list c64-storage-class     (lambda indices (make-rectangular (random-real) (random-real))) `(a 1))
                              (list c128-storage-class    (lambda indices (make-rectangular (random-real) (random-real))) `(a 1))
			      )))
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((domain (random-interval 1 6))
	   (builders (vector-ref array-builders (random-integer (vector-length array-builders))))
	   (storage-class (car builders))
	   (random-entry (cadr builders))
	   (invalid-entry (list-ref (caddr builders) (random 2)))
	   (Array (array->specialized-array (make-array domain random-entry)
					    storage-class
					    #t)) ; safe
	   (getter (array-getter Array))
	   (setter (array-setter Array))
	   (dimension (interval-dimension domain))
	   (valid-args (call-with-values
			   (lambda ()
			     (random-multi-index domain))
			 list)))
      (test (apply setter invalid-entry valid-args)
	    "array-setter: value cannot be stored in body: ")
      (set-car! valid-args 'a)
      (test (apply getter valid-args)
	    "array-getter: multi-index component is not an exact integer: ")
      (test (apply setter 10 valid-args)
	    "array-setter: multi-index component is not an exact integer: ")
      (set-car! valid-args 10000) ;; outside the range of any random-interval
      (test (apply getter valid-args)
	    "array-getter: domain does not contain multi-index: ")
      (test (apply setter 10 valid-args)
	    "array-setter: domain does not contain multi-index: " )
      (if (< 4 dimension)
	  (begin
	    (set! valid-args (cons 1 valid-args))
	    (test (apply getter valid-args)
		  "array-getter: multi-index is not the correct dimension: ")
	    (test (apply setter 10 valid-args)
		  "array-setter: multi-index is not the correct dimension: "))))))

(pp "array->list and list->specialized-array")

(test (array->list 'a)
      "array->list: object is not an array: ")

(test (list->specialized-array 'a 'b)
      "list->specialized-array: First argument is not a list: ")

(test (list->specialized-array '(0) 'b)
      "list->specialized-array: Second argument is not an interval: ")

(test (list->specialized-array '(0) (make-interval '#(0) '#(1)) 'a)
      "list->specialized-array: Third argument is not a storage-class: ")

(test (list->specialized-array '(0) (make-interval '#(0) '#(1)) generic-storage-class 'a)
      "list->specialized-array: Fourth argument is not a boolean: ")

;; (list->specialized-array '(0) (make-interval '#(0) '#(10)))

(test (list->specialized-array '(0) (make-interval '#(0) '#(10)))
      "list->specialized-array: The length of the first argument does not equal the volume of the second: ")

(test (list->specialized-array '(a) (make-interval '#(0) '#(1)) u1-storage-class)
      "list->specialized-array: Not every element of the list can be stored in the body of the array: " )


(let ((array-builders (vector (list u1-storage-class      (lambda indices (random 0 (expt 2 1))))
			      (list u8-storage-class      (lambda indices (random 0 (expt 2 8))))
			      (list u16-storage-class     (lambda indices (random 0 (expt 2 16))))
			      (list u32-storage-class     (lambda indices (random 0 (expt 2 32))))
			      (list u64-storage-class     (lambda indices (random 0 (expt 2 64))))
			      (list s8-storage-class      (lambda indices (random (- (expt 2 7))  (expt 2 7))))
			      (list s16-storage-class     (lambda indices (random (- (expt 2 15)) (expt 2 15))))
			      (list s32-storage-class     (lambda indices (random (- (expt 2 31)) (expt 2 31))))
			      (list s64-storage-class     (lambda indices (random (- (expt 2 63)) (expt 2 63))))
			      (list f32-storage-class     (lambda indices (random-real)))
			      (list f64-storage-class     (lambda indices (random-real)))
			      (list c64-storage-class     (lambda indices (make-rectangular (random-real) (random-real))))
			      (list c128-storage-class    (lambda indices (make-rectangular (random-real) (random-real))))
			      (list generic-storage-class (lambda indices indices)))))
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((domain (random-interval 1 6))
	   (builders (vector-ref array-builders (random-integer (vector-length array-builders))))
	   (storage-class (car builders))
	   (random-entry (cadr builders))
	   (Array (array->specialized-array (make-array domain random-entry)
					    storage-class
					    #t)) ; safe
	   (l (array->list Array))
	   (new-array (list->specialized-array l domain storage-class (zero? (random-integer 2)))))
      (test (myarray= Array new-array)
	    #t))))


(pp "Test code from the SRFI document")

(test (interval= (interval-dilate (make-interval '#(0 0) '#(100 100)) '#(1 1) '#(1 1))
		 (make-interval '#(1 1) '#(101 101)))
      #t)

(test (interval= (interval-dilate (make-interval '#(0 0) '#(100 100)) '#(-1 -1) '#(1 1))
		 (make-interval '#(-1 -1) '#(101 101)))
      #t)

(test (interval= (interval-dilate (make-interval '#(0 0) '#(100 100))  '#(0 0) '#(-50 -50))
		 (make-interval '#(0 0) '#(50 50)))
      #t)

(test (interval-dilate (make-interval '#(0 0) '#(100 100)) '#(0 0) '#(-500 -50))
      "interval-dilate: the resulting interval is empty: ")

(define a (make-array (make-interval '#(1 1) '#(11 11))
		      (lambda (i j)
			(if (= i j)
			    1
			    0))))

(test ((array-getter a) 3 3)
      1)

(test ((array-getter a) 2 3)
      0)

;; ((array-getter a) 11 0) is an error, but it isn't signalled

(define a (make-array (make-interval '#(0 0) '#(10 10))
		      list))

(test ((array-getter a) 3 4)
      '(3 4))

(define curried-a (array-curry a 1))

(test ((array-getter ((array-getter curried-a) 3)) 4)
      '(3 4))

(define sparse-array
  (let ((domain (make-interval '#(0 0) '#(1000000 1000000)))
	(sparse-rows (make-vector 1000000 '())))
    (make-array domain
		(lambda (i j)
		  (cond ((assv j (vector-ref sparse-rows i))
			 => cdr)
			(else
			 0.0)))
		(lambda (v i j)
		  (cond ((assv j (vector-ref sparse-rows i))
			 => (lambda (pair)
			      (set-cdr! pair v)))
			(else
			 (vector-set! sparse-rows i (cons (cons j v) (vector-ref sparse-rows i)))))))))

(test ((array-getter sparse-array) 12345 6789)
      0.)

(test ((array-getter sparse-array) 0 0)
      0.)

((array-setter sparse-array) 1.0 0 0)

(test ((array-getter sparse-array) 12345 6789)
      0.)

(test ((array-getter sparse-array) 0 0)
      1.)

(define make-pgm   cons)
(define pgm-greys  car)
(define pgm-pixels cdr)

(define (read-pgm file)

  (define (read-pgm-object port)
    (skip-white-space port)
    (let ((o (read port)))
      (read-char port) ; to skip the newline or next whitespace
      (if (eof-object? o)
	  (error "reached end of pgm file")
	  o)))

  (define (skip-to-end-of-line port)
    (let loop ((ch (read-char port)))
      (if (not (eq? ch #\newline))
	  (loop (read-char port)))))

  (define (white-space? ch)
    (case ch 
      ((#\newline #\space #\tab) #t)
      (else #f)))

  (define (skip-white-space port)
    (let ((ch (peek-char port)))
      (cond ((white-space? ch) (read-char port) (skip-white-space port))
	    ((eq? ch #\#) (skip-to-end-of-line port)(skip-white-space port))
	    (else #f))))

  (call-with-input-file
      file
    (lambda (port)
      (let* ((header (read-pgm-object port))
	     (columns (read-pgm-object port))
	     (rows (read-pgm-object port))
	     (greys (read-pgm-object port)))
	(make-pgm greys
		  (array->specialized-array
		   (make-array
		    (make-interval '#(0 0)
				   (vector rows columns))
		    (cond ((or (eq? header 'p5)                                     ;; pgm binary
			       (eq? header 'P5))
			   (if (< greys 256)
			       (lambda (i j)                                        ;; one byte/pixel
				 (char->integer (read-char port)))
			       (lambda (i j)                                        ;; two bytes/pixel, little-endian
				 (let* ((first-byte (char->integer (read-char port)))
					(second-byte (char->integer (read-char port))))
				   (+ (* second-byte 256) first-byte)))))
			  ((or (eq? header 'p2)                                     ;; pgm ascii
			       (eq? header 'P2))
			   (lambda (i j)
			     (read port)))
			  (else
			   (error "read-pgm: not a pgm file"))))))))))

(define a (read-pgm "test.pgm"))

(test (and (array? (pgm-pixels a))
	   (interval= (array-domain (pgm-pixels a))
		      (make-interval '#(0 0) '#(128 128)))
	   (= ((array-getter (pgm-pixels a)) 127 127)
	      225))
      #t)

(define m (array->specialized-array (make-array (make-interval '#(0 0) '#(40 30)) (lambda (i j) (exact->inexact (+ i j))))))

(define (array-sum a)
  (array-reduce + 0 a))
(define (array-max a)
  (array-reduce max -inf.0 a))

(define (max-norm a)
  (array-max (array-map abs a)))
(define (one-norm a)
  (array-sum (array-map abs a)))

(define (operator-max-norm a)
  (max-norm (array-map one-norm (array-curry (array-permute a '#(1 0)) 1))))
(define (operator-one-norm a)
  ;; The "permutation" to apply here is the identity, so we omit it.
  (max-norm (array-map one-norm (array-curry a 1))))

(test (operator-max-norm m) 1940.)

(test (operator-one-norm m) 1605.)

(define vector-map ##vector-map)

(define (all-second-differences image direction)
  (let ((image-domain (array-domain image)))
    (let loop ((i 1)
               (result '()))
      (let ((negative-scaled-direction
             (vector-map (lambda (j) (* -1 j i)) direction))
            (twice-negative-scaled-direction
             (vector-map (lambda (j) (* -2 j i)) direction)))
        (cond ((interval-intersect? image-domain
                                    (interval-translate image-domain negative-scaled-direction)
                                    (interval-translate image-domain twice-negative-scaled-direction))
               => (lambda (subdomain)
                    (loop (+ i 1)
                          (cons (array->specialized-array
                                 (array-map (lambda (f_i f_i+d f_i+2d)
                                              (+ f_i+2d
                                                 (* -2. f_i+d)
                                                 f_i))
                                            (array-extract image
                                                           subdomain)
                                            (array-extract (array-translate image
                                                                            negative-scaled-direction)
                                                           subdomain)
                                            (array-extract (array-translate image
                                                                            twice-negative-scaled-direction)
                                                           subdomain)))
                                result))))
              (else
               (reverse result)))))))

(define image (array->specialized-array (make-array (make-interval '#(0 0) '#(8 8))
						    (lambda (i j)
						      (exact->inexact (+ (* i i) (* j j)))))))

(define (expose difference-images)
  (pretty-print (map (lambda (difference-image)
		       (list (array-domain difference-image)
			     (array->list difference-image)))
		     difference-images)))
(begin
  (display "\nSecond-difference images in the direction $k\\times (1,0)$, $k=1,2,...$, wherever they're defined:\n")
  (expose (all-second-differences image '#(1 0)))
  (display "\nSecond-difference images in the direction $k\\times (1,1)$, $k=1,2,...$, wherever they're defined:\n")
  (expose (all-second-differences image '#(1 1)))
  (display "\nSecond-difference images in the direction $k\\times (1,-1)$, $k=1,2,...$, wherever they're defined:\n")
  (expose (all-second-differences image '#(1 -1))))

(define (1D-Haar-loop a)
  (let ((getter (array-getter a))
	(setter (array-setter a))
	(n (interval-upper-bound (array-domain a) 0)))
    (do ((i 0 (fx+ i 2)))
	((fx= i n))
      (let* ((a_i   (getter i))
	     (a_i+1 (getter (fx+ i 1)))
	     (scaled-sum        (fl/ (fl+ a_i a_i+1) (flsqrt 2.0)))
	     (scaled-difference (fl/ (fl- a_i a_i+1) (flsqrt 2.0))))
	(setter scaled-sum i)
	(setter scaled-difference (fx+ i 1))))))

(define (1D-Haar-transform a)
  ;; works only on specialized arrays with domains $[0, 2^k)$ for some $k$
  (let ((n (interval-upper-bound (array-domain a) 0)))
    (if (fx< 1 n)
	(begin
	  ;; calculate the scaled sums and differences
	  (1D-Haar-loop a)
	  ;; Apply the transform to the subarray of scaled sums
	  (1D-Haar-transform (specialized-array-share a
						      (make-interval '#(0) (vector (quotient n 2)))
						      (lambda (i)
							(fx* 2 i))))))))

(define (1D-Haar-inverse-transform a)
  ;; works only on specialized arrays with domains $[0, 2^k)$ for some $k$
  (let* ((n (interval-upper-bound (array-domain a) 0)))
    (if (fx< 1 n)
	(begin
	  ;; Apply the inverse transform to get the array of scaled sums
	  (1D-Haar-inverse-transform (specialized-array-share a
							      (make-interval '#(0) (vector (quotient n 2)))
							      (lambda (i)
								(fx* 2 i))))
	  ;; reconstruct the array values from the scaled sums and differences
	  (1D-Haar-loop a)))))

(define (make-separable-transform 1D-transform)
  (lambda (array)
    ;; Works on arrays of any dimension.
    (let* ((n
	    (array-dimension array))
	   (permutation
	    ;; we start with the identity permutation
	    (let ((result (make-vector n)))
	      (do ((i 0 (fx+ i 1)))
		  ((fx= i n) result)
		(vector-set! result i i)))))
      ;; We apply the one-dimensional transform in each coordinate direction.
      (do ((d 0 (fx+ d 1)))
	  ((fx= d n))
	;; Swap the d'th and n-1'st coordinates
	(vector-set! permutation (fx- n 1) d)
	(vector-set! permutation d (fx- n 1))
	;; Apply the transform in the d'th coordinate direction
	;; to all \"pencils\" in that direction
	;; array-permute re-orders the coordinates to put the
	;; d'th coordinate at the end, array-curry returns
	;; an $n-1$-dimensional array of one-dimensional subarrays,
	;; and 1D-transform is applied to each of those
	;; one-dimensional sub-arrays.
	(array-for-each 1D-transform
			(array-curry (array-permute array permutation)
				     (fx- n 1)))
	;; return the permutation to the identity
	(vector-set! permutation d d)
	(vector-set! permutation (fx- n 1) (fx- n 1))))))

(define Haar-transform
  (make-separable-transform 1D-Haar-transform))
(define Haar-inverse-transform
  (make-separable-transform 1D-Haar-inverse-transform))

(let ((image (array->specialized-array (make-array (make-interval '#(0 0) '#(4 4))
						   (lambda (i j)
						     (if (fx< i 2) 1. -1.))))))
  (display "\nInitial image: \n")
  (pretty-print (list (array-domain image)
		      (array->list image)))
  (Haar-transform image)
  (display "\nArray of Haar wavelet coefficients: \n")
  (pretty-print (list (array-domain image)
		      (array->list image)))
  (Haar-inverse-transform image)
  (display "\nArray reconstructed from Haar wavelet coefficients: \n")
  (pretty-print (list (array-domain image)
		      (array->list image))))
