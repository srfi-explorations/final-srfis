;;(declare (standard-bindings)(extended-bindings)(block)(not safe) (fixnum))
(declare (inlining-limit 0))
(define tests 20)

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
	 (pp (list ',expr" => " result ", not " ,value #\newline)))))

(define-macro (test-multiple-values expr vals)
  `(call-with-values
       (lambda () ,expr)
     (lambda args
       (if (not (equal? args ,vals))
	   (pp (list ',expr  " => " args ", not " ,vals #\newline))))))

(define (random a #!optional b)
  (if b
      (+ a (random-integer (- b a)))
      (random-integer a)))

(include "generic-arrays.scm")

(declare (generic))

(define (random-index interval)
  (apply values (map random (map list
				 (interval-lower-bounds->list interval)
				 (interval-upper-bounds->list interval)))))

(pp "Interval error tests")

(test (interval 1 '#(3 4))
      "interval: lower-bounds must be a vector: ")

(test (interval '#(1 1)  3)
      "interval: upper-bounds must be a vector: ")

(test (interval '#(1 1)  '#(3))
      "interval: lower-bounds and upper-bounds must be the same length: ")

(test (interval '#()  '#())
      "interval: lower-bounds and upper-bounds must be nonempty vectors: ")

(test (interval '#(1.)  '#(1))
      "interval: All lower-bounds must be exact integers: ")

(test (interval '#(1 #f)  '#(1 2))
      "interval: All lower-bounds must be exact integers: ")

(test (interval '#(1)  '#(1.))
      "interval: All upper-bounds must be exact integers: ")

(test (interval '#(1 1)  '#(1 #f))
      "interval: All upper-bounds must be exact integers: ")

(test (interval '#(1)  '#(1))
      "interval: Each lower-bound must be less than the associated upper-bound: ")

(test (interval '#(1 2 3)  '#(4 2 6))
      "interval: Each lower-bound must be less than the associated upper-bound: ")

(pp "interval result tests")

(test (interval '#(11111)  '#(11112))
      (interval '#(11111) '#(11112)))

(test (interval '#(1 2 3)  '#(4 5 6))
      (interval '#(1 2 3) '#(4 5 6)))

(pp "interval? result tests")

(test (interval? #t)
      #f)

(test (interval? (interval '#(1 2 3) '#(4 5 6)))
      #t)


(pp "interval-dimension error tests")

(test (interval-dimension 1)
      "interval-dimension: argument is not an interval: ")

(pp "interval-dimension result tests")

(test (interval-dimension (interval '#(1 2 3) '#(4 5 6)))
      3)

(pp "interval-lower-bound error tests")

(test (interval-lower-bound 1 0)
      "interval-lower-bound: argument is not an interval: ")

(test (interval-lower-bound (interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-lower-bound: argument is not an exact integer: ")

(test (interval-lower-bound (interval '#(1 2 3) '#(4 5 6)) 1.)
      "interval-lower-bound: argument is not an exact integer: ")

(test (interval-lower-bound (interval '#(1 2 3) '#(4 5 6)) -1)
      "interval-lower-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-lower-bound (interval '#(1 2 3) '#(4 5 6)) 3)
      "interval-lower-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-lower-bound (interval '#(1 2 3) '#(4 5 6)) 4)
      "interval-lower-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(pp "interval-upper-bound error tests")

(test (interval-upper-bound 1 0)
      "interval-upper-bound: argument is not an interval: ")

(test (interval-upper-bound (interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-upper-bound: argument is not an exact integer: ")

(test (interval-upper-bound (interval '#(1 2 3) '#(4 5 6)) 1.)
      "interval-upper-bound: argument is not an exact integer: ")

(test (interval-upper-bound (interval '#(1 2 3) '#(4 5 6)) -1)
      "interval-upper-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-upper-bound (interval '#(1 2 3) '#(4 5 6)) 3)
      "interval-upper-bound: index is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-upper-bound (interval '#(1 2 3) '#(4 5 6)) 4)
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
    (let ((interval (interval (list->vector lower)
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
    (let ((interval (interval (list->vector lower)
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

(test (interval-curry (interval '#(0) '#(1)) #t)
      "interval-curry: the dimension of the interval is not greater than 1: " )


(test (interval-curry (interval '#(0 0) '#(1 1)) 1/2)
      "interval-curry: argument is not an exact integer: ")

(test (interval-curry (interval '#(0 0) '#(1 1)) 1.)
      "interval-curry: argument is not an exact integer: ")

(test (interval-curry (interval '#(0 0) '#(1 1)) 0)
      "interval-curry: argument is not between 0 and (interval-dimension interval) (exclusive): ")

(test (interval-curry (interval '#(0 0) '#(1 1)) 2)
      "interval-curry: argument is not between 0 and (interval-dimension interval) (exclusive): ")

(pp "interval-curry result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 3 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower))
	 (left-dimension (random 1 (- (length lower) 1))))
    (test-multiple-values
     (interval-curry (interval (list->vector lower)
			       (list->vector upper))
		     left-dimension)
     (list (interval (list->vector (reverse (list-tail (reverse lower) (- (length lower) left-dimension))))
		     (list->vector (reverse (list-tail (reverse upper) (- (length upper) left-dimension)))))
	   (interval (list->vector (list-tail lower left-dimension))
		     (list->vector (list-tail upper left-dimension)))))))

(pp "interval-distinguish-one-axis error tests")

(test (interval-distinguish-one-axis 1 1)
      "interval-distinguish-one-axis: argument is not an interval: ")

(test (interval-distinguish-one-axis (interval '#(0) '#(1)) 1)
      "interval-distinguish-one-axis: The dimension of the argument is not greater than one: " )

(test (interval-distinguish-one-axis (interval '#(0) '#(1)) 1/2)
      "interval-distinguish-one-axis: argument is not an exact integer: ")

(test (interval-distinguish-one-axis (interval '#(0 0) '#(1 1)) 1/2)
      "interval-distinguish-one-axis: argument is not an exact integer: ")

(test (interval-distinguish-one-axis (interval '#(0 0) '#(1 1)) 1.)
      "interval-distinguish-one-axis: argument is not an exact integer: ")

(test (interval-distinguish-one-axis (interval '#(0 0) '#(1 1)) -1)
      "interval-distinguish-one-axis: argument is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(test (interval-distinguish-one-axis (interval '#(0 0) '#(1 1)) 2)
      "interval-distinguish-one-axis: argument is not between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

(pp "interval-distinguish-one-axis result tests")

(define (remove-from-list l index)
  (if (= index 0)
      (cdr l)
      (cons (car l) (remove-from-list (cdr l) (- index 1)))))

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 3 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower))
	 (index (random (- (length lower) 1))))
    (test-multiple-values
     (interval-distinguish-one-axis (interval (list->vector lower)
					      (list->vector upper))
				    index)
     (list (interval (list->vector (remove-from-list lower index))
		     (list->vector (remove-from-list upper index)))
	   (interval (vector (list-ref lower index))
		     (vector (list-ref upper index)))))))

(pp "interval-contains-multi-index? error tests")



(pp "interval-volume error tests")

(test (interval-volume #f)
      "interval-volume: argument is not an interval: ")

(pp "interval-volume result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (test (interval-volume (interval (list->vector lower)
				     (list->vector upper)))
	  (apply * (map - upper lower)))))

(pp "interval= error tests")

(test (interval= #f (interval '#(1 2 3) '#(4 5 6)))
      "interval=: Not all arguments are intervals: ")

(test (interval= (interval '#(1 2 3) '#(4 5 6)) #f)
      "interval=: Not all arguments are intervals: ")

(pp "interval= result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 1 6)))))
	 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
	 (lower2 (map (lambda (x) (random 2)) lower1))
	 (upper2 (map (lambda (x) (+ 1 (random 1 3) x)) lower2)))
    (test (interval= (interval (list->vector lower1)
			       (list->vector upper1))
		     (interval (list->vector lower2)
			       (list->vector upper2)))
	  (and (equal? lower1 lower2)                              ;; the probability of this happening is about 1/16
	       (equal? upper1 upper2)))))

(pp "interval-subset? error tests")

(test (interval-subset? #f (interval '#(1 2 3) '#(4 5 6)))
      "interval-subset?: Not all arguments are intervals: ")

(test (interval-subset? (interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-subset?: Not all arguments are intervals: ")

(pp "interval-subset? result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 1 6)))))
	 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
	 (lower2 (map (lambda (x) (random 2)) lower1))
	 (upper2 (map (lambda (x) (+ (random 1 3) x)) lower2)))
    (test (interval-subset? (interval (list->vector lower1)
				      (list->vector upper1))
			    (interval (list->vector lower2)
				      (list->vector upper2)))
	  (and (every >= lower1 lower2)
	       (every <= upper1 upper2)))))

(pp "interval-contains-multi-index?  error tests")

(test (interval-contains-multi-index? 1 1)
      "interval-contains-multi-index?: argument is not an interval: ")

(test (interval-contains-multi-index? (interval '#(1 2 3) '#(4 5 6)) 1)
      "interval-contains-multi-index?: dimension of interval does not match number of arguments: ")

(test (interval-contains-multi-index? (interval '#(1 2 3) '#(4 5 6)) 1 1/2 0.1)
      "interval-contains-multi-index?: at least one multi-index component is not an exact integer: ")

(pp "interval-contains-multi-index?  result tests")

(let ((interval   (interval '#(1 2 3) '#(4 5 6)))
      (interval-2 (interval '#(10 11 12) '#(13 14 15))))
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

(test (interval-for-each 1 (interval '#(3) '#(4)))
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
				 (interval (list->vector lower)
					   (list->vector upper))
				 #t)
	      result)
	    (reverse (all-elements lower upper))))))

;;; can't come up with a reasonable test for interval-for-each

(pp "interval-reduce error tests")

(test (interval-reduce #t #t #t #t)
      "interval-reduce: Argument is not a interval: ")

(test (interval-reduce #f #f #f (interval '#(2 3) '#(4 5)))
      "interval-reduce: Argument is not a procedure: ")

(test (interval-reduce (lambda (x) x) #f #f (interval '#(2 3) '#(4 5)))
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
			   (interval (list->vector lower)
				     (list->vector upper)))
	  result)
    (test (interval-reduce list
			   (lambda (x y) (cons y x))
			   '()
			   (interval (list->vector lower)
				     (list->vector upper))
			   #f)
	  result)))

(pp "array error tests")

(test (array 1 values)
      "array: domain is not an interval: ")

(test (array (interval '#(3) '#(4)) 1)
      "array: getter is not a procedure: ")

(pp "array result tests")

(let ((getter (lambda args 1.)))
  (test (array (interval '#(3) '#(4)) getter)
	(make-##array-base (interval '#(3) '#(4))
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
       (array    (array (interval '#(3) '#(4)) getter)))
  (test (array? #f)
	#f)
  (test (array? array)
	#t)
  (test (array-domain array)
	(interval '#(3) '#(4)))
  (test (array-getter array)
	getter))

(pp "mutable-array error tests")

(test (mutable-array #f 1 1)
      "mutable-array: domain is not an interval: ")

(test (mutable-array (interval '#(3) '#(4)) 1 1)
      "mutable-array: getter is not a procedure: ")

(test (mutable-array (interval '#(3) '#(4))
		     (lambda args 1.)
		     #f)
      "mutable-array: setter is not a procedure: ")

(pp "mutable-array result tests")

(let ((result #f))
  (let ((getter (lambda (i) result))
	(setter   (lambda (v i) (set! result v)))
	(domain   (interval '#(3) '#(4))))
    (test (mutable-array domain
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
	(domain   (interval '#(3) '#(4))))
    (let ((array (mutable-array domain
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

(pp "indexer= error tests")

(test (indexer= 1 1 1)
      "indexer=: argument is not an interval ")

(test (indexer= 1 1 (interval '#(1) '#(2)))
      "indexer=: argument is not a procedure ")

(test (indexer= + 1 (interval '#(1) '#(2)))
      "indexer=: argument is not a procedure ")

(pp " indexer= result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower-bounds (map (lambda (x) (random 2))
			    (vector->list (make-vector (random 1 7)))))
	 (upper-bounds (map (lambda (x) (+ x (random 1 3)))
			    lower-bounds))
	 (interval (interval (list->vector lower-bounds)
			     (list->vector upper-bounds)))
	 (increments1 (map (lambda (x) (random -2 3))
			   lower-bounds))
	 (base1 (random 2))
	 (increments2 (map (lambda (x) (random -2 3))
			   lower-bounds))
	 (base2 (random 2)))
    (test (myindexer= (my-indexer base1 lower-bounds increments1)
		      (my-indexer base2 lower-bounds increments2)
		      interval)
	  (indexer= (my-indexer base1 lower-bounds increments1)
		    (my-indexer base2 lower-bounds increments2)
		    interval))))

(pp "compute-indexer result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower-bounds (map (lambda (x) (random 2))
			    (vector->list (make-vector (random 1 7)))))
	 (upper-bounds (map (lambda (x) (+ x (random 1 3)))
			    lower-bounds))
	 (interval (interval (list->vector lower-bounds)
			     (list->vector upper-bounds)))
	 (increments (map (lambda (x) (random -2 3))
			  lower-bounds))
	 (base (random 2)))
    (if (not (indexer= (compute-indexer base lower-bounds increments)
		       (my-indexer base lower-bounds increments)
		       interval))
	(pp (list lower-bounds
		  upper-bounds
		  interval
		  increments
		  base)))))

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
	  (interval (list->vector lower-bounds)
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
    (if (not (and (indexer= (lambda args
			      (call-with-values
				  (lambda () (apply new-domain->old-domain args))
				old-indexer))
			    (compose-indexers old-indexer new-domain  new-domain->old-domain)
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

(let ((a (mutable-array (interval '#(0 0) '#(1 1)) ;; not valid
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

(pp "array->specialized-array error tests")

(test (array->specialized-array #f generic-storage-class)
      "array->specialized-array: Argument is not an array: ")

(test (array->specialized-array (array (interval '#(1) '#(2))
				       list)
				#f)
      "array->specialized-array: result-storage-class is not a storage-class: ")


(pp "array->specialized-array result tests")

(set! specialized-array-default-safe? #t)

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
	  (interval (list->vector lower-bounds)
		    (list->vector upper-bounds)))
	 (array1
	  (let ((alist '()))
	    (mutable-array
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

(set! specialized-array-default-safe? #f)

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
	  (interval (list->vector lower-bounds)
		    (list->vector upper-bounds)))
	 (array1
	  (let ((alist '()))
	    (mutable-array
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

(test (array-map list 1 (array (interval '#(3) '#(4))
			       list))
      "array-map: Not all arguments are arrays: ")

(test (array-map list (array (interval '#(3) '#(4))
			     list) 1)
      "array-map: Not all arguments are arrays: ")

(test (array-map list
		 (array (interval '#(3) '#(4))
			list)
		 (array (interval '#(3 4) '#(4 5))
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

(test (array-for-each list 1 (array (interval '#(3) '#(4))
				    list))
      "array-for-each: Not all arguments are arrays: ")

(test (array-for-each list (array (interval '#(3) '#(4))
				  list) 1)
      "array-for-each: Not all arguments are arrays: ")

(test (array-for-each list
		      (array (interval '#(3) '#(4))
			     list)
		      (array (interval '#(3 4) '#(4 5))
			     list))
      "array-for-each: Not all arrays have the same domain: ")


(pp "array-map, array-reduce, and array-for-each result tests")

(set! specialized-array-default-safe? #t)

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
		 (vector->list (make-vector (random 1 7)))))
	   (upper-bounds
	    (map (lambda (x) (+ x (random 1 5)))
		 lower-bounds))
	   (array-length
	    (lambda (a)
	      (let ((upper-bounds (interval-upper-bounds->list (array-domain a)))
		    (lower-bounds (interval-lower-bounds->list (array-domain a))))
		(apply * (map - upper-bounds lower-bounds)))))
	   (domain
	    (interval (list->vector lower-bounds)
		      (list->vector upper-bounds)))
	   (arrays
	    (map (lambda (ignore)
		   (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
		     (array->specialized-array (array domain
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
	    (array domain
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

(set! specialized-array-default-safe? #f)

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
		 (vector->list (make-vector (random 1 7)))))
	   (upper-bounds
	    (map (lambda (x) (+ x (random 1 5)))
		 lower-bounds))
	   (domain
	    (interval (list->vector lower-bounds)
		      (list->vector upper-bounds)))
	   (arrays
	    (map (lambda (ignore)
		   (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
		     (array->specialized-array (array domain
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
	    (array domain
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
(pp "TODO:  MUTATE ARRAY_CURRY tests")


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
	    (interval (list->vector lower-bounds)
		      (list->vector upper-bounds)))
	   (array-builder
	    (vector-ref array-builders (random (vector-length array-builders))))
	   (Array
	    (array->specialized-array (array domain
					     (cadr array-builder))
				      (car array-builder)))
	   (outer-dimension
	    (random 1 (interval-dimension domain)))
	   (immutable-curry
	    (array-curry Array outer-dimension))
	   (mutable-curry
	    (array-curry Array outer-dimension 'mutable))
	   (specialized-curry
	    (array-curry Array outer-dimension 'specialized))
	   (immutable-curry-from-definition
	    (call-with-values
		(lambda () (interval-curry (array-domain Array) outer-dimension))
	      (lambda (outer-interval inner-interval)
		(array outer-interval
		       (lambda outer-multi-index
			 (array inner-interval
				(lambda inner-multi-index
				  (apply (array-getter Array) (append outer-multi-index inner-multi-index)))))))))
	   (mutable-curry-from-definition
	    (call-with-values
		(lambda () (interval-curry (array-domain Array) outer-dimension))
	      (lambda (outer-interval inner-interval)
		(array outer-interval
		       (lambda outer-multi-index
			 (mutable-array inner-interval
					(lambda inner-multi-index
					  (apply (array-getter Array) (append outer-multi-index inner-multi-index)))
					(lambda (v . inner-multi-index)
					  (apply (array-setter Array) v (append outer-multi-index inner-multi-index)))))))))
	   (specialized-curry-from-definition
	    (call-with-values
		(lambda () (interval-curry (array-domain Array) outer-dimension))
	      (lambda (outer-interval inner-interval)
		(array outer-interval
		       (lambda outer-multi-index
			 (specialized-array-share Array
						  inner-interval
						  (lambda inner-multi-index
						    (apply values (append outer-multi-index inner-multi-index))))))))))
      (if (not (and (array-every? array? immutable-curry)
		    (array-every? (lambda (a) (not (mutable-array? a))) immutable-curry)
		    (array-every? mutable-array? mutable-curry)
		    (array-every? (lambda (a) (not (specialized-array? a))) mutable-curry)
		    (array-every? specialized-array? specialized-curry)
		    (array-every? (lambda (xy) (apply myarray= xy))
				  (array-map list immutable-curry immutable-curry-from-definition))
		    (array-every? (lambda (xy) (apply myarray= xy))
				  (array-map list mutable-curry mutable-curry-from-definition))
		    (array-every? (lambda (xy) (apply myarray= xy))
				  (array-map list specialized-curry specialized-curry-from-definition))))
	  (error "Arghh")))))

(pp "specialized-array-share error tests")

(test (specialized-array-share 1 1 1)
      "specialized-array-share: array is not a specialized-array: ")

(test (specialized-array-share (specialized-array domain: (interval '#(1) '#(2))
						  storage-class: generic-storage-class)
			       1 1)
      "specialized-array-share: new-domain is not an interval: ")

(test (specialized-array-share (specialized-array domain: (interval '#(1) '#(2))
						  storage-class: generic-storage-class)
			       (interval '#(0) '#(1))
			       1)
      "specialized-array-share: new-domain->old-domain is not a procedure: ")


(pp "specialized-array-share result tests")

(define (vector-permute v)
  (let ((result (vector-copy v))
	(n (vector-length v)))
    (do ((i 0 (+ i 1)))
	((= i n) result)
      (let ((j (random i n)))
	(let ((temp (vector-ref result i)))
	  (vector-set! result i (vector-ref result j))
	  (vector-set! result j temp))))))

(define (permute v permutation)
  (let* ((n (vector-length v))
	 (result (make-vector n)))
    (do ((i 0 (+ i 1)))
	((= i n) result)
      (vector-set! result i (vector-ref v (vector-ref permutation i))))))

(set! specialized-array-default-safe? #t)

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((axes (iota 0 (random 1 5)))
	 (lower-bounds (list->vector (map (lambda (x) (random -10 10)) axes)))
	 (upper-bounds (list->vector (map (lambda (l) (+ l (random 1 4))) (vector->list lower-bounds))))
	 (a (array->specialized-array (array (interval lower-bounds
						       upper-bounds)
					     list)
				      generic-storage-class
				      ))
	 (new-axis-order (vector-permute (list->vector axes)))
	 (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
    (let ((b (array (interval (permute lower-bounds new-axis-order)
			      (permute upper-bounds new-axis-order))
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
				      (interval (permute lower-bounds new-axis-order)
						(permute upper-bounds new-axis-order))
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

(set! specialized-array-default-safe? #f)

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((axes (iota 0 (random 1 5)))
	 (lower-bounds (list->vector (map (lambda (x) (random -10 10)) axes)))
	 (upper-bounds (list->vector (map (lambda (l) (+ l (random 1 4))) (vector->list lower-bounds))))
	 (a (array->specialized-array (array (interval lower-bounds
						       upper-bounds)
					     list)
				      generic-storage-class
				      ))
	 (new-axis-order (vector-permute (list->vector axes)))
	 (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
    (let ((b (array (interval (permute lower-bounds new-axis-order)
			      (permute upper-bounds new-axis-order))
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
				      (interval (permute lower-bounds new-axis-order)
						(permute upper-bounds new-axis-order))
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

(pp "Test code from the SRFI document")

(test (interval= (interval-dilate (interval '#(0 0) '#(100 100)) '#(1 1) '#(1 1))
		 (interval '#(1 1) '#(101 101)))
      #t)

(test (interval= (interval-dilate (interval '#(0 0) '#(100 100)) '#(-1 -1) '#(1 1))
		 (interval '#(-1 -1) '#(101 101)))
      #t)

(test (interval= (interval-dilate (interval '#(0 0) '#(100 100))  '#(0 0) '#(-50 -50))
		 (interval '#(0 0) '#(50 50)))
      #t)

(test (interval-dilate (interval '#(0 0) '#(100 100)) '#(0 0) '#(-500 -50))
      "interval-dilate: the resulting interval is empty: ")

(define a (array (interval '#(1 1) '#(11 11))
		 (lambda (i j)
		   (if (= i j)
		       1
		       0))))

(test ((array-getter a) 3 3)
      1)

(test ((array-getter a) 2 3)
      0)

;; ((array-getter a) 11 0) is an error, but it isn't signalled

(define a (array (interval '#(0 0) '#(10 10))
		 list))

(test ((array-getter a) 3 4)
      '(3 4))

(define curried-a (array-curry a 1))

(test ((array-getter ((array-getter curried-a) 3)) 4)
      '(3 4))

(define sparse-array
  (let ((domain (interval '#(0 0) '#(1000000 1000000)))
	(sparse-rows (make-vector 1000000 '())))
    (mutable-array domain
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
		   (array
		    (interval '#(0 0)
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
		      (interval '#(0 0) '#(128 128)))
	   (= ((array-getter (pgm-pixels a)) 127 127)
	      225))
      #t)

(define m (array->specialized-array (array (interval '#(0 0) '#(40 30)) (lambda (i j) (exact->inexact (+ i j))))))

(define (array-sum a)
  (array-reduce + 0 a))
(define (array-max a)
  (array-reduce max -inf.0 a))

(define (max-norm a)
  (array-max (array-map abs a)))
(define (one-norm a)
  (array-sum (array-map abs a)))

(define (operator-max-norm a)
  (max-norm (array-map one-norm (array-distinguish-one-axis a 0))))
(define (operator-one-norm a)
  (max-norm (array-map one-norm (array-distinguish-one-axis a 1))))

(test (operator-max-norm m) 1940.)

(test (operator-one-norm m) 1605.)
