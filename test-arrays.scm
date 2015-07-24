;;(declare (standard-bindings)(extended-bindings)(block)(not safe) (fixnum))
(declare (inlining-limit 0))
(define tests 20)

(define-macro (test expr value)
  `(let* (;(ignore (pretty-print ',expr))
	  (result (call-with-current-continuation
		   (lambda (c)
		     (with-exception-handler
		      (lambda (args)
			
			(c (error-exception-message args)))
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

(pp "Interval error tests")

(test (Interval 1 '#(3 4))
      "Interval: lower-bounds must be a vector: ")

(test (Interval '#(1 1)  3)
      "Interval: upper-bounds must be a vector: ")

(test (Interval '#(1 1)  '#(3))
      "Interval: lower-bounds and upper-bounds must be the same length: ")

(test (Interval '#()  '#())
      "Interval: lower-bounds and upper-bounds must be nonempty vectors: ")

(test (Interval '#(1.)  '#(1))
      "Interval: All lower-bounds must be exact integers: ")

(test (Interval '#(1 #f)  '#(1 2))
      "Interval: All lower-bounds must be exact integers: ")

(test (Interval '#(1)  '#(1.))
      "Interval: All upper-bounds must be exact integers: ")

(test (Interval '#(1 1)  '#(1 #f))
      "Interval: All upper-bounds must be exact integers: ")

(test (Interval '#(1)  '#(1))
      "Interval: Each lower-bound must be less than the associated upper-bound: ")

(test (Interval '#(1 2 3)  '#(4 2 6))
      "Interval: Each lower-bound must be less than the associated upper-bound: ")

(pp "Interval result tests")

(test (Interval '#(11111)  '#(11112))
      (Interval '#(11111) '#(11112)))

(test (Interval '#(1 2 3)  '#(4 5 6))
      (Interval '#(1 2 3) '#(4 5 6)))

(pp "Interval? result tests")

(test (Interval? #t)
      #f)

(test (Interval? (Interval '#(1 2 3) '#(4 5 6)))
      #t)


(pp "Interval-dimension error tests")

(test (Interval-dimension 1)
      "Interval-dimension: argument is not an interval: ")

(pp "Interval-dimension result tests")

(test (Interval-dimension (Interval '#(1 2 3) '#(4 5 6)))
      3)

(pp "Interval-lower-bound error tests")

(test (Interval-lower-bound 1 0)
      "Interval-lower-bound: argument is not an interval: ")

(test (Interval-lower-bound (Interval '#(1 2 3) '#(4 5 6)) #f)
      "Interval-lower-bound: argument is not an exact integer: ")

(test (Interval-lower-bound (Interval '#(1 2 3) '#(4 5 6)) 1.)
      "Interval-lower-bound: argument is not an exact integer: ")

(test (Interval-lower-bound (Interval '#(1 2 3) '#(4 5 6)) -1)
      "Interval-lower-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(test (Interval-lower-bound (Interval '#(1 2 3) '#(4 5 6)) 3)
      "Interval-lower-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(test (Interval-lower-bound (Interval '#(1 2 3) '#(4 5 6)) 4)
      "Interval-lower-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(pp "Interval-upper-bound error tests")

(test (Interval-upper-bound 1 0)
      "Interval-upper-bound: argument is not an interval: ")

(test (Interval-upper-bound (Interval '#(1 2 3) '#(4 5 6)) #f)
      "Interval-upper-bound: argument is not an exact integer: ")

(test (Interval-upper-bound (Interval '#(1 2 3) '#(4 5 6)) 1.)
      "Interval-upper-bound: argument is not an exact integer: ")

(test (Interval-upper-bound (Interval '#(1 2 3) '#(4 5 6)) -1)
      "Interval-upper-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(test (Interval-upper-bound (Interval '#(1 2 3) '#(4 5 6)) 3)
      "Interval-upper-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(test (Interval-upper-bound (Interval '#(1 2 3) '#(4 5 6)) 4)
      "Interval-upper-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(pp "Interval-lower-bounds->list error tests")

(test (Interval-lower-bounds->list 1)
      "Interval-lower-bounds->list: argument is not an interval: ")

(pp "Interval-upper-bounds->list error tests")

(test (Interval-upper-bounds->list #f)
      "Interval-upper-bounds->list: argument is not an interval: ")

(pp "Interval-lower-bound, Interval-upper-bound, Interval-lower-bounds->list, and Interval-upper-bounds->list result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (let ((interval (Interval (list->vector lower)
			      (list->vector upper)))
	  (offset (random (length lower))))
      (test (Interval-lower-bound interval offset)
	    (list-ref lower offset))
      (test (Interval-upper-bound interval offset)
	    (list-ref upper offset))
      (test (Interval-lower-bounds->list interval)
	    lower)
      (test (Interval-upper-bounds->list interval)
	    upper))))

(pp "Interval-lower-bounds->vector error tests")

(test (Interval-lower-bounds->vector 1)
      "Interval-lower-bounds->vector: argument is not an interval: ")

(pp "Interval-upper-bounds-> error tests")

(test (Interval-upper-bounds->vector #f)
      "Interval-upper-bounds->vector: argument is not an interval: ")

(pp "Interval-lower-bound, Interval-upper-bound, Interval-lower-bounds->vector, and Interval-upper-bounds->vector result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (let ((interval (Interval (list->vector lower)
			      (list->vector upper)))
	  (offset (random (length lower))))
      (test (Interval-lower-bound interval offset)
	    (list-ref lower offset))
      (test (Interval-upper-bound interval offset)
	    (list-ref upper offset))
      (test (Interval-lower-bounds->vector interval)
	    (list->vector lower))
      (test (Interval-upper-bounds->vector interval)
	    (list->vector upper)))))

(pp "Interval-curry error tests")

(test (Interval-curry 1 1)
      "Interval-curry: argument is not an interval: ")

(test (Interval-curry (Interval '#(0) '#(1)) #t)
      "Interval-curry: the dimension of the interval is not greater than 1: " )


(test (Interval-curry (Interval '#(0 0) '#(1 1)) 1/2)
      "Interval-curry: argument is not an exact integer: ")

(test (Interval-curry (Interval '#(0 0) '#(1 1)) 1.)
      "Interval-curry: argument is not an exact integer: ")

(test (Interval-curry (Interval '#(0 0) '#(1 1)) 0)
      "Interval-curry: argument is not between 0 and (Interval-dimension interval) (exclusive): ")

(test (Interval-curry (Interval '#(0 0) '#(1 1)) 2)
      "Interval-curry: argument is not between 0 and (Interval-dimension interval) (exclusive): ")

(pp "Interval-curry result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 3 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower))
	 (left-dimension (random 1 (- (length lower) 1))))
    (test-multiple-values
     (Interval-curry (Interval (list->vector lower)
			       (list->vector upper))
		     left-dimension)
     (list (Interval (list->vector (reverse (list-tail (reverse lower) (- (length lower) left-dimension))))
		     (list->vector (reverse (list-tail (reverse upper) (- (length upper) left-dimension)))))
	   (Interval (list->vector (list-tail lower left-dimension))
		     (list->vector (list-tail upper left-dimension)))))))

(pp "Interval-distinguish-one-axis error tests")

(test (Interval-distinguish-one-axis 1 1)
      "Interval-distinguish-one-axis: argument is not an interval: ")

(test (Interval-distinguish-one-axis (Interval '#(0) '#(1)) 1)
      "Interval-distinguish-one-axis: The dimension of the argument is not greater than one: " )

(test (Interval-distinguish-one-axis (Interval '#(0) '#(1)) 1/2)
      "Interval-distinguish-one-axis: argument is not an exact integer: ")

(test (Interval-distinguish-one-axis (Interval '#(0 0) '#(1 1)) 1/2)
      "Interval-distinguish-one-axis: argument is not an exact integer: ")

(test (Interval-distinguish-one-axis (Interval '#(0 0) '#(1 1)) 1.)
      "Interval-distinguish-one-axis: argument is not an exact integer: ")

(test (Interval-distinguish-one-axis (Interval '#(0 0) '#(1 1)) -1)
      "Interval-distinguish-one-axis: argument is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(test (Interval-distinguish-one-axis (Interval '#(0 0) '#(1 1)) 2)
      "Interval-distinguish-one-axis: argument is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): ")

(pp "Interval-distinguish-one-axis result tests")

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
     (Interval-distinguish-one-axis (Interval (list->vector lower)
					      (list->vector upper))
				    index)
     (list (Interval (list->vector (remove-from-list lower index))
		     (list->vector (remove-from-list upper index)))
	   (Interval (vector (list-ref lower index))
		     (vector (list-ref upper index)))))))

(pp "Interval-contains-multi-index? error tests")



(pp "Interval-volume error tests")

(test (Interval-volume #f)
      "Interval-volume: argument is not an interval: ")

(pp "Interval-volume result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (test (Interval-volume (Interval (list->vector lower)
				     (list->vector upper)))
	  (apply * (map - upper lower)))))

(pp "Interval= error tests")

(test (Interval= #f (Interval '#(1 2 3) '#(4 5 6)))
      "Interval=: Not all arguments are intervals: ")

(test (Interval= (Interval '#(1 2 3) '#(4 5 6)) #f)
      "Interval=: Not all arguments are intervals: ")

(pp "Interval= result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 1 6)))))
	 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
	 (lower2 (map (lambda (x) (random 2)) lower1))
	 (upper2 (map (lambda (x) (+ 1 (random 1 3) x)) lower2)))
    (test (Interval= (Interval (list->vector lower1)
			       (list->vector upper1))
		     (Interval (list->vector lower2)
			       (list->vector upper2)))
	  (and (equal? lower1 lower2)                              ;; the probability of this happening is about 1/16
	       (equal? upper1 upper2)))))

(pp "Interval-subset? error tests")

(test (Interval-subset? #f (Interval '#(1 2 3) '#(4 5 6)))
      "Interval-subset?: Not all arguments are intervals: ")

(test (Interval-subset? (Interval '#(1 2 3) '#(4 5 6)) #f)
      "Interval-subset?: Not all arguments are intervals: ")

(pp "Interval-subset? result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 1 6)))))
	 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
	 (lower2 (map (lambda (x) (random 2)) lower1))
	 (upper2 (map (lambda (x) (+ (random 1 3) x)) lower2)))
    (test (Interval-subset? (Interval (list->vector lower1)
				      (list->vector upper1))
			    (Interval (list->vector lower2)
				      (list->vector upper2)))
	  (and (every >= lower1 lower2)
	       (every <= upper1 upper2)))))

(pp "Interval-contains-multi-index?  error tests")

(test (Interval-contains-multi-index? 1 1)
      "Interval-contains-multi-index?: argument is not an Interval: ")

(test (Interval-contains-multi-index? (Interval '#(1 2 3) '#(4 5 6)) 1)
      "Interval-contains-multi-index?: dimension of interval does not match number of arguments: ")

(test (Interval-contains-multi-index? (Interval '#(1 2 3) '#(4 5 6)) 1 1/2 0.1)
      "Interval-contains-multi-index?: at least one multi-index component is not an exact integer: ")

(pp "Interval-contains-multi-index?  result tests")

(let ((interval   (Interval '#(1 2 3) '#(4 5 6)))
      (interval-2 (Interval '#(10 11 12) '#(13 14 15))))
  (if (not (Interval-reduce list
			    (lambda (result x)
			      (and result (apply Interval-contains-multi-index? interval x)))
			    #t
			    interval))
      (error "these should all be true"))
  (if (not (Interval-reduce list
			    (lambda (result x)
			      (and result (not (apply Interval-contains-multi-index? interval x))))
			    #t
			    interval-2))
      (error "these should all be false")))

(pp "Interval-for-each error tests")

(test (Interval-for-each (lambda (x) x) 1)
      "Interval-for-each: Argument is not a interval: ")

(test (Interval-for-each 1 (Interval '#(3) '#(4)))
      "Interval-for-each: Argument is not a procedure: ")

(test (Interval-for-each-serial (lambda (x) x) 1)
      "Interval-for-each-serial: Argument is not a interval: ")

(test (Interval-for-each-serial 1 (Interval '#(3) '#(4)))
      "Interval-for-each-serial: Argument is not a procedure: ")

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

(pp "Interval-for-each result tests")

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
	      (Interval-for-each-serial f
					(Interval (list->vector lower)
						  (list->vector upper)))
	      result)
	    (reverse (all-elements lower upper))))))

;;; can't come up with a reasonable test for Interval-for-each

(pp "Interval-reduce error tests")

(test (Interval-reduce #t #t #t #t)
      "Interval-reduce: Argument is not a interval: ")

(test (Interval-reduce #f #f #f (Interval '#(2 3) '#(4 5)))
      "Interval-reduce: Argument is not a procedure: ")

(test (Interval-reduce (lambda (x) x) #f #f (Interval '#(2 3) '#(4 5)))
      "Interval-reduce: Operator is not a procedure: ")

(test (Interval-reduce-serial #t #t #t #t)
      "Interval-reduce-serial: Argument is not a interval: ")

(test (Interval-reduce-serial #f #f #f (Interval '#(2 3) '#(4 5)))
      "Interval-reduce-serial: Argument is not a procedure: ")

(test (Interval-reduce-serial (lambda (x) x) #f #f (Interval '#(2 3) '#(4 5)))
      "Interval-reduce-serial: Operator is not a procedure: ")

(pp "Interval-reduce result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10))
		     (vector->list (make-vector (random 1 7)))))
	 (upper (map (lambda (x) (+ (random 1 4) x))
		     lower))
	 (result (reverse (all-elements lower upper))))
    
    (define (f . args)
      args)
    
    (test (Interval-reduce list
			   (lambda (x y) (cons y x))
			   '()
			   (Interval (list->vector lower)
				     (list->vector upper)))
	  result)
    (test (Interval-reduce-serial list
				  (lambda (x y) (cons y x))
				  '()
				  (Interval (list->vector lower)
					    (list->vector upper)))
	  result)))

(pp "Dilation error tests")

(test (Dilation 1 '#(3 4))
      "Dilation: lower-bounds must be a vector: ")

(test (Dilation '#(1 1)  3)
      "Dilation: upper-bounds must be a vector: ")

(test (Dilation '#(1 1)  '#(3))
      "Dilation: lower-bounds and upper-bounds must be the same length: ")

(test (Dilation '#()  '#())
      "Dilation: lower-bounds and upper-bounds must be nonempty vectors: ")

(test (Dilation '#(1.)  '#(1))
      "Dilation: All lower-bounds must be exact integers: ")

(test (Dilation '#(1 #f)  '#(1 2))
      "Dilation: All lower-bounds must be exact integers: ")

(test (Dilation '#(1)  '#(1.))
      "Dilation: All upper-bounds must be exact integers: ")

(test (Dilation '#(1 1)  '#(1 #f))
      "Dilation: All upper-bounds must be exact integers: ")

(pp "Dilation result tests")

(test (Dilation '#(11111)  '#(11112))
      (Dilation '#(11111) '#(11112)))

(test (Dilation '#(1 2 3)  '#(4 5 6))
      (Dilation '#(1 2 3) '#(4 5 6)))

(pp "Dilation? result tests")

(test (Dilation? #t)
      #f)

(test (Dilation? (Dilation '#(1 2 3) '#(4 5 6)))
      #t)


(pp "Dilation-dimension error tests")

(test (Dilation-dimension 1)
      "Dilation-dimension: argument is not a dilation: ")

(pp "Dilation-dimension result tests")

(test (Dilation-dimension (Dilation '#(1 2 3) '#(4 5 6)))
      3)

(pp "Dilation-lower-bound error tests")

(test (Dilation-lower-bound 1 0)
      "Dilation-lower-bound: argument is not a dilation: ")

(test (Dilation-lower-bound (Dilation '#(1 2 3) '#(4 5 6)) #f)
      "Dilation-lower-bound: argument is not an exact integer: ")

(test (Dilation-lower-bound (Dilation '#(1 2 3) '#(4 5 6)) 1.)
      "Dilation-lower-bound: argument is not an exact integer: ")

(test (Dilation-lower-bound (Dilation '#(1 2 3) '#(4 5 6)) -1)
      "Dilation-lower-bound: index is not between 0 (inclusive) and (Dilation-dimension interval) (exclusive): ")

(test (Dilation-lower-bound (Dilation '#(1 2 3) '#(4 5 6)) 3)
      "Dilation-lower-bound: index is not between 0 (inclusive) and (Dilation-dimension interval) (exclusive): ")

(test (Dilation-lower-bound (Dilation '#(1 2 3) '#(4 5 6)) 4)
      "Dilation-lower-bound: index is not between 0 (inclusive) and (Dilation-dimension interval) (exclusive): ")

(pp "Dilation-upper-bound error tests")

(test (Dilation-upper-bound 1 0)
      "Dilation-upper-bound: argument is not a dilation: ")

(test (Dilation-upper-bound (Dilation '#(1 2 3) '#(4 5 6)) #f)
      "Dilation-upper-bound: argument is not an exact integer: ")

(test (Dilation-upper-bound (Dilation '#(1 2 3) '#(4 5 6)) 1.)
      "Dilation-upper-bound: argument is not an exact integer: ")

(test (Dilation-upper-bound (Dilation '#(1 2 3) '#(4 5 6)) -1)
      "Dilation-upper-bound: index is not between 0 (inclusive) and (Dilation-dimension interval) (exclusive): ")

(test (Dilation-upper-bound (Dilation '#(1 2 3) '#(4 5 6)) 3)
      "Dilation-upper-bound: index is not between 0 (inclusive) and (Dilation-dimension interval) (exclusive): ")

(test (Dilation-upper-bound (Dilation '#(1 2 3) '#(4 5 6)) 4)
      "Dilation-upper-bound: index is not between 0 (inclusive) and (Dilation-dimension interval) (exclusive): ")

(pp "Dilation-lower-bounds->list error tests")

(test (Dilation-lower-bounds->list 1)
      "Dilation-lower-bounds->list: argument is not a dilation: ")

(pp "Dilation-upper-bounds->list error tests")

(test (Dilation-upper-bounds->list #f)
      "Dilation-upper-bounds->list: argument is not a dilation: ")

(pp "Dilation-lower-bound, Dilation-upper-bound, Dilation-lower-bounds->list, and Dilation-upper-bounds->list result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (let ((interval (Dilation (list->vector lower)
			      (list->vector upper)))
	  (offset (random (length lower))))
      (test (Dilation-lower-bound interval offset)
	    (list-ref lower offset))
      (test (Dilation-upper-bound interval offset)
	    (list-ref upper offset))
      (test (Dilation-lower-bounds->list interval)
	    lower)
      (test (Dilation-upper-bounds->list interval)
	    upper))))

(pp "Dilation-lower-bounds->vector error tests")

(test (Dilation-lower-bounds->vector 1)
      "Dilation-lower-bounds->vector: argument is not a dilation: ")

(pp "Dilation-upper-bounds-> error tests")

(test (Dilation-upper-bounds->vector #f)
      "Dilation-upper-bounds->vector: argument is not a dilation: ")

(pp "Dilation-lower-bound, Dilation-upper-bound, Dilation-lower-bounds->vector, and Dilation-upper-bounds->vector result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
	 (upper (map (lambda (x) (+ (random 1 11) x)) lower)))
    (let ((interval (Dilation (list->vector lower)
			      (list->vector upper)))
	  (offset (random (length lower))))
      (test (Dilation-lower-bound interval offset)
	    (list-ref lower offset))
      (test (Dilation-upper-bound interval offset)
	    (list-ref upper offset))
      (test (Dilation-lower-bounds->vector interval)
	    (list->vector lower))
      (test (Dilation-upper-bounds->vector interval)
	    (list->vector upper)))))

(pp "Dilation= error tests")

(test (Dilation= #f (Dilation '#(1 2 3) '#(4 5 6)))
      "Dilation=: Not all arguments are dilations: ")

(test (Dilation= (Dilation '#(1 2 3) '#(4 5 6)) #f)
      "Dilation=: Not all arguments are dilations: ")

(pp "Dilation= result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 1 6)))))
	 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
	 (lower2 (map (lambda (x) (random 2)) lower1))
	 (upper2 (map (lambda (x) (+ 1 (random 1 3) x)) lower2)))
    (test (Dilation= (Dilation (list->vector lower1)
			       (list->vector upper1))
		     (Dilation (list->vector lower2)
			       (list->vector upper2)))
	  (and (equal? lower1 lower2)                              ;; the probability of this happening is about 1/16
	       (equal? upper1 upper2)))))

(pp "Array error tests")

(test (Array 1 values)
      "Array: domain is not an interval: ")

(test (Array (Interval '#(3) '#(4)) 1)
      "Array: getter is not a procedure: ")

(pp "Array result tests")

(let ((getter (lambda args 1.)))
  (test (Array (Interval '#(3) '#(4)) getter)
	(make-##Array-base (Interval '#(3) '#(4))
			   getter
			   #f
			   #f
			   #f
			   #f
			   #f)))

(pp "Array-domain and Array-getter error tests")

(test (Array-domain #f)
      "Array-domain: object is not an array: ")

(test (Array-getter #f)
      "Array-getter: object is not an array: ")

(pp "Array?, Array-domain, and Array-getter result tests")

(let* ((getter (lambda args 1.))
       (array    (Array (Interval '#(3) '#(4)) getter)))
  (test (Array? #f)
	#f)
  (test (Array? array)
	#t)
  (test (Array-domain array)
	(Interval '#(3) '#(4)))
  (test (Array-getter array)
	getter))

(pp "Mutable-array error tests")

(test (Mutable-array #f 1 1)
      "Mutable-array: domain is not an interval: ")

(test (Mutable-array (Interval '#(3) '#(4)) 1 1)
      "Mutable-array: getter is not a procedure: ")

(test (Mutable-array (Interval '#(3) '#(4))
		     (lambda args 1.)
		     #f)
      "Mutable-array: setter is not a procedure: ")

(pp "Mutable-array result tests")

(let ((result #f))
  (let ((getter (lambda (i) result))
	(setter   (lambda (v i) (set! result v)))
	(domain   (Interval '#(3) '#(4))))
    (test (Mutable-array domain
			 getter
			 setter)
	  (make-##Array-base domain
			     getter
			     setter
			     #f
			     #f
			     #f
			     #f))))

(pp "Array-setter error tests")

(test (Array-setter #f)
      "Array-setter: object is not an mutable array: ")

(pp "Mutable-array? and Array-setter result tests")

(let ((result (cons #f #f)))
  (let ((getter (lambda (i) (car result)))
	(setter   (lambda (v i) (set-car! result v)))
	(domain   (Interval '#(3) '#(4))))
    (let ((array (Mutable-array domain
				getter
				setter)))
      (test (Array? array)
	    #t)
      (test (Mutable-array? array)
	    #t)
      (test (Mutable-array? 1)
	    #f)
      (test (Array-setter array)
	    setter)
      (test (Array-getter array)
	    getter)
      (test (Array-domain array)
	    domain))))

(define (myindexer= indexer1 indexer2 interval)
  (Interval-reduce (lambda args
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

(test (indexer= 1 1 (Interval '#(1) '#(2)))
      "indexer=: argument is not a procedure ")

(test (indexer= + 1 (Interval '#(1) '#(2)))
      "indexer=: argument is not a procedure ")

(pp " indexer= result tests")

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((lower-bounds (map (lambda (x) (random 2))
			    (vector->list (make-vector (random 1 7)))))
	 (upper-bounds (map (lambda (x) (+ x (random 1 3)))
			    lower-bounds))
	 (interval (Interval (list->vector lower-bounds)
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
	 (interval (Interval (list->vector lower-bounds)
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
	  (Interval (list->vector lower-bounds)
		    (list->vector upper-bounds)))
	 (new-domain-dimension
	  (Interval-dimension new-domain))
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

(define (myArray= array1 array2)
  (and (Interval= (Array-domain array1)
		  (Array-domain array2))
       (let ((getter1 (Array-getter array1))
	     (getter2 (Array-getter array2)))
	 (Interval-reduce
	  (lambda indices
	    (equal? (apply getter1 indices)
		    (apply getter2 indices)))
	  (lambda (x y)
	    (and x y))
	  #t
	  (Array-domain array1)))))

(pp "Array body, indexer, manipulators, and safe? error tests")

(let ((a (Mutable-array (Interval '#(0 0) '#(1 1)) ;; not valid
			values
			values)))
  (test (Array-body a)
	"Array-body: argument is not a fixed array: ")
  (test (Array-indexer a)
	"Array-indexer: argument is not a fixed array: ")
  (test (Array-manipulators a)
	"Array-manipulators: argument is not a fixed array: ")
  (test (Array-safe? a)
	"Array-safe?: argument is not a fixed array: "))

(pp "Array->Fixed-array error tests")

(test (Array->Fixed-array #f generic-array-manipulators)
      "Array->Fixed-array: Argument is not an array: ")

(test (Array->Fixed-array (Array (Interval '#(1) '#(2))
				 list)
			  #f)
      "Array->Fixed-array: result-manipulators are not Array-manipulators: ")

(test (Array->Fixed-array-serial #f generic-array-manipulators)
      "Array->Fixed-array-serial: Argument is not an array: ")

(test (Array->Fixed-array-serial (Array (Interval '#(1) '#(2))
					list)
				 #f)
      "Array->Fixed-array-serial: result-manipulators are not Array-manipulators: ")

(pp "Array->Fixed-array result tests")

(set! Fixed-array-default-safe? #t)

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
	  (Interval (list->vector lower-bounds)
		    (list->vector upper-bounds)))
	 (array1
	  (let ((alist '()))
	    (Mutable-array
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
	  (Array->Fixed-array array1 generic-array-manipulators))
	 (setter1
	  (Array-setter array1))
	 (setter2
	  (Array-setter array2)))
    (do ((j 0 (+ j 1)))
	((= j 25))
      (let ((v (random 1000))
	    (indices (map random lower-bounds upper-bounds)))
	(apply setter1 v indices)
	(apply setter2 v indices)))
    (or (myArray= array1 array2) (pp "test1"))
    (or (myArray= (Array->Fixed-array        array1 generic-array-manipulators ) array2) (pp "test3"))
    (or (myArray= (Array->Fixed-array-serial array1 generic-array-manipulators ) array2) (pp "test4"))
    ))

(set! Fixed-array-default-safe? #f)

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
	  (Interval (list->vector lower-bounds)
		    (list->vector upper-bounds)))
	 (array1
	  (let ((alist '()))
	    (Mutable-array
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
	  (Array->Fixed-array array1 generic-array-manipulators ))
	 (setter1
	  (Array-setter array1))
	 (setter2
	  (Array-setter array2)))
    (do ((j 0 (+ j 1)))
	((= j 25))
      (let ((v (random 1000))
	    (indices (map random lower-bounds upper-bounds)))
	(apply setter1 v indices)
	(apply setter2 v indices)))
    (or (myArray= array1 array2) (pp "test1"))
    (or (myArray= (Array->Fixed-array        array1 generic-array-manipulators ) array2) (pp "test3"))
    (or (myArray= (Array->Fixed-array-serial array1 generic-array-manipulators ) array2) (pp "test4"))
    ))

(pp "Array-map error tests")

(test (Array-map 1 #f)
      "Array-map: Argument is not a procedure: ")

(test (Array-map list 1 (Array (Interval '#(3) '#(4))
			       list))
      "Array-map: Not all arguments are arrays: ")

(test (Array-map list (Array (Interval '#(3) '#(4))
			     list) 1)
      "Array-map: Not all arguments are arrays: ")

(test (Array-map list
		 (Array (Interval '#(3) '#(4))
			list)
		 (Array (Interval '#(3 4) '#(4 5))
			list))
      "Array-map: Not all arrays have the same domain: ")


(pp "Array-reduce error tests")

(test (Array-reduce 1 1 1)
      "Array-reduce: operator is not a procedure: ")

(test (Array-reduce list 1 1)
      "Array-reduce: argument is not an array: ")

(test (Array-reduce-serial 1 1 1)
      "Array-reduce-serial: operator is not a procedure: ")

(test (Array-reduce-serial list 1 1)
      "Array-reduce-serial: argument is not an array: ")

(pp "Array-for-each error tests")

(test (Array-for-each 1 #f)
      "Array-for-each: Argument is not a procedure: ")

(test (Array-for-each list 1 (Array (Interval '#(3) '#(4))
				    list))
      "Array-for-each: Not all arguments are arrays: ")

(test (Array-for-each list (Array (Interval '#(3) '#(4))
				  list) 1)
      "Array-for-each: Not all arguments are arrays: ")

(test (Array-for-each list
		      (Array (Interval '#(3) '#(4))
			     list)
		      (Array (Interval '#(3 4) '#(4 5))
			     list))
      "Array-for-each: Not all arrays have the same domain: ")

(test (Array-for-each-serial 1 #f)
      "Array-for-each-serial: Argument is not a procedure: ")

(test (Array-for-each-serial list 1 (Array (Interval '#(3) '#(4))
					   list))
      "Array-for-each-serial: Not all arguments are arrays: ")

(test (Array-for-each-serial list (Array (Interval '#(3) '#(4))
					 list) 1)
      "Array-for-each-serial: Not all arguments are arrays: ")

(test (Array-for-each-serial list
			     (Array (Interval '#(3) '#(4))
				    list)
			     (Array (Interval '#(3 4) '#(4 5))
				    list))
      "Array-for-each-serial: Not all arrays have the same domain: ")

(pp "Array-map, Array-reduce, and Array-for-each result tests")

(set! Fixed-array-default-safe? #t)

(let ((array-builders (vector (list u1-array-manipulators      (lambda indices (random 0 (expt 2 1))))
			      (list u8-array-manipulators      (lambda indices (random 0 (expt 2 8))))
			      (list u16-array-manipulators     (lambda indices (random 0 (expt 2 16))))
			      (list u32-array-manipulators     (lambda indices (random 0 (expt 2 32))))
			      (list u64-array-manipulators     (lambda indices (random 0 (expt 2 64))))
			      (list s8-array-manipulators      (lambda indices (random (- (expt 2 7))  (expt 2 7))))
			      (list s16-array-manipulators     (lambda indices (random (- (expt 2 15)) (expt 2 15))))
			      (list s32-array-manipulators     (lambda indices (random (- (expt 2 31)) (expt 2 31))))
			      (list s64-array-manipulators     (lambda indices (random (- (expt 2 63)) (expt 2 63))))
			      (list f32-array-manipulators     (lambda indices (random-real)))
			      (list f64-array-manipulators     (lambda indices (random-real)))
			      (list generic-array-manipulators (lambda indices indices)))))
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((lower-bounds
	    (map (lambda (x) (random 4))
		 (vector->list (make-vector (random 1 7)))))
	   (upper-bounds
	    (map (lambda (x) (+ x (random 1 5)))
		 lower-bounds))
	   (domain
	    (Interval (list->vector lower-bounds)
		      (list->vector upper-bounds)))
	   (arrays
	    (map (lambda (ignore)
		   (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
		     (Array->Fixed-array (Array domain
						(cadr array-builder))
					 (car array-builder)
					 )))
		 (iota 0 (random 1 7))))
	   (result-array-1
	    (apply Array-map
		   list
		   arrays))
	   (result-array-2
	    (Array->Fixed-array
	     (apply Array-map
		    list
		    arrays)))
	   (getters
	    (map Array-getter arrays))
	   (result-array-3
	    (Array domain
		   (lambda indices
		     (map (lambda (g) (apply g indices)) getters)))))
      (if (not (and (myArray= result-array-1 result-array-2)
		    (myArray= result-array-2 result-array-3)
		    (equal? (vector->list (Array-body result-array-2))
			    (reverse (Array-reduce (lambda (x y) (cons y x))
						   '()
						   result-array-2)))
		    (equal? (vector->list (Array-body result-array-2))
			    (reverse (let ((result '()))
				       (Array-for-each (lambda (f)
							 (set! result (cons f result)))
						       result-array-2)
				       result)))))
	  (pp "Arghh")))))

(set! Fixed-array-default-safe? #f)

(let ((array-builders (vector (list u1-array-manipulators      (lambda indices (random (expt 2 1))))
			      (list u8-array-manipulators      (lambda indices (random (expt 2 8))))
			      (list u16-array-manipulators     (lambda indices (random (expt 2 16))))
			      (list u32-array-manipulators     (lambda indices (random (expt 2 32))))
			      (list u64-array-manipulators     (lambda indices (random (expt 2 64))))
			      (list s8-array-manipulators      (lambda indices (random (- (expt 2 7))  (expt 2 7))))
			      (list s16-array-manipulators     (lambda indices (random (- (expt 2 15)) (expt 2 15))))
			      (list s32-array-manipulators     (lambda indices (random (- (expt 2 31)) (expt 2 31))))
			      (list s64-array-manipulators     (lambda indices (random (- (expt 2 63)) (expt 2 63))))
			      (list f32-array-manipulators     (lambda indices (random-real)))
			      (list f64-array-manipulators     (lambda indices (random-real)))
			      (list generic-array-manipulators (lambda indices indices)))))
  (do ((i 0 (+ i 1)))
      ((= i tests))
    (let* ((lower-bounds
	    (map (lambda (x) (random 4))
		 (vector->list (make-vector (random 1 7)))))
	   (upper-bounds
	    (map (lambda (x) (+ x (random 1 5)))
		 lower-bounds))
	   (domain
	    (Interval (list->vector lower-bounds)
		      (list->vector upper-bounds)))
	   (arrays
	    (map (lambda (ignore)
		   (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
		     (Array->Fixed-array (Array domain
						(cadr array-builder))
					 (car array-builder)
					 )))
		 (iota 0 (random 1 7))))
	   (result-array-1
	    (apply Array-map
		   list
		   arrays))
	   (result-array-2
	    (Array->Fixed-array
	     (apply Array-map
		    list
		    arrays)))
	   (getters
	    (map Array-getter arrays))
	   (result-array-3
	    (Array domain
		   (lambda indices
		     (map (lambda (g) (apply g indices)) getters)))))
      (if (not (and (myArray= result-array-1 result-array-2)
		    (myArray= result-array-2 result-array-3)
		    (equal? (vector->list (Array-body result-array-2))
			    (reverse (Array-reduce (lambda (x y) (cons y x))
						   '()
						   result-array-2)))
		    (equal? (vector->list (Array-body result-array-2))
			    (reverse (let ((result '()))
				       (Array-for-each (lambda (f)
							 (set! result (cons f result)))
						       result-array-2)
				       result)))))
	  (pp "Arghh")))))


(pp "Fixed-array-share! error tests")

(test (Fixed-array-share! 1 1 1)
      "Fixed-array-share!: array is not a Fixed-array: ")

(test (Fixed-array-share! (Fixed-array domain: (Interval '#(1) '#(2))
				       manipulators: generic-array-manipulators)
			  1 1)
      "Fixed-array-share!: new-domain is not an Interval: ")

(test (Fixed-array-share! (Fixed-array domain: (Interval '#(1) '#(2))
				       manipulators: generic-array-manipulators)
			  (Interval '#(0) '#(1))
			  1)
      "Fixed-array-share!: new-domain->old-domain is not a procedure: ")


(pp "Fixed-array-share! result tests")

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

(set! Fixed-array-default-safe? #t)

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((axes (iota 0 (random 1 5)))
	 (lower-bounds (list->vector (map (lambda (x) (random -10 10)) axes)))
	 (upper-bounds (list->vector (map (lambda (l) (+ l (random 1 4))) (vector->list lower-bounds))))
	 (a (Array->Fixed-array (Array (Interval lower-bounds
						 upper-bounds)
				       list)
				generic-array-manipulators
				))
	 (new-axis-order (vector-permute (list->vector axes)))
	 (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
    (let ((b (Array (Interval (permute lower-bounds new-axis-order)
			      (permute upper-bounds new-axis-order))
		    (lambda multi-index
		      (apply (Array-getter a)
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
	  (c (Fixed-array-share! a
				 (Interval (permute lower-bounds new-axis-order)
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
      (if (not (myArray= b c))
	  (pp (list "piffle"
		    a b c))))))

(set! Fixed-array-default-safe? #f)

(do ((i 0 (+ i 1)))
    ((= i tests))
  (let* ((axes (iota 0 (random 1 5)))
	 (lower-bounds (list->vector (map (lambda (x) (random -10 10)) axes)))
	 (upper-bounds (list->vector (map (lambda (l) (+ l (random 1 4))) (vector->list lower-bounds))))
	 (a (Array->Fixed-array (Array (Interval lower-bounds
						 upper-bounds)
				       list)
				generic-array-manipulators
				))
	 (new-axis-order (vector-permute (list->vector axes)))
	 (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
    (let ((b (Array (Interval (permute lower-bounds new-axis-order)
			      (permute upper-bounds new-axis-order))
		    (lambda multi-index
		      (apply (Array-getter a)
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
	  (c (Fixed-array-share! a
				 (Interval (permute lower-bounds new-axis-order)
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
      (if (not (myArray= b c))
	  (pp (list "piffle"
		    a b c))))))

(pp "Test code from the SRFI document")

(test (Interval= (Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(1 1) '#(1 1)))
		 (Interval '#(1 1) '#(101 101)))
      #t)

(test (Interval= (Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(-1 -1) '#(1 1)))
		 (Interval '#(-1 -1) '#(101 101)))
      #t)

(test (Interval= (Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(0 0) '#(-50 -50)))
		 (Interval '#(0 0) '#(50 50)))
      #t)

(test (Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(0 0) '#(-500 -50)))
      "Interval-dilate: the resulting interval is empty: ")

(define a (Array (Interval '#(1 1) '#(11 11))
		       (lambda (i j)
			 (if (= i j)
			     1
			     0))))

(test ((Array-getter a) 3 3)
      1)

(test ((Array-getter a) 2 3)
      0)

;; ((Array-getter a) 11 0) is an error, but it isn't signalled

(define a (Array (Interval '#(0 0) '#(10 10))
		 list))

(test ((Array-getter a) 3 4)
      '(3 4))

(define curried-a (Array-curry a 1))

(test ((Array-getter ((Array-getter curried-a) 3)) 4)
      '(3 4))

(define sparse-array
  (let ((domain (Interval '#(0 0) '#(1000000 1000000)))
	(sparse-rows (make-vector 1000000 '())))
    (Mutable-array domain
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

(test ((Array-getter sparse-array) 12345 6789)
      0.)

(test ((Array-getter sparse-array) 0 0)
      0.)

((Array-setter sparse-array) 1.0 0 0)

(test ((Array-getter sparse-array) 12345 6789)
      0.)

(test ((Array-getter sparse-array) 0 0)
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
		  (Array->Fixed-array-serial
		   (Array
		    (Interval '#(0 0)
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

(test (and (Array? (pgm-pixels a))
	   (Interval= (Array-domain (pgm-pixels a))
		      (Interval '#(0 0) '#(128 128)))
	   (= ((Array-getter (pgm-pixels a)) 127 127)
	      225))
      #t)

(define c32-array-manipulators
  (make-Array-manipulators (lambda (body i)                                         ;; getter
			     (make-rectangular (f32vector-ref body (* 2 i))
					       (f32vector-ref body (+ (* 2 i) 1))))
			   (lambda (body i obj)                                     ;; setter
			     (f32vector-set! body (* 2 i)       (real-part obj))
			     (f32vector-set! body (+ (* 2 i) 1) (imag-part obj)))
			   (lambda (obj)                                            ;; checker
			     (and (complex? obj)
				  (inexact? (real-part obj))
				  (inexact? (imag-part obj))))
			   (lambda (n val)                                          ;; maker
			     (let ((l (* 2 n))
				   (re (real-part val))
				   (im (imag-part val)))
			       (let ((result (make-f32vector l)))
				 (do ((i 0 (+ i 2)))
				     ((= i l) result)
				   (f32vector-set! result i re)
				   (f32vector-set! result (+ i 1) im)))))
			   (lambda (body)                                           ;; length
			     (quotient (f32vector-length body) 2))
			   0.+0.i                                                   ;; default
			   ))

(define a (Array->Fixed-array (Array (Interval '#(0 0) '#(10 10))
				     (lambda (i j)
				       (cond ((< i j) 1.0+0.0i)
					     ((< j i) 0.0+1.0i)
					     (else
					      0.0+0.0i))))
			      c32-array-manipulators
			      #t))

(test (and (eqv? ((Array-getter a) 2 2) 0.0+0.0i)
	   (eqv? ((Array-getter a) 2 3) 1.0+0.0i)
	   (eqv? ((Array-getter a) 3 2) 0.0+1.0i))
      #t)

((Array-setter a) 2.0+2.0i 2 3)

(test (and (eqv? ((Array-getter a) 2 2) 0.0+0.0i)
	   (eqv? ((Array-getter a) 2 3) 2.0+2.0i)
	   (eqv? ((Array-getter a) 3 2) 0.0+1.0i))
      #t)

(test ((Array-getter a) 10 10)
      "Array-getter: domain does not contain multi-index: ")

(test ((Array-setter a) 1+1i 2 2)
      "Array-setter: value cannot be stored in body: ")
