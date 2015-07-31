;;; declarations to reduce the size of the .o file

(declare (standard-bindings)(extended-bindings)(block)(fixnum)(not safe))

;;; The following macro is used to determine whether certain keyword arguments
;;; were omitted.  It is specific to Gambit-C's compiler.
;;; Redefine it for other schemes.


(##define-macro (macro-absent-obj)  `',(##type-cast -6 2))

;;; This is Gambit-specific to make it a bit faster

(##define-macro (##exact-integer? x)
  `(let ((x ,x))
     (or (##fixnum? x)
	 (##bignum? x))))

(declare (inline))

(define (exact-integer? x)
  (##exact-integer? x))

(declare (not inline))

;;; We need a multi-argument every, but not something as fancy as in Olin Shiver's
;;; list library.  (Shiver's version works fine, though, for our purposes.)

(define (every pred list . lists)
  (if (pair? lists)
      (let loop ((lists (cons list lists)))
	(or (null? (car lists))
	    (and (apply pred (map car lists))
		 (loop (map cdr lists)))))
      (let loop ((list list))
	(or (null? list)
	    (and (pred (car list))
		 (loop (cdr list)))))))

(define (vector-every pred vec #!optional (vec2 (macro-absent-obj)) #!rest vecs)
  
  (define (every1 vec i)
    (or (< i 0)
	(and (pred (vector-ref vec i))
	     (every1 vec (- i 1)))))
  
  (define (every2 vec1 vec2 i)
    (or (< i 0)
	(and (pred (vector-ref vec1 i) (vector-ref vec2 i))
	     (every2 vec1 vec2 (- i 1)))))

  (define (every-general vecs i)
    (or (< i 0)
	(and (apply pred (map (lambda (vec) (vector-ref vec i)) vecs))
	     (every-general vecs (- i 1)))))

  (cond ((eq? vec2 (macro-absent-obj))
	 (every1 vec (- (vector-length vec) 1)))
	((null? vecs)
	 (every2 vec vec2 (- (vector-length vec) 1)))
	(else
	 (every-general (cons vec (cons vec2 vecs)) (- (vector-length vec) 1)))))
    
(declare (inline))

;;; An Interval is a cross product of multi-indices

;;; [l_0,u_0) x [l_1,u_1) x ... x [l_n-1,u_n-1)

;;; where l_i < u_i for 0 <= i < n, and n > 0 is the dimension of the Interval

(define-structure ##Interval
  lower-bounds            ;; a vector of exact integers l_0,...,l_n-1
  upper-bounds)           ;; a vector of exact integers u_0,...,u_n-1

(define (Interval? x)
  (##Interval? x))

(declare (not inline))

(define (Interval lower-bounds upper-bounds)
  (cond ((not (vector? lower-bounds))
	 (error "Interval: lower-bounds must be a vector: " lower-bounds))
	((not (vector? upper-bounds))
	 (error "Interval: upper-bounds must be a vector: " upper-bounds))
	((not (= (vector-length lower-bounds) (vector-length upper-bounds)))
	 (error "Interval: lower-bounds and upper-bounds must be the same length: " lower-bounds upper-bounds))
	((not (< 0 (vector-length lower-bounds)))
	 (error "Interval: lower-bounds and upper-bounds must be nonempty vectors: " lower-bounds upper-bounds))
	((not (vector-every exact-integer? lower-bounds))
	 (error "Interval: All lower-bounds must be exact integers: " lower-bounds))
	((not (vector-every exact-integer? upper-bounds))
	 (error "Interval: All upper-bounds must be exact integers: " upper-bounds))
	((not (vector-every (lambda (x y) (< x y)) lower-bounds upper-bounds))
	 (error "Interval: Each lower-bound must be less than the associated upper-bound: " lower-bounds upper-bounds))
	(else
	 (make-##Interval (vector-copy lower-bounds) (vector-copy upper-bounds)))))


(declare (inline))

(define (##Interval-dimension interval)
  (vector-length (##Interval-lower-bounds interval)))

(define (##Interval-lower-bound interval i)
  (vector-ref (##Interval-lower-bounds interval) i))

(define (##Interval-upper-bound interval i)
  (vector-ref (##Interval-upper-bounds interval) i))

(define (##make-list n v)
  (if (= n 0)
      '()
      (cons v (##make-list (- n 1) v))))

(define (##Interval-lower-bounds->vector interval)
  (##vector-copy (##Interval-lower-bounds interval)))

(define (##Interval-upper-bounds->vector interval)
  (##vector-copy (##Interval-upper-bounds interval)))

(define (##Interval-lower-bounds->list interval)
  (vector->list (##Interval-lower-bounds interval)))

(define (##Interval-upper-bounds->list interval)
  (vector->list (##Interval-upper-bounds interval)))

(declare (not inline))

(define (Interval-dimension interval)
  (cond ((not (Interval? interval))
	 (error "Interval-dimension: argument is not an interval: " interval))
	(else
	 (##Interval-dimension interval))))

(define (Interval-lower-bound interval i)
  (cond ((not (Interval? interval))
	 (error "Interval-lower-bound: argument is not an interval: " interval i))
	((not (##exact-integer? i))
	 (error "Interval-lower-bound: argument is not an exact integer: " interval i))
	((not (< -1 i (##Interval-dimension interval)))
	 (error "Interval-lower-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): " interval i))
	(else
	 (##Interval-lower-bound interval i))))

(define (Interval-upper-bound interval i)
  (cond ((not (Interval? interval))
	 (error "Interval-upper-bound: argument is not an interval: " interval i))
	((not (##exact-integer? i))
	 (error "Interval-upper-bound: argument is not an exact integer: " interval i))
	((not (< -1 i (##Interval-dimension interval)))
	 (error "Interval-upper-bound: index is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): " interval i))
	(else
	 (##Interval-upper-bound interval i))))

(define (Interval-lower-bounds->vector interval)
  (cond ((not (Interval? interval))
	 (error "Interval-lower-bounds->vector: argument is not an interval: " interval))
	(else
	 (##Interval-lower-bounds->vector interval))))

(define (Interval-upper-bounds->vector interval)
  (cond ((not (Interval? interval))
	 (error "Interval-upper-bounds->vector: argument is not an interval: " interval))
	(else
	 (##Interval-upper-bounds->vector interval))))

(define (Interval-lower-bounds->list interval)
  (cond ((not (Interval? interval))
	 (error "Interval-lower-bounds->list: argument is not an interval: " interval))
	(else
	 (##Interval-lower-bounds->list interval))))

(define (Interval-upper-bounds->list interval)
  (cond ((not (Interval? interval))
	 (error "Interval-upper-bounds->list: argument is not an interval: " interval))
	(else
	 (##Interval-upper-bounds->list interval))))

(define (Interval-curry interval left-dimension)
  (cond ((not (Interval? interval))
	 (error "Interval-curry: argument is not an interval: " interval left-dimension))
	((not (< 1 (##Interval-dimension interval)))  ;; redundant check, but useful error message
	 (error "Interval-curry: the dimension of the interval is not greater than 1: " interval left-dimension))
	((not (##exact-integer? left-dimension))
	 (error "Interval-curry: argument is not an exact integer: " interval left-dimension))
	((not (< 0 left-dimension (##Interval-dimension interval)))
	 (error "Interval-curry: argument is not between 0 and (Interval-dimension interval) (exclusive): " interval left-dimension))
	(else
	 (let ((n (##Interval-dimension interval)))
	   (let ((lower-bounds (##Interval-lower-bounds interval))
		 (upper-bounds (##Interval-upper-bounds interval))
		 (left-lower-bounds (make-vector left-dimension))
		 (left-upper-bounds (make-vector left-dimension))
		 (right-lower-bounds (make-vector (- n left-dimension)))
		 (right-upper-bounds (make-vector (- n left-dimension))))
	     (do ((i 0 (+ i 1)))
		 ((= i left-dimension)
		  (do ((i i (+ i 1)))
		      ((= i n)
		       (values (make-##Interval left-lower-bounds
						left-upper-bounds)
			       (make-##Interval right-lower-bounds
						right-upper-bounds)))
		    (vector-set! right-lower-bounds (- i left-dimension) (vector-ref lower-bounds i))
		    (vector-set! right-upper-bounds (- i left-dimension) (vector-ref upper-bounds i))))
	       (vector-set! left-lower-bounds i (vector-ref lower-bounds i))
	       (vector-set! left-upper-bounds i (vector-ref upper-bounds i))))))))

(define (Interval-distinguish-one-axis interval index)
  (cond ((not (Interval? interval))
	 (error "Interval-distinguish-one-axis: argument is not an interval: " interval index))
	((not (##exact-integer? index))
	 (error "Interval-distinguish-one-axis: argument is not an exact integer: " interval index))
	((not (< 1 (##Interval-dimension interval)))
	 (error "Interval-distinguish-one-axis: The dimension of the argument is not greater than one: " interval index))
	((not (< -1 index (##Interval-dimension interval)))
	 (error "Interval-distinguish-one-axis: argument is not between 0 (inclusive) and (Interval-dimension interval) (exclusive): " interval index))
	(else
	 (let ((n (##Interval-dimension interval)))
	   (let* ((lower-bounds (##Interval-lower-bounds interval))
		  (upper-bounds (##Interval-upper-bounds interval))
		  (left-lower-bounds (make-vector (- n 1)))
		  (left-upper-bounds (make-vector (- n 1))))
	     (let loop ((i 0)
			(j 0))
	       (cond ((= i n)
		      (values (make-##Interval left-lower-bounds
					       left-upper-bounds)
			      (make-##Interval (vector (vector-ref lower-bounds index))
					       (vector (vector-ref upper-bounds index)))))
		     ((= i index)
		      (loop (+ i 1)
			    j))
		     (else
		      (vector-set! left-lower-bounds j (vector-ref lower-bounds i))
		      (vector-set! left-upper-bounds j (vector-ref upper-bounds i))
		      (loop (+ i 1)
			    (+ j 1))))))))))

(define (Interval-dilate interval lower-diffs upper-diffs)
  (cond ((not (Interval? interval))
	 (error "Interval-dilate: first argument is not an interval: " interval lower-diffs upper-diffs))
	((not (vector? lower-diffs))
	 (error "Interval-dilate: second argument must be a vector: " interval lower-diffs upper-diffs))
	((not (vector? upper-diffs))
	 (error "Interval-dilate: third argument must be a vector: " interval lower-diffs upper-diffs))
	((not (= (vector-length lower-diffs) (vector-length upper-diffs)))
	 (error "Interval-dilate: The second and third arguments must have the same length: " interval lower-diffs upper-diffs))
	((not (= (##Interval-dimension interval) (vector-length lower-diffs)))
	 (error "Interval-dilate: The second and third arguments must have the same length as the dimension of the first argument: " interval lower-diffs upper-diffs))
	(else
	 (let* ((interval-lower-bounds (##Interval-lower-bounds interval))
		(interval-upper-bounds (##Interval-upper-bounds interval))
		(dilation-lower-bounds lower-diffs)
		(dilation-upper-bounds upper-diffs)
		(n (vector-length interval-lower-bounds))
		(new-lower-bounds (make-vector n))
		(new-upper-bounds (make-vector n)))
	   (do ((i 0 (+ i 1)))
	       ((= i n) (make-##Interval new-lower-bounds new-upper-bounds))
	     (vector-set! new-lower-bounds i (+ (vector-ref interval-lower-bounds i)
						(vector-ref dilation-lower-bounds i)))
	     (vector-set! new-upper-bounds i (+ (vector-ref interval-upper-bounds i)
						(vector-ref dilation-upper-bounds i)))
	     (if (not (< (vector-ref new-lower-bounds i)
			 (vector-ref new-upper-bounds i)))
		 (error "Interval-dilate: the resulting interval is empty: " interval lower-diffs upper-diffs)))))))

(define (##Interval-volume interval)
  (do ((i (- (##Interval-dimension interval) 1) (- i 1))
       (result 1 (let ()
		   (* result (- (##Interval-upper-bound interval i)
				(##Interval-lower-bound interval i))))))
      ((< i 0) result)))

(define (Interval-volume interval)
  (cond ((not (Interval? interval))
	 (error "Interval-volume: argument is not an interval: " interval))
	(else
	 (##Interval-volume interval))))

(define (##Interval= interval1 interval2)
  (and (equal? (##Interval-upper-bounds interval1)
	       (##Interval-upper-bounds interval2))
       (equal? (##Interval-lower-bounds interval1)
	       (##Interval-lower-bounds interval2))))

(define (Interval= interval1 interval2)
  (cond ((not (and (Interval? interval1)
		   (Interval? interval2)))
	 (error "Interval=: Not all arguments are intervals: " interval1 interval2))
	(else
	 (##Interval= interval1 interval2))))

(define (##Interval-subset? interval1 interval2)
  (and (= (##Interval-dimension interval1) (##Interval-dimension interval2))
       (let loop ((i (- (##Interval-dimension interval1) 1)))
	 (or (< i 0)
	     (and (>= (##Interval-lower-bound interval1 i)
		      (##Interval-lower-bound interval2 i))
		  (<= (##Interval-upper-bound interval1 i)
		      (##Interval-upper-bound interval2 i))
		  (loop (- i 1)))))))

(define (Interval-subset? interval1 interval2)
  (cond ((not (and (Interval? interval1)
		   (Interval? interval2)))
	 (error "Interval-subset?: Not all arguments are intervals: " interval1 interval2))
	(else
	 (##Interval-subset? interval1 interval2))))

(declare (inline))

(define (##Interval-contains-multi-index?-1 interval i)
  (and (<= (##Interval-lower-bound interval 0) i) (< i (##Interval-upper-bound interval 0))))

(define (##Interval-contains-multi-index?-2 interval i j)
  (and (<= (##Interval-lower-bound interval 0) i) (< i (##Interval-upper-bound interval 0))
       (<= (##Interval-lower-bound interval 1) j) (< j (##Interval-upper-bound interval 1))))

(define (##Interval-contains-multi-index?-3 interval i j k)
  (and (<= (##Interval-lower-bound interval 0) i) (< i (##Interval-upper-bound interval 0))
       (<= (##Interval-lower-bound interval 1) j) (< j (##Interval-upper-bound interval 1))
       (<= (##Interval-lower-bound interval 2) k) (< k (##Interval-upper-bound interval 2))))

(define (##Interval-contains-multi-index?-4 interval i j k l)
  (and (<= (##Interval-lower-bound interval 0) i) (< i (##Interval-upper-bound interval 0))
       (<= (##Interval-lower-bound interval 1) j) (< j (##Interval-upper-bound interval 1))
       (<= (##Interval-lower-bound interval 2) k) (< k (##Interval-upper-bound interval 2))
       (<= (##Interval-lower-bound interval 3) l) (< l (##Interval-upper-bound interval 3))))

(declare (not inline))

(define (##Interval-contains-multi-index? interval i #!optional j k l #!rest tail)
  (cond ((not j)
	 (##Interval-contains-multi-index?-1 interval i))
	((not k)
	 (##Interval-contains-multi-index?-2 interval i j))
	((not l)
	 (##Interval-contains-multi-index?-3 interval i j k))
	((null? tail)
	 (##Interval-contains-multi-index?-4 interval i j k l))
	(else
	 (let loop ((i 0)
		    (multi-index `(i j k l ,@tail)))
	   (or (null? multi-index)
	       (let ((component (car multi-index)))
		 (and (<= (##Interval-lower-bound interval i) component)
		      (< component (##Interval-upper-bound interval i))
		      (loop (+ i 1)
			    (cdr multi-index)))))))))

(define (Interval-contains-multi-index? interval i #!rest multi-index-tail)

  ;; this is relatively slow, but (a) I haven't seen a need to use it yet, and (b) this formulation
  ;; significantly simplifies testing the error checking

  (cond ((not (Interval? interval))
	 (error "Interval-contains-multi-index?: argument is not an Interval: " interval))
	(else
	 (let ((multi-index (cons i multi-index-tail)))
	   (cond ((not (= (##Interval-dimension interval) (length multi-index)))
		  (apply error "Interval-contains-multi-index?: dimension of interval does not match number of arguments: " interval multi-index))
		 ((not (every exact-integer? multi-index))
		  (apply error "Interval-contains-multi-index?: at least one multi-index component is not an exact integer: " interval multi-index))
		 (else
		  (let loop ((i 0)
			     (multi-index multi-index))
		    (or (null? multi-index)
			(let ((component (car multi-index)))
			  (and (<= (##Interval-lower-bound interval i) component)
			       (< component (##Interval-upper-bound interval i))
			       (loop (+ i 1)
				     (cdr multi-index))))))))))))

;;; Applies f to every element of the domain; assumes that f is thread-safe,
;;; the order of application is not specified

(define (Interval-for-each f interval)
  (cond ((not (Interval? interval))
	 (error "Interval-for-each: Argument is not a interval: " interval))
	((not (procedure? f))
	 (error "Interval-for-each: Argument is not a procedure: " f))
	(else
	 (##Interval-for-each f interval))))

(define (##Interval-for-each f interval)
  (##Interval-for-each-serial f interval))

;;; Applies f to every element of the domain, in lexicographical order

(define (Interval-for-each-serial f interval)
  (cond ((not (Interval? interval))
	 (error "Interval-for-each-serial: Argument is not a interval: " interval))
	((not (procedure? f))
	 (error "Interval-for-each-serial: Argument is not a procedure: " f))
	(else
	 (##Interval-for-each-serial f interval))))

(define (##Interval-for-each-serial f interval)
  (case (##Interval-dimension interval)
    ((1) (let ((lower-i (##Interval-lower-bound interval 0))
	       (upper-i (##Interval-upper-bound interval 0)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (begin
		   (f i)
		   (i-loop (+ i 1)))))))
    ((2) (let ((lower-i (##Interval-lower-bound interval 0))
	       (lower-j (##Interval-lower-bound interval 1))
	       (upper-i (##Interval-upper-bound interval 0))
	       (upper-j (##Interval-upper-bound interval 1)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (let j-loop ((j lower-j))
		   (if (< j upper-j)
		       (begin
			 (f i j)
			 (j-loop (+ j 1)))
		       (i-loop (+ i 1))))))))
    ((3) (let ((lower-i (##Interval-lower-bound interval 0))
	       (lower-j (##Interval-lower-bound interval 1))
	       (lower-k (##Interval-lower-bound interval 2))
	       (upper-i (##Interval-upper-bound interval 0))
	       (upper-j (##Interval-upper-bound interval 1))
	       (upper-k (##Interval-upper-bound interval 2)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (let j-loop ((j lower-j))
		   (if (< j upper-j)
		       (let k-loop ((k lower-k))
			 (if (< k upper-k)
			     (begin
			       (f i j k)
			       (k-loop (+ k 1)))
			     (j-loop (+ j 1))))
		       (i-loop (+ i 1))))))))
    ((4) (let ((lower-i (##Interval-lower-bound interval 0))
	       (lower-j (##Interval-lower-bound interval 1))
	       (lower-k (##Interval-lower-bound interval 2))
	       (lower-l (##Interval-lower-bound interval 3))
	       (upper-i (##Interval-upper-bound interval 0))
	       (upper-j (##Interval-upper-bound interval 1))
	       (upper-k (##Interval-upper-bound interval 2))
	       (upper-l (##Interval-upper-bound interval 3)))
	   (let i-loop ((i lower-i))
	     (if (< i upper-i)
		 (let j-loop ((j lower-j))
		   (if (< j upper-j)
		       (let k-loop ((k lower-k))
			 (if (< k upper-k)
			     (let l-loop ((l lower-l))
			       (if (< l upper-l)
				   (begin
				     (f i j k l)
				     (l-loop (+ l 1)))
				   (k-loop (+ k 1))))
			     (j-loop (+ j 1))))
		       (i-loop (+ i 1))))))))
    (else

     (let* ((lower-bounds (##Interval-lower-bounds->list interval))
	    (upper-bounds (##Interval-upper-bounds->list interval))
	    (arg          (map values lower-bounds)))                ; copy lower-bounds
       
       ;; I'm not particularly happy with set! here because f might capture the continuation
       ;; and then funny things might pursue ...
       ;; But it seems that the only way to have this work efficiently without the set
       ;; is to have arrays with fortran-style numbering.
       ;; blah

       (define (iterate lower-bounds-tail
			upper-bounds-tail
			arg-tail)
	 (let ((lower-bound (car lower-bounds-tail))
	       (upper-bound (car upper-bounds-tail)))
	   (if (null? (cdr arg-tail))
	       (let loop ((i lower-bound))
		 (if (< i upper-bound)
		     (begin
		       (set-car! arg-tail i)
		       (apply f arg)
		       (loop (+ i 1)))))
	       (let loop ((i lower-bound))
		 (if (< i upper-bound)
		     (begin
		       (set-car! arg-tail i)
		       (iterate (cdr lower-bounds-tail)
				(cdr upper-bounds-tail)
				(cdr arg-tail))
		       (loop (+ i 1))))))))

       (iterate lower-bounds
		upper-bounds
		arg)))))

;;; Calculates
;;;
;;; (...(operator (operator (operator identity (f multi-index_1)) (f multi-index_2)) (f multi-index_3)) ...)
;;;
;;; where multi-index_1, multi-index_2, ... are the elements of interval in lexicographical order
;;; This version assumes, and may use, that f is thread-safe and that operator is associative.
;;; The order of application of f and operator is not specified.

(define (Interval-reduce f operator identity interval)
  (cond ((not (Interval? interval))
	 (error "Interval-reduce: Argument is not a interval: " interval))
	((not (procedure? f))
	 (error "Interval-reduce: Argument is not a procedure: " f))
	((not (procedure? operator))
	 (error "Interval-reduce: Operator is not a procedure: " operator))
	(else
	 (##Interval-reduce f operator identity interval))))

;;; This version applies f to the elements of interval in lexicographical order and applies
;;; operator in the order given

(define (Interval-reduce-serial f operator identity interval)
  (cond ((not (Interval? interval))
	 (error "Interval-reduce-serial: Argument is not a interval: " interval))
	((not (procedure? f))
	 (error "Interval-reduce-serial: Argument is not a procedure: " f))
	((not (procedure? operator))
	 (error "Interval-reduce-serial: Operator is not a procedure: " operator))
	(else
	 (##Interval-reduce-serial f operator identity interval))))

(define (##Interval-reduce f operator identity interval)
  (##Interval-reduce-serial f operator identity interval))

(define (##Interval-reduce-serial f operator identity interval)
  (case (##Interval-dimension interval)
    ((1) (let ((lower-i (##Interval-lower-bound interval 0))
	       (upper-i (##Interval-upper-bound interval 0)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (i-loop (+ i 1) (operator result (f i)))))))
    ((2) (let ((lower-i (##Interval-lower-bound interval 0))
	       (lower-j (##Interval-lower-bound interval 1))
	       (upper-i (##Interval-upper-bound interval 0))
	       (upper-j (##Interval-upper-bound interval 1)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (let j-loop ((j lower-j) (result result))
		   (if (= j upper-j)
		       (i-loop (+ i 1) result)
		       (j-loop (+ j 1) (operator result (f i j)))))))))
    ((3) (let ((lower-i (##Interval-lower-bound interval 0))
	       (lower-j (##Interval-lower-bound interval 1))
	       (lower-k (##Interval-lower-bound interval 2))
	       (upper-i (##Interval-upper-bound interval 0))
	       (upper-j (##Interval-upper-bound interval 1))
	       (upper-k (##Interval-upper-bound interval 2)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (let j-loop ((j lower-j) (result result))
		   (if (= j upper-j)
		       (i-loop (+ i 1) result)
		       (let k-loop ((k lower-k) (result result))
			 (if (= k upper-k)
			     (j-loop (+ j 1) result)
			     (k-loop (+ k 1) (operator result (f i j k)))))))))))
    ((4) (let ((lower-i (##Interval-lower-bound interval 0))
	       (lower-j (##Interval-lower-bound interval 1))
	       (lower-k (##Interval-lower-bound interval 2))
	       (lower-l (##Interval-lower-bound interval 3))
	       (upper-i (##Interval-upper-bound interval 0))
	       (upper-j (##Interval-upper-bound interval 1))
	       (upper-k (##Interval-upper-bound interval 2))
	       (upper-l (##Interval-upper-bound interval 3)))
	   (let i-loop ((i lower-i) (result identity))
	     (if (= i upper-i)
		 result
		 (let j-loop ((j lower-j) (result result))
		   (if (= j upper-j)
		       (i-loop (+ i 1) result)
		       (let k-loop ((k lower-k) (result result))
			 (if (= k upper-k)
			     (j-loop (+ j 1) result)
			     (let l-loop ((l lower-l) (result result))
			       (if (= l upper-l)
				   (k-loop (+ k 1) result)
				   (l-loop (+ l 1) (operator result (f i j k l)))))))))))))
    (else
     (let* ((lower-bounds (##Interval-lower-bounds->list interval))
	    (upper-bounds (##Interval-upper-bounds->list interval))
	    (arg          (map values lower-bounds)))                ; copy lower-bounds
       
       ;; I'm not particularly happy with set! here because f or operator might capture
       ;; the continuation and then funny things might pursue ...
       ;; But it seems that the only way to have this work efficiently without the set~
       ;; is to have arrays with fortran-style numbering.
       ;; blah
       
       (define (iterate lower-bounds-tail
			upper-bounds-tail
			arg-tail
			result)
	 (let ((lower-bound (car lower-bounds-tail))
	       (upper-bound (car upper-bounds-tail)))
	   (if (null? (cdr arg-tail))
	       (let loop ((i lower-bound)
			  (result result))
		 (if (= i upper-bound)
		     result
		     (begin
		       (set-car! arg-tail i)
		       (loop (+ i 1)
			     (operator result (apply f arg))))))
	       (let loop ((i lower-bound)
			  (result result))
		 (if (= i upper-bound)
		     result
		     (begin
		       (set-car! arg-tail i)
		       (loop (+ i 1)
			     (iterate (cdr lower-bounds-tail)
				      (cdr upper-bounds-tail)
				      (cdr arg-tail)
				      result))))))))

       (iterate lower-bounds
		upper-bounds
		arg
		identity)))))

;; We'll use the same basic container for all types of arrays.

(declare (inline))

(define-structure ##Array-base
  ;; Part of all arrays
  domain                  ;; an Interval
  getter                  ;; (lambda (i_0 ... i_n-1) ...) returns a value for (i_0,...,i_n-1) in (Array-domain a)
  ;; Part of mutable arrays
  setter                  ;; (lambda (v i_0 ... i_n-1) ...) sets a value for (i_0,...,i_n-1) in (Array-domain a)
  ;; Part of Fixed arrays
  manipulators            ;; a Fixed-array-manipulator
  body                    ;; the backing store for this array
  indexer                 ;; see below
  safe?                   ;; do we check whether bounds (in getters and setters) and values (in setters) are valid
  )

(define Fixed-array-default-safe? #f)

(set! Fixed-array-default-safe? Fixed-array-default-safe?)

(declare (not inline))

;; An array has a domain (which is an interval) and an getter that maps that domain into some type of
;; Scheme objects

(define (Array domain getter)
  (cond ((not (Interval? domain))
	 (error "Array: domain is not an interval: " domain))
	((not (procedure? getter))
	 (error "Array: getter is not a procedure: " getter))
	(else
	 (make-##Array-base domain
			    getter
			    #f        ; setter
			    #f        ; manipulators
			    #f        ; body
			    #f        ; indexer
			    #f        ; safe?
			    ))))

(define (Array? x)
  (##Array-base? x))

(define (Array-domain obj)
  (cond ((not (Array? obj))
	 (error "Array-domain: object is not an array: " obj))
	(else
	 (##Array-base-domain obj))))

(define (Array-getter obj)
  (cond ((not (Array? obj))
	 (error "Array-getter: object is not an array: " obj))
	(else
	 (##Array-base-getter obj))))

#| Never used, and easy to write.  removed for now.

(define (Array->list array)
  (cond ((not (Array? array))
	 (error "Array->list: object is not an array: " array))
	(else
	 (reverse (Array-reduce (lambda (result a_i)
				  (cons a_i result))
				'()
				array)))))

|#

(define (Array-extract array new-domain)
  (cond ((not (Array? array))
	 (error "Array-extract: first argument is not an Array: " array new-domain))
	((not (Interval? new-domain))
	 (error "Array-extract: second argument is not an Interval: " array new-domain))
	((not (##Interval-subset? new-domain (Array-domain array)))
	 (error "Array-extract: the second argument is not a subset of the domain of the first argument: " array new-domain))
	(else
	 (Array new-domain
		(Array-getter array)))))

#|

A Mutable array has, in addition a setter, that satisfies, roughly

If (i_1, ..., i_n)\neq (j_1, ..., j_n) \in (Array-domain a)

and

((Array-getter a) j_1 ... j_n) => x

then "after"

((Array-setter a) v i_1 ... i_n)

we have

((Array-getter a) j_1 ... j_n) => x

and

((Array-getter a) i_1 ... i_n) => v

|#

(define (Mutable-array domain getter setter)
  (cond ((not (Interval? domain))
	 (error "Mutable-array: domain is not an interval: " domain))
	((not (procedure? getter))
	 (error "Mutable-array: getter is not a procedure: " getter))
	((not (procedure? setter))
	 (error "Mutable-array: setter is not a procedure: " setter))
	(else
	 (make-##Array-base domain
			    getter
			    setter
			    #f        ; manipulators
			    #f        ; body
			    #f        ; indexer
			    #f        ; safe?
			    ))))

(define (Mutable-array? obj)
  (and (Array? obj)
       (not (eq? (##Array-base-setter obj) #f))))

(define (Array-setter obj)
  (cond ((not (Mutable-array? obj))
	 (error "Array-setter: object is not an mutable array: " obj))
	(else
	 (##Array-base-setter obj))))

#|

Array-manipulators contains functions and objects to manipulate the
backing store of a Fixed-array.

getter:   (lambda (body i) ...)   returns the value of body at index i
setter:   (lambda (body i v) ...) sets the value of body at index i to v
checker:  (lambda (val) ...)      checks that val is an appropriate value for storing in (maker n)
maker:    (lambda (n) ...)        makes a body of length n
length:   (lambda (body) ...)     returns the number of objects in body
default:  object                  is the default value with which to fill body

|#

(define-structure Array-manipulators getter setter checker maker length default)

#|
We define specialized array manipulators for:

32- and 64-bit floating-point numbers,
8-, 16-, 32-, and 64-bit signed integers,
8-, 16-, 32-, and 64-bit unsigned integers, and
1-bit unsigned integers

as well as generic objects.
|#

(define-macro (make-array-manipulators)
  
  (define (symbol-concatenate . symbols)
    (string->symbol (apply string-append (map (lambda (s)
						(if (string? s)
						    s
						    (symbol->string s)))
					      symbols))))
  
  `(begin
     ,@(map (lambda (name prefix default checker)
	      `(define ,(symbol-concatenate name '-array-manipulators)
		 (make-Array-manipulators
		  ;; getter:
		  (lambda (v i)
		    (,(symbol-concatenate prefix 'vector-ref) v i))
		  ;; setter:
		  (lambda (v i val)
		    (,(symbol-concatenate prefix 'vector-set!) v i val))
		  ;; checker
		  ,checker
		  ;; maker:
		  ,(symbol-concatenate 'make- prefix 'vector)
		  ;; length:
		  ,(symbol-concatenate prefix 'vector-length)
		  ;; default:
		  ,default)))
	    '(generic s8 u8 s16 u16 s32 u32 s64 u64 f32 f64)
	    '(""      s8 u8 s16 u16 s32 u32 s64 u64 f32 f64)
	    '(#f       0  0   0   0   0   0   0   0 0.0 0.0)
	    `((lambda (x) #t)                             ; generic
	      (lambda (x)                                 ; s8
		(and (##fixnum? x)
		     (fx<= ,(- (expt 2 7))
			   x
			   ,(- (expt 2 7) 1))))
	      (lambda (x)                                ; u8
		(and (##fixnum? x)
		     (fx<= 0
			   x
			   ,(- (expt 2 8) 1))))
	      (lambda (x)                               ; s16
		(and (##fixnum? x)
		     (fx<= ,(- (expt 2 15))
			   x
			   ,(- (expt 2 15) 1))))
	      (lambda (x)                               ; u16
		(and (##fixnum? x)
		     (fx<= 0
			   x
			   ,(- (expt 2 16) 1))))
	      (lambda (x)                               ; s32
		(declare (generic))
		(and (##exact-integer? x)
		     (<= ,(- (expt 2 31))
			 x
			 ,(- (expt 2 31) 1))))
	      (lambda (x)                               ; u32
		(declare (generic))
		(and (##exact-integer? x)
		     (<= 0
			 x
			 ,(- (expt 2 32) 1))))
	      (lambda (x)                              ; s64
		(declare (generic))
		(and (##exact-integer? x)
		     (<= ,(- (expt 2 63))
			 x
			 ,(- (expt 2 63) 1))))
	      (lambda (x)                              ; u64
		(declare (generic))
		(and (##exact-integer? x)
		     (<= 0
			 x
			 ,(- (expt 2 64) 1))))
	      (lambda (x) (##flonum? x))               ; f32
	      (lambda (x) (##flonum? x))               ; f64
	      ))))

(make-array-manipulators)

;;; for bit-arrays, body is a vector, the first element of which is the actual number of elements,
;;; the second element of which is a u16vector that contains the bit string

(define u1-array-manipulators
  (make-Array-manipulators
   ;; getter:
   (lambda (v i)
     (let ((index (##fixnum.arithmetic-shift-right i 4))
	   (shift (##fixnum.bitwise-and i 15))
	   (bodyv (vector-ref v  1)))
       (##fixnum.bitwise-and
	(##fixnum.arithmetic-shift-right
	 (u16vector-ref bodyv index)
	 shift)
	1)))
   ;; setter: 
   (lambda (v i val)
     (let ((index (##fixnum.arithmetic-shift-right i 4))
	   (shift (##fixnum.bitwise-and i 15))
	   (bodyv (vector-ref v  1)))
       (u16vector-set! bodyv index (##fixnum.bitwise-ior
				    (##fixnum.arithmetic-shift-left val shift)
				    (##fixnum.bitwise-and
				     (u16vector-ref bodyv index)
				     (##fixnum.bitwise-not
				      (##fixnum.arithmetic-shift-left 1 shift)))))))
   ;; checker
   (lambda (val)
     (and (##fixnum? val)
	  (eq? 0 (##fixnum.bitwise-and -2 val))))
   ;; maker:
   (lambda (size initializer)
     (let ((u16-size (##fixnum.arithmetic-shift-right (+ size 15) 4)))
       (vector size (make-u16vector u16-size (if (zero? initializer) 0 65535)))))
   ;; length:
   (lambda (v)
     (vector-ref v 0))
   ;; default:
   0))

#|

Conceptually, an indexer is itself a 1-1 Array that maps one interval to another; thus, it is
an example of an array that can return multiple values.

Rather than trying to formalize this idea, and trying to get it to work with Array-map,
Array-reduce, ..., we'll just manipulate the getter functions of these conceptual Arrays.

Indexers are 1-1 affine maps from one interval to another.

The indexer field of a Fixed-array obj is a 1-1 mapping from

(Array-domain obj)

to [0, top), where top is 

((Array-manipulators-length (Array-manipulators obj)) (Array-body obj))

|#

#|

compute-indexer specializes

(define (my-indexer base lower-bounds increments)
  (lambda multi-index
    (apply + base (map * increments (map - multi-index lower-bounds)))))

|#

(define (compute-indexer base lower-bounds increments)
  (case (length lower-bounds)
    ((1)  (indexer-1 base
		     (car lower-bounds)
		     (car increments)))
    ((2)  (indexer-2 base
		     (car lower-bounds) (cadr lower-bounds)
		     (car increments)   (cadr increments)))
    ((3)  (indexer-3 base
		     (car lower-bounds) (cadr lower-bounds) (caddr lower-bounds)
		     (car increments)   (cadr increments)   (caddr increments)))
    ((4)  (indexer-4 base
		     (car lower-bounds) (cadr lower-bounds) (caddr lower-bounds) (cadddr lower-bounds)
		     (car increments)   (cadr increments)   (caddr increments)   (cadddr increments)))
    (else (indexer-generic base lower-bounds increments))))

;; unfortunately, the next two functions were written by hand, so beware of bugs.

(define (indexer-1 base
		   low-0
		   increment-0)
  (if (zero? base)
      (if (zero? low-0)
	  (cond ((= 1 increment-0)    (lambda (i) i))
		((= -1 increment-0)   (lambda (i) (- i)))
		(else                 (lambda (i) (* i increment-0))))
	  (cond ((= 1 increment-0)    (lambda (i) (- i low-0)))
		((= -1 increment-0)   (lambda (i) (- low-0 i)))
		(else                 (lambda (i) (* increment-0 (- i low-0))))))
      (if (zero? low-0)
	  (cond ((= 1 increment-0)    (lambda (i) (+ base i)))
		((= -1 increment-0)   (lambda (i) (- base i)))
		(else                 (lambda (i) (+ base (* increment-0 i)))))
	  (cond ((= 1 increment-0)    (lambda (i) (+ base (- i low-0))))
		((= -1 increment-0)   (lambda (i) (+ base (- low-0 i))))
		(else                 (lambda (i) (+ base (* increment-0 (- i low-0)))))))))

(define (indexer-2 base
		   low-0       low-1
		   increment-0 increment-1)
  (if (zero? base)
      (if (zero? low-0)
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ i j)))
			   ((= -1 increment-1)   (lambda (i j) (+ i (- j))))
			   (else                 (lambda (i j) (+ i (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ i (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ i (- low-1 j))))
			   (else                 (lambda (i j) (+ i (* increment-1 (- j low-1))))))))
		((= -1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- j                           i)))
			   ((= -1 increment-1)  (lambda (i j) (- (- j)                       i)))
			   (else                (lambda (i j) (- (* increment-1 j)           i))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (- j low-1)                 i)))
			   ((= -1 increment-1)  (lambda (i j) (- (- low-1 j)                 i)))
			   (else                (lambda (i j) (- (* increment-1 (- j low-1)) i))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 i) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 i) (- j))))
			   (else                (lambda (i j) (+ (* increment-0 i) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 i) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 i) (- low-1 j))))
			   (else                (lambda (i j) (+ (* increment-0 i) (* increment-1 (- j low-1)))))))))
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ (- i low-0) j)))
			   ((= -1 increment-1)   (lambda (i j) (+ (- i low-0) (- j))))
			   (else                 (lambda (i j) (+ (- i low-0) (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ (- i low-0) (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ (- i low-0) (- low-1 j))))
			   (else                 (lambda (i j) (+ (- i low-0) (* increment-1 (- j low-1))))))))
		((= -1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- j                           (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (- j)                       (- i low-0))))
			   (else                (lambda (i j) (- (* increment-1 j)           (- i low-0)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (- j low-1)                 (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (- low-1 j)                 (- i low-0))))
			   (else                (lambda (i j) (- (* increment-1 (- j low-1)) (- i low-0)))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 (- i low-0)) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 (- i low-0)) (- j))))
			   (else                (lambda (i j) (+ (* increment-0 (- i low-0)) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ (* increment-0 (- i low-0)) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ (* increment-0 (- i low-0)) (- low-1 j))))
			   (else                (lambda (i j) (+ (* increment-0 (- i low-0)) (* increment-1 (- j low-1))))))))))
      (if (zero? low-0)
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base i j)))
			   ((= -1 increment-1)   (lambda (i j) (+ base i (- j))))
			   (else                 (lambda (i j) (+ base i (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base i (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ base i (- low-1 j))))
			   (else                 (lambda (i j) (+ base i (* increment-1 (- j low-1))))))))
		((= -1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base j)                           i)))
			   ((= -1 increment-1)  (lambda (i j) (- (- base j)                           i)))
			   (else                (lambda (i j) (- (+ base (* increment-1 j))           i))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base (- j low-1))                 i)))
			   ((= -1 increment-1)  (lambda (i j) (- (+ base (- low-1 j))                 i)))
			   (else                (lambda (i j) (- (+ base (* increment-1 (- j low-1))) i))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 i) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 i) (- j))))
			   (else                (lambda (i j) (+ base (* increment-0 i) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 i) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 i) (- low-1 j))))
			   (else                (lambda (i j) (+ base (* increment-0 i) (* increment-1 (- j low-1)))))))))
	  (cond ((= 1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base (- i low-0) j)))
			   ((= -1 increment-1)   (lambda (i j) (+ base (- i low-0) (- j))))
			   (else                 (lambda (i j) (+ base (- i low-0) (* increment-1 j)))))
		     (cond ((= 1 increment-1)    (lambda (i j) (+ base (- i low-0) (- j low-1))))
			   ((= -1 increment-1)   (lambda (i j) (+ base (- i low-0) (- low-1 j))))
			   (else                 (lambda (i j) (+ base (- i low-0) (* increment-1 (- j low-1))))))))
		((= -1 increment-0)
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base j)                           (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (- base j)                           (- i low-0))))
			   (else                (lambda (i j) (- (+ base (* increment-1 j))           (- i low-0)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (- (+ base (- j low-1))                 (- i low-0))))
			   ((= -1 increment-1)  (lambda (i j) (- (+ base (- low-1 j))                 (- i low-0))))
			   (else                (lambda (i j) (- (+ base (* increment-1 (- j low-1))) (- i low-0)))))))
		(else
		 (if (zero? low-1)
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 (- i low-0)) j)))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 (- i low-0)) (- j))))
			   (else                (lambda (i j) (+ base (* increment-0 (- i low-0)) (* increment-1 j)))))
		     (cond ((= 1 increment-1)   (lambda (i j) (+ base (* increment-0 (- i low-0)) (- j low-1))))
			   ((= -1 increment-1)  (lambda (i j) (+ base (* increment-0 (- i low-0)) (- low-1 j))))
			   (else                (lambda (i j) (+ base (* increment-0 (- i low-0)) (* increment-1 (- j low-1))))))))))))

;;; after this we basically punt

(define (indexer-3 base
		   low-0       low-1       low-2
		   increment-0 increment-1 increment-2)
  (if (= 0 low-0 low-1 low-2)
      (if (= base 0)
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   k))
	      (lambda (i j k)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k))))
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   k))
	      (lambda (i j k)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)))))
      (if (= base 0)
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (- k low-2)))
	      (lambda (i j k)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2)))))
	  (if (= increment-2 1)
	      (lambda (i j k)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (- k low-2)))
	      (lambda (i j k)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))))))))

(define (indexer-4 base
		   low-0       low-1       low-2       low-3
		   increment-0 increment-1 increment-2 increment-3)
  (if (= 0 low-0 low-1 low-2 low-3)
      (if (= base 0)
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   l))
	      (lambda (i j k l)
		(+ (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   (* increment-3 l))))
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   l))
	      (lambda (i j k l)
		(+ base
		   (* increment-0 i)
		   (* increment-1 j)
		   (* increment-2 k)
		   (* increment-3 l)))))
      (if (= base 0)
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (- l low-3)))
	      (lambda (i j k l)
		(+ (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (* increment-3 (- l low-3)))))
	  (if (= increment-3 1)
	      (lambda (i j k l)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (- l low-3)))
	      (lambda (i j k l)
		(+ base
		   (* increment-0 (- i low-0))
		   (* increment-1 (- j low-1))
		   (* increment-2 (- k low-2))
		   (* increment-3 (- l low-3))))))))

(define (indexer-generic base lower-bounds increments)
  (lambda multi-index
    (do ((multi-index  multi-index  (cdr multi-index))
	 (lower-bounds lower-bounds (cdr lower-bounds))
	 (increments   increments   (cdr increments))
	 (result       base         (+ result (* (car increments)
						 (- (car multi-index)
						    (car lower-bounds))))))
	((null? multi-index) result))))


(define (indexer= indexer1 indexer2 interval)
  (cond ((not (Interval? interval))
	 (error "indexer=: argument is not an interval " interval))
	((not (procedure? indexer1))
	 (error "indexer=: argument is not a procedure " indexer1))
	((not (procedure? indexer2))
	 (error "indexer=: argument is not a procedure " indexer2))
	(else
	 (let ((lower-bounds (##Interval-lower-bounds->list interval))
	       (upper-bounds (##Interval-upper-bounds->list interval)))
	   (let ((multi-index (map (lambda (x) x) lower-bounds)))
	     (let outer ((i multi-index)
			 (l lower-bounds)
			 (u upper-bounds))
	       (and (= (apply indexer1 multi-index)
		       (apply indexer2 multi-index))
		    (or (null? i)
			(begin
			  (if (< (+ 1 (car l)) (car u))
			      (set-car! i (+ (car i) 1)))
			  (outer (cdr i) (cdr l) (cdr u)))))))))))

#|

The default getter and the setter of a Fixed-array a are given by

(lambda (i_0 ... i_n-1)
  ((Array-manipulators-getter a)
   (Array-body a)
   ((Array-indexer a) i_0 ... i_n-1)))

(lambda (v i_0 ... i_n-1)
  ((Array-manipulators-setter a)
   (Array-body a)
   ((Array-indexer a) i_0 ... i_n-1)
   v))

The default initializer-value is 

(Array-manipulators-default manipulators)

The default body is

((Array-manipulators-maker manipulators)
 (Interval-volume domain)
 initializer-value)

The default indexer is the mapping of
the domain to the natural numbers in lexicographical order.

|#

(define (Fixed-array? obj)
  (and (Mutable-array? obj)
       (not (eq? (##Array-base-body obj) #f))))

(define (Array-body obj)
  (cond ((not (Fixed-array? obj))
	 (error "Array-body: argument is not a fixed array: " obj))
	(else
	 (##Array-base-body obj))))

(define (Array-indexer obj)
  (cond ((not (Fixed-array? obj))
	 (error "Array-indexer: argument is not a fixed array: " obj))
	(else
	 (##Array-base-indexer obj))))

(define (Array-manipulators obj)
  (cond ((not (Fixed-array? obj))
	 (error "Array-manipulators: argument is not a fixed array: " obj))
	(else
	 (##Array-base-manipulators obj))))

(define (Array-safe? obj)
  (cond ((not (Fixed-array? obj))
	 (error "Array-safe?: argument is not a fixed array: " obj))
	(else
	 (##Array-base-safe? obj))))

(define (##finish-Fixed-array domain manipulators body indexer safe?)
  (let ((manipulator-getter (Array-manipulators-getter manipulators))
	(manipulator-setter (Array-manipulators-setter manipulators))
	(checker (Array-manipulators-checker manipulators))
	(indexer indexer)
	(body body))

    ;;; we write the following three macros to specialize the setters and getters in the
    ;;; non-safe case to reduce one more function call.

    (define-macro (expand-manipulators original-suffix replacement-suffix expr)
      
      (define (symbol-append . args)
	(string->symbol (apply string-append (map (lambda (x) (if (symbol? x) (symbol->string x) x)) args))))
      
      (define (replace old-symbol new-symbol expr)
	(let loop ((expr expr))
	  (cond ((pair? expr)
		 (map loop expr))
		((eq? expr old-symbol)
		 new-symbol)
		(else
		 expr))))
      
      `(cond ,@(map (lambda (name prefix)
		      `((eq? manipulators ,(symbol-append name '-array-manipulators))
			,(replace (symbol-append 'manipulator original-suffix)
				  (symbol-append prefix 'vector replacement-suffix)
				  expr)))
		    '(generic s8 u8 s16 u16 s32 u32 s64 u64 f32 f64)
		    '(""      s8 u8 s16 u16 s32 u32 s64 u64 f32 f64))
	     (else
	      ,expr)))
    
    (define-macro (expand-getters expr)
      `(expand-manipulators -getter -ref ,expr))
    
    (define-macro (expand-setters expr)
      `(expand-manipulators -setter -set! ,expr))
    
    (let ((getter (if safe?
		      (case (##Interval-dimension domain)
			((1)  (lambda (i)
				(cond ((not (##exact-integer? i))
				       (error "Array-getter: multi-index component is not an exact integer: " i))
				      ((not (##Interval-contains-multi-index?-1 domain i))
				       (error "Array-getter: domain does not contain multi-index: "    domain i))
				      (else
				       (manipulator-getter body (indexer i))))))
			((2)  (lambda (i j)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)))
				       (error "Array-getter: multi-index component is not an exact integer: " i j))
				      ((not (##Interval-contains-multi-index?-2 domain i j))
				       (error "Array-getter: domain does not contain multi-index: "    domain i j))
				      (else
				       (manipulator-getter body (indexer i j))))))
			((3)  (lambda (i j k)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)))
				       (error "Array-getter: multi-index component is not an exact integer: " i j k))
				      ((not (##Interval-contains-multi-index?-3 domain i j k))
				       (error "Array-getter: domain does not contain multi-index: "    domain i j k))
				      (else
				       (manipulator-getter body (indexer i j k))))))
			((4)  (lambda (i j k l)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)
						 (##exact-integer? l)))
				       (error "Array-getter: multi-index component is not an exact integer: " i j k l))
				      ((not (##Interval-contains-multi-index?-4 domain i j k l))
				       (error "Array-getter: domain does not contain multi-index: "    domain i j k l))
				      (else
				       (manipulator-getter body (indexer i j k l))))))
			(else (lambda multi-index
				(cond ((not (every exact-integer? multi-index))
				       (apply error "Array-getter: multi-index component is not an exact integer: " multi-index))
				      ((not (= (##Interval-dimension domain) (length multi-index)))
				       (apply error "Array-getter: multi-index is not the correct dimension: " domain multi-index))
				      ((not (apply Interval-contains-multi-index? domain multi-index))
				       (apply error "Array-getter: domain does not contain multi-index: "    domain multi-index))
				      (else
				       (manipulator-getter body (apply indexer multi-index)))))))
		      (case (##Interval-dimension domain)
			((1)  (expand-getters (lambda (i)         (manipulator-getter body (indexer i)))))
			((2)  (expand-getters (lambda (i j)       (manipulator-getter body (indexer i j)))))
			((3)  (expand-getters (lambda (i j k)     (manipulator-getter body (indexer i j k)))))
			((4)  (expand-getters (lambda (i j k l)   (manipulator-getter body (indexer i j k l)))))
			(else (lambda multi-index (manipulator-getter body (apply indexer multi-index)))))))
	  (setter (if safe?
		      (case (##Interval-dimension domain)
			((1)  (lambda (value i)
				(cond ((not (##exact-integer? i))
				       (error "Array-setter: multi-index component is not an exact integer: " i))
				      ((not (##Interval-contains-multi-index?-1 domain i))
				       (error "Array-setter: domain does not contain multi-index: "    domain i))
				      ((not (checker value))
				       (error "Array-setter: value cannot be stored in body: " value))
				      (else
				       (manipulator-setter body (indexer i) value)))))
			((2)  (lambda (value i j)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)))
				       (error "Array-setter: multi-index component is not an exact integer: " i j))
				      ((not (##Interval-contains-multi-index?-2 domain i j))
				       (error "Array-setter: domain does not contain multi-index: "    domain i j))
				      ((not (checker value))
				       (error "Array-setter: value cannot be stored in body: " value))
				      (else
				       (manipulator-setter body (indexer i j) value)))))
			((3)  (lambda (value i j k)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)))
				       (error "Array-setter: multi-index component is not an exact integer: " i j k))
				      ((not (##Interval-contains-multi-index?-3 domain i j k))
				       (error "Array-setter: domain does not contain multi-index: "    domain i j k))
				      ((not (checker value))
				       (error "Array-setter: value cannot be stored in body: " value))
				      (else
				       (manipulator-setter body (indexer i j k) value)))))
			((4)  (lambda (value i j k l)
				(cond ((not (and (##exact-integer? i)
						 (##exact-integer? j)
						 (##exact-integer? k)
						 (##exact-integer? l)))
				       (error "Array-setter: multi-index component is not an exact integer: " i j k l))
				      ((not (##Interval-contains-multi-index?-4 domain i j k l))
				       (error "Array-setter: domain does not contain multi-index: "    domain i j k l))
				      ((not (checker value))
				       (error "Array-setter: value cannot be stored in body: " value))
				      (else
				       (manipulator-setter body (indexer i j k l) value)))))
			(else (lambda (value . multi-index)
				(cond ((not (every exact-integer? multi-index))
				       (apply error "Array-setter: multi-index component is not an exact integer: " multi-index))
				      ((not (= (##Interval-dimension domain) (length multi-index)))
				       (apply error "Array-setter: multi-index is not the correct dimension: " domain multi-index))
				      ((not (apply Interval-contains-multi-index? domain multi-index))
				       (apply error "Array-setter: domain does not contain multi-index: "    domain multi-index))
				      ((not (checker value))
				       (error "Array-setter: value cannot be stored in body: " value))
				      (else
				       (manipulator-setter body (apply indexer multi-index) value))))))
		      (case (##Interval-dimension domain)
			((1)  (expand-setters (lambda (value i)             (manipulator-setter body (indexer i)                 value))))
			((2)  (expand-setters (lambda (value i j)           (manipulator-setter body (indexer i j)               value))))
			((3)  (expand-setters (lambda (value i j k)         (manipulator-setter body (indexer i j k)             value))))
			((4)  (expand-setters (lambda (value i j k l)       (manipulator-setter body (indexer i j k l)           value))))
			(else (lambda (value . multi-index) (manipulator-setter body (apply indexer multi-index) value)))))))
      (make-##Array-base domain
			 getter
			 setter
			 manipulators
			 body
			 indexer
			 safe?))))


(define (Fixed-array #!key
		     (domain            (macro-absent-obj))
		     (manipulators      (macro-absent-obj))
		     (body              (macro-absent-obj))
		     (indexer           (macro-absent-obj))
		     (initializer-value (macro-absent-obj))
		     (safe?             (macro-absent-obj)))
  
  (cond ((eq? (macro-absent-obj) domain)
	 (error "Fixed-array: the domain must be given: " domain))
	((not (Interval? domain))
	 (error "Fixed-array: domain is not an interval: " domain))
	((eq? (macro-absent-obj) manipulators)
	 (error "Fixed-array: the manipulators must be given: " manipulators))
	((not (Array-manipulators? manipulators))
	 (error "Fixed-array: manipulators are not fixed-array-manipulators: " manipulators))
	((and (not (eq? (macro-absent-obj) indexer))
	      (not (procedure? indexer)))
	 (error "Fixed-array:  indexer is not a procedure: " indexer))
	((not (or (eq? safe? (macro-absent-obj))
		  (boolean? safe?)))
	 (error "Fixed-array: safe? must be a boolean: " safe?))
	((not (or (eq? body              (macro-absent-obj))
		  (eq? initializer-value (macro-absent-obj))))
	 (error "Fixed-array: you can not pass both body and initializer-value as arguments " body initializer-value))
	(else
	 (let* ((safe? (if (eq? safe? (macro-absent-obj))
			   Fixed-array-default-safe?
			   safe?))
		(initializer-value (if (eq? initializer-value (macro-absent-obj))
				       (Array-manipulators-default manipulators)
				       initializer-value))
		(body (if (eq? body (macro-absent-obj))
			  ((Array-manipulators-maker manipulators) (##Interval-volume domain) initializer-value)
			  body))
		(indexer (if (eq? indexer (macro-absent-obj))
			     (case (Interval-dimension domain)
			       ((1) (let ((low-0 (Interval-lower-bound domain 0))
					  (increment-0 1))
				      (indexer-1 0 low-0 increment-0)))
			       ((2) (let* ((low-0 (##Interval-lower-bound domain 0))
					   (low-1 (##Interval-lower-bound domain 1))
					   (increment-1 1)
					   (increment-0 (* increment-1
							   (- (##Interval-upper-bound domain 1)
							      (##Interval-lower-bound domain 1)))))
				      (indexer-2 0
						 low-0 low-1
						 increment-0 increment-1)))
			       ((3) (let* ((low-0 (##Interval-lower-bound domain 0))
					   (low-1 (##Interval-lower-bound domain 1))
					   (low-2 (##Interval-lower-bound domain 2))
					   (increment-2 1)
					   (increment-1 (* increment-2
							   (- (##Interval-upper-bound domain 2)
							      (##Interval-lower-bound domain 2))))
					   (increment-0 (* increment-1
							   (- (##Interval-upper-bound domain 1)
							      (##Interval-lower-bound domain 1)))))
				      (indexer-3 0
						 low-0 low-1 low-2
						 increment-0 increment-1 increment-2)))
			       ((4) (let* ((low-0 (##Interval-lower-bound domain 0))
					   (low-1 (##Interval-lower-bound domain 1))
					   (low-2 (##Interval-lower-bound domain 2))
					   (low-3 (##Interval-lower-bound domain 3))
					   (increment-3 1)
					   (increment-2 (* increment-3
							   (- (##Interval-upper-bound domain 3)
							      (##Interval-lower-bound domain 3))))
					   (increment-1 (* increment-2
							   (- (##Interval-upper-bound domain 2)
							      (##Interval-lower-bound domain 2))))
					   (increment-0 (* increment-1
							   (- (##Interval-upper-bound domain 1)
							      (##Interval-lower-bound domain 1)))))
				      (indexer-4 0
						 low-0 low-1 low-2 low-3
						 increment-0 increment-1 increment-2 increment-3)))
			       (else
				(let ((lower-bounds (##Interval-lower-bounds->list domain))
				      (upper-bounds (##Interval-upper-bounds->list domain)))
				  (let ((ranges (map (lambda (u l) (- u l)) upper-bounds lower-bounds)))
				    (do ((ranges (reverse ranges) (cdr ranges))
					 (increments (list 1) (cons (* (car increments) (car ranges))
								    increments)))
					((null? (cdr ranges)) (indexer-generic 0 lower-bounds increments)))))))
			     indexer)))
	   (##finish-Fixed-array domain
				 manipulators
				 body
				 indexer
				 safe?)))))

#|

The domain of the result is the same as the domain of the argument.

Builds a new Fixed-array and populates the body of the result with
(Array-getter array) applied to the elementf of (Array-domain array)
in no particular order.

Assumes, and may exploit, that (Array-getter a) is thread-safe.

|#

(define (Array->Fixed-array array #!optional (result-manipulators (macro-absent-obj)) (safe? (macro-absent-obj)))
  (cond ((not (Array? array))
	 (error "Array->Fixed-array: Argument is not an array: " array))
	((not (or (eq? result-manipulators (macro-absent-obj))
		  (Array-manipulators? result-manipulators)))
	 (error "Array->Fixed-array: result-manipulators are not Array-manipulators: " result-manipulators))
	((not (or (eq? safe? (macro-absent-obj))
		  (boolean? safe?)))
	 (error "Array->Fixed-array: safe? is not a boolean: " safe?))
	(else
	 (##Array->Fixed-array (if (eq? result-manipulators (macro-absent-obj))
				   generic-array-manipulators
				   result-manipulators)
			       array
			       (if (eq? safe? (macro-absent-obj))
				   Fixed-array-default-safe?
				   safe?)
			       ##Interval-for-each))))

;;; This version evaluates (Array-getter array) on the elements of (Array-domain array) in
;;; lexicographical order

(define (Array->Fixed-array-serial array #!optional (result-manipulators (macro-absent-obj)) (safe? (macro-absent-obj)))
  (cond ((not (Array? array))
	 (error "Array->Fixed-array-serial: Argument is not an array: " array))
	((not (or (eq? result-manipulators (macro-absent-obj))
		  (Array-manipulators? result-manipulators)))
	 (error "Array->Fixed-array-serial: result-manipulators are not Array-manipulators: " result-manipulators))
	((not (or (eq? safe? (macro-absent-obj))
		  (boolean? safe?)))
	 (error "Array->Fixed-array-serial: safe? is not a boolean: " safe?))
	(else
	 (##Array->Fixed-array (if (eq? result-manipulators (macro-absent-obj))
				   generic-array-manipulators
				   result-manipulators)
			       array
			       (if (eq? safe? (macro-absent-obj))
				   Fixed-array-default-safe?
				   safe?)
			       ##Interval-for-each-serial))))

(define (##Array->Fixed-array result-manipulators array safe? interval-for-each)
  (let ((domain (Array-domain array)))
    (let ((result (Fixed-array
		   domain: domain
		   manipulators: result-manipulators
		   safe?: safe?)))
      (let ((range-setter (Array-setter result))
	    (domain-getter (Array-getter array)))
	(interval-for-each (case (##Interval-dimension domain)
			     ((1)  (lambda (i)       (range-setter (domain-getter i)       i)))
			     ((2)  (lambda (i j)     (range-setter (domain-getter i j)     i j)))
			     ((3)  (lambda (i j k)   (range-setter (domain-getter i j k)   i j k)))
			     ((4)  (lambda (i j k l) (range-setter (domain-getter i j k l) i j k l)))
			     (else (lambda multi-index
				     (apply range-setter (apply domain-getter multi-index) multi-index))))
			   domain)
	result))))

#|

In the next function, old-indexer is an affine 1-1 mapping from an interval to [0,N), for some N.

new-domain->old-domain is an affine 1-1 mapping from new-domain to the domain of old-indexer.

|#

(define (compose-indexers old-indexer new-domain new-domain->old-domain)
  (case (##Interval-dimension new-domain)
    ((1) (let* ((lower-0 (##Interval-lower-bound new-domain 0))
		(upper-0 (##Interval-upper-bound new-domain 0))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1)))
				      old-indexer)
				    base)
				 0)))
	   (indexer-1 base lower-0 increment-0)))
    
    ((2) (let* ((lower-0 (##Interval-lower-bound new-domain 0))
		(lower-1 (##Interval-lower-bound new-domain 1))
		(upper-0 (##Interval-upper-bound new-domain 0))
		(upper-1 (##Interval-upper-bound new-domain 1))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0 lower-1))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1) lower-1))
				      old-indexer)
				    base)
				 0))
		(increment-1 (if (< (+ lower-1 1) upper-1)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 (+ lower-1 1)))
				      old-indexer)
				    base)
				 0)))
	   (indexer-2 base lower-0 lower-1 increment-0 increment-1)))
    ((3) (let* ((lower-0 (##Interval-lower-bound new-domain 0))
		(lower-1 (##Interval-lower-bound new-domain 1))
		(lower-2 (##Interval-lower-bound new-domain 2))
		(upper-0 (##Interval-upper-bound new-domain 0))
		(upper-1 (##Interval-upper-bound new-domain 1))
		(upper-2 (##Interval-upper-bound new-domain 2))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0 lower-1 lower-2))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1) lower-1 lower-2))
				      old-indexer)
				    base)
				 0))
		(increment-1 (if (< (+ lower-1 1) upper-1)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 (+ lower-1 1) lower-2))
				      old-indexer)
				    base)
				 0))
		(increment-2 (if (< (+ lower-2 1) upper-2)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 lower-1 (+ lower-2 1)))
				      old-indexer)
				    base)
				 0)))
	   (indexer-3 base lower-0 lower-1 lower-2 increment-0 increment-1 increment-2)))
    ((4) (let* ((lower-0 (##Interval-lower-bound new-domain 0))
		(lower-1 (##Interval-lower-bound new-domain 1))
		(lower-2 (##Interval-lower-bound new-domain 2))
		(lower-3 (##Interval-lower-bound new-domain 3))
		(upper-0 (##Interval-upper-bound new-domain 0))
		(upper-1 (##Interval-upper-bound new-domain 1))
		(upper-2 (##Interval-upper-bound new-domain 2))
		(upper-3 (##Interval-upper-bound new-domain 3))
		(base (call-with-values
			  (lambda () (new-domain->old-domain lower-0 lower-1 lower-2 lower-3))
			old-indexer))
		(increment-0 (if (< (+ lower-0 1) upper-0)
				 (- (call-with-values
					(lambda () (new-domain->old-domain (+ lower-0 1) lower-1 lower-2 lower-3))
				      old-indexer)
				    base)
				 0))
		(increment-1 (if (< (+ lower-1 1) upper-1)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 (+ lower-1 1) lower-2 lower-3))
				      old-indexer)
				    base)
				 0))
		(increment-2 (if (< (+ lower-2 1) upper-2)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 lower-1 (+ lower-2 1) lower-3))
				      old-indexer)
				    base)
				 0))
		(increment-3 (if (< (+ lower-3 1) upper-3)
				 (- (call-with-values
					(lambda () (new-domain->old-domain lower-0 lower-1 lower-2 (+ lower-3 1)))
				      old-indexer)
				    base)
				 0)))
	   (indexer-4 base lower-0 lower-1 lower-2 lower-3 increment-0 increment-1 increment-2 increment-3)))
    (else
     (let* ((lower-bounds (##Interval-lower-bounds->list new-domain))
	    (upper-bounds (##Interval-upper-bounds->list new-domain))
	    (base (call-with-values
		      (lambda () (apply new-domain->old-domain lower-bounds))
		    old-indexer))
	    (increments (let ((increments   (map (lambda (x) 0) lower-bounds))
			      (lower-bounds (map (lambda (x) x) lower-bounds)))
			  (let loop ((l lower-bounds)
				     (u upper-bounds)
				     (i increments)
				     (base base))
			    (if (null? l)
				increments
				(let ((new-base
				       (if (< (+ (car l) 1)
					      (car u))
					   (begin
					     (set-car! l (+ (car l) 1))
					     (let ((new-base (call-with-values
								 (lambda () (apply new-domain->old-domain lower-bounds))
							       old-indexer)))
					       (set-car! i (- new-base base))
					       new-base))
					   base)))
				  (loop (cdr l)
					(cdr u)
					(cdr i)
					new-base)))))))
       (indexer-generic base lower-bounds increments)))))

#|

You want to share the backing store of array.

So you specify a new domain and an affine 1-1 mapping from the new-domain to the old-domain.

|#

(define (Fixed-array-share! array
			    new-domain
			    new-domain->old-domain
			    #!optional (safe? (macro-absent-obj)))
  (cond ((not (Fixed-array? array))
	 (error "Fixed-array-share!: array is not a Fixed-array: " array))
	((not (Interval? new-domain))
	 (error "Fixed-array-share!: new-domain is not an Interval: " new-domain))
	((not (procedure? new-domain->old-domain))
	 (error "Fixed-array-share!: new-domain->old-domain is not a procedure: " new-domain->old-domain))
	((not (or (eq? safe? (macro-absent-obj))
		  (boolean? safe?)))
	 (error "Fixed-array-share!: safe? is not a boolean: " safe?))
	(else
	 (let ((old-domain       (Array-domain       array))
	       (old-indexer      (Array-indexer      array))
	       (body             (Array-body         array))
	       (manipulators     (Array-manipulators array))
	       (safe?            (if (eq? safe? (macro-absent-obj)) Fixed-array-default-safe? safe?)))
	   (##finish-Fixed-array new-domain
				 manipulators
				 body
				 (compose-indexers old-indexer new-domain new-domain->old-domain)
				 safe?)))))

(define (Array-curry array left-dimension)
  (cond ((not (Array? array))
	 (error "Array-curry: argument is not an array: " array left-dimension))
	((not (##exact-integer? left-dimension))
	 (error "Array-curry: argument is not an exact integer: " array left-dimension))
	((not (< 0 left-dimension (##Interval-dimension (Array-domain array))))
	 (error "Array-curry: argument is not between 0 and (Interval-dimension (Array-domain array)) (exclusive): " array left-dimension))
	(else
	 (call-with-values
	     (lambda () (Interval-curry (Array-domain array) left-dimension))
	   (lambda (left-interval right-interval)
	     (let ((getter (Array-getter array)))
	       (Array left-interval
		      (case (##Interval-dimension left-interval)
			((1)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i)      (Array right-interval (lambda (j)         (getter i j)))))
				((2)  (lambda (i)      (Array right-interval (lambda (j k)       (getter i j k)))))
				((3)  (lambda (i)      (Array right-interval (lambda (j k l)     (getter i j k l)))))
				(else (lambda (i)      (Array right-interval (lambda multi-index (apply getter i multi-index)))))))
			((2)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i j)    (Array right-interval (lambda   (k)       (getter i j k)))))
				((2)  (lambda (i j)    (Array right-interval (lambda   (k l)     (getter i j k l)))))
				(else (lambda (i j)    (Array right-interval (lambda multi-index (apply getter i j multi-index)))))))
			((3)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i j k)  (Array right-interval (lambda     (l)     (getter i j k l)))))
				(else (lambda (i j k)  (Array right-interval (lambda multi-index (apply getter i j k multi-index)))))))
			(else (lambda left-multi-index
				(Array right-interval
				       (lambda right-multi-index
					 (apply getter (append left-multi-index right-multi-index))))))))))))))

(define (Mutable-array-curry array left-dimension)
  (cond ((not (Mutable-array? array))
	 (error "Mutable-array-curry: argument is not a Mutable-array: " array left-dimension))
	((not (##exact-integer? left-dimension))
	 (error "Mutable-array-curry: argument is not an exact integer: " array left-dimension))
	((not (< 0 left-dimension (##Interval-dimension (Array-domain array))))
	 (error "Mutable-array-curry: argument is not between 0 and (Interval-dimension (Array-domain array)) (exclusive): " array left-dimension))
	(else
	 (call-with-values
	     (lambda () (Interval-curry (Array-domain array) left-dimension))
	   (lambda (left-interval right-interval)
	     (let ((getter (Array-getter array))
		   (setter   (Array-setter   array)))
	       (Array left-interval
		      (case (##Interval-dimension left-interval)
			((1)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i)     (Mutable-array right-interval
								     (lambda (  j)     (getter   i j))
								     (lambda (v j)     (setter v i j)))))
				((2)  (lambda (i)     (Mutable-array right-interval
								     (lambda (  j k)   (getter   i j k))
								     (lambda (v j k)   (setter v i j k)))))
				((3)  (lambda (i)     (Mutable-array right-interval
								     (lambda (  j k l) (getter   i j k l))
								     (lambda (v j k l) (setter v i j k l)))))
				(else (lambda (i)     (Mutable-array right-interval
								     (lambda      multi-index  (apply getter   i     multi-index))
								     (lambda (v . multi-index) (apply setter v i     multi-index)))))))
			((2)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i j)   (Mutable-array right-interval
								     (lambda (    k)   (getter   i j k))
								     (lambda (v   k)   (setter v i j k)))))
				((2)  (lambda (i j)   (Mutable-array right-interval
								     (lambda (    k l) (getter   i j k l))
								     (lambda (v   k l) (setter v i j k l)))))
				(else (lambda (i j)   (Mutable-array right-interval
								     (lambda      multi-index  (apply getter   i j   multi-index))
								     (lambda (v . multi-index) (apply setter v i j   multi-index)))))))
			((3)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i j k) (Mutable-array right-interval
								     (lambda (      l) (getter   i j k l))
								     (lambda (v     l) (setter v i j k l)))))
				(else (lambda (i j k) (Mutable-array right-interval
								     (lambda      multi-index  (apply getter   i j k multi-index))
								     (lambda (v . multi-index) (apply setter v i j k multi-index)))))))
			(else (lambda left-multi-index
				(Mutable-array right-interval
					       (lambda      right-multi-index  (apply getter   (append left-multi-index right-multi-index)))
					       (lambda (v . right-multi-index) (apply setter v (append left-multi-index right-multi-index))))))))))))))

(define (Fixed-array-curry array left-dimension #!optional (safe? (macro-absent-obj)))
  (cond ((not (Fixed-array? array))
	 (error "Fixed-array-curry: argument is not a Fixed-array: " array left-dimension))
	((not (##exact-integer? left-dimension))
	 (error "Fixed-array-curry: argument is not an exact integer: " array left-dimension))
	((not (< 0 left-dimension (##Interval-dimension (Array-domain array))))
	 (error "Fixed-array-curry: argument is not between 0 and (Interval-dimension (Array-domain array)) (exclusive): " array left-dimension))
	((not (or (eq? safe? (macro-absent-obj))
		  (boolean? safe?)))
	 (error "Fixed-array-curry: safe? is not a boolean: " safe?))
	(else
	 (call-with-values
	     (lambda () (Interval-curry (Array-domain array) left-dimension))
	   (lambda (left-interval right-interval)
	     (let ((safe? (if (eq? safe? (macro-absent-obj))
			      Fixed-array-default-safe?
			      safe?)))
	       (Array left-interval
		      (case (##Interval-dimension left-interval)
			((1)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i)     (Fixed-array-share! array right-interval (lambda (j)                         (values i j    )) safe?)))
				((2)  (lambda (i)     (Fixed-array-share! array right-interval (lambda (j k)                       (values i j k  )) safe?)))
				((3)  (lambda (i)     (Fixed-array-share! array right-interval (lambda (j k l)                     (values i j k l)) safe?)))
				(else (lambda (i)     (Fixed-array-share! array right-interval (lambda multi-index (apply values i     multi-index)) safe?)))))
			((2)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i j)   (Fixed-array-share! array right-interval (lambda (  k)                       (values i j k  )) safe?)))
				((2)  (lambda (i j)   (Fixed-array-share! array right-interval (lambda (  k l)                     (values i j k l)) safe?)))
				(else (lambda (i j)   (Fixed-array-share! array right-interval (lambda multi-index (apply values i j   multi-index)) safe?)))))
			((3)  (case (##Interval-dimension right-interval)
				((1)  (lambda (i j k) (Fixed-array-share! array right-interval (lambda (    l)                    (values i j k l)) safe?)))
				(else (lambda (i j k) (Fixed-array-share! array right-interval (lambda multi-index (apply values i j k multi-index)) safe?)))))
			(else (lambda left-multi-index 
				(Fixed-array-share! array right-interval (lambda right-multi-index (apply values (append left-multi-index right-multi-index)) safe?))))))))))))


(define (insert-arg-into-arg-list arg arg-list index)
  (if (= index 0)
      (cons arg arg-list)
      (cons arg (insert-arg-into-arg-list arg (cdr arg-list) (- index 1)))))

(define (Array-distinguish-one-axis array index)
  (cond ((not (Array? array))
	 (error "Array-distinguish-one-axis: argument is not an array: " array index))
	((not (##exact-integer? index))
	 (error "Array-distinguish-one-axis: argument is not an exact integer: " array index))
	((not (< 1 (##Interval-dimension (Array-domain array))))
	 (error "Array-distinguish-one-axis:  (Interval-dimension (Array-domain array)) is not greater than 1 : " array index))
	((not (< -1 index (##Interval-dimension (Array-domain array))))
	 (error "Array-distinguish-one-axis: argument is not between 0 (inclusive) and (Interval-dimension (Array-domain array)) (exclusive): " array index))
	(else
	 (call-with-values
	     (lambda () (Interval-distinguish-one-axis (Array-domain array) index))
	   (lambda (outer-interval inner-interval)
	     (let ((getter (Array-getter array)))
	       (Array outer-interval
		      (case (##Interval-dimension outer-interval)
			((1) (case index
			       ((0) (lambda (  j)       (Array inner-interval (lambda (i) (getter i j)))))
			       ((1) (lambda (i  )       (Array inner-interval (lambda (j) (getter i j)))))))
			((2) (case index
			       ((0) (lambda (  j k)     (Array inner-interval (lambda (i) (getter i j k)))))
			       ((1) (lambda (i   k)     (Array inner-interval (lambda (j) (getter i j k)))))
			       ((2) (lambda (i j  )     (Array inner-interval (lambda (k) (getter i j k)))))))
			((3) (case index
			       ((0) (lambda (  j k l)   (Array inner-interval (lambda (i) (getter i j k l)))))
			       ((1) (lambda (i   k l)   (Array inner-interval (lambda (j) (getter i j k l)))))
			       ((2) (lambda (i j   l)   (Array inner-interval (lambda (k) (getter i j k l)))))
			       ((3) (lambda (i j k  )   (Array inner-interval (lambda (l) (getter i j k l)))))))
			(else       (lambda outer-index (Array inner-interval (lambda (m) (apply getter (insert-arg-into-arg-list m outer-index index))))))))))))))

(define (Mutable-array-distinguish-one-axis array index)
  (cond ((not (Mutable-array? array))
	 (error "Mutable-array-distinguish-one-axis: argument is not an array: " array index))
	((not (##exact-integer? index))
	 (error "Mutable-array-distinguish-one-axis: argument is not an exact integer: " array index))
	((not (< 1 (##Interval-dimension (Array-domain array))))
	 (error "Mutable-array-distinguish-one-axis:  (Interval-dimension (Array-domain array)) is not greater than 1 : " array index))
	((not (< -1 index (##Interval-dimension (Array-domain array))))
	 (error "Mutable-array-distinguish-one-axis: argument is not between 0 (inclusive) and (Interval-dimension (Array-domain array)) (exclusive): " array index))
	(else
	 (call-with-values
	     (lambda () (Interval-distinguish-one-axis (Array-domain array) index))
	   (lambda (outer-interval inner-interval)
	     (let ((getter (Array-getter array))
		   (setter   (Array-setter array)))
	       (Array outer-interval
		      (case (##Interval-dimension outer-interval)
			((1) (case index
			       ((0) (lambda (  j)     (Mutable-array inner-interval
								     (lambda (  i) (getter   i j))
								     (lambda (v i) (setter v i j)))))
			       ((1) (lambda (i  )     (Mutable-array inner-interval
								     (lambda (  j) (getter   i j))
								     (lambda (v j) (setter v i j)))))))
			((2) (case index
			       ((0) (lambda (  j k)   (Mutable-array inner-interval
								     (lambda (  i) (getter   i j k))
								     (lambda (v i) (setter v i j k)))))
			       ((1) (lambda (i   k)   (Mutable-array inner-interval
								     (lambda (  j) (getter   i j k))
								     (lambda (v j) (setter v i j k)))))
			       ((2) (lambda (i j  )   (Mutable-array inner-interval
								     (lambda (  k) (getter   i j k))
								     (lambda (v k) (setter v i j k)))))))
			((3) (case index
			       ((0) (lambda (  j k l) (Mutable-array inner-interval
								     (lambda (  i) (getter   i j k l))
								     (lambda (v i) (setter v i j k l)))))
			       ((1) (lambda (i   k l) (Mutable-array inner-interval
								     (lambda (  j) (getter   i j k l))
								     (lambda (v j) (setter v i j k l)))))
			       ((2) (lambda (i j   l) (Mutable-array inner-interval
								     (lambda (  k) (getter   i j k l))
								     (lambda (v k) (setter v i j k l)))))
			       ((3) (lambda (i j k  ) (Mutable-array inner-interval
								     (lambda (  l) (getter   i j k l))
								     (lambda (v l) (setter v i j k l)))))))
			(else (lambda outer-index
				(Mutable-array inner-interval
					       (lambda (  m) (apply getter   (insert-arg-into-arg-list m outer-index index)))
					       (lambda (v m) (apply setter v (insert-arg-into-arg-list m outer-index index))))))))))))))


(define (Fixed-array-distinguish-one-axis array index #!optional (safe? (macro-absent-obj)))
  (cond ((not (Fixed-array? array))
	 (error "Fixed-array-distinguish-one-axis: argument is not an Fixed-array: " array index))
	((not (##exact-integer? index))
	 (error "Fixed-array-distinguish-one-axis: argument is not an exact integer: " array index))
	((not (< 1 (##Interval-dimension (Array-domain array))))
	 (error "Fixed-array-distinguish-one-axis: (Interval-dimension (Array-domain array)) is not greater than 1 : " array index))
	((not (< -1 index (##Interval-dimension (Array-domain array))))
	 (error "Fixed-array-distinguish-one-axis: argument is not between 0 (inclusive) and (Interval-dimension (Array-domain array)) (exclusive): " array index))
	((not (or (eq? safe? (macro-absent-obj))
		  (boolean? safe?)))
	 (error "Fixed-array-distinguish-one-axis: safe? is not a boolean: " safe?))
	(else
	 (call-with-values
	     (lambda () (Interval-distinguish-one-axis (Array-domain array) index))
	   (lambda (outer-interval inner-interval)
	     (let ((safe? (if (eq? safe? (macro-absent-obj))
			      Fixed-array-default-safe?
			      safe?)))
	       (Array outer-interval 
		      (case (##Interval-dimension outer-interval)
			((1) (case index
			       ((0) (lambda (  j)     (Fixed-array-share! array inner-interval (lambda (i) (values i j)) safe?)))
			       ((1) (lambda (i  )     (Fixed-array-share! array inner-interval (lambda (j) (values i j)) safe?)))))
			((2) (case index
			       ((0) (lambda (  j k)   (Fixed-array-share! array inner-interval (lambda (i) (values i j k)) safe?)))
			       ((1) (lambda (i   k)   (Fixed-array-share! array inner-interval (lambda (j) (values i j k)) safe?)))
			       ((2) (lambda (i j  )   (Fixed-array-share! array inner-interval (lambda (k) (values i j k)) safe?)))))
			((3) (case index
			       ((0) (lambda (  j k l) (Fixed-array-share! array inner-interval (lambda (i) (values i j k l)) safe?)))
			       ((1) (lambda (i   k l) (Fixed-array-share! array inner-interval (lambda (j) (values i j k l)) safe?)))
			       ((2) (lambda (i j   l) (Fixed-array-share! array inner-interval (lambda (k) (values i j k l)) safe?)))
			       ((3) (lambda (i j k  ) (Fixed-array-share! array inner-interval (lambda (l) (values i j k l)) safe?)))))
			(else (lambda outer-index
				(Fixed-array-share! array inner-interval (lambda (m) (apply values (insert-arg-into-arg-list m outer-index index)) safe?))))))))))))


#|

Array-map returns an array whose domain is the same as the common domain of (cons array arrays)
and whose getter is

(lambda multi-index
  (apply f (map (lambda (g) (apply g multi-index)) (map Array-getter (cons array arrays)))))

This function is also used in Array-for-each, so we try to specialize the this
function to speed things up a bit.

|#

(define (specialize-function-applied-to-array-getters f array arrays)
  (let ((domain (Array-domain array))
	(getter-0 (Array-getter array)))
    (case (length arrays)
      ((0) (case (##Interval-dimension domain)
	     ((1)  (lambda (i)         (f (getter-0 i))))
	     ((2)  (lambda (i j)       (f (getter-0 i j))))
	     ((3)  (lambda (i j k)     (f (getter-0 i j k))))
	     ((4)  (lambda (i j k l)   (f (getter-0 i j k l))))
	     (else (lambda multi-index (f (apply getter-0 multi-index))))))
      
      ((1) (let ((getter-1 (Array-getter (car arrays))))
	     (case (##Interval-dimension domain)
	       ((1)  (lambda (i)         (f (getter-0 i)
					    (getter-1 i))))
	       ((2)  (lambda (i j)       (f (getter-0 i j)
					    (getter-1 i j))))
	       ((3)  (lambda (i j k)     (f (getter-0 i j k)
					    (getter-1 i j k))))
	       ((4)  (lambda (i j k l)   (f (getter-0 i j k l)
					    (getter-1 i j k l))))
	       (else (lambda multi-index (f (apply getter-0 multi-index)
					    (apply getter-1 multi-index)))))))
      ((2) (let ((getter-1 (Array-getter (car arrays)))
		 (getter-2 (Array-getter (cadr arrays))))
	     (case (##Interval-dimension domain)
	       ((1)  (lambda (i)         (f (getter-0 i)
					    (getter-1 i)
					    (getter-2 i))))
	       ((2)  (lambda (i j)       (f (getter-0 i j)
					    (getter-1 i j)
					    (getter-2 i j))))
	       ((3)  (lambda (i j k)     (f (getter-0 i j k)
					    (getter-1 i j k)
					    (getter-2 i j k))))
	       ((4)  (lambda (i j k l)   (f (getter-0 i j k l)
					    (getter-1 i j k l)
					    (getter-2 i j k l))))
	       (else (lambda multi-index (f (apply getter-0 multi-index)
					    (apply getter-1 multi-index)
					    (apply getter-2 multi-index))))))) 
      (else
       (let ((getters (cons getter-0 (map Array-getter arrays))))
	 (case (##Interval-dimension domain)
	   ((1)  (lambda (i)         (apply f (map (lambda (g) (g i))                 getters))))
	   ((2)  (lambda (i j)       (apply f (map (lambda (g) (g i j))               getters))))
	   ((3)  (lambda (i j k)     (apply f (map (lambda (g) (g i j k))             getters))))
	   ((4)  (lambda (i j k l)   (apply f (map (lambda (g) (g i j k l))           getters))))
	   (else (lambda multi-index (apply f (map (lambda (g) (apply g multi-index)) getters))))))))))

(define (Array-map f array #!rest arrays)
  (cond ((not (procedure? f))
	 (error "Array-map: Argument is not a procedure: " f))
	((not (every Array? (cons array arrays)))
	 (apply error "Array-map: Not all arguments are arrays: " array arrays))
	((not (every (lambda (d) (##Interval= d (Array-domain array))) (map Array-domain arrays)))
	 (apply error "Array-map: Not all arrays have the same domain: " array arrays))
	(else (Array (Array-domain array)
		     (specialize-function-applied-to-array-getters f array arrays)))))

;;; applies f to the elements of the arrays in lexicographical order.

(define (Array-for-each f array #!rest arrays)
  (cond ((not (procedure? f))
	 (error "Array-for-each: Argument is not a procedure: " f))
	((not (every Array? (cons array arrays)))
	 (apply error "Array-for-each: Not all arguments are arrays: " array arrays))
	((not (every (lambda (d) (##Interval= d (Array-domain array))) (map Array-domain arrays)))
	 (apply error "Array-for-each: Not all arrays have the same domain: " array arrays))
	(else
	 (##Interval-for-each (specialize-function-applied-to-array-getters f array arrays) (Array-domain array)))))

(define (Array-for-each-serial f array #!rest arrays)
  (cond ((not (procedure? f))
	 (error "Array-for-each-serial: Argument is not a procedure: " f))
	((not (every Array? (cons array arrays)))
	 (apply error "Array-for-each-serial: Not all arguments are arrays: " array arrays))
	((not (every (lambda (d) (##Interval= d (Array-domain array))) (map Array-domain arrays)))
	 (apply error "Array-for-each-serial: Not all arrays have the same domain: " array arrays))
	(else
	 (##Interval-for-each-serial (specialize-function-applied-to-array-getters f array arrays) (Array-domain array)))))


;;; Calculates
;;;
;;; (...(operator (operator (operator identity ((Array-getter a) multi-index_1)) ((Array-getter a) multi-index_2)) ((Array-getter a) multi-index_3)) ...)
;;;
;;; where multi-index_1, multi-index_2, ... are the elements of (Array-domain a) in lexicographical order
;;; This version assumes, and may use, that (Array-getter a) is thread-safe and that operator is associative.
;;; The order of application of (Array-getter) and operator is not specified.

(define (Array-reduce op id a)
  (cond ((not (procedure? op))
	 (error "Array-reduce: operator is not a procedure: " op))
	((not (Array? a))
	 (error "Array-reduce: argument is not an array: " a))
	(else
	 (##Interval-reduce (Array-getter a) op id (Array-domain a)))))

;;; This version applies (Array-getter a) to the elements of (Array-domain a) in lexicographical order and applies
;;; operator in the order given

(define (Array-reduce-serial op id a)
  (cond ((not (procedure? op))
	 (error "Array-reduce-serial: operator is not a procedure: " op))
	((not (Array? a))
	 (error "Array-reduce-serial: argument is not an array: " a))
	(else
	 (##Interval-reduce-serial (Array-getter a) op id (Array-domain a)))))

#|

I've never used the following routines, so I'm removing them from the SRFI for now.

(define (Array-every? proc a)
  (cond ((not (procedure? proc))
	 (error "Array-every?: First argument is not a procedure: " proc a))
	((not (Array? a))
	 (error "Array-every?: Second argument is not an array: " proc a))
	(else
	 (##Interval-reduce (Array-getter a)
			    (lambda (result x)
			      (and result (proc x)))
			    #t
			    (Array-domain a)))))

(define (Array-every?-serial proc a)
  (cond ((not (procedure? proc))
	 (error "Array-every?-serial: First argument is not a procedure: " proc a))
	((not (Array? a))
	 (error "Array-every?-serial: Second argument is not an array: " proc a))
	(else
	 (##Interval-reduce-serial (Array-getter a)
				   (lambda (result x)
				     (and result (proc x)))
				   #t
				   (Array-domain a)))))

|#

(declare (inline))
