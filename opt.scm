;;; array opt
;;; 1997 - 2001 Jussi Piitulainen

;;; Execute (opt:make 'type2 "array:" 4) to display the source code
;;; for a type2 optimizer that handles dimensions below 4 with name
;;; "array:optimize".

;;; (opt:make type lib r)
;;; makes the optimizer code `lib:optimize' for dimensions below r.
;;; There are three types: type1 accommodates an extra argument for
;;; every index procedure, type2 only for the unoptimized case, and
;;; type3 represents index functions as coefficient lists.

(define (opt:make type lib r)
   (let ((opt (lambda (string)
		 (string->symbol (string-append lib string)))))
      `(begin
          (define (,(opt "opt-make")) (list ',type ,lib ,r))
	  (define (,(opt "optimize") f r empty?)
            (if empty?
                ,(case type
                   ((type1 type2) '(lambda ks -1))
                   ((type3) '(cons -1 (do ((k 0 (+ k 1))
                                           (t '() (cons 0 t)))
                                        ((= k r) t))))
                   (else `(unknown type ,type)))
                (case r
                  ,@(map (opt:dispatch opt) (opt:between 0 r))
                  (else
		   (let ((v (do ((k 0 (+ k 1))
                                 (v '() (cons 0 v)))
                              ((= k r) v))))
                     (let ((n0 (apply f v)))
                       (,(opt "n")
                        n0 (,(opt "coefficients") f n0 v v))))))))
	  (define (,(opt "coefficients") f n0 vs vp)
            (case vp
              ((()) '())
              (else
               (set-car! vp 1)
               (let ((n (- (apply f vs) n0)))
                 (set-car! vp 0)
                 (cons n
                       (,(opt "coefficients") f n0 vs (cdr vp)))))))
	  ,@(map (opt:make-optimizer type opt) (opt:between 0 r))
	  ;;
	  ;; The multidimensional index function _must_ be able to
	  ;; accommodate more indexes than coefficients (one more).
	  ;; Therefore, it is important that the end condition is
	  ;; on `ns', not on `ks'.  (This is for `array-set!'.)
	  ;;
	  (define (,(opt "n") n0 ns)
	    ,(case type
	       ((type1 type2)
		'(lambda ks
		   (do ((ns ns (cdr ns))
			(ks ks (cdr ks))
			(dx n0 (+ dx (* (car ns) (car ks)))))
		       ((null? ns)
			dx))))
	       ((type3) '(cons n0 ns))
	       (else `(unknown type ,type)))))))

(define (opt:between low past)
   (if (< low past)
      (cons low (opt:between (+ low 1) past))
      '()))

(define (opt:dispatch opt)
   (lambda (d)
      (let ((ks (opt:between 1 (+ d 1))))
	 `((,d) (let ((n0 (f ,@(map (lambda (_) 0) ks))))
		   (,(opt:case-name opt d '())
		    n0
		    ,@(map (lambda (j)
			      `(- (f ,@(map (lambda (k)
					       (if (= j k) 1 0))
					  ks))
				  n0))
			 ks)))))))


;;; ((opt:make-optimizer opt) d)
;;; returns linear optimizer _code_ for d dimensions.
;;;
;;; The 17 is just random, not magic: it is not one of -1, 0, 1, so it
;;; takes the else branches in case expressions.

(define (opt:make-optimizer type opt)
   (lambda (d)
      (let ((ns (opt:names d "n")))
	 `(begin
	     (define (,(opt:case-name opt d '()) ,@ns)
		(if (= n0 0)
		   ,(opt:make-giant-case type opt d '(0) (cdr ns) ns)
		   ,(opt:make-giant-case type opt d '(17) (cdr ns) ns)))
	     ,@(opt:make-little-cases type opt d ns (opt:names d "k"))))))

(define (opt:names d n)
   (do      ((ns '() (cons (string-append n (number->string k)) ns))
	     (k d (- k 1)))
	 ((= k 0)
	  (map string->symbol (cons (string-append n "0") ns)))))

(define (opt:make-giant-case type opt d rvs ms ns)
   (if (pair? ms)
      `(case ,(car ms)
	  ,@(map (lambda (v)
		    `(,(case v
			  ((-1 0 1) `(,v))
			  (else 'else))
		      ,(opt:make-giant-case type opt d
			  (cons v rvs)
			  (cdr ms)
			  ns)))
	       '(-1 0 1 17)))
      (let ((vs (reverse rvs)))
	 `(,(opt:case-name opt d vs)
	   ,@(apply append
		(map (lambda (n v)
			(case v
			   ((-1 0 1) '())
			   (else `(,n))))
		   ns
		   vs))))))

(define (opt:case-name opt d vs)
   (do      ((s (symbol->string (opt (number->string d)))
		(string-append s "."
		   (case (car ns)
		      ((-1 0 1) (number->string (car ns)))
		      (else "n"))))
	     (ns vs (cdr ns)))
	 ((null? ns)
	  (string->symbol s))))

(define (opt:make-little-cases type opt d ns ks)
   (map (lambda (vs)
	   (opt:make-little-case type opt d ns vs ks))
      (apply opt:cross '(0 17)
	 (vector->list (make-vector d '(-1 0 1 17))))))

;;; (opt:cross list ...)
;;; returns a list of lists that are all combinations of elements of
;;; lists.

(define (opt:cross . lists)
   (if (null? lists)
      '(())
      (apply append
	 (map (lambda (pre)
		 (map (lambda (suf)
			 (cons pre suf))
		    (apply opt:cross (cdr lists))))
	    (car lists)))))

(define (opt:make-little-case type opt d ns vs ks)
   (let ((exp `(+ ,(case (car vs)
		     ((-1 0 1) (car vs))
		     (else (car ns)))
		  ,@(map (lambda (n v k)
			   `(* ,(case v
				  ((-1 0 1) v)
				  (else n))
			       ,k))
			 (cdr ns)
			 (cdr vs)
			 (cdr ks)))))
      `(define (,(opt:case-name opt d vs)
		,@(append
		     (if (= (car vs) 0)
			'()
			`(,(car ns)))
		     (apply append
			(map (lambda (n v)
				(case v
				   ((-1 0 1) '())
				   (else `(,n))))
			   (cdr ns)
			   (cdr vs)))))
	 ,(case type
	    ((type1)
	     `(lambda (,@(cdr ks) . _)
		,(opt:opt exp)))
	    ((type2)
	     `(lambda ,(cdr ks)
		,(opt:opt exp)))
	    ((type3)
	     `(list ,@(map (lambda (v n)
			     (case v
			       ((-1 0 1) v)
			       (else n)))
			   vs
			   ns)))
	    (else `(unknown type ,type))))))


;;; (opt:opt sum)
;;; returns an optimized version of the sum. The sum is of the
;;; form (+ n0 (* n1 k1) ...) with some of nk constants.

;(define (opt:opt sum) sum)

(define (opt:opt sum)
  (apply (lambda (+ n0 . products)
           (let ((terms (apply append
                               (case n0
                                 ((0) '())
                                 (else `(,n0)))
                               (map (lambda (term)
                                      (apply (lambda (* n k)
                                               (case n
                                                 ((-1) `((- ,k)))
                                                 ((0) '())
                                                 ((1) `(,k))
                                                 (else `((* ,n ,k)))))
                                             term))
                                    products))))
             (case (length terms)
               ((0) 0)
               ((1) (car terms))
               (else `(+ . ,terms)))))
         sum))
