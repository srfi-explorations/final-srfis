;;;;;; - Vector Manipulation Operation Library -
;;;;;; - Written by Taylor Campbell -

;;;;;; - Copyright -
;; You may do as you please with this code, as long as you refrain from
;; removing this copyright notice or holding me liable for any damages
;; that may be caused by it; and you may quote sections from it as you
;; please, as long as you credit me with the quotation.  Send questions
;; or bugs or anything of the sort to:
;; campbell (at) evdev (dot) ath (dot) cx
;; (the (at) and (dot) stuff is to prevent spammers who crawl all the
;; files on the web from picking up the actual email address.)

;; Requires: SRFI 1 (list-lib), SRFI 23 (error), SRFI 26 (cut/cute)

;;; Index of functions exported from this library:
;;
;;; Constructors:
;;
;; make-vector vector vector-tabulate vector-copy vector-iota
;;
;;; Predicates:
;;
;; vector? vector-empty? vector-nonempty? vector=
;;
;;; Accessors:
;;
;; vector-ref
;; vector-first vector-second vector-third vector-fourth vector-fifth
;; vector-sixth vector-seventh vector-eighth vector-ninth vector-tenth
;; vector-last
;; vector-take       vector-drop
;; vector-take-right vector-drop-right
;; vector-split-at
;;
;;; Miscellaneous:
;;
;; vector-length vector-copy! vector-append
;; vector-concatentate vector-concatentate*
;; vector-intersperse
;; vector-join vector-join*
;; vector-reverse vector-reverse! vector-append-reverse
;; vector-zip vector-unzip
;; vector-unzip1 vector-unzip2 vector-unzip3 vector-unzip4
;; vector-unzip5
;; vector-count
;;
;;; Iterators:
;;
;; vector-fold   vector-fold-right
;; vector-reduce vector-reduce-right
;; vector-unfold vector-unfold-right
;; vector-map vector-map!
;; vector-map-left vector-map-left!
;; vector-map-right vector-map-right!
;; vector-filter-map vector-append-map
;; vector-for-each vector-for-each-right
;;
;;; Filtering:
;;
;; vector-filter vector-partition
;; vector-remove
;;
;;; Searching:
;;
;; vector-find vector-find-tail
;; vector-take-while vector-drop-while
;; vector-span vector-break
;; vector-any vector-every
;; vector-index vector-index-right
;; vector-skip  vector-skip-right
;;
;;; Deletion:
;;
;; vector-delete vector-delete-duplicates
;;
;;; Association vectors:
;;
;; avector-search avector-searchq avector-searchv
;; avector-cons avector-copy avector-delete
;; avector-change avector-change!
;;
;; vector-set! vector-swap!
;; vector-set-first! vector-set-second! vector-set-third!
;; vector-set-fourth!
;; vector-set-fifth! vector-set-sixth! vector-set-seventh!
;; vector-set-eighth! vector-set-ninth! vector-set-tenth!
;; vector-fill!
;;
;; vector->string string->vector
;; avector->alist alist->avector
;; vector->list list->vector

;; Error checking in this file is often redundant, to help pinpoint
;; exactly where the error took place from a debugger -- many functions
;; in this file call other functions in this file, and it is often
;; preferred to have the outermost function calls report the error if
;; there is something wrong with their arguments, instead of having the
;; error be reported in some obscure corner of the file that you would
;; never think of connecting to that outermost function call.

;; Simple, runtime type checking.  In most cases in this file, the type
;; checking is done in such a way that not very much clarity is lost,
;; but at times debugging help is given up for efficiency.
;;
;; If your Scheme implementation supports better type checking (i.e.,
;; maybe its own compile-time type checker), then you can simply
;; discard all of the calls to 'check-arg' and replace it with whatever
;; your Scheme's type checker needs.
(define (check-arg pred? val caller)
  (if (pred? val)
      #t
      ;; Recurs so that if the user gets a debugger running and puts
      ;; the right value here, it will continue along nicely.
      (check-arg
       pred?
       (error (string-append (if (symbol? caller)
                                 (symbol->string caller)
                                 caller)
                             ": Bad argument")
              val
              pred?)
       caller)))

(define (check-index vec i caller)
  (check-arg vector? vec caller)
  (check-arg integer? i caller)
  (let ((s-caller (symbol->string caller)))
    (if (< i 0)
        (check-index
         vec
         (error (string-append s-caller
                               ": i out of bounds -- i < 0")
                i vec)
         caller))
    (let ((len (vector-length vec)))
      (if (>= i (vector-length vec))
          (check-index
           vec
           (error (string-append s-caller
                                 ": i out of bounds -- i >= len")
                  i len vec)
           caller)))))

(define (check-indices vec start start-name end end-name caller)
  (check-arg vector? vec caller)
  (check-arg integer? start caller)
  (check-arg integer? end caller)
  (let ((s-caller (symbol->string caller))
        (s-end-name (symbol->string end-name))
        (s-start-name (symbol->string start-name))
        (len (vector-length vec)))
    (if (> start end)
        ;; I dunno if this will work, but somehow a new start or a new
        ;; end should be inputted into the debugger, since possibly the
        ;; start is right while the end is wrong, so if it looped with
        ;; a new start, that would do nothing; and if it were vice
        ;; versa, the same could occur.
        (call-with-values
         (lambda ()
           (error (string-append s-caller
                                 ": "
                                 s-start-name
                                 "/"
                                 s-end-name
                                 " out of bounds -- "
                                 s-start-name
                                 " > "
                                 s-end-name)))
         (lambda (new-start new-end)
           (check-indices vec
                          new-start start-name
                          new-end end-name
                          caller))))
    (if (< start 0)
        (check-indices
         vec
         (error (string-append s-caller
                               ": "
                               s-start-name
                               " out of bounds -- "
                               s-start-name
                               " < 0")
                start vec)
         start-name end end-name caller))
    (if (> end len)
        (check-indices
         vec start start-name
         (error (string-append s-caller
                               ": "
                               s-end-name
                               " out of bounds -- "
                               s-end-name
                               " > len")
                end len vec)
         end-name caller))
    (if (>= start len)
        (check-indices
         vec
         (error (string-append s-caller
                               ": "
                               s-start-name
                               " out of bounds -- "
                               s-start-name
                               " >= len")
                start len vec)
         start-name end end-name caller))))

;; A few other useful functions -- producing predicates, getting
;; optional values.
(define (function-and f . rest)
  (let ((funs (map (lambda (f)
                     (and (check-arg procedure? f 'function-and) f))
                   (cons f rest))))
    (lambda args
      (every (cut apply <> args) funs))))
(define (function-or f . rest)
  (let ((funs (map (lambda (f)
                     (and (check-arg procedure? f 'function-or) f))
                   (cons f rest))))
    (lambda args
      (any (cut apply <> args) funs))))

(define (function-not f)
  (lambda args (not (apply f args))))

(define (identity x) x)

(define-syntax let-optionals*
  (syntax-rules ()
    ((_ ?rest () ?e1 ?e2 ...)
     (begin ?e1 ?e2 ...))
    ((_ ?rest ((?var ?default) . ?more) ?e1 ?e2 ...)
     (let* ((rest ?rest)
            (?var (if (null? rest) ?default (car rest)))
            (next-rest (if (null? rest) '() (cdr rest))))
       (let-optionals* next-rest ?more ?e1 ?e2 ...)))))

(define inc (cut + <> 1))

(define (nonneg-integer? x)
  (and (integer? x)
       (not (negative? x))))

(define (smallest-length vecs)
  (fold (lambda (vec current-min)
          (min (vector-length vec) current-min))
        (vector-length (car vecs))
        (cdr vecs)))

(define (check-vector-args vl caller)
  (for-each (cut check-arg vector? <> caller) vl))

;; In any case where:
;; (define name name)
;; is used, it is only to indicate that the library exports that
;; function.

;;;;;; - Constructors -

;; These two are already defined in R5RS.
(define make-vector make-vector)
(define vector vector)
;; Examples:  (make-vector 5 3)         ==> #(3 3 3 3 3)
;;            (vector 3 3 3 3 3)        ==> #(3 3 3 3 3)

;; vector-tabulate : (Integer -> Value) Integer -> Vector
;; Like make-vector, but instead of using a single value, it calls a
;; function to produce the value to initialize the slot with.
(define (vector-tabulate f len)
  (check-arg procedure? f 'vector-tabulate)
  (check-arg nonneg-integer? len 'vector-tabulate)
  (let ((new (make-vector len)))
    (do ((i 0 (+ i 1)))
        ((= i len) new)
      (vector-set! new i (f i)))))
;; Examples:  (vector-tabulate - 5)     ==> #(0 -1 -2 -3 -4)
;;            (vector-tabulate (lambda (x) (* x x)) 5)
;;                                      ==> #(0 1 4 9 16)

;; vector-copy : Vector [Integer [Integer]] -> Vector
;; Allocates a new vector with references to the same elements, or
;; creates a smaller vector with references to the same elements in a
;; given range.  No 'subvector' function is available since it is
;; really only vector-copy but with a fixed number of arguments.
(define (vector-copy vec . start+end)
  (check-arg vector? vec 'vector-copy)
  (let ((len (vector-length vec)))
    (let-optionals* start+end ((start 0) (end len))
      (check-indices vec start 'start end 'end 'vector-copy)
      (let* ((new-len (- end start))
             (new (make-vector new-len)))
        (do ((i start (+ i 1))
             (j 0 (+ j 1)))
            ((= i end) new)
          (vector-set! new j (vector-ref vec i)))))))
;; Examples:  (vector-copy '#(a b c d e f g h i))
;;                 ==> #(a b c d e f g h i)
;;            (vector-copy '#(a b c d e f g h i) 6)
;;                 ==> #(g h i)
;;            (vector-copy '#(a b c d e f g h i) 3 6)
;;                 ==> #(d e f)

;; vector-iota : Integer [Integer [Integer]] -> Vector
;; Creates a vector of the size 'count,' whose elements are as many
;; numbers as 'count,' starting at 'start' (which defaults to 0) and
;; incremented by 'step' (which defaults to 1).
(define (vector-iota count . start+step)
  (check-arg number? count 'vector-iota)
  (let-optionals* start+step ((start 0) (step 1))
    (check-arg number? start 'vector-iota)
    (check-arg number? step 'vector-iota)
    (cond
     ((zero? count) '#())
     ((zero? step) (make-vector count start))
     (else
      (let ((last-val (+ start (* (- count 1) step)))
            (new (make-vector count)))
        (do ((i (- count 1) (- i 1))
             (val last-val (- val step)))
            ((< i 0) new)
          (vector-set! new i val)))))))
;; Examples:  (vector-iota 5)                ==> #(0 1 2 3 4)
;;            (vector-iota 5 3)              ==> #(3 4 5 6 7)
;;            (vector-iota 5 3 -2)           ==> #(3 1 -1 -3 -5)
;;            (vector-iota 4 1+2i -1+3i)
;;                 ==> #(1+2i 0+5i -1+8i -2+11i)
;;              Only works if your Scheme supports complex numbers.

;;;;;; - Predicates -

;; vector? : Value -> Boolean
;; Tests if a value is a vector.
(define vector? vector?)
;; Examples:  (vector? '#(a b c))     ==> #t
;;            (vector? '(a b c))      ==> #f
;;            (vector? #t)            ==> #f
;;            (vector? '#())          ==> #t
;;            (vector? '())           ==> #f

;; vector-empty? : Vector -> Boolean
;; Tests if a vector has zero elements.
(define (vector-empty? vec)
  (check-arg vector? vec 'vector-empty?)
  (= (vector-length vec) 0))
;; Examples:  (vector-empty? '#(a))       ==> #f
;;            (vector-empty? '#(()))      ==> #f
;;            (vector-empty? '#(#()))     ==> #f
;;            (vector-empty? '#())        ==> #t

;; vector-nonempty? : Vector -> Boolean
;; Tests if a vector has more than zero elements.
(define (vector-nonempty? vec)
  (check-arg vector? vec 'vector-nonempty?)
  (not (vector-empty? vec)))
;; Examples:  (vector-nonempty? '#(a))       ==> #t
;;            (vector-nonempty? '#(()))      ==> #t
;;            (vector-nonempty? '#(#()))     ==> #t
;;            (vector-nonempty? '#())        ==> #f

;; vector= : (Value+ -> Boolean) Vector+ -> Boolean
;; Compares vectors based on its first argument.  The first argument
;; must be a function that takes as many arguments as vectors applied
;; to vector=.
;;
;; This code was influenced majorly from Olin's SRFI 1 reference
;; implementation (ok, it was basically copied, with list-specific
;; stuff changed to vector stuff.  Is this legal, or do I have to slap
;; Olin's copyright here?).
(define (vector= elt=? . vecs)
  (check-vector-args vecs 'vector=)
  (case (length vecs)
    ((0) #t)
    ((1) #t)
    (else
     (let ((len (vector-length (car vecs))))
       (check-vector-args vecs 'vector=)
       (let loop1 ((vec-a (car vecs)) (others (cdr vecs)))
         (or (null? others)
             (let ((vec-b (car others))
                   (others (cdr others)))
               ;; Fast path -- if the two are eq?, why bother checking
               ;; farther?
               (if (eq? vec-a vec-b)
                   (loop1 vec-b others)
                   (let ((len-a (vector-length vec-a))
                         (len-b (vector-length vec-b)))
                     (let loop2 ((i 0))
                       (if (>= i len-a)
                           (and (>= i len-b)
                                (loop1 vec-b others))
                           (and (< i len-b)
                                (elt=? (vector-ref vec-a i)
                                      (vector-ref vec-b i))
                                (loop2 (+ i 1))))))))))))))
;; Examples:  (vector= eq? '#(a b c d) '#(a b c d))     ==> #t
;;            (vector= eq? '#(a b c d) '#(a b d c))     ==> #f
;;            (vector= = '#(1 2 3 4 5) '#(1 2 3 4))     ==> #f
;;            (vector= = '#(1 2 3 4) '#(1 2 3 4))       ==> #t
;;            (vector= eq?)                             ==> #t
;;            (vector= eq? '#(a))                       ==> #t

;;;;;; - Accessors -

;; vector-ref : Vector Integer -> Value
;; References a value at an index in a vector.  The index must be zero
;; or positive and it must be smaller than its length (since its
;; indices are zero-based).
(define vector-ref vector-ref)
;; Example:   (vector-ref '#(a b c d) 2)     ==> c

;; vector-first through vector-tenth : Vector -> Value
;; Common vector accessors.
(define (vector-first   vec)
  (check-arg vector? vec 'vector-first)
  (check-index vec 0 'vector-first)
  (vector-ref vec 0))
(define (vector-second  vec)
  (check-arg vector? vec 'vector-second)
  (check-index vec 1 'vector-second)
  (vector-ref vec 1))
(define (vector-third   vec)
  (check-arg vector? vec 'vector-third)
  (check-index vec 2 'vector-third)
  (vector-ref vec 2))
(define (vector-fourth  vec)
  (check-arg vector? vec 'vector-fourth)
  (check-index vec 3 'vector-fourth)
  (vector-ref vec 3))
(define (vector-fifth   vec)
  (check-arg vector? vec 'vector-fifth)
  (check-index vec 4 'vector-fifth)
  (vector-ref vec 4))
(define (vector-sixth   vec)
  (check-arg vector? vec 'vector-sixth)
  (check-index vec 5 'vector-sixth)
  (vector-ref vec 5))
(define (vector-seventh vec)
  (check-arg vector? vec 'vector-seventh)
  (check-index vec 6 'vector-seventh)
  (vector-ref vec 6))
(define (vector-eighth  vec)
  (check-arg vector? vec 'vector-eighth)
  (check-index vec 7 'vector-eighth)
  (vector-ref vec 7))
(define (vector-ninth   vec)
  (check-arg vector? vec 'vector-ninth)
  (check-index vec 8 'vector-ninth)
  (vector-ref vec 8))
(define (vector-tenth   vec)
  (check-arg vector? vec 'vector-tenth)
  (check-index vec 9 'vector-tenth)
  (vector-ref vec 9))

;; vector-last : Vector -> Value
;; Returns the last element in vec.
(define (vector-last vec)
  (check-arg (function-and vector? vector-nonempty?) vec 'vector-last)
  (let ((len (vector-length vec)))
    (if (zero? len)
        (error "vector-last: expected nonempty vector"
               `(got: ,vec))
        (vector-ref vec (- len 1)))))
;; Example:   (vector-last '#(a b c))     ==> c

;; vector-take : Vector Integer -> Vector
;; Takes the first i elements in vec.
(define (vector-take vec i)
  (check-arg vector? vec 'vector-take)
  (check-arg integer? i 'vector-take)
  (check-index vec i 'vector-take)
  (let ((new (make-vector i)))
    (do ((i (- i 1) (- i 1)))
        ((negative? i) new)
      (vector-set! new i (vector-ref vec i)))))
;; Example:   (vector-take '#(1 2 3 4 5) 2)     ==> #(1 2)

;; vector-drop : Vector Integer -> Vector
;; Takes all but the first i elements of vec.
(define (vector-drop vec i)
  (check-arg vector? vec 'vector-drop)
  (check-arg integer? i 'vector-drop)
  (check-index vec i 'vector-drop)
  (let* ((len (vector-length vec))
         (new (make-vector (- len i))))
    (if (> i len)
        (error "vector-drop: i out of bounds -- i > len"
               i len))
    (do ((i i (+ i 1))
         (j 0 (+ j 1)))
        ((= i len) new)
      (vector-set! new j (vector-ref vec i)))))
;; Example:  (vector-drop '#(1 2 3 4 5) 2)     ==> #(3 4 5)

;; vector-take-right : Vector Integer -> Vector
;; Takes the last i elements of vec.
(define (vector-take-right vec i)
  (check-arg vector? vec 'vector-take-right)
  (check-arg nonneg-integer? i 'vector-take-right)
  (let ((new (make-vector i))
        (len (vector-length vec)))
    ;; Don't use check-index here, because i can actually be len, while
    ;; with check-index, it has to be less than len.
    (if (> i len)
        (vector-take-right
         vec
         (error "vector-take-right: i out of bounds -- i > len"
                i len vec)))
    (do ((i (+ i 1) (+ i 1))
         (j 0 (+ j 1)))
        ((>= i len) new)
      (vector-set! new j (vector-ref vec i)))))
;; Example:   (vector-take-right '#(a b c d e) 2)     ==> #(d e)

;; vector-drop-right : Vector Integer -> Vector
;; Takes all but the last i elements from vec.
(define (vector-drop-right vec i)
  (check-arg vector? vec 'vector-drop-right)
  (check-arg nonneg-integer? i 'vector-drop-right)
  (let ((new (make-vector (+ i 1)))
        (len (vector-length vec)))
    ;; Again, don't use check-index here.
    (if (> i len)
        (vector-drop-right
         vec
         (error "vector-drop-right: i out of bounds -- i > len"
                i len)))
    (do ((i i (- i 1)))
        ((negative? i) new)
      (vector-set! new i (vector-ref vec i)))))
;; Example:   (vector-drop-right '#(a b c d e) 2)     ==> #(a b c)

;; vector-split-at : Vector Integer -> Vector Vector
;; Splits vec at index i.
(define (vector-split-at vec i)
  (check-arg vector? vec 'vector-split-at)
  (check-arg nonneg-integer? i 'vector-spit-at)
  (let ((len (vector-length vec)))
    (cond
     ((= i len) (values vec '#()))
     ((zero? i) (values '#() vec))
     (else
      (check-index vec i 'vector-split-at)
      (let ((first (make-vector i))
            (second (make-vector (- len i))))
        (let loop ((to 0) (from 0) (first? #t))
          (cond
           ((= from len) (values first second))
           (first?
            (if (= to i)
                (loop 0 from #f)
                (begin
                  (vector-set! first to (vector-ref vec from))
                  (loop (+ to 1) (+ from 1) #t))))
           (else
            (vector-set! second to (vector-ref vec from))
            (loop (+ to 1) (+ from 1) #f)))))))))
;; Example:   (vector-split-at '#(a b c d e f g h) 3)
;;                 ==> #(a b c) #(d e f g h)

;;;;;; - Miscellaneous -

;; vector-length : Vector -> Integer
;; Returns the length of a vector.
(define vector-length vector-length)
;; Example:   (vector-length '#(a b c))     ==> 3

;; vector-copy! : Vector Integer Vector [Integer [Integer]]
;;   -> Unspecified
;; Copies a vector destructively into target from vec, where the first
;; element mutated in target is tstart, the first element taken from
;; vec is fstart which defaults to 0, and the last element taken from
;; vec is fend, which defaults to the length of vec.  It is an error to
;; apply vector-copy! to a larger vec than target.
(define (vector-copy! target tstart vec . fstart+fend)
  (check-arg vector? target 'vector-copy!)
  (check-arg nonneg-integer? tstart 'vector-copy!)
  (check-arg vector? vec 'vector-copy)
  (let ((target-len (vector-length target))
        (vec-len (vector-length vec)))
    (let-optionals* fstart+fend ((fstart 0) (fend (- vec-len 1)))
      (check-arg nonneg-integer? fstart 'vector-copy!)
      ;; fend can't be zero.
      (check-arg integer? fend 'vector-copy!)
      ;; Here, more specific bounds checking can be done, so
      ;; check-indices isn't used.
      (if (> fstart fend)
          (error "vector-copy!: fstart out of bounds -- fstart > fend"
                 fstart fend `(vec was: ,vec) `(target was: ,target)))
      (if (> fstart vec-len)
          (error "vector-copy!: fstart out of bounds -- fstart > len"
                 fstart vec-len vec `(target was: ,target)))
      (if (> fend vec-len)
          (error "vector-copy!: fend out of bounds -- fend > len"
                 fend vec-len vec `(target was: ,target)))

      (do ((to tstart (+ to 1))
           (from fstart (+ from 1)))
          ((= from fend))
        (vector-set! target to (vector-ref vec from))))))
;; Example:   (let ((target (vector 1 3 2 4 5 6 7)))
;;              (vector-copy! target 1 '#(-1 0 1 2 3 4 5) 3 5)
;;              target)     ==> #(1 2 3 4 5 6 7)

;; vector-append : Vector* -> Vector
;; Appends its arguments together.
(define (vector-append . vecs)
  (check-vector-args vecs 'vector-append)
  (vector-concatentate* vecs))
;; Examples:  (vector-append '#(x) '#(y))           ==> #(x y)
;;            (vector-append '#(a) '#(b c d)        ==> #(a b c d)
;;            (vector-append '#(a (b)) '#((c)))     ==> #(a (b) (c))

;; vector-concatentate : (Vector-of Vector) -> Vector
;; Equivalent to:
;;   (apply vector-append (vector->list vector-of-vectors))
;; but no conversions to lists are involved, and some implementations
;; don't support large enough argument lists at times to concatentate
;; them with a single call to vector-append, so vector-concatentate
;; isn't implemented that way.
(define (vector-concatentate vector-of-vectors . delim+grammar)
  (check-arg vector? vector-of-vectors 'vector-concatentate)
  (vector-every (cut check-arg vector? <> 'vector-concatentate)
                vector-of-vectors)
  (let* ((len (vector-fold (lambda (vec len)
                             (+ len (vector-length vec)))
                           0 vector-of-vectors))
         (new (make-vector len)))
    (let loop1 ((current-vec 0) (to 0))
      (if (= current-vec len)
          new
          (let* ((vec (vector-ref vector-of-vectors current-vec))
                 (len (vector-length vec)))
            (check-arg vector? vec 'vector-concatentate)
            (let loop2 ((from 0) (to to))
              (cond
               ((< from len)
                (vector-set! new to (vector-ref vec from))
                (loop2 (+ from 1) (+ to 1)))
               (else (loop1 (+ current-vec 1) to)))))))))
;; Example:   (vector-concatentate '#(#(a b) #(c d)))
;;                 ==> #(a b c d)

;; vector-concatentate* : (List-of Vector) -> Vector
;; Similar to vector-concatentate, but given a list of vectors instead
;; of a vector of vectors.
(define (vector-concatentate* list-of-vectors)
  (check-arg proper-list? list-of-vectors 'vector-concatentate*)
  (check-vector-args list-of-vectors 'vector-concatentate*)
  (let* ((len (fold (lambda (vec len) (+ len (vector-length vec)))
                    0 list-of-vectors))
         (new (make-vector len)))
    (let loop1 ((vecs list-of-vectors) (to 0))
      (if (null? vecs)
          new
          (let* ((vec (car vecs))
                 (len (vector-length vec)))
            (check-arg vector? vec 'vector-concatentate*)
            (let loop2 ((from 0) (to to))
              (cond
               ((< from len)
                (vector-set! new to (vector-ref vec from))
                (loop2 (+ from 1) (+ to 1)))
               (else (loop1 (cdr vecs) to)))))))))
;; Example:   (vector-concatentate* '(#(a b) #(c d)))
;;                 ==> #(a b c d)

;; At first, I thought these functions might be a good idea, but then
;; I couldn't really think of a situation in which they're used, and
;; the level of complexity that they're engulfed in makes them even
;; less useful.  Therefore they are commented out, from both this
;; reference implementation and the SRFI document.

;; vector-intersperse :
;;   Vector (Value -> Value) (Value -> Value) Value [Symbol] -> Vector
;; For each index in vec, put the result of applying f to the current
;; seed value, which is generated by applying g to the last seed value,
;; the first of which is 'seed,' between the value at that index and
;; the value at the next index.  A complicated description -- see the
;; examples.
;;
;; The grammar argument can be one of the symbols infix, suffix, or
;; prefix.  In the case of infix, a delimiter is created between each
;; element of vec, and only in those places.  In the case of suffix, a
;; delimiter can be found as the last element of the returned vector,
;; and in the case of suffix, it can be found as the first element.
; (define (vector-intersperse vec f g seed . maybe-grammar)
;   (check-arg vector? vec 'vector-intersperse)
;   (check-arg procedure? f 'vector-intersperse)
;   (check-arg procedure? g 'vector-intersperse)
;   (if (vector-empty? vec)
;       '#()
;       (let-optionals* maybe-grammar ((grammar 'infix))
;         ((case grammar
;            ((infix) infix-vector-intersperse)
;            ((suffix) suffix-vector-intersperse)
;            ((prefix) prefix-vector-intersperse)
;            (else
;             (error
;              "vector-intersperse: expected infix, suffix, or prefix"
;              grammar)))
;          vec f g seed))))
;; Examples:  (vector-intersperse '#(a b c d e f g h i)
;;                                (lambda (x) (* x x))
;;                                inc
;;                                1 'infix)
;;                 ==> #(a 1 b 4 c 9 d 16 e 25 f 36 g 49 h 64 i)
;;            (vector-intersperse '#(a b c d e f g h i)
;;                                (lambda (x) (* x x))
;;                                inc
;;                                1 'prefix)
;;                 ==> #(1 a 4 b 9 c 16 d 25 e 36 f 49 g 64 h 81 i)
;;            (vector-intersperse '#(a b c d e f g h i)
;;                                (lambda (x) (* x x))
;;                                inc
;;                                1 'suffix)
;;                 ==> #(a 1 b 4 c 9 d 16 e 25 f 36 g 49 h 64 i 81)

;; intersperse -- helper function for vector-join*.  Perhaps it should
;; have been in SRFI 1?
; (define (intersperse lst f g seed . maybe-grammar)
;   (check-arg proper-list? lst 'intersperse)
;   (check-arg procedure? f 'intersperse)
;   (check-arg procedure? g 'intersperse)
;   (if (null-list? lst)
;       '()
;       (let-optionals* maybe-grammar ((grammar 'infix))
;         ((case grammar
;            ((infix) infix-intersperse)
;            ((suffix) suffix-intersperse)
;            ((prefix) prefix-intersperse)
;            (else
;             (error "intersperse: exected infix, suffix, or prefix"
;                    grammar)))
;          lst f g seed))))

; (define (infix-intersperse lst f g seed)
;   (if (null-list? lst)
;       '()
;       (let loop ((k (lambda (x) x))
;                  (lst lst)
;                  (z seed))
;         (if (null-list? (cdr lst))
;             (k (list (car lst)))
;             (loop (lambda (x) (k (cons* (car lst) (f z) x)))
;                   (cdr lst)
;                   (g z))))))

; (define (suffix-intersperse lst f g seed)
;   (let loop ((k (lambda (x) x))
;              (lst lst)
;              (z seed))
;     (if (null-list? lst)
;         (k '())
;         (loop (lambda (x) (k (cons* (car lst) (f z) x)))
;               (cdr lst)
;               (g z)))))

; (define (prefix-intersperse lst f g seed)
;   (let loop ((k (lambda (x) x))
;              (lst lst)
;              (z seed))
;     (if (null-list? lst)
;         (k '())
;         (loop (lambda (x) (k (cons* (f z) (car lst) x)))
;               (cdr lst)
;               (g z)))))

; (define (infix-vector-intersperse vec f g seed)
;   (let* ((len (- (* (vector-length vec) 2) 1))
;          (new (make-vector len)))
;      (vector-set! new 0 (vector-ref vec 0))
;      (do ((i 2 (+ i 2))
;           (j 1 (+ j 1))
;           (x seed (g x)))
;          ((> i len) new)
;        (vector-set! new i (vector-ref vec j))
;        (vector-set! new (- i 1) (f x)))))

; (define (suffix-vector-intersperse vec f g seed)
;   (let* ((real-len (vector-length vec))
;          (len (* real-len 2))
;          (new (make-vector len)))
;     (do ((i 0 (+ i 2))
;          (j 0 (+ j 1))
;          (x seed (g x)))
;         ((= j real-len) new)
;       (vector-set! new i (vector-ref vec j))
;       (vector-set! new (+ i 1) (f x)))))

; (define (prefix-vector-intersperse vec f g seed)
;   (let* ((real-len (vector-length vec))
;          (len (* real-len 2))
;          (new (make-vector len)))
;     (do ((i 1 (+ i 2))
;          (j 0 (+ j 1))
;          (x seed (g x)))
;         ((> i len) new)
;       (vector-set! new i (vector-ref vec j))
;       (vector-set! new (- i 1) (f x)))))

;; Vector-join :
;;   (Vector-of Vector) [(Value -> Vector) [(Value -> Value) [Symbol]]]
;;     -> Vector
;; A combination of vector-intersperse and vector-join -- concatentates
;; a vector of vectors, interspersing another generated vector between
;; each before the concatentation.  If only one argument is applied,
;; vector-join is equivalent to vector-concatentate.
; (define (vector-join vector-of-vectors . maybe-f+g+seed+grammar)
;   (check-arg vector? vector-of-vectors 'vector-join)
;   (vector-for-each (cut check-arg vector? <> 'vector-join)
;                    vector-of-vectors)
;   (if (null? maybe-f+g+seed+grammar)
;       (vector-concatentate vector-of-vectors)
;       (let-optionals* maybe-f+g+seed+grammar
;                       ((f identity)
;                        (g identity)
;                        (seed '#())
;                        (grammar 'infix))
;         (vector-concatentate (vector-intersperse vector-of-vectors
;                                                  f g seed
;                                                  grammar)))))
;; Examples:  (vector-join '#(#(a b) #(c d) #(e))
;;                         identity
;;                         identity
;;                         '#(x y)
;;                         'infix)
;;                 ==> #(a b x y c d x y e)
;;            (vector-join '#(#(a b) #() #(e f))
;;                         (lambda (x) (vector (expt x 2) (expt 2 x)))
;;                         inc
;;                         0
;;                         'suffix)
;;                 ==> #(a b 0 1 1 2 e f 4 4)

;; vector-join* :
;;   (List-of Vector) [(Value -> Vector) [(Value -> Value) [Value]]]
;;     -> Vector
;; vector-join* is to vector-join as vector-concatentate* is to
;; vector-concatentate -- vector-join* takes a list of vectors instead
;; of a vector of vectors and produces the same type of result as
;; vector-join.
; (define (vector-join* list-of-vectors . maybe-f+g+seed+grammar)
;   (check-arg proper-list? list-of-vectors 'vector-join*)
;   (check-vector-args list-of-vectors 'vector-join*)
;   (if (null? maybe-f+g+seed+grammar)
;       (vector-concatentate* list-of-vectors)
;       (let-optionals* maybe-f+g+seed+grammar
;                       ((f identity)
;                        (g identity)
;                        (seed '#())
;                        (grammar 'infix))
;         (vector-concatentate* (intersperse list-of-vectors
;                                            f g seed
;                                            grammar)))))
;; Examples:  (vector-join* '(#(a b) #(c d) #(e))
;;                          identity
;;                          identity
;;                          '#(x y)
;;                          'infix)
;;                 ==> #(a b x y c d x y e)
;;            (vector-join* '(#(a b) #() #(e f))
;;                          (lambda (x) (vector (expt x 2) (expt 2 x)))
;;                          inc
;;                          0
;;                          'suffix)
;;                 ==> #(a b 0 1 1 2 e f 4 4)


;; vector-reverse : Vector [Integer [Integer]] -> Vector
;; Reverses the elements in vec, but not deeply.
(define (vector-reverse vec . start+end)
  (check-arg vector? vec 'vector-reverse)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-reverse)
    (let* ((len (- end start))
           (new (make-vector len)))
      (do ((i (- end 1) (- i 1))
           (j 0 (+ j 1)))
          ((= j len) new)
        (vector-set! new j (vector-ref vec i))))))
;; Examples:  (vector-reverse '#(a b c))     ==> #(c b a)
;;            (vector-reverse '#(a (b c) d (e (f))))
;;                                           ==> #((e (f)) d (b c) a)
;;            (vector-reverse '#(a b c d e f g h i) 3 6)
;;                                           ==> #(f e d)

;; vector-reverse! : Vector [Integer [Integer]] -> Vector
;; Destructive variant of vector-reverse.
(define (vector-reverse! vec . start+end)
  (check-arg vector? vec 'vector-reverse!)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-reverse!)
    (do ((i (- end 1) (- i 1))
         (j start (+ j 1)))
        ((<= i j) vec)
      (vector-swap! vec i j))))
;; Example:   (let ((x (vector-copy '#(a b c d e f g h i))))
;;              (vector-reverse! x)
;;              x)     ==> #(i h g f e d c b a)

;; vector-append-reverse : Vector Vector -> Vector
;; Reverses rev-head and prepends it to tail.
(define (vector-append-reverse rev-head tail)
  (check-arg vector? rev-head 'vector-append-reverse)
  (check-arg vector? tail 'vector-append-reverse)
  (vector-append (vector-reverse rev-head) tail))
;; Example:   (vector-append-reverse '#(c b a) '#(d e))
;;                 ==> #(a b c d e)

;; vector-zip : Vector+ -> Vector
;; Produce a vector based on its arguments.  As it is difficult to
;; describe with English, see the examples.
(define (vector-zip vec . rest)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-zip)
    (apply vector-map vector vecs)))
;; Examples:  (vector-zip '#(one two three) '#(1 2 3) '#(odd even odd))
;;                 ==> #(#(one 1 odd) #(two 2 even) #(three 3 odd))
;;            (vector-zip '#(1 2 3))     ==> #(#(1) #(2) #(3))

;; vector-unzip : (Vector-of Vector) Integer -> Vector+
;; Reverses the process of zipping.  See examples.
(define (vector-unzip vec i)
  (check-arg (function-and vector? (cut vector-every vector? <>))
             vec 'vector-unzip)
  (check-arg nonneg-integer? i 'vector-unzip)
  (let loop ((vecs '()) (i (- i 1)))
    (if (negative? i)
        (apply values vecs)
        (loop (cons (vector-map (cut vector-ref <> i) vec)
                    vecs)
              (- i 1)))))
;; Example:   (vector-unzip (vector-zip '#(1 2 3) '#(one two three)) 2)
;;                 ==> #(1 2 3) #(one two three)

;; Convenient abbreviations for common unzipping.
(define (vector-unzip1 vec)
  (check-arg vector? vec 'vector-unzip1)
  (vector-unzip vec 0))
(define (vector-unzip2 vec)
  (check-arg vector? vec 'vector-unzip2)
  (vector-unzip vec 1))
(define (vector-unzip3 vec)
  (check-arg vector? vec 'vector-unzip3)
  (vector-unzip vec 2))
(define (vector-unzip4 vec)
  (check-arg vector? vec 'vector-unzip4)
  (vector-unzip vec 3))
(define (vector-unzip5 vec)
  (check-arg vector? vec 'vector-unzip5)
  (vector-unzip vec 5))

;; vector-count : (Value+ -> Boolean) Vector+ -> Integer
;; Finds the number of elements in all of the vectors applied that
;; satisfy the predicate 'pred?'.  For each vector applied, 'pred?'
;; should take one more argument -- if four vectors are applied,
;; 'pred?' should take four arguments
(define (vector-count pred? vec . rest)
  (check-arg procedure? pred? 'count)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-count)
    (let ((len (smallest-length vecs)))
      (do ((i 0 (+ i 1))
           (counter 0
                    (if (apply pred? (map (cut vector-ref <> i) vecs))
                        (+ counter 1)
                        counter)))
          ((= i len) counter)))))
;; Examples:  (vector-count even? '#(3 1 4 1 5 9 2 5 6))
;;                 ==> 3
;;            (vector-count < '#(1 2 4 8) '#(2 4 6 8 10 12 14 16))
;;                 ==> 3

;;;;;; - Iterators -

;; vector-fold : (Value Value -> Value) Value Vector
;; The fundamental vector iterator.  
(define (vector-fold kons knil vec . rest)
  (check-arg procedure? kons 'vector-fold)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-fold)
    (let ((len (smallest-length vecs)))
      (do ((i 0 (+ i 1))
           (knil knil
                 (apply kons
                        (append (map (cut vector-ref <> i)
                                     vecs)
                                (list knil)))))
          ((= i len) knil)))))
;; Example:   (vector-fold-right (lambda (s m)
;;                                 (max (string-length s) m))
;;                               0 '#("foo" "bar" "baz" "quux" "zot"))
;;                 ==> 4

;; vector-fold-right : (Value+ Value -> Value) Value Vector+ -> Value
;; Traverses the vector right to left, unlike left to right in the case
;; of vector-fold.
(define (vector-fold-right kons knil vec . rest)
  (check-arg procedure? kons 'vector-fold-right)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-fold-right)
    (let ((len (smallest-length vecs)))
      (let loop ((i (- len 1)) (knil knil))
        (if (negative? i)
            knil
            (loop (- i 1)
                  (apply kons
                         (append (map (cut vector-ref <> i)
                                      vecs)
                                 (list knil)))))))))
;; Example:   (vector-fold-right cons '() '#(a b c d))
;;                 ==> (a b c d)
;;                Convert a vector to a list.

;; vector-reduce : (Value Value -> Value) Value Vector
;; If vec is empty, then: ridentity is returned; otherwise equivalent
;; to:
;;   (vector-fold f (vector-first vec) (vector-drop vec 1))
;; But slightly faster, since it doesn't have to deal with multiple
;; arguments.
(define (vector-reduce f ridentity vec)
  (check-arg procedure? f 'vector-reduce)
  (check-arg vector? vec 'vector-reduce)
  (if (vector-empty? vec)
      ridentity
      (let ((len (vector-length vec)))
        (do ((i 1 (+ i 1))
             (x (vector-first vec) (f (vector-ref vec i) x)))
            ((= i len) x)))))
;; Example:   (vector-reduce max 0 nums)
;;              In case 'nums' is empty, 0 is automatically returned
;;              and max is never applied.  This example really doesn't
;;              affect performance much when compared with fold, but if
;;              'f' involves complicated operations like database
;;              queries, it could be a key difference in efficiency.

;; vector-reduce-right : (Value Value -> Value) Value Vector
;; Traverses the vector right to left unlike vector-reduce.
(define (vector-reduce-right f ridentity vec)
  (check-arg procedure? f 'vector-reduce-right)
  (check-arg vector? vec 'vector-reduce-right)
  (if (vector-empty? vec)
      ridentity
      (let ((len (vector-length vec)))
        (do ((i (- len 2) (- i 1))
             (x (vector-last vec) (f (vector-ref vec i) x)))
            ((negative? i) x)))))
;; Example:   (vector-reduce-right append '() vector-of-lists)
;;              Append a vector of lists, much like:
;;                (apply append (vector->list vector-of-lists)
;;              but undoubtedly much safer (since many Schemes only let
;;              you pass a certain number of arguments to any given
;;              function).

;; vector-unfold :
;;   (Value -> Boolean) (Value -> Value) (value -> Value) Value
;;     -> Vector
;; The fundamental vector constructor.
;;
;; Since the size of the vector is unknown when the function is called,
;; list->vector and the list unfold function are used, because
;; otherwise a new vector would need to be allocated, whose size is one
;; greater than that of the previous vector, for every iteration.
(define (vector-unfold done? gen inc seed)
  (check-arg procedure? done? 'vector-unfold)
  (check-arg procedure? gen 'vector-unfold)
  (check-arg procedure? inc 'vector-unfold)
  (list->vector (unfold done? gen inc seed)))
;; Example:   (vector-unfold (lambda (x) (> x 10))
;;                           (lambda (x) (* x x))
;;                           (lambda (x) (+ x 1))
;;                           1)
;;              Returns a vector of all the integers' squares from 1 to
;;              10.

;; vector-unfold-right :
;;   (Value -> Boolean) (Value -> Value) (Value -> Value) Value
;;     -> Vector
;; Creates a vector from right to left instead of left to right.
(define (vector-unfold-right p f g seed)
  (check-arg procedure? p 'vector-unfold-right)
  (check-arg procedure? f 'vector-unfold-right)
  (check-arg procedure? g 'vector-unfold-right)
  (list->vector (unfold-right p f g seed)))

;; vector-map : (Value+ -> Value) Vector+ -> Vector
;; Creates a vector by applying f to each element of the vectors.
;; Order of application is unspecified.  The iteration ends when the
;; end of the shortest vector is reached.
(define (vector-map f vec . rest)
  (check-arg procedure? f 'vector-map)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-map)
    (let* ((len (smallest-length vecs))
           (new (make-vector len)))
      (do ((i 0 (+ i 1)))
          ((= i len) new)
        (vector-set! new i (apply f (map (cut vector-ref <> i)
                                         vecs)))))))
;; Examples:  (vector-map (lambda (x) (* x x)) '#(1 2 3 4))
;;                 ==> #(1 4 9 16)
;;            (vector-map * '#(1 2 3 4 5) '#(5 4 3 2 1))
;;                 ==> #(5 8 9 8 5)

;; vector-map! : (Value+ -> Value) Vector+ -> Unspecific
;; Destructive variant of vector-map!.  
(define (vector-map! f vec . rest)
  (check-arg procedure? f 'vector-map)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-map!)
    (let ((len (smallest-length vecs)))
      (do ((i 0 (+ i 1)))
          ((= i len) vec)
        (vector-set! vec i (apply f (map (cut vector-ref <> i)
                                         vecs)))))))

;; vector-map-left : (Value+ -> Value) Vector+ -> Vector
;; An anomaly of names in this file -- the '-left' is explicit here,
;; because vector-map's order of application is unspecified, while here
;; it's specified to be left to right.  The iteration ends when the end
;; of the shortest vector is hit.
;;
;; In this implementation, they're actually the same.
(define vector-map-left vector-map)
;; Example:   (let ((count 0))
;;              (vector-map-left (lambda (ignored)
;;                                 (set! count (+ count 1))
;;                                 count)
;;                               '#(a b c d)))
;;                 ==> #(1 2 3 4)

;; vector-map-left! : (Value+ -> Value) Vector -> Unspecific
;; Destructive variant of vector-map-left!, and since vector-map and
;; vector-map! are implemented similarly, this, too, is actually the
;; same as vector-map! in this implementation, as vector-map is the
;; same as vector-map-left.
(define vector-map-left! vector-map!)

;; vector-map-right : (Value+ -> Value) Vector+ -> Vector
;; Like vector-map-left, but traverses the vector from right to left,
;; and requires all the vectors to all have the same length.
(define (vector-map-right f vec . rest)
  (check-arg procedure? f 'vector-map-right)
  (check-arg vector? vec 'vector-map-right)
  (let ((len (vector-length vec)))
    (for-each
     (lambda (v)
       (check-arg vector? v 'vector-map-right)
       (if (not (= (vector-length v) len))
           (error
            "vector-map-right: all vectors must have the same length"
            len v)))
     rest)
    (let ((vecs (cons vec rest)))
      (let ((new (make-vector len)))
        (do ((i (- len 1) (- i 1)))
            ((negative? i) new)
          (vector-set! new i (apply f (map (cut vector-ref <> i)
                                           vecs))))))))
;; Example:   (let ((count 0))
;;              (vector-map-right (lambda (ignored)
;;                                  (set! count (+ count 1))
;;                                  count)
;;                                '#(a b c d)))
;;                 ==> #(4 3 2 1)

;; vector-map-right : (Value+ -> Value) Vector+ -> Unspecific
;; Destructive variant of vector-map-right.
(define (vector-map-right! f vec . rest)
  (check-arg procedure? f 'vector-map-right!)
  (check-arg vector? vec 'vector-map-right!)
  (let ((len (vector-length vec)))
    (for-each
     (lambda (v)
       (check-arg vector? v 'vector-map-right!)
       (if (not (= (vector-length v) len))
           (error
            "vector-map-right!: all vectors must have the same length"
            len v)))
     rest)
    (let ((vecs (cons vec rest)))
      (do ((i (- len 1) (- i 1)))
          ((negative? i) vec)
        (vector-set! vec i (apply f (map (cut vector-ref <> i)
                                         vecs)))))))

;; vector-map/index : (Value+ Integer -> Value) Vector+ -> Vector
;; Applies f to each element in all the vectors, just like vector-map,
;; but also applies it to the index of those elements.
(define (vector-map/index f vec . rest)
  (check-arg procedure? f 'vector-map)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-map/index)
    (let* ((len (smallest-length vecs))
           (new (make-vector len)))
      (do ((i 0 (+ i 1)))
          ((= i len) new)
        (vector-set! new i
                     (apply f (append (map (cut vector-ref <> i) vecs)
                                      (list i))))))))
;; Example:   (vector-map/index (lambda (x y) (+ x y)) '#(1 2 3 4))
;;                 ==> #(1 3 5 7)

;; vector-map/index! : (Value+ Integer -> Value) Vector+ -> Unspecific
;; Destructive variant of vector-map/index.
(define (vector-map/index! f vec . rest)
  (check-arg procedure? f 'vector-map/index!)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-map/index)
    (let ((len (smallest-length vecs)))
      (do ((i 0 (+ i 1)))
          ((= i len) vec)
        (vector-set! vec i
                     (apply f (append (map (cut vector-ref <> i) vecs)
                                      (list i))))))))

;; vector-for-each : (Value+ -> Value) Vector+ -> Unspecified
;; Applies f to each element of the vectors, but returns unspecified,
;; and no accumulated vector is produced.  Order of application is
;; explicitly left to right.  The iteration stops when the end of the
;; shortest vector is reached.
(define (vector-for-each f vec . rest)
  (check-arg procedure? f 'vector-for-each)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-for-each)
    (let ((len (smallest-length vecs)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (apply f (map (cut vector-ref <> i) vecs))))))
;; Example:   (vector-for-each (lambda (x) (display x) (newline))
;;                             '#("foo" "bar" "baz" "quux" "zot"))
;;              displays: foo
;;                        bar
;;                        baz
;;                        quux
;;                        zot

;; vector-for-each-right : (Value+ -> Value) Vector+ -> Unspecified
;; Like vector-for-each, but order of application is explicitly right
;; to left, and all vectors must have the same length.
(define (vector-for-each-right f vec . rest)
  (check-arg procedure? f 'vector-for-each-right)
  (check-arg vector? vec 'vector-for-each-right)
  (let ((len (vector-length vec)))
    (for-each
     (lambda (v)
       (check-arg vector? v 'vector-for-each-right)
       (if (not (= (vector-length v) len))
           (error
            "vector-for-each-right: all vectors must have same length"
            len v)))
     rest)
    (let ((vecs (cons vec rest)))
      (do ((i (- len 1) (- i 1)))
          ((negative? i))
        (apply f (map (cut vector-ref <> i) vecs))))))
;; Example:   (vector-for-each-right (lambda (x) (display x) (newline))
;;                                   '#("foo" "bar" "baz" "quux"))
;;              displays: quux
;;                        baz
;;                        bar
;;                        foo

;; vector-append-map : (Value+ -> Value) Vector+ -> Vector
;; Appends the result of mapping f to each vector.
(define (vector-append-map f vec . rest)
  (check-arg procedure? f 'vector-append-map)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-append-map)
    (vector-concatentate (apply vector-map f vec rest))))
;; Example:   (vector-append-map (lambda (x) (vector x (- x)))
;;                               '#(1 3 8))
;;                 ==> #(1 -1 3 -3 8 -8)

;; vector-filter-map (Value+ -> Boolean) Vector+ -> Vector
;; Returns essentially:
;;   (vector-filter pred? (apply vector-map pred? vec rest))
;; But shouldn't iterate through the vector twice.
(define (vector-filter-map f vec . rest)
  (check-arg procedure? f 'vector-filter-map)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-filter-map)
    (let* ((len (smallest-length vecs))
           (new (make-vector len)))
      (let loop ((to 0) (from 0))
        (if (= from len)
            (if (= to len)
                new
                (vector-copy new 0 to))
            (let ((x (apply f (map (cut vector-ref <> from) vecs))))
              (if x
                  (begin
                    (vector-set! new to x)
                    (loop (+ to 1) (+ from 1)))
                  (loop to (+ from 1)))))))))
;; Example:   (vector-filter-map (lambda (x) (and (number? x) (* x x)))
;;                               '#(a 1 b 3 c 7))
;;                 ==> #(1 9 49)

;;;;;; - Filtering -

;; vector-filter : (Value -> Boolean) Vector
;; Removes all elements of vec which satisfy pred?.
(define (vector-filter pred? vec . start+end)
  (check-arg procedure? pred? 'vector-filter)
  (check-arg vector? vec 'vector-filter)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-filter)
    (let* ((search-len (- end start))
           (new (make-vector search-len)))
      (let loop ((to 0) (from 0))
        (if (= from search-len)
            (if (= to search-len)
                new
                (vector-copy new 0 to))
            (let ((x (vector-ref vec from)))
              (if (pred? x)
                  (begin
                    (vector-set! new to x)
                    (loop (+ to 1) (+ from 1)))
                  (loop to (+ from 1)))))))))
;; Example:   (vector-filter symbol? '#(a 1 b 2 c 3))     ==> #(a b c)

;; vector-partition : (Value -> Boolean) Vector -> Vector Vector
;; Given a predicate and a vector, returns a vector of all the elements
;; that satisfy the predicate in the vector, and also a vector of all
;; those that do -not- satisfy the predicate.
(define (vector-partition pred? vec . start+end)
  (check-arg procedure? pred? 'vector-partition)
  (check-arg vector? vec 'vector-partition)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-partition)
    (let* ((search-len (- end start))
           (filtered-in (make-vector search-len))
           (filtered-out (make-vector search-len)))
      (let loop ((in 0) (out 0) (from start))
        (if (= from end)
            ;; A little bit of optimisation here, so vector-copy isn't
            ;; called needlessly.
            (cond
             ((= in search-len)
              (values filtered-in '#()))
             ((= out search-len)
              (values '#() filtered-out))
             (else
              (values (vector-copy filtered-in 0 in)
                      (vector-copy filtered-out 0 out))))
            (let ((x (vector-ref vec from)))
              (if (pred? x)
                  (begin
                    (vector-set! filtered-in in x)
                    (loop (+ in 1) out (+ from 1)))
                  (begin
                    (vector-set! filtered-out out x)
                    (loop in (+ out 1) (+ from 1))))))))))
;; Example:   (vector-partition symbol? '#(one 2 3 four five 6))
;;                 ==> #(one four five) #(2 3 6)

;; vector-remove : (Value -> Boolean) Vector [Integer [Integer]]
;;   -> Vector
;; Equivalent to:
;;   (vector-filter (lambda (x) (not (pred? x))) vec)
(define (vector-remove pred? vec . start+end)
  (check-arg procedure? pred? 'vector-remove)
  (check-arg vector? vec 'vector-remove)
  (apply vector-filter (function-not pred?) vec start+end))
;; Example:   (vector-remove integer? '#(a 2 b 4 c))     ==> #(a b c)

;;;;;; - Searching -

;; vector-find : (Value -> Boolean) Vector [Integer [Integer]] -> Value
;; Iterates over every element in vec, applying pred? to each element
;; along the way, and in the first instance pred? returns a true value,
;; that value is returned.  If there is no value for which pred?
;; returns a true value, #f is returned.
(define (vector-find pred? vec . start+end)
  (check-arg procedure? pred? 'vector-find)
  (check-arg vector? vec 'vector-find)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (let loop ((i start))
      (if (= i end)
          #f
          (let ((x (vector-ref vec i)))
            (if (pred? x)
                x
                (loop (+ i 1))))))))
;; Examples:  (vector-find even? '#(1 2 3))     ==> 2
;;            (vector-find even? '#(1 7 3))     ==> #f

;; vector-find-tail : (Value -> Boolean) Vector -> Vector
;; Returns the element and all the elements after the first for which
;; pred? returns a true value.
(define (vector-find-tail pred? vec)
  (check-arg procedure? pred? 'vector-find-tail)
  (check-arg vector? vec 'vector-find-tail)
  (vector-drop-while (function-not pred?) vec))
;; Example:   (vector-find-tail even? '#(3 1 37 -8 -5 0 0))
;;                 ==> #(-8 -5 0 0)

;; vector-take-while : (Value -> Boolean) Vector [Integer [Integer]]
;;   -> Vector
;; Returns the first elements for which pred? returns a true value when
;; applied to them.
(define (vector-take-while pred? vec . start+end)
  (check-arg procedure? pred? 'vector-take-while)
  (check-arg vector? vec 'vector-take-while)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-take-while)
    (let loop ((i start))
      (if (= i end)
          vec
          (let ((x (vector-ref vec i)))
            (if (pred? x)
                (loop (+ i 1))
                (vector-copy vec start i)))))))
;; Example:   (vector-take-while even? '#(2 18 3 10 22 9))
;;                 ==> #(2 18)

;; vector-drop-while : (Value -> Boolean) Vector [Integer [Integer]]
;;   -> Vector
;; Returns a vector of all the elements after the first ones which
;; satisfy pred?.
(define (vector-drop-while pred? vec . start+end)
  (check-arg procedure? pred? 'vector-drop-while)
  (check-arg vector? vec 'vector-drop-while)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-drop-while)
    (let loop ((i start))
      (if (= i end)
          '#()
          (let ((x (vector-ref vec i)))
            (if (pred? x)
                (loop (+ i 1))
                (vector-copy vec i end)))))))
;; Example:   (vector-drop-while even? '#(2 18 3 10 22 9))
;;                 ==> #(3 10 22 9)

;; vector-span : (Value -> Boolean) Vector [Integer [Integer]]
;;   -> Vector
;; Equivalent to:
;;   (values (vector-take-while pred? vec [start [end]])
;;           (vector-drop-while pred? vec [start [end]]))
(define (vector-span pred? vec . start+end)
  (check-arg procedure? pred? 'vector-span)
  (check-arg vector? vec 'vector-span)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-span)
    (let loop ((i start))
      (cond
       ((= i end) (values vec '#()))
       ((pred? (vector-ref vec i)) (loop (+ i 1)))
       (else (vector-split-at vec i))))))
;; Example:   (vector-span even? '#(2 18 3 10 22 9))
;;                 ==> #(2 18) #(3 10 22 9)

;; vector-break : (Value -> Boolean) Vector -> Vector
;; Equivalent to:
;;   (vector-span (lambda (x) (not (pred? x))) vec)
(define (vector-break pred? vec . start+end)
  (check-arg procedure? pred? 'vector-break)
  (check-arg vector? vec 'vector-break)
  (apply vector-span (function-not pred?) vec start+end))
;; Example:   (vector-break even? '#(3 1 4 1 5 9))
;;                 ==> #(3 1) #(4 1 5 9)

;; vector-any : (Value+ -> Boolean) Vector+ -> Value
;; Iterates over each element in the vectors, and returns the first
;; true value returned by pred? when pred? is applied to each
;; respective element in the vectors.  If no true value is returned,
;; and the end of the shortest vector is reached, #f is returned.  The
;; fact that the name does not end with a question mark is because any
;; true value can be returned, not just #t.
(define (vector-any pred? vec . rest)
  (check-arg procedure? pred? 'vector-any)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-any)
    (let ((len (smallest-length vecs)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred? (map (cut vector-ref <> i) vecs)) => identity)
         (else (loop (+ i 1))))))))
;; Examples:  (vector-any integer? '#(a 3 b 2.7))            ==> #t
;;            (vector-any < '#(3 1 4 1 5) '#(2 7 1 8 2))     ==> #t
;;            (vector-any complex? '#(a b c d e f))          ==> #f
;;            (vector-any (lambda (x) (and (integer? x) (* x x)))
;;                        '#(1.2 3+4i 5 21/6))     ==> 25

;; vector-every : (Value+ -> Boolean) Vector+ -> Value
;; Like vector-any, but stops as soon as #f is returned.  If a true
;; value is returned for every one, the last true value is returned.
(define (vector-every pred? vec . rest)
  (check-arg procedure? pred? 'vector-every)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-every)
    (let ((len-1 (- (smallest-length vecs) 1)))
      (if (negative? len-1) ;; If the smallest vector is empty.
          #t
          (let loop ((i 0))
            (cond
             ((= i len-1)
              ;; Get the result if we're at the last element.
              (apply pred? (map (cut vector-ref <> i) vecs)))
             ((not (apply pred? (map (cut vector-ref <> i) vecs))) #f)
             (else (loop (+ i 1)))))))))
;; Example:   (vector-every (lambda (x) (and (integer? x) (* x x)))
;;                          '#(1 2 3 4 5))     ==> 25

;; vector-index : (Value+ -> Boolean) Vector+ -> Integer
;; Returns the index of the first objects in the vectors applied which
;; satisfy pred?, searching from left to right.
;;
;; The current vector-index behaves similarly to SRFI 1's list-index,
;; but this is most likely going to change.
(define (vector-index pred? vec . rest)
  (check-arg procedure? pred? 'vector-index)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-index)
    (let ((len (smallest-length vecs)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred? (map (cut vector-ref <> i) vecs)) i)
         (else (loop (+ i 1))))))))
;; Examples:   (vector-index even? '#(3 1 4 1 5 9))     ==> 2
;;             (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
;;                  ==> 1
;;             (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
;;                  ==> #f

;; vector-index-right : (Value+ -> Boolean) Vector+
;;   -> (False-or Integer)
;; Returns the index of the first objects in the vectors applied which
;; satisfy pred?, searching from right to left.
(define (vector-index-right pred? vec . rest)
  (check-arg procedure? pred? 'vector-index-right)
  (check-arg vector? vec 'vector-index-right)
  (let ((len (vector-length vec))
        (vecs (cons vec rest)))
    (for-each
     (lambda (v)
       (check-arg vector? v 'vector-index-right)
       (if (not (= (vector-length v) len))
           (error
            "vector-index-right: all vectors must have same length"
            len
            v)))
     rest)
    (let loop ((i (- len 1)))
      (cond
       ((negative? i) #f)
       ((apply pred? (map (cut vector-ref <> i) vecs)) i)
       (else (loop (- i 1)))))))

;; vector-skip : (Value+ -> Boolean) Vector+ -> (False-or Integer)
;; Equivalent to:
;;   (apply vector-index (lambda (x) (not (pred? x))) vec rest)
(define (vector-skip pred? vec . rest)
  (check-arg procedure? pred? 'vector-skip)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-skip)
    (apply vector-index (function-not pred?) vecs)))
;; Example:   (vector-skip number? '#(1 2 a b 3 4 c d))     ==> 2

;; vector-skip-right : (Value+ -> Boolean) Vector+
;;   -> (False-or Integer)
;; Equivalent to:
;;   (apply vector-index-right (lambda (x) (not (pred? x))) vec rest)
(define (vector-skip-right pred? vec . rest)
  (check-arg procedure? pred? 'vector-skip-right)
  (check-arg vector? vec 'vector-skip-right)
  (let ((len (vector-length vec)))
    (for-each
     (lambda (vec)
       (check-arg vector? vec 'vector-skip-right)
       (if (not (= (vector-length vec) len))
           (error
            "vector-skip-right: all vectors must have same length"
            len vec)))
     rest)
    (apply vector-index-right (function-not pred?)
           (cons vec rest))))

;; These next three are commented out because they're horribly
;; inefficient and not really necessary anyways -- in the case that you
;; want to get the next element after that which was applied to one of
;; these, you can just use vector-index and increment the returned
;; index (with lists, mem(q|v|ber) is useful because element lookup is
;; not constant time, but with vectors, it is).

;; vector-member : Value Vector [(Value Value -> Boolean)] -> Vector
;; Returns the value and the tail of the vector after that value for
;; which maybe-elt=?, which defaults to equal?, returns a true value
;; when applied to x and the element in vec.
; (define (vector-member x vec . maybe-elt=?)
;   (check-arg vector? vec 'vector-member)
;   (let-optionals* maybe-elt=? ((elt=? equal?))
;     (check-arg procedure? elt=? 'vector-member)
;     (let ((vec (vector-find-tail (cut elt=? x <>) vec)))
;       (if (vector-empty? vec)
;           #f
;           vec))))

;; Common variants of vector-member that automatically use eq? and
;; eqv?, respectively.
; (define (vector-memq x vec) (vector-member x vec eq?))
; (define (vector-memv x vec) (vector-member x vec eqv?))
;; Examples:  (vector-memq 'a '#(a b c))                ==> #(a b c)
;;            (vector-memq 'b '#(a b c))                ==> #(b c)
;;            (vector-memq 'a '#(b c d))                ==> #f
;;            (vector-memq (list 'a) '#(b (a) c))       ==> #f
;;            (vector-member (list 'a) '#(b (a) c))     ==> #((a) c)
;;            (vector-memq 101 '#(100 101 102))         ==> unspecified
;;            (vector-memv 101 '#(100 101 102))         ==> #(101 102)
;;            (vector-member 4+2i '#(4.2 4+2i foo))     ==> #(4+2i foo)

;;;;;; - Deletion -

;; vector-delete : Value Vector [(Value Value -> Boolean)] -> Vector
;; Removes a certain object from a vector, where comparison is done by
;; maybe-elt=?, which defaults to equal?, and returns the vector
;; without it, without actually modifying the original vector.
(define (vector-delete x vec . maybe-elt=?)
  (check-arg vector? vec 'vector-delete)
  (let-optionals* maybe-elt=? ((elt=? equal?))
    (check-arg procedure? elt=? 'vector-delete)
    (vector-remove (lambda (y) (elt=? x y)) vec)))
;; Example:   (vector-delete 'z '#(a b z c d e f g))
;;                 ==> #(a b c d e f g)

;; vector-delete-duplicates : Vector [(Value Value -> Boolean)]
;;   -> Vector
;; Deletes all of the duplicates in vec.
;;
;; WARNING: If this is passed long vectors with lots of unique
;; elements, it will be -really- slow, since it keeps looking up
;; elements in the newly created vector, by iterating over each element
;; in it.  If your implementation provides sets or hash tables, then
;; use this sample implementation instead, where:
;;
;;   - (make-set elt=?)
;;       Returns a set whose values are compared with 'elt=?', which
;;       must be a procedure of two arguments returning one value.
;;
;;   - (set-add! set value)
;;       Adds 'value' to all the values in 'set'.
;;
;;   - (set-remove! set value)
;;       Removes 'value' from all the values in 'set'.
;;
;;   - (set-contains? set value)
;;       Returns #t if (elt=? value <one of the elements in 'set'>)
;;       returns a true value, where 'elt=?' is the argument passed to
;;       'make-set' to create that set; #f otherwise
;;
;; (define (vector-delete-duplicates vec . maybe-elt=?)
;;   (check-arg vector? vec 'vector-delete-duplicates)
;;   (let-optionals* maybe-elt=? ((elt=? equal?))
;;     (check-arg procedure? elt=? 'vector-delete-duplicates)
;;     (let* ((len (vector-length vec))
;;            (new (make-vector len))
;;            (set (make-set elt=?)))
;;       (let loop ((to 0) (from 0))
;;         (if (= from len)
;;             (if (= to len)
;;                 new
;;                 (vector-copy new 0 to))
;;             (let ((x (vector-ref vec i)))
;;               (if (set-contains? set x)
;;                   (loop to (+ from 1))
;;                   (begin
;;                     (set-add! set x)
;;                     (vector-set! new to x)
;;                     (loop (+ to 1) (+ from 1))))))))))
(define (vector-delete-duplicates vec . maybe-elt=?)
  (check-arg vector? vec 'vector-delete-duplicates)
  (let-optionals* maybe-elt=? ((elt=? equal?))
    (check-arg procedure? elt=? 'vector-delete-duplicates)
    (let* ((len (vector-length vec))
           (new (make-vector len)))
      (let loop ((to 0) (from 0))
        (if (= from len)
            (if (= to from)
                new
                (vector-copy new 0 to))
            (let ((x (vector-ref vec from)))
              (if (vector-find (cut elt=? x <>) new)
                  (loop to (+ from 1))
                  (begin
                    (vector-set! new to x)
                    (loop (+ to 1) (+ from 1))))))))))
;; Example:   (vector-delete-duplicates '#(a b a c a b c z))
;;                 ==> #(a b c z)

;;;;;; - Association Vectors ('avectors') -

;; Should these 'avector' functions really be here?

;; avector? : Value -> Boolean
;; Tests if its argument is an avector.
(define (avector? x)
  (and (vector? x)
       (vector-every avector-cell? x)))

;; Internal function.
(define (avector-cell? x)
  (and (vector? x)
       (= (vector-length x) 2)))

;; avector-search : Value Avector [(Value Value -> Boolean)] ->
;;   (False-or Avector)
;; Searches avector for the association whose key is 'x'.
(define (avector-search x avector . maybe-elt=?)
  (check-arg vector? avector 'avector-search)
  (let-optionals* maybe-elt=? ((elt=? equal?))
    (check-arg procedure? elt=? 'avector-search)
    (vector-find (lambda (y)
                   (check-arg avector-cell? y 'avector-search)
                   (elt=? (vector-first y) x))
                 avector)))

;; Common avector search functions.
(define (avector-searchq x avector) (avector-search x avector eq?))
(define (avector-searchv x avector) (avector-search x avector eqv?))

;; avector-cons : Value Value Avector -> Avector
;; Creates a new avector with a new association.
;; WARNING: Horribly inefficient, since it copies the whole avector to
;; add a single association.
(define (avector-cons key datum avector)
  (check-arg vector? avector 'avector-cons)
  (vector-append (vector key datum) avector))

;; avector-copy : Avector -> Avector
;; Duplicates an avector, copying not only the whole vector, but each
;; vector of association inside, making it safe to mutate cells in the
;; copy if the vector was originally a literal constant.  See note
;; after the example for avector-change! about literal constants.
(define (avector-copy avector . start+end)
  (check-arg vector? avector 'avector-copy)
  (let-optionals* start+end ((start 0) (end (vector-length avector)))
    (check-indices avector start 'start end 'end)
    (let ((new (make-vector (- end start))))
      (do ((i start (+ i 1))
           (j 0 (+ j 1)))
          ((= i end) new)
        (let ((x (vector-ref avector i)))
          (check-arg avector-cell? x 'avector-copy)
          (vector-set! new j (vector-copy x)))))))

;; avector-delete : Value Avector [(Value Value -> Boolean)] -> Avector
;; Removes an association from avector and returns the result.  The
;; vector isn't modified.
(define (avector-delete key avector . maybe-elt=?)
  (check-arg vector? avector 'avector-delete)
  (let-optionals* maybe-elt=? ((elt=? equal?))
    (check-arg procedure? elt=? 'avector-delete)
    (vector-remove (lambda (x) (elt=? (car x) key)) avector)))

;; avector-change : Value Avector Value [(Value Value -> Boolean)]
;;   -> Avector
;; Creates a new avector with a changed value for key.
;; WARNING: Should be very inefficient, since it copies the whole
;; vector. Use avector-change! if possible.
(define (avector-change key avector new . maybe-elt=?)
  (check-arg vector? avector 'avector-change)
  (let-optionals* maybe-elt=? ((elt=? equal?))
    (check-arg procedure? elt=? 'avector-change)
    (let* ((copy (avector-copy avector))
           (cell (avector-search key copy elt=?)))
      (if cell
          (begin
            (vector-set-second! cell new)
            copy)
          avector))))
;; Example:   (avector-change 'foo '#(#(baz quux) #(foo zot)) 'bar)
;;                 ==> #(#(baz quux) #(foo bar))
;;              Note the usage of literal constants in this example:
;;              because the vector is copied, and each vector inside is
;;              copied (with avector-copy), it is perfectly safe to
;;              mutate the second element of one of the cells, since
;;              the cells aren't literal constants after being copied.
;;              See note after example for avector-change! about
;;              mutating literal constants.

;; avector-change! : Value Avector Value [(Value Value -> Boolean)]
;;   -> Unspecific
;; Destructive variant of avector-change.
(define (avector-change! key avector new . maybe-elt=?)
  (check-arg vector? avector 'avector-change!)
  (let-optionals* maybe-elt=? ((elt=? equal?))
    (check-arg procedure? elt=? 'avector-change!)
    (let ((cell (avector-search key avector elt=?)))
      (if cell
          (begin
            (vector-set-second! cell new)
            avector)
          avector))))
;; Example:   (avector-change! "foo" (vector (vector "baz" "quux")
;;                                           (vector "foo" "zot"))
;;                             "bar" string=?)
;;                 ==> #(#("baz" "quux") #("foo" "bar"))
;;              Note the use of 'vector' and not the literal vector
;;              syntax; this is because in some implementations, the
;;              actual value that the syntax points to is constant, and
;;              avector-change! can change the avector, so what:
;;              '#("foo" "zot") points to would -actually- change to
;;              the value of '#("foo" "bar").  In some other
;;              implementations, it's an error to modify literal
;;              constants, too.  Thus, don't use this on vector
;;              literals.

;;;;;; - Mutators -

;; vector-set! : Vector Integer Value -> Unspecified
;; Assigns the value that the Integer refers to in the Vector.
(define vector-set! vector-set!)

;; vector-swap! : Vector Integer Integer -> Unspecified
;; Swap the values at the indices.
(define (vector-swap! vec i j)
  (check-arg vector? vec 'vector-swap!)
  (let ((x (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j x)))

;; vector-set-first! through vector-set-tenth! : Vector Value
;;   -> Unspecified
;; Common vector mutators.
(define (vector-set-first!   vec value)
  (check-arg vector? vec 'vector-set-first!)
  (if (< (vector-length vec) 1)
      (error "vector-set-first!: first index out of bounds"
             vec))
  (vector-set! vec 0 value))
(define (vector-set-second!  vec value)
  (check-arg vector? vec 'vector-set-second!)
  (if (< (vector-length vec) 2)
      (error "vector-set-second!: second index out of bounds"
             vec))
  (vector-set! vec 1 value))
(define (vector-set-third!   vec value)
  (check-arg vector? vec 'vector-set-third!)
  (if (< (vector-length vec) 3)
      (error "vector-set-third!: third index out of bounds"
             vec))
  (vector-set! vec 2 value))
(define (vector-set-fourth!  vec value)
  (check-arg vector? vec 'vector-set-fourth!)
  (if (< (vector-length vec) 4)
      (error "vector-set-fourth!: fourth index out of bounds"
             vec))
  (vector-set! vec 3 value))
(define (vector-set-fifth!   vec value)
  (check-arg vector? vec 'vector-set-fifth!)
  (if (< (vector-length vec) 5)
      (error "vector-set-fifth!: fifth index out of bounds"
             vec))
  (vector-set! vec 4 value))
(define (vector-set-sixth!   vec value)
  (check-arg vector? vec 'vector-set-sixth!)
  (if (< (vector-length vec) 6)
      (error "vector-set-sixth!: sixth index out of bounds"
             vec))
  (vector-set! vec 5 value))
(define (vector-set-seventh! vec value)
  (check-arg vector? vec 'vector-set-seventh!)
  (if (< (vector-length vec) 7)
      (error "vector-set-seventh!: seventh index out of bounds"
             vec))
  (vector-set! vec 6 value))
(define (vector-set-eighth!  vec value)
  (check-arg vector? vec 'vector-set-eighth!)
  (if (< (vector-length vec) 8)
      (error "vector-set-eighth!: eighth index out of bounds"
             vec))
  (vector-set! vec 7 value))
(define (vector-set-ninth!   vec value)
  (check-arg vector? vec 'vector-set-ninth!)
  (if (< (vector-length vec) 9)
      (error "vector-set-ninth!: ninth index out of bounds"
             vec))
  (vector-set! vec 8 value))
(define (vector-set-tenth!   vec value)
  (check-arg vector? vec 'vector-set-tenth!)
  (if (< (vector-length vec) 10)
      (error "vector-set-tenth!: tenth index out of bounds"
             vec))
  (vector-set! vec 9 value))

;; vector-fill! : Vector Value [Integer [Integer]] -> Unspecified
;; Fills vec with x, starting at start and ending at end.
(define (vector-fill! vec x . start+end)
  (check-arg vector? vec 'vector-fill!)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector-fill!)
    (do ((i start (+ i 1)))
        ((= i end))
      (vector-set! vec i x))))
;; Example:   (let ((v (make-vector 10 'a)))
;;              (vector-fill! v 'b 3 8)
;;              v)     ==> #(a a a b b b b b a a)

;;;;;; - Conversion -

;; vector->string : Vector [Integer [Integer]] -> String
;; Converts a vector of characters to a string.
;; Despite the fact that strings essentially are vectors but with
;; different operation names, this function does occasionally come in
;; handy.  A range can also be specified.
(define (vector->string vec . start+end)
  (check-arg vector? vec 'vector->string)
  (let-optionals* start+end ((start 0) (end (vector-length vec)))
    (check-indices vec start 'start end 'end 'vector->string)
    (let* ((len (- end start))
           (new (make-string len)))
      (do ((i 0 (+ i 1))
           (j start (+ j 1)))
          ((= j end) new)
        (string-set! new i (vector-ref vec j))))))

;; string->vector : String [Integer [Integer]] -> Vector
;; Converts a string to a vector.  A range can also be specified.
(define (string->vector str . start+end)
  (check-arg string? str 'string->vector)
  (let-optionals* start+end ((start 0) (end (string-length str)))
    ;; check-indices only works on vectors, unfortunately, so we have
    ;; to inline a variant of it here.
    (let ((str-len (string-length str)))
      (let check ((start start) (end end))
        (if (> start end)
            (call-with-values
             (lambda ()
               (error
                "string->vector: start/end out of bounds - start > end"
                start end str))
             (lambda (new-start new-end)
               (check new-start new-end))))
        (if (< start 0)
            (check
             (error "string->vector: start out of bounds -- start < 0"
                    start str)
             end))
        (if (> end str-len)
            (check
             start
             (error "string->vector: end out of bounds -- end > len"
                    end str-len str)))
        (if (>= start str-len)
            (check
             (error
              "vector->string: start out of bounds -- start >= len"
              start str-len str)
             end))))

    (let* ((len (- end start))
           (new (make-vector len)))
      (do ((i 0 (+ i 1))
           (j start (+ j 1)))
          ((= j end) new)
        (vector-set! new i (string-ref str j))))))

;; avector->alist : Avector [Integer [Integer]] -> Alist
;; Produce an alist from an avector, possibly between start and end.
(define (avector->alist avector . start+end)
  (check-arg vector? avector 'avector->alist)
  (let-optionals* start+end ((start 0) (end (vector-length avector)))
    (do ((i (- end 1) (- i 1))
         (result '()
                 (let ((cell (vector-ref avector i)))
                   (check-arg avector-cell? cell 'avector->alist)
                   (cons (cons (vector-first cell)
                               (vector-second cell))
                         result))))
        ((< i start) result))))
;; Example:     (avector->alist '#(#(0 1) #(2 3) #(a b) #(c d)) 2 4)
;;                   ==> ((a . b) (c . d))

;; alist->avector : Alist -> Avector
;; Produce an avector from an equivalent alist.
(define (alist->avector alist)
  (check-arg proper-list? alist 'alist->avector)
  (let* ((len (length alist))
         (new (make-vector len)))
    (do ((i 0 (+ i 1))
         (alist alist (cdr alist)))
        ((null? alist) new)
      (vector-set! new i (vector (caar alist) (cdar alist))))))
;; Example:    (alist->vector '((a . b) (c . d)))
;;                  ==> #(#(a b) #(c d))

;; These next two are most likely implemented very efficiently by the
;; implementation you're using, so unless they're -really- bad, you
;; should probably not use the sample implementation; or if they aren't
;; actually provided for some odd reason you should use the sample
;; implementation.
;;
;; One thing you should -not- do is:
;;   (define (list->vector lst)
;;     (apply vector lst))
;; because many Schemes have a limit as to the number of arguments you
;; can apply a function to, and vector might be implemented in terms of
;; list->vector, which would be -extremely- bad.

;; vector->list : Vector [Integer [Integer]]-> List
;; Converts a vector to a list, possibly within the given start and
;; end.  The start and end arguments are provided as an alternative to
;; doing:
;;   (vector->list (vector-copy vec start end))
;; because vector->list with start and end arguments can be implemented
;; more efficiently; no new vectors need be allocated.
(define vector->list
  (let ((%vector->list vector->list))
    (lambda (vec . start+end)
      (if (null? start+end)
          (%vector->list vec)
          (let-optionals* start+end ((start 0)
                                     (end (vector-length vec)))
            (check-indices vec start 'start end 'end 'vector->list)
            (do ((i (- end 1) (- i 1))
                 (result '() (cons (vector-ref vec i) result)))
                ((< i start) result)))))))
;; Sample implementation: just turn vector->list into:
;;   (define (vector->list vec . start+end)
;;     (let-optionals* start+end ((start 0) (end (vector-length vec)))
;;       <as in the uncommented one>))

;; list->vector : List -> Vector
;; Converts a list to a vector.
(define list->vector list->vector)
;; Sample implementation:
;;   (define (list->vector lst)
;;     (let* ((len (length lst))
;;            (new (make-vector len)))
;;       (do ((i 0 (+ i 1))
;;            (lst lst (cdr lst))
;;           ((null? lst) new)
;;         (vector-set! new i (car lst)))))
;; Should be reasonably efficient, if 'length' is implemented
;; reasonably efficiently (i.e., if the system knows about a list's
;; length and can get it constant time, that should be good --
;; otherwise it's still reasonably efficient anyways, but it iterates
;; over the list twice).
