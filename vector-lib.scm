;;;;;; - Vector Manipulation Operation Library -
;;;;;; - Written by Taylor Campbell -

;;;;;; - Copyright -
;; You may do as you please with this code, as long as you refrain from
;; removing this copyright notice or holding me liable for any damages
;; that may be caused by it; and you may quote sections from it as you
;; please, as long as you credit me with the quotation.  Send questions
;; or bugs or anything of the sort to:
;;
;;   campbell (at) evdev (dot) ath (dot) cx

;; Requires: SRFI 1 (list-lib), SRFI 23 (error), SRFI 26 (cut/cute)

;;; Index of functions exported from this library:
;;
;;; Constructors:
;;
;; make-vector vector vector-tabulate vector-copy vector-resize
;;
;;; Predicates:
;;
;; vector? vector-empty? vector=
;;
;;; Accessors:
;;
;; vector-ref
;;
;;; Miscellaneous:
;;
;; vector-length vector-copy!
;; vector-append vector-concatenate
;; vector-reverse vector-reverse!
;; vector-count
;;
;;; Iterators:
;;
;; vector-fold      vector-fold-right
;; vector-map       vector-map!
;; vector-map/index vector-map/index!
;; vector-for-each  vector-for-each/index
;;
;;; Searchers:
;;
;; vector-index vector-index-right
;; vector-skip  vector-skip-right
;; vector-binary-search
;;
;;; Mutators:
;;
;; vector-set! vector-swap!
;; vector-fill!
;; vector-insert! vector-delete!
;; vector-rotate!
;;
;;; Converters:
;;
;; vector->list     list->vector
;; vector->string string->vector

;;;;;; - Utility Functions -

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
;; If your Scheme implementation supports better type checking (e.g.:
;; maybe its own compile-time type checker), then you can simply
;; discard all of the calls to CHECK-ARG and replace it with whatever
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
                               ": I out of bounds -- I < 0")
                i vec)
         caller))
    (let ((len (vector-length vec)))
      (if (>= i (vector-length vec))
          (check-index
           vec
           (error (string-append s-caller
                                 ": I out of bounds -- I >= LEN")
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

(define (conjoin . fs)
  (lambda args
    (every (cut apply <> args) fs)))

(define (disjoin . fs)
  (lambda args
    (any (cut apply <> args) fs)))

(define (compose . fs)
  (fold (lambda (f g)
          (lambda args
            (call-with-values
              (lambda () (apply f args))
              g)))
        values
        fs))

(define (complement f) (compose not f))

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

(define (inc x) (+ x 1))

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

;;;;;; - vector-lib -

;; In any case where:
;;   (define name name)
;; is used, it is only to indicate that the library exports that
;; function.

;;;;;; - Constructors -

;; Defined by R5RS.
(define make-vector make-vector)
(define vector vector)
;; Examples:  (make-vector 5 3)         ==> #(3 3 3 3 3)
;;            (vector 3 3 3 3 3)        ==> #(3 3 3 3 3)

;; vector-tabulate : (Integer -> Value) Integer -> Vector
;; Like MAKE-VECTOR, but instead of using a single value, it calls F
;; with the index of each slot, and the value returned by F is put into
;; the respective slot.
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
;; given range.  No SUBVECTOR function is available since it would be
;; really only VECTOR-COPY but with a fixed number of arguments.
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

;; vector-resize : Vector Integer [Value] -> Vector
;; Produces a new vector larger than the original with the original
;; elements in their respective places in the new vector.  The last,
;; optional, argument is the value that will initialize the unfilled
;; slots in the new vector, whose default value is unspecified.
(define (vector-resize vec new-size . maybe-default)
  (let-optionals* maybe-default ((default (if #f #f)))
    (let ((len (vector-length vec)))
      (check-arg vector? vec 'vector-resize)
      (check-arg (conjoin integer?
                          (lambda (x) (>= x len)))
                 new-size
                 'vector-resize)
      (let ((new (make-vector new-size default)))
        (vector-copy! new 0 vec)
        new))))
;; Example:  (vector-resize '#(1 2 3 4 5) 10 #t)
;;                ==> #(1 2 3 4 5 #t #t #t #t #t)

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

;; vector= : (Value+ -> Boolean) Vector+ -> Boolean
;; Compares vectors based on its first argument.  The first argument
;; must be a function that takes exactly two arguments and return a
;; single value, which be treated as a boolean.
;;
;; This code was influenced majorly from Olin's SRFI 1 reference
;; implementation (ok, it was basically copied, with list-specific
;; stuff changed to vector stuff.  Is this legal, or do I have to slap
;; Olin's copyright here?).
(define (vector= elt=? . vecs)
  (check-vector-args vecs 'vector=)
  (or (memv (length vecs) '(0 1))
      (let ((len (vector-length (car vecs))))
        (check-vector-args vecs 'vector=)
        (let loop1 ((vec-a (car vecs)) (others (cdr vecs)))
          (or (null? others)
              (let ((vec-b (car others))
                    (others (cdr others)))
                ;; Fast path -- if the two are EQ?, why bother checking
                ;; further?
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
                                 (loop2 (+ i 1)))))))))))))
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

;;;;;; - Miscellaneous -

;; vector-length : Vector -> Integer
;; Returns the length of a vector.
(define vector-length vector-length)
;; Example:   (vector-length '#(a b c))     ==> 3

;; vector-copy! : Vector Integer Vector [Integer [Integer]]
;;   -> Unspecified
;; Copies a vector destructively into TARGET from VEC, where the first
;; element mutated in TARGET is TSTART, the first element taken from
;; VEC is FSTART which defaults to 0, and the last element taken from
;; vec is FEND - 1, where FEND defaults to the length of VEC.
(define (vector-copy! target tstart vec . fstart+fend)
  (check-arg vector? target 'vector-copy!)
  (check-arg nonneg-integer? tstart 'vector-copy!)
  (check-arg vector? vec 'vector-copy)
  (let ((vec-len (vector-length vec)))
    (let-optionals* fstart+fend ((fstart 0) (fend vec-len))
      (check-arg nonneg-integer? fstart 'vector-copy!)
      ;; FEND can't be zero.
      (check-arg (conjoin integer? positive?) fend 'vector-copy!)
      ;; Here, more specific bounds checking can be done, so
      ;; CHECK-INDICES isn't used.
      (if (> fstart fend)
          (error "vector-copy!: FSTART out of bounds -- FSART > FEND"
                 fstart fend `(vec was: ,vec) `(target was: ,target)))
      (if (> fstart vec-len)
          (error "vector-copy!: FSTART out of bounds -- FSTART > LEN"
                 fstart vec-len vec `(target was: ,target)))
      (if (> fend vec-len)
          (error "vector-copy!: FEND out of bounds -- FEND > LEN"
                 fend vec-len vec `(target was: ,target)))

      ;; Do this only when they're not EQ? -- when they are, why
      ;; bother?
      (if (not (eq? target vec))
          (do ((to tstart (+ to 1))
               (from fstart (+ from 1)))
              ((= from fend))
            (vector-set! target to (vector-ref vec from)))))))
;; Example:   (let ((target (vector 1 3 2 4 5 6 7)))
;;              (vector-copy! target 1 '#(-1 0 1 2 3 4 5) 3 5)
;;              target)     ==> #(1 2 3 4 5 6 7)

;; vector-append : Vector* -> Vector
;; Appends its arguments together.
(define (vector-append . vecs)
  (check-vector-args vecs 'vector-append)
  (vector-concatenate vecs))
;; Examples:  (vector-append '#(x) '#(y))           ==> #(x y)
;;            (vector-append '#(a) '#(b c d)        ==> #(a b c d)
;;            (vector-append '#(a (b)) '#((c)))     ==> #(a (b) (c))

;; vector-concatenate : (Vector-of Vector) -> Vector
;; Equivalent to:
;;   (apply vector-append vectors)
;; but some implementations don't support large enough argument lists
;; at times to concatenate them with a single call to VECTOR-APPEND, so
;; VECTOR-CONCATENATE isn't implemented that way (indeed, VECTOR-APPEND
;; is implemented in terms of VECTOR-CONCATENATE).
(define (vector-concatenate vectors)
  (check-arg proper-list? vectors 'vector-concatenate)
  (let* ((len (fold (lambda (vec len) (+ len (vector-length vec)))
                    0
                    vectors))
         (new (make-vector len)))
    (let loop1 ((vecs vectors) (to 0))
      (if (null? vecs)
          new
          (let* ((vec (car vecs))
                 (len (vector-length vec)))
            (check-arg vector? vec 'vector-concatenate)
            (let loop2 ((from 0) (to to))
              (cond
               ((< from len)
                (vector-set! new to (vector-ref vec from))
                (loop2 (+ from 1) (+ to 1)))
               (else (loop1 (cdr vecs) to)))))))))
;; Example:   (vector-concatenate '(#(a b) #(c d)))
;;                 ==> #(a b c d)

;; vector-reverse : Vector [Integer [Integer]] -> Vector
;; Reverses the elements in VEC, but not deeply.
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
;; Destructive variant of VECTOR-REVERSE.
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

;; vector-count : (Value+ -> Boolean) Vector+ -> Integer
;; Find the count of elements in all of the vectors that satisfy PRED?,
;; which should take as many arguments as there are vectors.
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

;; vector-fold : (Value+ Value -> Value) Value Vector+ -> Value
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
;; Example:   (vector-fold (lambda (s m)
;;                           (max (string-length s) m))
;;                         0 '#("foo" "bar" "baz" "quux" "zot"))
;;                 ==> 4

;; vector-fold-right : (Value+ Value -> Value) Value Vector+ -> Value
;; Traverses the vector right to left, unlike left to right in the case
;; of VECTOR-FOLD.
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

;; vector-map : (Value+ -> Value) Vector+ -> Vector
;; Creates a vector by applying F to each element of the vectors.
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

;; vector-map! : (Value+ -> Value) Vector+ -> Unspecified
;; Destructive variant of VECTOR-MAP.
(define (vector-map! f vec . rest)
  (check-arg procedure? f 'vector-map)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-map!)
    (let ((len (smallest-length vecs)))
      (do ((i 0 (+ i 1)))
          ((= i len) vec)
        (vector-set! vec i (apply f (map (cut vector-ref <> i)
                                         vecs)))))))

;; vector-map/index : (Value+ Integer -> Value) Vector+ -> Vector
;; Applies F to each element in all the vectors, just like VECTOR-MAP,
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

;; vector-map/index! : (Value+ Integer -> Value) Vector+ -> Unspecified
;; Destructive variant of VECTOR-MAP/INDEX.
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

;; vector-for-each : (Value+ -> <ignored>) Vector+ -> Unspecified
;; Applies F to each element of the vectors, but returns unspecified,
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

;; vector-for-each/index :
;;   (Value+ Integer -> <ignored>) Vector+ -> Unspecified
;; VECTOR-FOR-EACH/INDEX is to VECTOR-FOR-EACH as VECTOR-MAP/INDEX is
;; to VECTOR-MAP.
(define (vector-for-each/index f vec . rest)
  (check-arg procedure? f 'vector-for-each/index)
  (let ((vecs (cons vec rest)))
    (check-vector-args vecs 'vector-for-each/index)
    (let ((len (smallest-length vecs)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (apply f (append (map (cut vector-ref <> i) vecs)
                         (list i)))))))

;;;;;; - Searchers -

;; vector-index : (Value+ -> Boolean) Vector+ -> Integer
;; Returns the index of the first object in the vectors that satisfies
;; PRED?, searching from left to right.
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
;; Returns the index of the first object in the vectors that satisfies
;; PRED?, searching from right to left.
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
  (apply vector-index (complement pred?) vecs))
;; Example:   (vector-skip number? '#(1 2 a b 3 4 c d))     ==> 2

;; vector-skip-right : (Value+ -> Boolean) Vector+
;;   -> (False-or Integer)
;; Equivalent to:
;;   (apply vector-index-right (lambda (x) (not (pred? x))) vec rest)
(define (vector-skip-right pred? vec . rest)
  (apply vector-index-right (complement pred?) vec rest))

;; vector-binary-search :
;;   Vector Value (Value Value -> ('LT | 'EQ | 'GT)) ->
;;     (False-or Integer)
;; Like VECTOR-INDEX, but binary-searches to find the index, instead of
;; going strictly left to right (or right to left with
;; VECTOR-INDEX-RIGHT).
(define (vector-binary-search vec val cmp . start+end)
  (check-arg vector? vec 'vector-binary-search)
  (check-arg procedure? cmp 'vector-binary-search)
  (let ((len (vector-length vec)))
    (let-optionals* start+end ((start 0) (end len))
      (check-indices vec start 'start end 'end 'vector-binary-search)
      (let loop ((start start) (end end) (prev-i #f))
        (let ((i (quotient (+ start end) 2)))
          (if (and prev-i (= i prev-i))
              #f
              (case (cmp val (vector-ref vec i))
                ((lt) (loop start i i))
                ((eq) i)
                ((gt) (loop i end i)))))))))

;;;;;; - Mutators -

;; vector-set! : Vector Integer Value -> Unspecified
;; Assigns the value at a certain index in a vector.
(define vector-set! vector-set!)

;; vector-swap! : Vector Integer Integer -> Unspecified
;; Swap the values at the indices.
(define (vector-swap! vec i j)
  (check-arg vector? vec 'vector-swap!)
  (let ((x (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j x)))

;; vector-fill! : Vector Value [Integer [Integer]] -> Unspecified
;; Fills vec with X, starting at START, which defaults to 0, and ending
;; at END, which defaults to the length of VEC.
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

;; vector-insert! : Vector Integer Value -> Unspecified
;; Inserts ELT at I in VEC, shifting values whose indices are greater
;; than I right, dropping off the last value.
(define (vector-insert! vec i elt)
  (check-arg vector? vec 'vector-insert!)
  (check-index vec i 'vector-insert!)
  (let ((len (vector-length vec))
        (next-val (vector-ref vec i)))
    (vector-set! vec i elt)
    (let loop ((i (+ i 1)) (this-val next-val))
      (if (not (= i len))
          (let ((next-val (vector-ref vec i)))
            (vector-set! vec i this-val)
            (loop (+ i 1) next-val))))))
;; Example:  (let ((vec (vector 0 1 3 4 5)))
;;             (vector-insert! vec 2 2)
;;             vec)
;;                ==> #(0 1 2 3 4 5)

;; vector-delete! : Vector Integer [Value] -> Unspecified
;; Removes the element at I, shifting values whose indices are greater
;; than I left, setting the last slot to LAST, which defaults to an
;; unspecified value.
(define (vector-delete! vec i . maybe-last)
  (check-arg vector? vec 'vector-delete!)
  (check-index vec i 'vector-delete!)
  (let-optionals* maybe-last ((last (if #f #f)))
    (let* ((len (vector-length vec))
           (next-val (vector-ref vec (- len 1))))
      (vector-set! vec (- len 1) last)
      (let loop ((j (- len 2)) (this-val next-val))
        (if (not (< j i))
            (let ((next-val (vector-ref vec j)))
              (vector-set! vec j this-val)
              (loop (- j 1) next-val)))))))
;; Example:  (let ((vec (vector 0 1 2 2 3)))
;;             (vector-delete! vec 2 4)
;;             vec)
;;                ==> #(0 1 2 3 4)

;; vector-rotate! : Vector [Integer] -> Unspecific
;; Rotates each element in VEC right N (or if N is negative, left)
;; elements, wrapping.
(define (vector-rotate! vec . maybe-n)
  (check-arg vector? vec 'vector-rotate!)
  (let-optionals* maybe-n ((n 1))
    (check-arg integer? n 'vector-rotate!)
    (let ((len (vector-length vec)))
      (let n-loop ((n n))
        (cond
         ;; Don't bother shifting at all if it's 0.
         ((zero? n) (if #f #f))
         ((> n len) (n-loop (- n len)))
         ((< n (- len)) (n-loop (+ n len)))
         (else (really-rotate! vec len n)))))))
;; Examples:  (let ((vec (vector 1 2 3 4 0)))
;;              (vector-rotate! vec)
;;              vec)
;;                 ==> #(0 1 2 3 4)
;;
;;            (let ((vec (vector 3 4 0 1 2)))
;;              (vector-rotate! vec -2)
;;              vec)
;;                 ==> #(0 1 2 3 4)

;; Utility functions for VECTOR-ROTATE!.
(define (really-rotate! vec len n)
  (let loop ((j 0) (v (if #f #f)))
    (if (= (abs j) len)
        (vector-set*! vec 0 v)
        (let ((next-value (vector-ref* vec j)))
          (vector-set*! vec j v)
          (loop (let ((next-j (+ j n)))
                  (cond ((> next-j len) (- next-j len))
                        ((and (negative? next-j)
                              (> (- next-j) len))
                         (+ len next-j))
                        (else next-j)))
                next-value)))))

(define (vector-ref* vec i)
  (vector-ref vec (real-index vec i)))
(define (vector-set*! vec i val)
  (vector-set! vec (real-index vec i) val))

(define (real-index vec i)
  (let ((len (vector-length vec)))
    (cond ((and (negative? i)
                (< (abs i) len))
           (+ len i))
          ((negative? i)
           (real-index vec (+ len i)))
          ((< i len) i)
          (else (real-index vec (- i len))))))

;;;;;; - Converters -

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
;; can apply a function to, and VECTOR might be implemented in terms of
;; LIST->VECTOR, which would quite simply cause infinite recursion --
;; and that is definitely not a vector, the desired result.

;; vector->list : Vector [Integer [Integer]]-> List
;; Converts a vector to a list, possibly within the given start and
;; end.  The start and end arguments are provided as an alternative to
;; doing:
;;   (vector->list (vector-copy vec start end))
;; because VECTOR->LIST with start and end arguments can be implemented
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
;;       (check-indices vec start 'start end 'end 'vector->list)
;;       (do ((i (- end 1) (- i 1))
;;            (result '() (cons (vector-ref vec i) result)))
;;           ((< i start) result))))

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
;; Should be reasonably efficient, if LENGTH is implemented reasonably
;; efficiently (i.e. if the system knows about a list's length and can
;; retrieve it constant time, that should be good -- otherwise, it's
;; still reasonably efficient anyways, but it iterates over the list
;; twice).

;; vector->string : Vector [Integer [Integer]] -> String
;; Converts a vector of characters to a string.
;;
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
    ;; CHECK-INDICES only works on vectors, unfortunately, so we have
    ;; to inline a variant of it here.
    (let ((str-len (string-length str)))
      (let check ((start start) (end end))
        (if (> start end)
            (call-with-values
             (lambda ()
               (error
                "string->vector: START/END out of bounds - START > END"
                start end str))
             (lambda (new-start new-end)
               (check new-start new-end))))
        (if (< start 0)
            (check
             (error "string->vector: START out of bounds -- START < 0"
                    start str)
             end))
        (if (> end str-len)
            (check
             start
             (error "string->vector: end out of bounds -- END > LEN"
                    end str-len str)))
        (if (>= start str-len)
            (check
             (error
              "vector->string: start out of bounds -- START >= LEN"
              start str-len str)
             end))))

    (let* ((len (- end start))
           (new (make-vector len)))
      (do ((i 0 (+ i 1))
           (j start (+ j 1)))
          ((= j end) new)
        (vector-set! new i (string-ref str j))))))
