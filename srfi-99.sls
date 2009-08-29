;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SRFI 99.  ERR5RS Records.
;
; This is a quick-and-dirty reference implementation that favors
; simplicity over quality error messages and performance.  It is
; implemented using the R6RS procedural and inspection layers,
; with which it interoperates nicely.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This library breaks a circular interdependence between the
; procedural and inspection layers.

(library (srfi :99-helpers records rtd?)
  (export rtd?)
  (import (rnrs base) (rnrs records procedural))

  (define rtd? record-type-descriptor?)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (srfi :99 records inspection)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(library (srfi :99 records inspection)

  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?)

  (import (rnrs base)
          (rnrs lists)
          (rnrs records inspection)
          (srfi :99-helpers records rtd?))

  ; The record? predicate is already defined by (rnrs records inspection).
  
  ; The record-rtd procedure is already defined by (rnrs records inspection).
  
  (define rtd-name record-type-name)
  
  (define rtd-parent record-type-parent)
  
  (define rtd-field-names record-type-field-names)
  
  (define (rtd-all-field-names rtd)
    (define (loop rtd othernames)
      (let ((parent (rtd-parent rtd))
            (names (append (vector->list
                            (rtd-field-names rtd))
                           othernames)))
        (if parent
            (loop parent names)
            (list->vector names))))
    (loop rtd '()))
  
  (define (rtd-field-mutable? rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-field-mutable? rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-field-mutable?
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (srfi :99 records procedural)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (srfi :99 records procedural)

  (export make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator)

  (import (rnrs base)
          (rnrs lists)
          (rnrs records procedural)
          (srfi :99 records inspection))

  ; Note: the options are permitted by ERR5RS,
  ; but are not part of ERR5RS.

  (define (make-rtd name fieldspecs . rest)
    (let* ((parent (if (null? rest) #f (car rest)))
           (options (if (null? rest) '() (cdr rest)))
           (sealed? (and (memq 'sealed options) #t))
           (opaque? (and (memq 'opaque options) #t))
           (uid (let ((probe (memq 'uid options)))
                  (if (and probe (not (null? (cdr probe))))
                      (cadr probe)
                      #f))))
      (make-record-type-descriptor
       name
       parent
       uid
       sealed?
       opaque?
       (vector-map (lambda (fieldspec)
                     (if (symbol? fieldspec)
                         (list 'mutable fieldspec)
                         fieldspec))
                   fieldspecs))))
  
  (define rtd? record-type-descriptor?)
  
  (define (rtd-constructor rtd . rest)
  
    ; Computes permutation and allocates permutation buffer
    ; when the constructor is created, not when the constructor
    ; is called.  More error checking is recommended.
  
    (define (make-constructor fieldspecs allnames maker)
      (let* ((k (length fieldspecs))
             (n (length allnames))
             (buffer (make-vector n))
             (reverse-all-names (reverse allnames)))
  
        (define (position fieldname)
          (let ((names (memq fieldname reverse-all-names)))
            (assert names)
            (- (length names) 1)))
  
        (let ((indexes (map position fieldspecs)))
  
          ; The following can be made quite efficient by
          ; hand-coding it in some lower-level language,
          ; e.g. Larceny's mal.  Even case-lambda would
          ; be good enough in most systems.
  
          (lambda args
            (assert (= (length args) k))
            (for-each (lambda (arg posn)
                        (vector-set! buffer posn arg))
                      args indexes)
            (apply maker (vector->list buffer))))))
  
    (if (null? rest)
        (record-constructor
         (make-record-constructor-descriptor rtd #f #f))
        (begin (assert (null? (cdr rest)))
               (make-constructor
                (vector->list (car rest))
                (vector->list (rtd-all-field-names rtd))
                (record-constructor
                 (make-record-constructor-descriptor rtd #f #f))))))
  
  (define rtd-predicate record-predicate)
  
  (define (rtd-accessor rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-accessor rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-accessor
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))
  
  (define (rtd-mutator rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-mutator rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-mutator
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS records, syntactic layer.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (srfi :99 records syntactic)

  (export define-record-type)

  (import (for (rnrs base) run expand)
          (for (rnrs lists) run expand)
          (for (rnrs syntax-case) run expand)
          (srfi :99 records procedural))

  (define-syntax define-record-type
    (syntax-rules ()
     ((_ (type-name parent) constructor-spec predicate-spec . field-specs)
      (define-record-type-helper0
       type-name parent constructor-spec predicate-spec . field-specs))
     ((_ type-name constructor-spec predicate-spec . field-specs)
      (define-record-type-helper0
       type-name #f constructor-spec predicate-spec . field-specs))))

  (define-syntax define-record-type-helper0
    (lambda (x)

      ; Given syntax objects, passes them to helper macro.

      (define (construct-record-type-definitions
               tname fields parent cspec pred afields mfields)
        (let ()

          (define (frob x)
            (cond ((identifier? x)
                   x)
                  ((pair? x)
                   (cons (frob (car x)) (frob (cdr x))))
                  ((vector? x)
                   (vector-map frob x))
                  (else
                   (datum->syntax tname x))))

          #`(#,(frob #'define-record-type-helper)
             #,(frob tname)
             #,(frob fields)
             #,(frob parent)
             #,(frob cspec)
             #,(frob pred)
             #,(frob afields)
             #,(frob mfields))))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object for its first element.

      (define (syntax-car x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'x0)))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object obtained by omitting the first
      ; element of that list.

      (define (syntax-cdr x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'(x1 ...))))

      ; Given a syntax object that represents a non-empty list,
      ; returns the corresponding list of syntax objects.

      (define (syntax->list x)
        (syntax-case x ()
         (()
          x)
         ((x0)
          x)
         ((x0 . x1)
          (cons #'x0 (syntax->list #'x1)))))

      (define (complain)
        (syntax-violation 'define-record-type "illegal syntax" x))

      ; tname and pname are always identifiers here.

      (syntax-case x ()
       ((_ tname pname constructor-spec predicate-spec . field-specs)
        (let* ((type-name (syntax->datum #'tname))
               (cspec (syntax->datum #'constructor-spec))
               (pspec (syntax->datum #'predicate-spec))
               (fspecs (syntax->datum #'field-specs))
               (type-name-string
                (begin (if (not (symbol? type-name))
                           (complain))
                       (symbol->string type-name)))
               (constructor-name
                (cond ((eq? cspec #f)
                       #'constructor-spec)
                      ((eq? cspec #t)
                       (datum->syntax
                        #'tname
                        (string->symbol
                         (string-append "make-" type-name-string))))
                      ((symbol? cspec)
                       #'constructor-spec)
                      ((and (pair? cspec) (symbol? (car cspec)))
                       (syntax-car #'constructor-spec))
                      (else (complain))))
               (constructor-args
                (cond ((pair? cspec)
                       (if (not (for-all symbol? cspec))
                           (complain)
                           (list->vector
                            (syntax->list (syntax-cdr #'constructor-spec)))))
                      (else #f)))
               (new-constructor-spec
                (if constructor-args
                    (list constructor-name constructor-args)
                    constructor-name))
               (predicate-name
                (cond ((eq? pspec #f)
                       #'predicate-spec)
                      ((eq? pspec #t)
                       (datum->syntax
                        #'tname
                        (string->symbol
                         (string-append type-name-string "?"))))
                      ((symbol? pspec)
                       #'predicate-spec)
                      (else (complain))))
               (field-specs
                (map (lambda (fspec field-spec)
                       (cond ((symbol? fspec)
                              (list 'immutable
                                    fspec
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string fspec)))))
                             ((not (pair? fspec))
                              (complain))
                             ((not (list? fspec))
                              (complain))
                             ((not (for-all symbol? fspec))
                              (complain))
                             ((null? (cdr fspec))
                              (list 'mutable
                                    (car fspec)
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string (car fspec))))
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string (car fspec))
                                      "-set!"))))
                             ((null? (cddr fspec))
                              (list 'immutable
                                    (car fspec)
                                    (syntax-car (syntax-cdr field-spec))))
                             ((null? (cdddr fspec))
                              (list 'mutable
                                    (car fspec)
                                    (syntax-car (syntax-cdr field-spec))
                                    (syntax-car (syntax-cdr
                                                 (syntax-cdr field-spec)))))
                             (else (complain))))
                     fspecs
                     (syntax->list #'field-specs)))
  
               (fields (list->vector (map cadr field-specs)))
  
               (accessor-fields
                (map (lambda (x) (list (caddr x) (cadr x)))
                     (filter (lambda (x) (>= (length x) 3))
                             field-specs)))
  
               (mutator-fields
                (map (lambda (x) (list (cadddr x) (cadr x)))
                     (filter (lambda (x) (= (length x) 4))
                             field-specs))))
  
          (construct-record-type-definitions
           #'tname
           fields
           #'pname
           new-constructor-spec
           predicate-name
           accessor-fields
           mutator-fields))))))
  
  (define-syntax define-record-type-helper
    (syntax-rules ()
  
     ((_ type-name fields parent #f predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (define-record-type-helper
       type-name fields parent ignored predicate
       ((accessor field) ...) ((mutator mutable-field) ...)))
  
     ((_ type-name fields parent constructor #f
         ((accessor field) ...) ((mutator mutable-field) ...))
      (define-record-type-helper
       type-name fields parent constructor ignored
       ((accessor field) ...) ((mutator mutable-field) ...)))
  
     ((_ type-name fields parent (constructor args) predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (begin (define type-name (make-rtd 'type-name 'fields parent))
             (define constructor (rtd-constructor type-name 'args))
             (define predicate (rtd-predicate type-name))
             (define accessor (rtd-accessor type-name 'field))
             ...
             (define mutator (rtd-mutator type-name 'mutable-field))
             ...))
  
     ((_ type-name fields parent constructor predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (begin (define type-name (make-rtd 'type-name 'fields parent))
             (define constructor (rtd-constructor type-name))
             (define predicate (rtd-predicate type-name))
             (define accessor (rtd-accessor type-name 'field))
             ...
             (define mutator (rtd-mutator type-name 'mutable-field))
             ...))))

  ) ; srfi :99 records syntactic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS records, composite library.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (srfi :99 records)

  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?

          make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator

          define-record-type)

  (import (srfi :99 records inspection)
          (srfi :99 records procedural)
          (srfi :99 records syntactic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS records, alias for composite library.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (srfi :99)

  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?

          make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator

          define-record-type)

  (import (srfi :99 records)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS aliases.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (err5rs records inspection)

  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?)

  (import (srfi :99 records inspection)))

(library (err5rs records procedural)

  (export make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator)

  (import (srfi :99 records procedural)))

(library (err5rs records syntactic)

  (export define-record-type)

  (import (srfi :99 records syntactic)))

(library (err5rs records)

  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?

          make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator

          define-record-type)

  (import (srfi :99 records)))

; end of file
