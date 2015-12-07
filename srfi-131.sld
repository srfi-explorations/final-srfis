;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This implementation of SRFI 131 was derived from the reference implementation
; for SRFI 99 by making the following changes:
;
;     removing #t as a possibility for <constructor spec>
;     removing #t as a possibility for <predicate spec>
;     removing <field name> and (<field name>) as possibilities
;         for <field spec>
;
; With those changes, it becomes possible to implement the reduced
; syntax using syntax-rules.
;
; FIXME: indentation and comments need to be improved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS records, syntactic layer.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (srfi :99 records syntactic)

  (export define-record-type)

  (import (except (scheme base) define-record-type)
          (srfi :99 records procedural))

(begin
  (define-syntax define-record-type
    (syntax-rules ()
     ((_ (type-name parent) constructor-spec predicate-spec . field-specs)
      (define-record-type-helper0
       type-name parent constructor-spec predicate-spec . field-specs))
     ((_ type-name constructor-spec predicate-spec . field-specs)
      (define-record-type-helper0
       type-name #f constructor-spec predicate-spec . field-specs))))

  ;; breaks the field-specs into two separate lists of accessors and mutators

  (define-syntax define-record-type-helper0
    (syntax-rules ()
     ((_ type-name parent constructor-spec predicate-spec . field-specs)
      (define-record-type-helper1
       type-name parent constructor-spec predicate-spec field-specs ()))))

  ;; reverses the field-specs before delegating to a second helper

  (define-syntax define-record-type-helper1
    (syntax-rules ()
     ((_ type-name parent constructor-spec predicate-spec () revspecs)
      (define-record-type-helper2
       type-name parent constructor-spec predicate-spec rev-specs () () ()))
     ((_ type-name parent constructor-spec predicate-spec
         (spec . field-specs) revspecs)
      (define-record-type-helper1
        type-name parent constructor-spec predicate-spec
        field-specs (spec . revspecs)))))

  (define-syntax define-record-type-helper2
    (syntax-rules ()
     ((_ type-name
         parent constructor-spec predicate-spec
         () accessors mutators fields)
      (define-record-type-helper
       type-name fields
       parent constructor-spec predicate-spec accessors mutators))
     ((_ type-name parent constructor-spec predicate-spec
         ((field-name accessor-name) . field-specs)
         accessors mutators fields)
      (define-record-type-helper2
        type-name parent constructor-spec predicate-spec
        field-specs
        ((accessor-name field-name) . accessors)
        mutators
        (field-name . fields)))
     ((_ type-name parent constructor-spec predicate-spec
         ((field-name accessor-name mutator-name) . field-specs)
         accessors mutators fields)
      (define-record-type-helper2
        type-name parent constructor-spec predicate-spec
        field-specs
        ((accessor-name field-name) . accessors)
        ((mutator-name field-name) . mutators)
        (field-name . fields)))))

  ;; Uses the SRFI 99 procedural layer for the real work.

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
)
  ) ; srfi :99 records syntactic

; end of file
