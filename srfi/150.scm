;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Syntax

;; Syntactic keyword used to access the information stored in record-type descriptors
(define-syntax :secret (syntax-rules ()))

;; The record-type definition keyword, which is exported from the library
(define-syntax define-record-type/150
  (ck-macro-transformer ()
    ((define-record-type/150 type-spec
       constructor-spec
       predicate-spec
       field-spec
       ...)
     ((parse-type-spec 'type-spec) => '(rtd-name parent-rtd))
     ((parse-predicate-spec 'predicate-spec) => 'predicate-name)
     ((parent-rtd ':secret) => '(parent-size parent-make-subtype parent-fields))
     ((parse-field-specs 'parent-size 'field-spec ...)
      => '(size
	   (field-name accessor-name mutator-name index)
           ...))
     ((parse-constructor-spec 'constructor-spec
			      'parent-fields
			      '((field-name accessor-name mutator-name) ...))
      => '(constructor-name mutators))
     `(begin
	(define-values (type-accessor type-constructor type-predicate instance-accessor make-subtype)
	  (parent-make-subtype #f))
	(define predicate-name (make-predicate type-predicate))
	(define accessor-name (make-accessor instance-accessor index))
	...
	(define mutator-name (make-mutator instance-accessor index))
	...
	(define constructor-name (make-constructor type-constructor size (list .  mutators)))
	,(define-rtd 'rtd-name
	   'size
	   'make-subtype
	   (ck-append
	    (ck-reverse '((field-name accessor-name mutator-name) ...))
	    'parent-fields))))))

;; Helper macros that set up the parameters of a record-type definition

(define-syntax parse-type-spec
  (ck-macro-transformer ()
    ((parse-type-spec '(rtd parent-rtd)) '(rtd parent-rtd))
    ((parse-type-spec '#f) '(rtd root-rtd))
    ((parse-type-spec 'rtd) '(rtd root-rtd))))

(define-syntax parse-predicate-spec
  (ck-macro-transformer ()
    ((parse-predicate-spec '#f) 'predicate-name)
    ((parse-predicate-spec 'predicate-name) 'predicate-name)))

(define-syntax parse-field-specs
  (ck-macro-transformer ()
    ((parse-field-specs 'index)
     '(index))
    ((parse-field-specs 'index '(name accessor) 'field-specs ...)
     (parse-field-specs 'index '(name accessor mutator) 'field-specs ...))
    ((parse-field-specs 'index '(name accessor mutator) 'field-specs ...)
     ((parse-field-specs '(+ 1 index) 'field-specs ...) => '(size fields ...))
     '(size (name accessor mutator index) fields ...))))

(define-syntax parse-constructor-spec
  (ck-macro-transformer ()
    ((parse-constructor-spec '#f 'parent-fields 'fields)
     (parse-constructor-spec '(constructor-name) 'parent-fields 'fields))
    ((parse-constructor-spec '(constructor-name field ...) 'parent-fields 'fields)
     `(constructor-name (,(get-mutator 'field 'parent-fields 'fields) ...)))    
    ((parse-constructor-spec 'constructor-name
			     '((parent-field-name parent-accessor parent-mutator) ...)
			     '((field-name accessor mutator) ...))
     `(constructor-name ,(ck-append (ck-reverse '(parent-mutator ...)) '(mutator ...))))))

;; Locates the mutator of a field in a record-type or one of its ancestors

(define-syntax get-mutator
  (ck-macro-transformer ()
    ((get-mutator 'field
		  '((parent-field-name parent-accessor parent-mutator) ...)
		  '((field-name accessor mutator) ...))
     (ck-cadr
      (ck-or (ck-assoc 'field '((field-name mutator) ...) ck-equal?)
	     (ck-assoc 'field '((accessor mutator) ...) ck-equal?)
	     (ck-assoc 'field '((parent-field-name parent-mutator) ...) ck-equal?/free)
	     (ck-assoc 'field '((parent-accessor parent-mutator) ...) ck-equal?/free)
	     (ck-error '"record field not found" 'field))))))

(define-syntax define-rtd
  (ck-macro-transformer ()
    ((define-rtd 'rtd 'size 'subtype-constructor 'fields)
     '(define-syntax rtd
	(ck-macro-transformer ::: ()
          ((rtd ':secret) '(size subtype-constructor fields))
	  ((rtd 'arg :::)
	   (ck-error "invalid use of record-type descriptor")))))))

;; Root record-type descriptor

(define-rtd 'root-rtd '0 'make-type '())

;; Auxiliary composable macros

;; Like ck-equal? but identifiers are compared using ck-free-identifier=?
(define-syntax ck-equal?/free
  (ck-macro-transformer ()
    ((ck-equal?/free 'a 'b)
     (ck-if (ck-and (ck-symbol? 'a) (ck-symbol? 'b))
	    (ck-free-identifier=? 'a 'b)
	    (ck-equal? 'a 'b)))))

;;; Procedures

(define (make-predicate type-predicate)
  type-predicate)

(define (make-accessor type-accessor index)
  (lambda (record)
    (vector-ref (type-accessor record) index)))

(define (make-mutator type-accessor index)
  (lambda (record value)
    (vector-set! (type-accessor record) index value)))

(define (make-constructor type-constructor size mutators)
  (lambda args
    (let ((record (type-constructor (make-vector size))))
      (for-each (lambda (arg mutator)
		  (mutator record arg))
		args mutators)
      record)))
