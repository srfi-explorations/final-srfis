;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights
;; Reserved.

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

(define-record-type <oset>
  (make-oset mapping)
  oset?
  (mapping oset-mapping))

(define (make-empty-oset comparator)
  (make-oset (mapping comparator)))

;; Constructors

(define (oset comparator . elements)
  (assume (comparator? comparator))
  (oset-unfold null? car cdr elements comparator))

(define oset/ordered oset)

(define (oset-unfold stop? mapper successor seed comparator)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (assume (comparator? comparator))
  (make-oset (mapping-unfold stop?
			    (lambda (seed)
			      (values (mapper seed) 1))
			    successor
			    seed
			    comparator)))


;; Predicates

(define (oset-contains? oset element)
  (assume (oset? oset))
  (mapping-contains? (oset-mapping oset) element))

(define (oset-empty? oset)
  (assume (oset? oset))
  (mapping-empty? (oset-mapping oset)))


(define (oset-disjoint? oset1 oset2)
  (assume (oset? oset1))
  (assume (oset? oset2))
  (mapping-disjoint? (oset-mapping oset1) (oset-mapping oset2)))


;; Accessors

(define (oset-member oset element default)
  (assume (oset? oset))
  (call/cc
   (lambda (return)
     (mapping-search (oset-mapping oset)
		     element
		     (lambda (insert ignore)
		       (return default))
		     (lambda (old-element old-count update remove)
		       (return old-element))))))


(define (oset-element-comparator oset)
  (assume (oset? oset))
  (mapping-key-comparator (oset-mapping oset)))

;; Updaters

(define (oset-adjoin oset . elements)
  (assume (oset? oset))
  (make-oset (fold (lambda (element mapping)
		    (receive (mapping value)
			(mapping-intern mapping element (lambda () 1))
		      mapping))
		  (oset-mapping oset) elements)))

(define oset-adjoin! oset-adjoin)
(define oset-replace! oset-replace)

(define oset-delete! oset-delete)
(define (oset-delete-all oset elements)
  (assume (oset? oset))
  (make-oset (mapping-delete-all (oset-mapping oset) elements)))

(define oset-delete-all! oset-delete-all)

;; The whole oset

(define (oset-size oset)
  (assume (oset? oset))
  (mapping-size (oset-mapping oset)))

(define (oset-find predicate oset failure)
  (assume (procedure? predicate))
  (assume (oset? oset))
  (assume (procedure? failure))
  ((call/cc
    (lambda (return-thunk) 
      (receive (element count)
	  (mapping-find (lambda (element count)
			  (predicate element))
			(oset-mapping oset)
			(lambda () (return-thunk failure)))
	(lambda () element))))))

(define (oset-count predicate oset)
  (assume (procedure? predicate))
  (assume (oset? oset))
  (mapping-count (lambda (element count)
		   (predicate element))
		 (oset-mapping oset)))

(define (oset-any? predicate oset)
  (assume (procedure? predicate))
  (assume (oset? oset))
  (mapping-any? (lambda (element count)
		  (predicate element))
		(oset-mapping oset)))

(define (oset-every? predicate oset)
  (assume (procedure? predicate))
  (assume (oset? oset))
  (mapping-every? (lambda (element count)
		    (predicate element))
		  (oset-mapping oset)))

;; Mapping and folding

(define (oset-map proc comparator oset)
  (assume (procedure? proc))
  (assume (comparator? comparator))
  (assume (oset? oset))
  (make-oset (mapping-map (lambda (element count)
			   (values (proc element)
				   count))
			 (oset-element-comparator oset)
			 (oset-mapping oset))))

(define oset-map/monotone oset-map)

(define (oset-for-each proc oset)
  (assume (procedure? proc))
  (assume (oset? oset))
  (mapping-for-each (lambda (element count)
		      (proc element))
		    (oset-mapping oset)))

(define (oset-fold proc nil oset)
  (assume (procedure? proc))
  (assume (oset? oset))
  (mapping-fold (lambda (element count nil)
		  (proc element nil))
		nil (oset-mapping oset)))

(define (oset-filter predicate oset)
  (assume (procedure? predicate))
  (assume (oset? oset))
  (make-oset (mapping-filter (lambda (element count)
			      (predicate element))
			    (oset-mapping oset))))

(define oset-filter! oset-filter)

(define (oset-remove predicate oset)
  (assume (procedure? predicate))
  (assume (oset? oset))
  (make-oset (mapping-remove (lambda (element count)
			      (predicate element))
			    (oset-mapping oset))))


(define oset-remove! oset-remove)

(define (oset-partition predicate oset)
  (assume (procedure? predicate))
  (assume (oset? oset))
  (receive (mapping1 mapping2)
      (mapping-partition (lambda (element count)
			(predicate element))
			 (oset-mapping oset))
    (values (make-oset mapping1) (make-oset mapping2))))


(define oset-partition! oset-partition)

;; Copying and conversion

(define (oset-copy oset)
  (assume (oset? oset))
  oset)


(define (oset->list oset)
  (assume (oset? oset))
  (mapping-fold/reverse (lambda (element count lst)
			  (cons element lst))
			'() (oset-mapping oset)))

(define (list->oset comparator lst)
  (assume (comparator? comparator))
  (assume (list? lst))
  (apply oset comparator lst))



;; Subsets

(define (oset=? oset . osets)
  (assume (oset? oset))
  (apply mapping=? (make-eqv-comparator) (oset-mapping oset) (map oset-mapping osets)))

(define (oset<? oset . osets)
  (assume (oset? oset))
  (apply mapping<? (make-eqv-comparator) (oset-mapping oset) (map oset-mapping osets)))

(define (oset>? oset . osets)
  (assume (oset? oset))
  (apply mapping>? (make-eqv-comparator) (oset-mapping oset) (map oset-mapping osets)))

(define (oset<=? oset . osets)
  (assume (oset? oset))
  (apply mapping<=? (make-eqv-comparator) (oset-mapping oset) (map oset-mapping osets)))

(define (oset>=? oset . osets)
  (assume (oset? oset))
  (apply mapping>=? (make-eqv-comparator) (oset-mapping oset) (map oset-mapping osets)))


;; Set theory operations

(define (oset-union oset . osets)
  (assume (oset? oset))
  (make-oset (apply mapping-union (oset-mapping oset) (map oset-mapping osets))))

(define (oset-intersection oset . osets)
  (assume (oset? oset))
  (make-oset (apply mapping-intersection (oset-mapping oset) (map oset-mapping osets))))

(define (oset-difference oset . osets)
  (assume (oset? oset))
  (make-oset (apply mapping-difference (oset-mapping oset) (map oset-mapping osets))))

(define (oset-xor oset1 oset2)
  (assume (oset? oset1))
  (assume (oset? oset2))  
  (make-oset (mapping-xor (oset-mapping oset1) (oset-mapping oset2))))

(define oset-union! oset-union)
(define oset-intersection! oset-intersection)
(define oset-difference! oset-difference)
(define oset-xor! oset-xor)

;; Comparators

(define mapping-ordering
  (comparator-ordering-predicate (make-mapping-comparator (make-default-comparator))))

(define (oset-ordering oset1 oset2)
  (mapping-ordering (oset-mapping oset1) (oset-mapping oset2)))

(define oset-comparator
  (make-comparator oset? oset=? oset-ordering #f))

(comparator-register-default! oset-comparator)
