(import (scheme base))
(import (scheme char))
(import (chibi test))
(import (srfi 153))
(import (srfi 128))

(define default-comparator (make-default-comparator))

(define number-comparator
  (make-comparator number? = < #f))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<? #f))

(define string-comparator
  (make-comparator string? string=? string<? #f))

(define eq-comparator (make-eq-comparator))

(define vlist '())

(define (failure) 'fail)

(define default-test-comparator (current-test-comparator))

(define (oset-equal? x y)
  (cond
    ((and (oset? x) (oset? y)) (oset=? x y))
    ((oset? x) #f)
    ((oset? y) #f)
    (else (default-test-comparator x y))))

(current-test-comparator oset-equal?)

; oset0 = {}
(define oset0 (oset number-comparator))

; oset1 = oset2 = {1, 2, 3, 4, 5} both settable
(define oset1 (oset number-comparator 5 4 3 2 1))
(define oset2 (oset/ordered number-comparator 1 2 3 4 5))

; oset3 = oset4 = {100, 200, 300, 400, 500}
; oset3 settable, oset4 not settable
(define oset3 (oset-unfold
                (lambda (x) (= x 1))
                (lambda (x) (* x 100))
                (lambda (x) (- x 1))
                5 number-comparator))

(define oset4 (oset-unfold/ordered
                (lambda (x) (= x 5))
                (lambda (x) (* x 100))
                (lambda (x) (+ x 1))
                1 number-comparator))

; oset5 = {"a", "b", "c", "d", "e"} case insensitive, settable
(define oset5 (oset string-ci-comparator "a" "b" "c" "d" "e"))

; oset6 = {1, 2, 3, 4, 5} not settable
(define oset6 (oset number-comparator 1 2 3 4 5))

; oset7 = {"a", "b", "c", "d", "e"} not settable
(define oset7 (oset string-comparator "a" "b" "c" "d" "e"))

; oset8 = {"a", "b", "c", "d", "e", 1, 2, 3, 4, 5}
(define oset8 (oset default-comparator "a" "b" "c" "d" "e" 1 2 3 4 5))

;; Constructors

(test-group "oset/constructors"
(test-assert (oset-contains? oset1 1))
(test-assert (oset-contains? oset1 2))
(test-assert (oset-contains? oset1 3))
(test-assert (oset-contains? oset1 4))
(test-assert (oset-contains? oset1 5))
(test 5 (oset-size oset1))

(test-assert (oset-contains? oset2 1))
(test-assert (oset-contains? oset2 2))
(test-assert (oset-contains? oset2 3))
(test-assert (oset-contains? oset2 4))
(test-assert (oset-contains? oset2 5))
(test 5 (oset-size oset2))

(test oset3 (oset number-comparator 100 200 300 400 500))
(test oset4 (oset number-comparator 100 200 300 400 500))
(test-assert (oset-contains? oset5 "a"))
(test-assert (oset-contains? oset5 "A"))
(test 10 (oset-size oset8))
)

;; Predicates

(test-group "osets/predicates"

(test-assert (oset? oset1))
(test-assert (oset? oset2))
(test-assert (oset? oset3))
(test-assert (oset? oset4))
(test-assert (oset? oset5))

(test-assert (oset-contains? oset1 3))
(test-not (oset-contains? oset1 10))

(test-assert (oset-empty? oset0))
(test-not (oset-empty? oset1))

(test-not (oset-disjoint? oset1 oset2))
(test-not (oset-disjoint? oset1 oset2))
)

;; Accessors

(test-group "osets/accessors"

(test 1 (oset-member oset1 1 (failure)))
(test 'fail (oset-member oset1 100 (failure)))
(test "a" (oset-member oset5 "A" (failure)))
(test 'fail (oset-member oset5 "z" (failure)))

(test-equal eq? number-comparator (oset-element-comparator oset4))
)

;; Updaters

(test-group "osets/updaters"

(test (oset number-comparator 1 2 3 4 5 6 7) (oset-adjoin oset1 6 7))
; oset2 = {1, 2, 3, 4, 5, 6, 7}
(set! oset2 (oset-adjoin! oset2 6 7))
(test oset2 (oset number-comparator 1 2 3 4 5 6 7))
(test oset5 (oset-adjoin oset5 "A"))

(test oset1 (oset-replace oset1 10))
(test (oset "A" "b" "c" "d" "e") (oset-replace oset5 "A"))
; oset5 = {"A", "b", "c", "d", "e"}
(set! oset5 (oset-replace! oset5 "A"))
(test oset5 (oset "A" "b" "c" "d" "e"))

(test (oset number-comparator 1 2 3) (oset-delete oset1 4 5))
(test (oset number-comparator 1 2 3) (oset-delete-all (list 4 5)))
; oset1 = {1, 2, 3, 4}
(set! oset1 (oset-delete! oset1 5))
(test oset1 (oset number-comparator 1 2 3 4))
; oset1 = {1, 2, 3}
(set! oset1 (oset-delete-all! oset1 (list 4)))
(test oset1 (oset number-comparator 1 2 3))

(test 'fail (oset-pop oset0 (lambda () failure)))
(test 'fail (oset-pop! oset0 (lambda () failure)))

(test-values (values (oset 2 3 4 5 6 7) 1) (oset-pop oset2 failure))

(set! vlist (call-with-values (lambda () (oset-pop! oset2 failure)) list))
; oset2 = {2, 3, 4, 5, 6, 7}
(set! oset2 (car vlist))
(test oset2 (oset 2 3 4 5 6 7))
(test 1 (cadr vlist))

(test-values (values (oset number-comparator 1 2 3 4 5 6) 7) (oset-pop/reverse oset2 failure))
(set! vlist (call-with-values (lambda () (oset-pop!/reverse oset2 failure)) list))
; oset2 = {2, 3, 4, 5, 6}
(set! oset2 (car vlist))
(test oset2 (oset 2 3 4 5 6))
(test 7 (cadr vlist))
)

;; The whole oset
(test-group "oset/whole"

(test 5 (oset-size oset6))
(test 1 (oset-find odd? oset6 failure))
(test 'fail (oset-find zero? oset6 failure))

(test 2 (oset-count even? oset6))
(test 0 (oset-count zero? oset6))

(test-assert (oset-any? odd? (oset number-comparator 1 2 4 6 8)))
(test-assert (oset-every? even? (oset number-comparator 2 4 6 8)))
)


;; Mapping and folding

(test-group "osets/mapping"
(test oset7 (oset-map string-ci-comparator symbol->string (oset eq-comparator 'a 'b 'c 'd 'e)))
(test oset7 (oset-map/monotone string-ci-comparator symbol->string (oset eq-comparator 'a 'b 'c 'd 'e)))

(test '(5 4 3 2 1)
      (let ((r '()))
        (oset-for-each
          (lambda (i) (set! r (cons i r)))
          oset6)))

(test 15 (oset-fold + 0 oset6))
(test "abcde" (oset-fold string-append "" oset7))
(test "edcba" (oset-fold/reverse string-append "" oset7))
(test oset6 (oset-filter number? oset8))
(test oset7 (oset-remove number? oset8))
(set! vlist
  (call-with-values
    (lambda () (oset-partition number? oset8))
    list))

(test vlist (list oset6 oset7))
)
