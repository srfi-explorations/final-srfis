; Copyright (c) 2005 Sebastian Egner and Jens Axel S{\o}gaard.
; 
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; ``Software''), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
; 
; -----------------------------------------------------------------------
; 
; Compare procedures SRFI (reference implementation)
; Sebastian.Egner@philips.com, Jensaxel@soegaard.net
; history of this file:
;   SE, 14-Oct-2004: first version
;   SE, 18-Oct-2004: 1st redesign: axioms for 'compare function'
;   SE, 29-Oct-2004: 2nd redesign: higher order reverse/map/refine/unite
;   SE,  2-Nov-2004: 3rd redesign: macros cond/refine-compare replace h.o.f's
;   SE, 10-Nov-2004: (im,re) replaced by (re,im) in compare-complex
;   SE, 11-Nov-2004: case-compare by case (not by cond); select-compare added
;   SE, 12-Jan-2005: compare-cdr
;   SE, 15-Feb-2005: stricter typing for compare-<type>; pairwise-not=?
;   SE, 16-Feb-2005: case-compare -> if-compare -> if3; <? </<? chain<? etc.
;   JS, 24-Feb-2005: selection-compare added
;   SE, 25-Feb-2005: selection-compare -> kth-largest modified; if<? etc.
;   JS, 28-Feb-2005: kth-largest modified - is "stable" now
;   SE, 28-Feb-2005: simplified pairwise-not=?/kth-largest; min/max debugged

; =============================================================================

; Reference Implementation
; ========================
;
; in R5RS (including hygienic macros)
;  + SRFI-16 (case-lambda) 
;  + SRFI-23 (error) 
;  + SRFI-27 (random-integer)

; Implementation remarks:
;   * In general, the emphasis of this implementation is on correctness
;     and portability, not on efficiency.
;   * Variable arity procedures are expressed in terms of case-lambda
;     in the hope that this will produce efficient code for the case
;     where the arity is simply a syntactic constant.
;   * In procedures that are required to type-check their arguments,
;     we use (compare x x) for executing extra checks.
;   * Care has been taken to reference comparison procedures of R5RS
;     only at the time the operations here are being defined. This
;     makes it possible to redefine these operations, if need be.
;   * For the sake of efficiency, some inlining has been done by hand.
;
; Hints for low-level implementation:
;   * The basis of this SRFI are the atomic compare procedures, 
;     i.e. compare-boolean, compare-char, etc. and the conditionals
;     if3, if=?, if<? etc. These should be lightning fast.
;   * For the sake of speed, the reference implementation does not
;     use a LET to save the comparison value c for the ERROR call.
;     This can be fixed in a low-level implementation at no cost.

; 3-sided conditional

(define-syntax if3
  (syntax-rules ()
    ((if3 c less equal greater)
     (case c
       ((-1) less)
       (( 0) equal)
       (( 1) greater)
       (else (error "comparison value not in {-1,0,1}"))))))


; 2-sided conditionals for comparisons

(define-syntax if=?
  (syntax-rules ()
    ((if=? c consequence)
     (if=? c consequence (if #f #f)))
    ((if=? c consequence alternate)
     (case c
       ((0)    consequence)
       ((-1 1) alternate)
       (else   (error "comparison value not in {-1,0,1}"))))))

(define-syntax if<?
  (syntax-rules ()
    ((if<? c consequence)
     (if<? c consequence (if #f #f)))
    ((if<? c consequence alternate)
     (case c
       ((-1)  consequence)
       ((0 1) alternate)
       (else   (error "comparison value not in {-1,0,1}"))))))

(define-syntax if>?
  (syntax-rules ()
    ((if>? c consequence)
     (if>? c consequence (if #f #f)))
    ((if>? c consequence alternate)
     (case c
       ((1)    consequence)
       ((-1 0) alternate)
       (else   (error "comparison value not in {-1,0,1}"))))))

(define-syntax if<=?
  (syntax-rules ()
    ((if<=? c consequence)
     (if<=? c consequence (if #f #f)))
    ((if<=? c consequence alternate)
     (case c
       ((-1 0) consequence)
       ((1)    alternate)
       (else   (error "comparison value not in {-1,0,1}"))))))

(define-syntax if>=?
  (syntax-rules ()
    ((if>=? c consequence)
     (if>=? c consequence (if #f #f)))
    ((if>=? c consequence alternate)
     (case c
       ((0 1) consequence)
       ((-1)  alternate)
       (else   (error "comparison value not in {-1,0,1}"))))))

(define-syntax if-not=?
  (syntax-rules ()
    ((if-not=? c consequence)
     (if-not=? c consequence (if #f #f)))
    ((if-not=? c consequence alternate)
     (case c
       ((-1 1) consequence)
       ((0)    alternate)
       (else   (error "comparison value not in {-1,0,1}"))))))


; predicates from compare procedures

(define =?
  (case-lambda
    ((x y)         (if=? (default-compare x y) #t #f))
    ((compare x y) (if=? (compare         x y) #t #f))))

(define <?
  (case-lambda
    ((x y)         (if<? (default-compare x y) #t #f))
    ((compare x y) (if<? (compare         x y) #t #f))))

(define >?
  (case-lambda
    ((x y)         (if>? (default-compare x y) #t #f))
    ((compare x y) (if>? (compare         x y) #t #f))))

(define <=?
  (case-lambda
    ((x y)         (if<=? (default-compare x y) #t #f))
    ((compare x y) (if<=? (compare         x y) #t #f))))

(define >=?
  (case-lambda
    ((x y)         (if>=? (default-compare x y) #t #f))
    ((compare x y) (if>=? (compare         x y) #t #f))))

(define not=?
  (case-lambda
    ((x y)         (if-not=? (default-compare x y) #t #f))
    ((compare x y) (if-not=? (compare         x y) #t #f))))


; chains of length 3

(define </<?
  (case-lambda
    ((x y z)
     (if<? (default-compare x y)
           (if<? (default-compare y z) #t #f)
           (begin (default-compare z z) #f)))
    ((compare x y z)
     (if<? (compare x y)
           (if<? (compare y z) #t #f)
           (begin (compare z z) #f)))))

(define </<=?
  (case-lambda
    ((x y z)
     (if<? (default-compare x y)
           (if<=? (default-compare y z) #t #f)
           (begin (default-compare z z) #f)))
    ((compare x y z)
     (if<? (compare x y)
           (if<=? (compare y z) #t #f)
           (begin (compare z z) #f)))))

(define <=/<?
  (case-lambda
    ((x y z)
     (if<=? (default-compare x y)
            (if<? (default-compare y z) #t #f)
            (begin (default-compare z z) #f)))
    ((compare x y z)
     (if<=? (compare x y)
            (if<? (compare y z) #t #f)
            (begin (compare z z) #f)))))

(define <=/<=?
  (case-lambda
    ((x y z)
     (if<=? (default-compare x y)
            (if<=? (default-compare y z) #t #f)
            (begin (default-compare z z) #f)))
    ((compare x y z)
     (if<=? (compare x y)
            (if<=? (compare y z) #t #f)
            (begin (compare z z) #f)))))

(define >/>?
  (case-lambda
    ((x y z)
     (if>? (default-compare x y)
           (if>? (default-compare y z) #t #f)
           (begin (default-compare z z) #f)))
    ((compare x y z)
     (if>? (compare x y)
           (if>? (compare y z) #t #f)
           (begin (compare z z) #f)))))

(define >/>=?
  (case-lambda
    ((x y z)
     (if>? (default-compare x y)
           (if>=? (default-compare y z) #t #f)
           (begin (default-compare z z) #f)))
    ((compare x y z)
     (if>? (compare x y)
           (if>=? (compare y z) #t #f)
           (begin (compare z z) #f)))))

(define >=/>?
  (case-lambda
    ((x y z)
     (if>=? (default-compare x y)
            (if>? (default-compare y z) #t #f)
            (begin (default-compare z z) #f)))
    ((compare x y z)
     (if>=? (compare x y)
            (if>? (compare y z) #t #f)
            (begin (compare z z) #f)))))

(define >=/>=?
  (case-lambda
    ((x y z)
     (if>=? (default-compare x y)
            (if>=? (default-compare y z) #t #f)
            (begin (default-compare z z) #f)))
    ((compare x y z)
     (if>=? (compare x y)
            (if>=? (compare y z) #t #f)
            (begin (compare z z) #f)))))


; chains of arbitrary length

(define chain=?
  (case-lambda
    ((compare)
     #t)
    ((compare x1)
     (begin (compare x1 x1) #t))
    ((compare x1 x2)
     (if=? (compare x1 x2) #t #f))
    ((compare x1 x2 x3)
     (if=? (compare x1 x2)
           (if=? (compare x2 x3) #t #f)
           (begin (compare x3 x3) #f)))
    ((compare x1 x2 . x3+)
     (if=? (compare x1 x2)
           (let chain? ((head x2) (tail x3+))
             (if (null? tail)
                 #t
                 (if=? (compare head (car tail))
                       (chain? (car tail) (cdr tail))
                       (begin (for-each (lambda (x) (compare x x))
                                        (cdr tail))
                              #f))))
           (begin (for-each (lambda (x) (compare x x))
                            x3+) 
                  #f)))))

(define chain<?
  (case-lambda
    ((compare)
     #t)
    ((compare x1)
     (begin (compare x1 x1) #t))
    ((compare x1 x2)
     (if<? (compare x1 x2) #t #f))
    ((compare x1 x2 x3)
     (if<? (compare x1 x2)
           (if<? (compare x2 x3) #t #f)
           (begin (compare x3 x3) #f)))
    ((compare x1 x2 . x3+)
     (if<? (compare x1 x2)
           (let chain? ((head x2) (tail x3+))
             (if (null? tail)
                 #t
                 (if<? (compare head (car tail))
                       (chain? (car tail) (cdr tail))
                       (begin (for-each (lambda (x) (compare x x))
                                        (cdr tail))
                              #f))))
           (begin (for-each (lambda (x) (compare x x))
                            x3+) 
                  #f)))))

(define chain>?
  (case-lambda
    ((compare)
     #t)
    ((compare x1)
     (begin (compare x1 x1) #t))
    ((compare x1 x2)
     (if>? (compare x1 x2) #t #f))
    ((compare x1 x2 x3)
     (if>? (compare x1 x2)
           (if>? (compare x2 x3) #t #f)
           (begin (compare x3 x3) #f)))
    ((compare x1 x2 . x3+)
     (if>? (compare x1 x2)
           (let chain? ((head x2) (tail x3+))
             (if (null? tail)
                 #t
                 (if>? (compare head (car tail))
                       (chain? (car tail) (cdr tail))
                       (begin (for-each (lambda (x) (compare x x))
                                        (cdr tail))
                              #f))))
           (begin (for-each (lambda (x) (compare x x))
                            x3+) 
                  #f)))))

(define chain<=?
  (case-lambda
    ((compare)
     #t)
    ((compare x1)
     (begin (compare x1 x1) #t))
    ((compare x1 x2)
     (if<=? (compare x1 x2) #t #f))
    ((compare x1 x2 x3)
     (if<=? (compare x1 x2)
            (if<=? (compare x2 x3) #t #f)
            (begin (compare x3 x3) #f)))
    ((compare x1 x2 . x3+)
     (if<=? (compare x1 x2)
            (let chain? ((head x2) (tail x3+))
              (if (null? tail)
                  #t
                  (if<=? (compare head (car tail))
                         (chain? (car tail) (cdr tail))
                         (begin (for-each (lambda (x) (compare x x))
                                          (cdr tail))
                                #f))))
            (begin (for-each (lambda (x) (compare x x))
                             x3+) 
                   #f)))))

(define chain>=?
  (case-lambda
    ((compare)
     #t)
    ((compare x1)
     (begin (compare x1 x1) #t))
    ((compare x1 x2)
     (if>=? (compare x1 x2) #t #f))
    ((compare x1 x2 x3)
     (if>=? (compare x1 x2)
            (if>=? (compare x2 x3) #t #f)
            (begin (compare x3 x3) #f)))
    ((compare x1 x2 . x3+)
     (if>=? (compare x1 x2)
            (let chain? ((head x2) (tail x3+))
              (if (null? tail)
                  #t
                  (if>=? (compare head (car tail))
                         (chain? (car tail) (cdr tail))
                         (begin (for-each (lambda (x) (compare x x))
                                          (cdr tail))
                                #f))))
            (begin (for-each (lambda (x) (compare x x))
                             x3+) 
                   #f)))))


; pairwise inequality

(define pairwise-not=?
  (let ((= =) (<= <=))
    (case-lambda
      ((compare)
       #t)
      ((compare x1)
       (begin (compare x1 x1) #t))
      ((compare x1 x2)
       (if-not=? (compare x1 x2) #t #f))
      ((compare x1 x2 x3)
       (if-not=? (compare x1 x2)
                 (if-not=? (compare x2 x3)
                           (if-not=? (compare x1 x3) #t #f)
                           #f)
                 (begin (compare x3 x3) #f)))
      ((compare . x1+)
       (let unequal? ((x x1+) (n (length x1+)) (unchecked? #t))
         (if (< n 2)
             (begin
               (if (and unchecked? (= n 1))
                   (compare (car x) (car x)))
               #t)
             (let* ((i-pivot (random-integer n))
                    (x-pivot (list-ref x i-pivot)))
               (let split ((i 0) (x x) (x< '()) (x> '()))
                 (if (null? x)
                     (and (unequal? x< (length x<) #f)
                          (unequal? x> (length x>) #f))
                     (if (= i i-pivot)
                         (split (+ i 1) (cdr x) x< x>)
                         (if3 (compare (car x) x-pivot)
                              (split (+ i 1) (cdr x) (cons (car x) x<) x>)
                              (begin (if unchecked?
                                         (for-each (lambda (u) (compare u u))
                                                   (cdr x)))
                                     #f)
                              (split (+ i 1) (cdr x) x< (cons (car x) x>)))))))))))))


; min/max

(define min-compare
  (case-lambda
    ((compare x1)
     (begin (compare x1 x1) x1))
    ((compare x1 x2)
     (if<=? (compare x1 x2) x1 x2))
    ((compare x1 x2 x3)
     (if<=? (compare x1 x2)
            (if<=? (compare x1 x3) x1 x3)
            (if<=? (compare x2 x3) x2 x3)))
    ((compare x1 x2 x3 x4)
     (if<=? (compare x1 x2)
            (if<=? (compare x1 x3)
                   (if<=? (compare x1 x4) x1 x4)
                   (if<=? (compare x3 x4) x3 x4))
            (if<=? (compare x2 x3)
                   (if<=? (compare x2 x4) x2 x4)
                   (if<=? (compare x3 x4) x3 x4))))
    ((compare x1 x2 . x3+)
     (let min ((xmin (if<=? (compare x1 x2) x1 x2)) (xs x3+))
       (if (null? xs)
           xmin
           (min (if<=? (compare xmin (car xs)) xmin (car xs))
                (cdr xs)))))))

(define max-compare
  (case-lambda
    ((compare x1)
     (begin (compare x1 x1) x1))
    ((compare x1 x2)
     (if>=? (compare x1 x2) x1 x2))
    ((compare x1 x2 x3)
     (if>=? (compare x1 x2)
            (if>=? (compare x1 x3) x1 x3)
            (if>=? (compare x2 x3) x2 x3)))
    ((compare x1 x2 x3 x4)
     (if>=? (compare x1 x2)
            (if>=? (compare x1 x3)
                   (if>=? (compare x1 x4) x1 x4)
                   (if>=? (compare x3 x4) x3 x4))
            (if>=? (compare x2 x3)
                   (if>=? (compare x2 x4) x2 x4)
                   (if>=? (compare x3 x4) x3 x4))))
    ((compare x1 x2 . x3+)
     (let max ((xmax (if>=? (compare x1 x2) x1 x2)) (xs x3+))
       (if (null? xs)
           xmax
           (max (if>=? (compare xmax (car xs)) xmax (car xs))
                (cdr xs)))))))


; kth-largest

(define kth-largest
  (let ((= =) (< <))
    (case-lambda
      ((compare k x0)
       (case (modulo k 1)
         ((0) (compare x0 x0) x0)
         (else (error "bad index" k))))
      ((compare k x0 x1)
       (case (modulo k 2)
         ((0) (if<=? (compare x0 x1) x0 x1))
         ((1) (if<=? (compare x0 x1) x1 x0))
         (else (error "bad index" k))))
      ((compare k x0 x1 x2)
       (case (modulo k 3)
         ((0) (if<=? (compare x0 x1)
                     (if<=? (compare x0 x2) x0 x2)
                     (if<=? (compare x1 x2) x1 x2)))
         ((1) (if3 (compare x0 x1)
                   (if<=? (compare x1 x2)
                          x1
                          (if<=? (compare x0 x2) x2 x0))
                   (if<=? (compare x0 x2) x1 x0)
                   (if<=? (compare x0 x2)
                          x0
                          (if<=? (compare x1 x2) x2 x1))))
         ((2) (if<=? (compare x0 x1)
                     (if<=? (compare x1 x2) x2 x1)
                     (if<=? (compare x0 x2) x2 x0)))
         (else (error "bad index" k))))
      ((compare k x0 . x1+) ; |x1+| >= 1
       (if (not (and (integer? k) (exact? k)))
           (error "bad index" k))
       (let ((n (+ 1 (length x1+))))
         (let kth ((k   (modulo k n))
                   (n   n)  ; = |x|
                   (rev #t) ; are x<, x=, x> reversed?
                   (x   (cons x0 x1+)))
           (let ((pivot (list-ref x (random-integer n))))
             (let split ((x x) (x< '()) (n< 0) (x= '()) (n= 0) (x> '()) (n> 0))
               (if (null? x)
                   (cond
                     ((< k n<)
                      (kth k n< (not rev) x<))
                     ((< k (+ n< n=))
                      (if rev
                          (list-ref x= (- (- n= 1) (- k n<)))
                          (list-ref x= (- k n<))))
                     (else
                      (kth (- k (+ n< n=)) n> (not rev) x>)))
                   (if3 (compare (car x) pivot)
                        (split (cdr x) (cons (car x) x<) (+ n< 1) x= n= x> n>)
                        (split (cdr x) x< n< (cons (car x) x=) (+ n= 1) x> n>)
                        (split (cdr x) x< n< x= n= (cons (car x) x>) (+ n> 1))))))))))))


; compare functions from predicates

(define (compare< < x y) (if (< x y) -1 (if (< y x)  1 0)))
(define (compare> > x y) (if (> x y)  1 (if (> y x) -1 0)))

(define (compare<= <= x y) (if (<= x y) (if (<= y x) 0 -1)  1))
(define (compare>= >= x y) (if (>= x y) (if (>= y x) 0  1) -1))

(define (compare=/< = < x y) (if (= x y) 0 (if (< x y) -1  1)))
(define (compare=/> = > x y) (if (= x y) 0 (if (> x y)  1 -1)))

(define (compare=/<= = <= x y) (if (= x y) 0 (if (<= x y) -1  1)))
(define (compare=/>= = >= x y) (if (= x y) 0 (if (>= x y)  1 -1)))

;   Compare=/<= is identical to Compare=/<, and similar =/>= and =/>.
;   To avoid confusion when the procedure names are printed, we
;   define them redundantly.


; refine and extend construction (reverse and transform are trivial)

(define-syntax refine-compare
  (syntax-rules ()
    ((refine-compare)
     0)
    ((refine-compare c1)
     c1)
    ((refine-compare c1 c2 cs ...)
     (if3 c1 -1 (refine-compare c2 cs ...) 1))))

(define-syntax select-compare
  (syntax-rules (else)
    ((select-compare x y clause ...)
     (let ((x-val x) (y-val y))
       (select-compare (x-val y-val clause ...))))
    ; used internally: (select-compare (x y clause ...))
    ((select-compare (x y))
     0)
    ((select-compare (x y (else c ...)))
     (refine-compare c ...))
    ((select-compare (x y (t? c ...) clause ...))
     (let ((t?-val t?))
       (let ((tx (t?-val x)) (ty (t?-val y)))
         (if tx
             (if ty (refine-compare c ...) -1)
             (if ty 1 (select-compare (x y clause ...)))))))))

(define-syntax cond-compare
  (syntax-rules (else)
    ((cond-compare)
     0)
    ((cond-compare (else cs ...))
     (refine-compare cs ...))
    ((cond-compare ((tx ty) cs ...) clause ...)
     (let ((tx-val tx) (ty-val ty))
       (if tx-val
           (if ty-val (refine-compare cs ...) -1)
           (if ty-val 1 (cond-compare clause ...)))))))


; R5RS atomic types

; Implementation remark:
;   Since these procedures may be called frequently, it is important
;   to have fast implementations for them.

(define-syntax compare:type-check
  (syntax-rules ()
    ((compare:type-check type? type-name x)
     (if (not (type? x))
         (error (string-append "not " type-name ":") x)))
    ((compare:type-check type? type-name x y)
     (begin (compare:type-check type? type-name x)
            (compare:type-check type? type-name y)))))

(define (compare-boolean x y)
  (compare:type-check boolean? "boolean" x y)
  (if x (if y 0 1) (if y -1 0)))

(define compare-char
  (let ((= char=?) (< char<?))
    (lambda (x y)
      (compare:type-check char? "char" x y)
      (if (= x y) 0 (if (< x y) -1 1)))))

(define compare-char-ci
  (let ((= char-ci=?) (< char-ci<?))
    (lambda (x y)
      (compare:type-check char? "char" x y)
      (if (= x y) 0 (if (< x y) -1 1)))))

(define compare-string
  (let ((= string=?) (< string<?))
    (lambda (x y)
      (compare:type-check string? "string" x y)
      (if (= x y) 0 (if (< x y) -1 1)))))

(define compare-string-ci
  (let ((= string-ci=?) (< string-ci<?))
    (lambda (x y)
      (compare:type-check string? "string" x y)
      (if (= x y) 0 (if (< x y) -1 1)))))

(define (compare-symbol x y)
  (compare:type-check symbol? "symbol" x y)
  (compare-string (symbol->string x) (symbol->string y)))

(define compare-integer
  (let ((= =) (< <))
    (lambda (x y)
      (compare:type-check integer? "integer" x y)
      (if (= x y) 0 (if (< x y) -1 1)))))

(define compare-rational
  (let ((= =) (< <))
    (lambda (x y)
      (compare:type-check rational? "rational" x y)
      (if (= x y) 0 (if (< x y) -1 1)))))

(define compare-real
  (let ((= =) (< <))
    (lambda (x y)
      (compare:type-check real? "real" x y)
      (if (= x y) 0 (if (< x y) -1 1)))))

(define (compare-complex x y)
  (compare:type-check complex? "complex" x y)
  (if (and (real? x) (real? y))
      (compare-real x y)
      (refine-compare (compare-real (real-part x) (real-part y))
                      (compare-real (imag-part x) (imag-part y)))))

(define (compare-number x y)
  (compare:type-check number? "number" x y)
  (compare-complex x y))


; R5RS compound data structures: dotted pair, list, vector

(define (compare-car compare)
  (lambda (x y)
    (compare (car x) (car y))))

(define (compare-cdr compare)
  (lambda (x y)
    (compare (cdr x) (cdr y))))

(define compare-pair
  (case-lambda
    
    ; dotted pair
    ((compare-car compare-cdr x y)
     (refine-compare (compare-car (car x) (car y))
                     (compare-cdr (cdr x) (cdr y))))
    
    ; possibly improper lists
    ((compare x y)
     (cond-compare 
      (((null? x) (null? y)) 0)
      (((pair? x) (pair? y)) (compare              (car x) (car y))
                             (compare-pair compare (cdr x) (cdr y)))
      (else                  (compare x y))))
    
    ; for convenience
    ((x y)
     (compare-pair default-compare x y))))

(define compare-list
  (case-lambda
    ((compare x y empty? head tail)
     (cond-compare
      (((empty? x) (empty? y)) 0)
      (else (compare              (head x) (head y))
            (compare-list compare (tail x) (tail y) empty? head tail))))
    
    ; for convenience
    ((        x y empty? head tail)
     (compare-list default-compare x y empty? head tail))
    ((compare x y              )
     (compare-list compare         x y null? car   cdr))
    ((        x y              )
     (compare-list default-compare x y null? car   cdr))))

(define compare-list-as-vector
  (case-lambda
    ((compare x y empty? head tail)
     (refine-compare
      (let compare-length ((x x) (y y))
        (cond-compare
         (((empty? x) (empty? y)) 0)
         (else (compare-length (tail x) (tail y)))))
      (compare-list compare x y empty? head tail)))
    
    ; for convenience
    ((        x y empty? head tail)
     (compare-list-as-vector default-compare x y empty? head tail))
    ((compare x y              )
     (compare-list-as-vector compare         x y null?  car  cdr))
    ((        x y              )
     (compare-list-as-vector default-compare x y null?  car  cdr))))

(define compare-vector
  (let ((= =))
    (case-lambda
      ((compare x y size ref)
       (let ((n (size x)) (m (size y)))
         (refine-compare 
          (compare-integer n m)
          (let compare-rest ((i 0)) ; compare x[i..n-1] y[i..n-1]
            (if (= i n)
                0
                (refine-compare (compare (ref x i) (ref y i))
                                (compare-rest (+ i 1))))))))
      
      ; for convenience
      ((        x y size ref)
       (compare-vector default-compare x y size          ref))
      ((compare x y           )
       (compare-vector compare         x y vector-length vector-ref))
      ((        x y           )
       (compare-vector default-compare x y vector-length vector-ref)))))

(define compare-vector-as-list
  (let ((= =))
    (case-lambda
      ((compare x y size ref)
       (let ((nx (size x)) (ny (size y)))
         (let ((n (min nx ny)))
           (let compare-rest ((i 0)) ; compare x[i..n-1] y[i..n-1]
             (if (= i n)
                 (compare-integer nx ny)
                 (refine-compare (compare (ref x i) (ref y i))
                                 (compare-rest (+ i 1))))))))
      
      ; for convenience
      ((        x y size ref)
       (compare-vector-as-list default-compare x y size          ref))
      ((compare x y           )
       (compare-vector-as-list compare         x y vector-length vector-ref))
      ((        x y           )
       (compare-vector-as-list default-compare x y vector-length vector-ref)))))


; default compare

(define (default-compare x y)
  (select-compare x y
                  (null?    0)
                  (pair?    (default-compare (car x) (car y))
                            (default-compare (cdr x) (cdr y)))
                  (boolean? (compare-boolean x y))
                  (char?    (compare-char    x y))
                  (string?  (compare-string  x y))
                  (symbol?  (compare-symbol  x y))
                  (number?  (compare-number  x y))
                  (vector?  (compare-vector default-compare x y))
                  (else (error "unrecognized type in default-compare" x y))))

; Note that we pass default-compare to compare-{pair,vector} explictly.
; This makes sure recursion proceeds with this default-compare, which 
; need not be the one in the lexical scope of compare-{pair,vector}.


; debug compare

(define (debug-compare c)
  
  (define (checked-value c x y)
    (let ((c-xy (c x y)))
      (if (or (eqv? c-xy -1) (eqv? c-xy 0) (eqv? c-xy 1))
          c-xy
          (error "compare value not in {-1,0,1}" c-xy (list c x y)))))
  
  (define (random-boolean)
    (zero? (random-integer 2)))
  
  (define q ; (u v w) such that u <= v, v <= w, and not u <= w
    '#(
       ;x < y   x = y   x > y   [x < z]
       0       0       0    ; y < z
               0    (z y x) (z y x) ; y = z
               0    (z y x) (z y x) ; y > z
               
               ;x < y   x = y   x > y   [x = z]
               (y z x) (z x y)    0    ; y < z
               (y z x)    0    (x z y) ; y = z
               0    (y x z) (x z y) ; y > z
               
               ;x < y   x = y   x > y   [x > z]
               (x y z) (x y z)    0    ; y < z
               (x y z) (x y z)    0    ; y = z
               0       0       0    ; y > z
               ))
  
  (let ((z? #f) (z #f)) ; stored element from previous call
    (lambda (x y)
      (let ((c-xx (checked-value c x x))
	    (c-yy (checked-value c y y))
	    (c-xy (checked-value c x y))
	    (c-yx (checked-value c y x)))
	(if (not (zero? c-xx))
	    (error "compare error: not reflexive" c x))
	(if (not (zero? c-yy))
	    (error "compare error: not reflexive" c y))
	(if (not (zero? (+ c-xy c-yx)))
	    (error "compare error: not anti-symmetric" c x y))
	(if z?
	    (let ((c-xz (checked-value c x z))
		  (c-zx (checked-value c z x))
		  (c-yz (checked-value c y z))
		  (c-zy (checked-value c z y)))
	      (if (not (zero? (+ c-xz c-zx)))
		  (error "compare error: not anti-symmetric" c x z))
	      (if (not (zero? (+ c-yz c-zy)))
		  (error "compare error: not anti-symmetric" c y z))
	      (let ((ijk (vector-ref q (+ c-xy (* 3 c-yz) (* 9 c-xz) 13))))
		(if (list? ijk)
		    (apply error
			   "compare error: not transitive"
			   c 
			   (map (lambda (i) (case i ((x) x) ((y) y) ((z) z)))
				ijk)))))
	    (set! z? #t))
	(set! z (if (random-boolean) x y)) ; randomized testing
	c-xy))))
