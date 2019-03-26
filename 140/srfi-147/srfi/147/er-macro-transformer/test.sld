;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

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

(define-library (srfi 147 er-macro-transformer test)
  (export run-tests)
  (include-library-declarations "../../../custom-macro-transformers.scm")
  (import (srfi 147 er-macro-transformer))
  (import (srfi 64))
  (begin
    (define (run-tests)
      (test-begin "SRFI 147: er-macro-transformer")

      (test-group "R7RS macros"

        (define-syntax foo
	  (er-macro-transformer
	   (lambda (expr rename compare)
	     42)))

	(test-equal 42 (foo))

	(test-equal 42 (let-syntax
			   ((foo
			     (er-macro-transformer
			      (lambda (expr rename compare)
				42))))
			 (foo)))

	(test-equal 42 (letrec-syntax
			   ((foo
			     (er-macro-transformer
			      (lambda (expr rename compare)
				42)))
			    (bar
			     (er-macro-transformer
			      (lambda (expr rename compare)
				`(,(rename 'foo))))))
			 (bar))))

      (test-group "Custom macro transformers"

	(define-syntax unhygienic-transformer
	  (syntax-rules ()
	    ((unhygienic-transformer transformer)
	     (er-macro-transformer
	      (lambda (expr rename compare)
		(transformer expr))))))

	(define-syntax bar-transformer
	  (unhygienic-transformer
	   (lambda (expr)
	     '(unhygienic-transformer
	       (lambda (expr)
		 ''bar)))))
	
	(define-syntax foo
	  (unhygienic-transformer
	   (lambda (expr)
	     42)))

	(define-syntax bar
	  (bar-transformer))
	
	(test-equal 42 (foo))
    
	(test-equal 42 (let-syntax
			   ((foo
			     (unhygienic-transformer
			       (lambda (expr)
				 42))))
			 (foo)))

	(test-equal 'bar (bar 42)))

      (test-end))))
