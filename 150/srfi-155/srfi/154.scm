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

(define-record-type <dynamic-extent>
  (make-dynamic-extent proc)
  dynamic-extent?
  (proc dynamic-extent-proc))

(define (current-dynamic-extent)
  (call-with-current-continuation
   (lambda (return)
     (let-values
	 (((k thunk)
	   (call-with-current-continuation
	    (lambda (c)
	       (return
		(make-dynamic-extent (lambda (thunk)
					    (call-with-current-continuation
					     (lambda (k)
					       (c k thunk))))))))))
       (call-with-values thunk k)))))

(define (dynamic-extent=? extent1 extent2)
  (eq? (dynamic-extent-proc extent1)
       (dynamic-extent-proc extent2)))

(define (with-dynamic-extent dynamic-extent thunk)
  ((dynamic-extent-proc dynamic-extent) thunk))
