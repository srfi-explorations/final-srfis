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
  (make-dynamic-extent point)
  dynamic-extent?
  (point dynamic-extent-point))

(define (current-dynamic-extent)
  (make-dynamic-extent (%dk)))

(define (with-dynamic-extent dynamic-extent thunk)
  (let ((here (%dk)))
    (cond
     ((eq? here (dynamic-extent-point dynamic-extent))
      (thunk))
     (else
      (travel-to-point! here (dynamic-extent-point dynamic-extent))
      (%dk (dynamic-extent-point dynamic-extent))
      (let ((result (thunk)))
	(travel-to-point! (%dk) here)
	(%dk here)
	result)))))

(define (dynamic-extent=? extent1 extent2)
  (eq? (dynamic-extent-point extent1)
       (dynamic-extent-point extent2)))
