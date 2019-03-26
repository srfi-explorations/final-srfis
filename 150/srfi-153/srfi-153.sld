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

(define-library (srfi-153)
  (export oset oset-unfold
	  oset? oset-contains? oset-empty? oset-disjoint?
	  oset-member oset-element-comparator
	  oset-adjoin oset-adjoin! oset-replace oset-replace!
	  oset-delete oset-delete! oset-delete-all oset-delete-all! oset-search!
	  oset-size oset-find oset-count oset-any? oset-every?
	  oset-map oset-for-each oset-fold
	  oset-filter oset-remove oset-partition
	  oset-filter! oset-remove! oset-partition!
	  oset-copy oset->list list->oset list->oset!
	  oset=? oset<? oset>? oset<=? oset>=?
	  oset-union oset-intersection oset-difference oset-xor
	  oset-union! oset-intersection! oset-difference! oset-xor!
	  oset-comparator
	  obag obag-unfold
	  obag? obag-contains? obag-empty? obag-disjoint?
	  obag-member obag-element-comparator
	  obag-adjoin obag-adjoin! obag-replace obag-replace!
	  obag-delete obag-delete! obag-delete-all obag-delete-all! obag-search!
	  obag-size obag-find obag-count obag-any? obag-every?
	  obag-map obag-for-each obag-fold
	  obag-filter obag-remove obag-partition
	  obag-filter! obag-remove! obag-partition!
	  obag-copy obag->list list->obag list->obag!
	  obag=? obag<? obag>? obag<=? obag>=?
	  obag-union obag-intersection obag-difference obag-xor
	  obag-union! obag-intersection! obag-difference! obag-xor!
	  obag-comparator
	  obag-sum obag-sum! obag-product obag-product!
	  obag-unique-size obag-element-count obag-for-each-unique obag-fold-unique
	  obag-increment! obag-decrement! obag->oset oset->obag oset->obag!
	  obag->alist alist->obag)
  (import (scheme base)
	  (scheme case-lambda)
          (srfi 128)
	  (srfi 125))
  (include "srfi-153-impl.scm"))
