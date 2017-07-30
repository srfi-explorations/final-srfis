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

(define-library (inferior expression)
  (export variable? variable-identifier
	  literal? literal-datum
	  lambda? lambda-formals lambda-body lambda-expression make-let
	  conditional? conditional-alternate?
	  conditional-test conditional-consequent conditional-alternate
	  assignment? assignment-variable assignment-expression
	  begin? begin-sequence
	  application? application-operator application-operands
	  named-let? named-let-tag named-let-bindings named-let-body
	  let? let-bindings let-body
	  let*? let*-bindings let*-body
	  letrec? letrec-bindings letrec-body letrec-variables letrec-inits
	  cond? cond-clauses
	  case? case-key case-clauses
	  and? and-tests
	  or? or-tests
	  do? do-bindings do-clause do-body
	  quasiquote? quasiquote-template
	  definition? definition-variable definition-expression
	  binding-variable binding-init
	  formals->list map-formals
	  tagged-list?)
  (import (scheme base)
	  (scheme cxr)
	  (inferior identifier))
  (include "expression.scm"))
