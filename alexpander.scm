;; alexpander.scm: a macro expander for scheme.
;; $Id$

;; Copyright 2002, 2003 Al Petrofsky <alexpander@petrofsky.org>

;; You may redistribute and/or modify this software under the terms of
;; the GNU General Public License as published by the Free Software
;; Foundation (fsf.org); either version 2, or (at your option) any
;; later version.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see
;; http://www.fsf.org/licenses/gpl.txt, or write to the Free Software
;; Foundation, Inc., 59 Temple Pl Ste 330, Boston MA 02111-1307, USA.

;; Feel free to ask me for different licensing terms. 


;; INTRO:

;; This file implements a macro-expander for r5rs scheme (plus some
;; interesting extensions).  There is no magic here to hook this into
;; your native eval system: this is a simple data-in, data-out program
;; that takes a macro-using program represented as scheme data and
;; produces an equivalent macro-free program represented as scheme
;; data.

;; This is mostly intended as a demonstration.  Although it certainly
;; could be useful for adding macros to a simple scheme system that
;; lacks any macros, it may not be feasible to get it to interact
;; properly with a low-level macro system or a module system.

;; The expander is written in portable r5rs scheme, except for one use
;; of the pretty-print procedure which you can easily comment out.

;; The expander supports all the features of the r5rs macro system,
;; plus several extensions in the way syntaxes can be specified and
;; used, which are best summarized in BNF:

;; Modified r5rs productions:
;;   <syntax spec> --> (<keyword> <syntax>)
;;   <syntax definition> ---> (define-syntax <keyword> <syntax>)
;;                          | (befin <syntax definition>*)
;;   <macro use> ---> (<syntax> <datum>*)
;;   <definition> ---> (define <variable> <expression>)
;;                   | (define (<variable> <def formals>) <body>)
;;                   | (begin <definition>*)
;;                   | <syntax definition>

;; New productions:
;;   <syntax> --> <transformer spec>
;;              | <keyword>
;;              | <macro use>
;;              | <syntax macro block>
;;   <syntax macro block> --> (let-syntax    (<syntax spec>*) <syntax body>)
;;                          | (letrec-syntax (<syntax spec>*) <syntax body>)
;;   <syntax body> ---> <syntax definition>* <syntax>


;; Also, syntax-rules transformers can specify the identifier to be
;; used as the ellipsis, and a list pattern may contain subpatterns
;; after an ellipsis as well as before it:

;;   <transformer spec> ---> (syntax-rules (<identifier>*) <syntax rule>*)
;;              | (syntax-rules <ellipsis> (<identifier>*) <syntax rule>*)
;;   
;;   <syntax rule> ---> (<pattern> <template>)
;;   
;;   <pattern> ---> <pattern identifier>
;;                | (<pattern>*)
;;                | (<pattern>+ . <pattern>)
;;                | (<pattern>* <pattern> <ellipsis> <pattern>*)
;;                | #(<pattern>*)
;;                | #(<pattern>* <pattern> <ellipsis> <pattern>*)
;;                | <pattern datum>
;;   
;;   <pattern identifier> ---> <identifier>
;;   
;;   <ellipsis> ---> <identifier>


;; BASIC USAGE:

;; There are four supported ways to use this:

;;   1. (alexpander-repl)
;;      This starts a read-expand-print-loop.  Type in a program
;;      and see its expansion as you go.
;;
;;   2. (expand-program list-of-the-top-level-forms-of-a-program)
;;      Returns a list of the top-level forms of an equivalent
;;      macro-free program.
;;
;;   3. (expand-top-level-forms! forms mstore)
;;      Returns some macro-expanded forms and side-effects mstore.
;;      First create an initial mutable store with (null-mstore), then
;;      you can pass a program in piecemeal, with the effects of
;;      top-level define-syntaxes saved in mstore between calls to
;;      expand-top-level-forms!.
;;
;;   4. (expand-top-level-forms forms store loc-n k)
;;      The purely-functional interface.
;;      Returns by making a tail call to k:
;;      (k expanded-forms new-store new-loc-n)
;;      Use null-store and null-loc-n for store and loc-n arguments
;;      when calling expand-top-level-forms with the first forms in a
;;      program.
;;
;; For options 3 and 4, you need to prepend null-output to the
;; resulting program.  Null-output contains some definitions like
;; (define _eqv?_7 eqv?), which create alternate names for some of the
;; builtin procedures.  These names are used by the standard case and
;; quasiquote macros so that they can keep working even if you
;; redefine one of the standard procedures.

;; The output programs use a small subset of the r5rs syntax, namely:
;; begin, define, delay, if, lambda, letrec, quote, and set!.
;; Furthermore, begin is only used for expressions, lambdas and
;; letrecs always have a single body expression and no internal
;; definitions, and defines are always of the simple (define
;; <variable> <expression>) form.  Any uses or definitions in the
;; original program of a top-level variable whose name begins with
;; "_", or whose name is one of the eight primitives just mentioned,
;; will be renamed.  This will only cause a problem if the program is
;; trying to use some nonstandard library variable that starts with
;; "_": any r5rs-conformant program will be translated to an
;; equivalent macro-free r5rs program, it just might have some of its
;; top-level variable names changed.


;; INTERNALS

;; [NOTE: this documentation is certainly not complete, and it kind of
;; dissolves after a few pages from verbose paragraphs into cryptic
;; sentence fragments.  Nonetheless, it might be enough to help
;; someone figure out the code.]

;; ENVIRONMENTS AND STORES

;; The two principal data structures are the environment and the
;; store.

;; These work similarly to the runtime environment and store described
;; in r5rs: in both that system and in ours, to determine the meaning
;; of an identifier, we lookup which location the environment
;; associates with the identifier, and then check what value the store
;; associates with that location.

;; In the runtime system, the identifiers mapped by the environment
;; are all variables, and the values in the store are the scheme
;; values the variables currently hold.  Environments may be locally
;; extended by LAMBDA to map some identifiers to new locations that
;; initially hold the values passed to the procedure.  Environments
;; may also be locally extended by internal DEFINE (a.k.a LETREC) to
;; map some identifiers to new locations that are empty and illegal to
;; access or SET! until the evaluation of all the initializers has
;; completed (at which time the results are stored into the
;; locations).  The store is modified when a SET! or top-level DEFINE
;; is evaluated, or when a set of internal DEFINE initializers'
;; evaluation completes, but environments are immutable.  The static
;; top-level environment maps every variable name to some location,
;; although most of these locations are illegal to access until the
;; evaluation of the initializer of the first top-level DEFINE of the
;; variable has completed.  (The exceptions are the locations to which
;; the standard procedure names are bound: these locations may be
;; accessed at any time, but they may not be SET! until after the
;; first top-level DEFINE of the procedure name.)

;; (R5rs actually does not completely specify how the top-level
;; environment works, and allows one to consider the top-level
;; environment to be dynamically extended, but the model I just
;; described fits within the r5rs parameters and plays well with our
;; macro system.  To recap: the difference between SET! and top-level
;; DEFINE is not that top-level DEFINE is able to create a new
;; binding, rather, the difference is that top-level DEFINE is allowed
;; to store into any location and SET! is not always allowed to store
;; into some locations.)

;; In our syntactic system, a value in the store may be either a
;; syntax (a builtin or a macro transformer) or a variable name.  When
;; we encounter a use of an identifier, we go through the environment
;; and the store to fetch its value.  If the value is a variable name,
;; we emit that variable name.  If the value is a syntax, we proceed
;; according to the rules of that syntax.  As in the runtime system,
;; environments are immutable and the static top-level environment is
;; infinite.  Environments may be locally extended by LAMBDA or
;; internal DEFINE to map some identifiers to new locations that hold
;; variable names.  Environments may also be extended by LET-SYNTAX to
;; map some identifiers to new locations that initially hold the
;; syntaxes resulting from the expansion of the initializers.  Lastly,
;; environments may be extended by LETREC-SYNTAX or internal
;; DEFINE-SYNTAX to map some identifiers to new locations that are
;; empty and illegal to access until the expansion of their
;; initializers has completed (at which time the resulting syntaxes
;; are stored into the locations).  The store is modified by top-level
;; DEFINE and DEFINE-SYNTAX, and when a set of internal DEFINE-SYNTAX
;; initializers' expansion completes.  The store is not altered by a
;; SET!, because a SET! does not change the fact that the identifier
;; is a variable: from our perspective a SET! of a variable is simply
;; a use of the variable.  A top-level DEFINE only alters the store if
;; an identifier whose location previously held a syntax is now being
;; defined as a variable.

;; The static top-level environment maps every name to some location.
;; Initially, the locations to which the environment maps the names of
;; the eleven builtins (BEGIN DEFINE DEFINE-SYNTAX IF LAMBDA QUOTE
;; SET! DELAY LET-SYNTAX LETREC-SYNTAX SYNTAX-RULES) hold as their
;; values those builtin syntaxes.  All other names are bound to
;; locations that hold the corresponding top-level variable name.

;; I said the top-level environment contains a binding for "every
;; name" rather than for "every identifier", because the new
;; identifiers created by a syntax-rules macro expansion are given
;; numbers rather than names, and the top-level environment has no
;; bindings for these.  If such an identifier is used in an
;; environment with no binding for it, then the location to which the
;; template literal in the macro was bound is used instead (to be
;; prepared for such a contingency, this location is stored along with
;; the numeric id in the "renamed-sid" (see below) that a macro
;; expansion inserts into the code).

;; REPRESENTATION OF ENVIRONMENTS AND STORES

;; An environment is represented by an alist mapping ids to local
;; (non-top-level) locations.  All environments are derived from the
;; top-level environment, so any symbolic id not in the alist is
;; implicitly mapped to the corresponding top-level location.

;; An id (identifier) is what we bind to a location in an environment.
;; Original ids are the symbols directly occuring in the source code.
;; Renamed ids are created by macro expansions and are represented by
;; integers.

;; id: original-id | renamed-id
;; original-id: symbol
;; renamed-id: integer

;; The static top-level environment maps every symbol to a location.
;; For simplicity, each of those locations is represented by the
;; symbol that is bound to it.  All other locations (those created by
;; lambda, let-syntax, letrec-syntax, and internal definitions) are
;; represented by integers.

;; env: ((id . local-location) ...)
;; store: ((location . val) ...)
;; location: toplevel-location | local-location  ;; a.k.a. symloc and intloc.
;; toplevel-location: symbol
;; local-location: integer
;; val: variable | syntax
;; variable: symbol  ; the symbol that is used in the output, e.g. _foo_42.
;; syntax: builtin | transformer
;; builtin: (symbol)
;; transformer: (env ellipsis literals rule ...)
;; ellipsis: id | #f   ;; #f if implicitly "..."
;; literals: (id ...)  ;; note: ids, not sids.  No need for the extra info.
;; rule: the unaltered sexp from the syntax-rules form.

;; REPRESENTATION OF THE CODE UNDERGOING EXPANSION (SEXPS).

;; Any variable named SEXP in the expander code holds a representation
;; of some code undergoing expansion.  It mostly looks like the
;; ordinary representation of scheme code, but it may contain some
;; identifiers that are encoded as two- or three-element vectors
;; called renamed-sids.  Any actual vector in the code will be
;; represented as a one-element vector whose element is a list of the
;; actual elements, i.e., each vector #(elt ...) is mapped to #((elt
;; ...)), so that we can distinguish these vectors from renamed-sids.

;; In contrast, a variable named OUTPUT is a bit of finished code, in
;; which vectors represent themselves and all renamed identifiers have
;; been mapped to suitable symbols.

;; A sid is the representation of an id within a sexp.
;; sid: original-id | renamed-sid

;; A renamed-sid includes the id's original name, which we will need
;; if the id gets used in a QUOTE expression.  The renamed-sid also
;; includes the location of the local binding (if any) of the template
;; literal that created the id: this is the location to use if the id
;; gets used freely (i.e., in an environment with no binding for it). 
;; renamed-sid: #(original-id renamed-id local-location)
;;            | #(original-id renamed-id)

;; Procedures that take a SEXP argument usually also take an ID-N
;; argument, which is the next higher number after the largest
;; renamed-id that occurs in the SEXP argument.  (This is to enable
;; adding new ids without conflict.)
;;
;; Similarly, a STORE argument is usually accompanied by a LOC-N
;; argument, which is the next higher number after the largest
;; local-location in the STORE argument.

;; SUMMARY OF MAJOR FUNCTIONS:

;; (lookup-sid sid env) => location
;; (lookup-location location store) => val | #f  ;; #f means letrec violation.
;; (lookup2 sid env store) => val ;; lookup-sid + lookup-location + fail if #f.
;; (extend-env env id location) => env
;; (extend-store store intloc val) => store
;; (substitute-in-store store loc val) => store
;; (compile-syntax-rules env ellipsis pattern-lits rules) => transformer
;; (apply-transformer trans sexp id-n env k) => (k sexp id-n)
;; (expand-any sexp id-n env store loc-n ek sk dk bk)
;;    => (ek output)
;;     | (sk syntax sexp store loc-n)
;;     | (dk builtin sexp id-n store loc-n)
;;     | (bk sexp id-n store loc-n)
;; (expand-syntax sexp id-n env store loc-n k) => (k syntax store loc-n)
;; (expand-expr sexp id-n env store loc-n) => output
;; (expand-top-level-forms forms store loc-n k)
;;   => (k outputs store loc-n)
;; (expand-body sexps id-n env store loc-n ek sk)
;;   => (ek output) | (sk syntax sexp store loc-n).
;; (expand-syntax-bindings bindings id-n syntax-env ienv store loc-n k)
;;   => (k store loc-n)


(define (id? x)           (or (symbol? x) (number? x)))
(define (sid? s)          (or (symbol? s) (renamed-sid? s)))
(define (renamed-sid? s)  (and (vector? s) (< 1 (vector-length s))))
(define (svector? s)      (and (vector? s) (= 1 (vector-length s))))
(define (svector->list s) (vector-ref s 0))
(define (list->svector l) (vector l))

(define (make-sid name renamed-id location)
  (if (eq? name location)
      (vector name renamed-id)
      (vector name renamed-id location)))

(define (sid-name sid) (if (symbol? sid) sid (vector-ref sid 0)))
(define (sid-id sid)   (if (symbol? sid) sid (vector-ref sid 1)))
(define (sid-location sid)
  (if (symbol? sid) sid (vector-ref sid (if (= 2 (vector-length sid)) 0 2))))

(define (list1? s) (and (pair? s) (null?  (cdr s))))
(define (list2? s) (and (pair? s) (list1? (cdr s))))

;; Map-vecs does a deep map of x, replacing any vector v with (f v).
;; We assume that f never returns #f.
;; If a subpart contains no vectors, we don't waste space copying it.
;; (Yes, this is grossly premature optimization.)
(define (map-vecs f x)
  ;; mv2 returns #f if there are no vectors in x.
  (define (mv2 x)
    (if (vector? x)
	(f x)
	(and (pair? x)
	     (let ((a (car x)) (b (cdr x)))
	       (let ((a-mapped (mv2 a)))
		 (if a-mapped
		     (cons a-mapped (mv b))
		     (let ((b-mapped (mv2 b)))
		       (and b-mapped (cons a b-mapped)))))))))
  (define (mv x) (or (mv2 x) x))
  (mv x))

(define (wrap-vec v) (list->svector (wrap-vecs (vector->list v))))
(define (wrap-vecs input) (map-vecs wrap-vec input))
(define (unwrap-vec v-sexp)
  (if (= 1 (vector-length v-sexp))
      (list->vector (unwrap-vecs (svector->list v-sexp)))
      (vector-ref v-sexp 0)))
(define (unwrap-vecs sexp) (map-vecs unwrap-vec sexp))

(define (variable? val) (symbol? val))
(define (syntax? val) (pair? val))

(define (builtin? syntax) (null? (cdr syntax)))
(define (transformer? syntax) (not (builtin? syntax)))

(define (builtin-name builtin) (car builtin))

(define (acons key val alist) (cons (cons key val) alist))

(define empty-env '())
(define empty-store '())

;; Lookup-sid looks up a sid in an environment.
;; If there is no binding in the environment, then:
;;   1. For an original-id, we return the like-named location, because
;;      the static top-level environment maps every name to a location.
;;   2. For a renamed id, we return the location to which the template
;;      literal that created it was bound.
(define (lookup-sid sid env)
  (cond ((assv (sid-id sid) env) => cdr)
	;; This works for both cases 1 and 2 above.
	(else (sid-location sid))))

;; Lookup-location looks up a location in the store.
;; If there is no value explictly listed in the store, then:
;;   1. For a top-level (named) location, return a top-level variable name.
;;   2. For a local location, return #f.  This can only happen for a
;;      location allocated by letrec-syntax or internal define-syntax
;;      and used before it is initialized,
;;      e.g. (letrec-syntax ((x x)) 1).
(define (lookup-location location store)
  (cond ((assv location store) => cdr)
	((symbol? location) (symloc->var location))
	(else #f)))

(define (lookup2 sid env store)
  (or (lookup-location (lookup-sid sid env) store)
      (error (string-append "Premature use of keyword bound by letrec-syntax"
			    " (or an internal define-syntax): ")
	     sid)))

(define (extend-env env id location) (acons id location env))
(define (extend-store store loc val) (acons loc val store))

;; Extend-store just adds to the front of the alist, whereas
;; substitute-in-store actually bothers to remove the old entry, and
;; to not add a new entry if it is just the default.
;; Substitute-in-store is only used by top-level define and
;; define-syntax.  Because nothing is ever mutated, we could just use
;; extend-store all the time, but we are endeavoring to keep down the
;; size of the store to make it more easily printed and examined.
(define (substitute-in-store store loc val)
  (let ((store (if (assv loc store)
		   (let loop ((store store))
		     (let ((p (car store)))
		       (if (eqv? loc (car p))
			   (cdr store)
			   (cons p (loop (cdr store))))))
		   store)))
    (if (and (symbol? loc) (variable? val))
	store
	(acons loc val store))))

;; Top-level variables must be renamed if they conflict with the
;; primitives or local variable names we use in the output.
(define (symloc->var sym)
  (define str (symbol->string sym))
  (define (rename) (string->symbol (string-append "_" str "_")))
  (case sym
    ((begin define delay if lambda letrec quote set!) (rename))
    (else (if (and (positive? (string-length str))
		   (char=? #\_ (string-ref str 0)))
	      (rename)
	      sym))))

;; make-local-var:
;; A simple (string->symbol (string-append "_" (number->string intloc)))
;; would work, but we use more verbose local variable names to make
;; the output more decipherable to humans.
(define (make-local-var intloc sid)
  (let ((str (symbol->string (sid-name sid))))
    (string->symbol (string-append "_" str "_" (number->string intloc)))))

(define (loc->var loc sid)
  (if (symbol? loc)
      (symloc->var loc)
      (make-local-var loc sid)))

(define (make-begin outputs)
  (if (list1? outputs) (car outputs) (cons 'begin outputs)))

;; flatten-dotted:
;; (a b . c) => (a b c), a => (a)
(define (flatten-dotted x)
  (if (pair? x)
      (cons (car x) (flatten-dotted (cdr x)))
      (list x)))

(define (dot-flattened x)
  (if (null? (cdr x))
      (car x)
      (cons (car x) (dot-flattened (cdr x)))))

;; Check lambda formals for non-identifiers or duplicates.
(define (check-formals formals)
  (define (bad-var x)
    (error "Non-identifier: " x " in lambda formals: " formals))
  (define (dup-var sid)
    (error "Duplicate variable: " sid " in lambda formals: " formals))
  (or (null? formals)
      (let ((formals (if (list? formals) formals (flatten-dotted formals))))
	(for-each (lambda (formal) (or (sid? formal) (bad-var formal)))
		  formals)
	(let ((ids (map sid-id formals)))
	  (for-each (lambda (formal)
		      (let ((id (sid-id formal)))
			(if (memv id (cdr (memv id ids)))
			    (dup-var formal))))
		    formals)))))

;; returns (k vars env store loc-n)
(define (make-var-bindings formals env store loc-n k)
  (let* ((dotted? (not (list? formals)))
	 (formals (if dotted? (flatten-dotted formals) formals)))
    (let loop
	((formals formals) (rvars '()) (env env) (store store) (loc-n loc-n))
      (if (null? formals)
	  (let* ((vars (reverse rvars))
		 (vars (if dotted? (dot-flattened vars) vars)))
	    (k vars env store loc-n))
	  (let* ((var (make-local-var loc-n (car formals)))
		 (env (extend-env env (sid-id (car formals)) loc-n))
		 (store (extend-store store loc-n var)))
	    (loop (cdr formals) (cons var rvars) env store (+ 1 loc-n)))))))

(define (check-syntax-bindings bindings)
  (define (check-binding b)
    (or (and (list2? b) (sid? (car b)))
	(error "Malformed syntax binding: " b)))
  (or (list? bindings) (error "Non-list syntax bindings list: " bindings))
  (for-each check-binding bindings)
  (do ((bs bindings (cdr bs)))
      ((null? bs))
    (let ((id (sid-id (caar bs))))
      (define (check b)
	(if (eqv? id (sid-id (car b)))
	    (error "Duplicate bindings for a keyword: " (car bs) " and: " b)))
      (for-each check (cdr bs)))))

;; returns (k store loc-n)
(define (expand-syntax-bindings bindings id-n syntax-env ienv store loc-n k)
  (let loop ((bs bindings) (syns '()) (store store) (loc-n loc-n))
    (if (not (null? bs))
	(expand-syntax (cadar bs) id-n syntax-env store loc-n
		       (lambda (syn error-sexp store loc-n)
			 (loop (cdr bs) (cons syn syns) store loc-n)))
	(let loop ((store store) (syns (reverse syns)) (bs bindings))
	  (if (not (null? syns))
	      (let* ((loc (lookup-sid (caar bs) ienv))
		     (store (extend-store store loc (car syns))))
		(loop store (cdr syns) (cdr bs)))
	      (k store loc-n))))))


;; (expand-any sexp id-n env store loc-n ek sk dk bk)
;;
;; Ek, sk, dk, and bk are continuations for expressions, syntaxes,
;; definitions and begins:
;;
;; If sexp is an expression, returns (ek output).
;;
;; If sexp is a syntax, returns (sk syntax error-sexp store loc-n).
;;   The error-sexp is just for use in error messages if the syntax is
;;   subsequently misued.  It is the sid that was bound to the syntax
;;   (unless the syntax is an anonymous transformer, as in
;;   ((syntax-rules () ((_ x) 'x)) foo), in which case the error-sexp
;;   will be the syntax-rules form).
;;
;; If sexp is a definition, returns (dk builtin sexp id-n store
;;   loc-n), where builtin is define or define-syntax.
;;
;; If sexp is a begin, returns (bk sexp id-n store loc-n).
;;
;; The car of the sexp passed to dk or bk is just for error reporting:
;; it is the sid that was bound to begin, define, or define-syntax.
;;
;; Expand-any signals an error if an improper list is encountered.  It
;; also signals an error if ek, sk, dk, or bk is #f and the
;; corresponding thing is encountered.
(define (expand-any sexp id-n env store loc-n ek sk dk bk)
  (define (get-k k sexp name)
    (or k (error (string-append name " used in bad context: ")
		 sexp)))
  (define (get-ek sexp) (get-k ek sexp "Expression"))
  (define (get-sk sexp) (get-k sk sexp "Syntax"))
  (define (get-dk sexp) (get-k dk sexp "Definition"))
  (define (get-bk sexp) (get-k bk sexp "Begin"))
  (let again ((sexp sexp) (id-n id-n) (store store) (loc-n loc-n))
    (define (expand-subexpr sexp) (expand-expr sexp id-n env store loc-n))
    (define (handle-syntax-use syntax head store loc-n)
      (let* ((tail (cdr sexp)) (sexp (cons head tail)))
	(if (transformer? syntax)
	    (apply-transformer syntax sexp id-n env
	      (lambda (sexp id-n) (again sexp id-n store loc-n)))
	    (let ((builtin (builtin-name syntax)) (len (length tail)))
	      (define (handle-let-syntax rec?)
		(or ek sk (error "Macro block used in bad context: " sexp))
		(or (>= len 2) (error "Malformed macro block: " sexp))
		(let ((bindings (car tail)))
		  (check-syntax-bindings bindings)
		  (let loop ((bs bindings) (loc-n loc-n) (ienv env))
		    (if (not (null? bs))
			(loop (cdr bs) (+ loc-n 1)
			      (extend-env ienv (sid-id (caar bs)) loc-n))
			(expand-syntax-bindings
			  bindings id-n (if rec? ienv env) ienv store loc-n
			  (lambda (store loc-n)
			    (expand-body (cdr tail) id-n ienv store loc-n
					 ek sk)))))))
	      (define (handle-expr-builtin)
		(define (expr-assert test)
		  (or test (error "Malformed expression: " sexp)))
		(cons builtin
		      (case builtin
			((lambda)
			 (expr-assert (>= len 2))
			 (check-formals (car tail))
			 (make-var-bindings (car tail) env store loc-n
			   (lambda (vars env store loc-n)
			     (expand-body (cdr tail) id-n env store loc-n
			       (lambda (output) (list vars output))
			       #f))))
			((quote)
			 (expr-assert (= len 1))
			 (list (unwrap-vecs (car tail))))
			((delay)
			 (expr-assert (= len 1))
			 (list (expand-subexpr (car tail))))
			((if)
			 (expr-assert (<= 2 len 3))
			 (map expand-subexpr tail))
			((set!)
			 (expr-assert (and (= len 2) (sid? (car tail))))
			 (let ((var (lookup2 (car tail) env store)))
			   (or (variable? var)
			       (error "Attempt to set a keyword: " sexp))
			   (list var (expand-subexpr (cadr tail))))))))
	      (case builtin
		((let-syntax) (handle-let-syntax #f))
		((letrec-syntax) (handle-let-syntax #t))
		((syntax-rules)
		 (if (< len 1) (error "Empty syntax-rules form: " sexp))
		 (let* ((ellipsis (and (sid? (car tail)) (sid-id (car tail))))
			(rest (if ellipsis (cdr tail) tail)))
		   (if (null? rest)
		       (error "Missing literals in syntax-rules: " sexp))
		   (let* ((literals (car rest))
			  (rules (cdr rest))
			  (syn (compile-syntax-rules env ellipsis literals rules)))
		     ((get-sk sexp) syn sexp store loc-n))))
		((begin) ((get-bk sexp) sexp id-n store loc-n))
		((define define-syntax)
		 (or (and (= 2 len) (sid? (car tail)))
		     (error "Malformed definition: " sexp))
		 ((get-dk sexp) builtin sexp id-n store loc-n))
		(else ((get-ek sexp) (handle-expr-builtin))))))))
    (define (handle-combination output)
      (ek (if (and (pair? output) (eq? 'lambda (car output))
		   (null? (cadr output)) (null? (cdr sexp)))
	      ;; simplifies ((lambda () <expr>)) to <expr>
	      (caddr output)
	      (cons output (map expand-subexpr (cdr sexp))))))
    ;;(pretty-print `(expand-any/again ,sexp))
    (cond ((sid? sexp)
	   (let ((val (lookup2 sexp env store)))
	     (if (variable? val)
		 ((get-ek sexp) val)
		 ((get-sk sexp) val sexp store loc-n))))
	  ((null? sexp) (error "Null used as an expression or syntax: " sexp))
	  ((list? sexp)
	   (expand-expr-or-syntax (car sexp) id-n env store loc-n
	     (and ek handle-combination)
	     handle-syntax-use))
	  ((or (number? sexp) (boolean? sexp) (string? sexp) (char? sexp))
	   ((get-ek sexp) sexp))
	  (else (error (cond ((pair? sexp) "Improper list: ")
			     ((vector? sexp) "Vector: ")
			     (else "Non-S-Expression: "))
		       sexp
		       " used as an expression, syntax, or definition.")))))

(define (expand-expr-or-syntax sexp id-n env store loc-n ek sk)
  (define (bk sexp id-n store loc-n)
    (if (null? (cdr sexp)) (error "Empty begin expression: " sexp))
    (ek (make-begin (expand-exprs (cdr sexp) id-n env store loc-n))))
  (expand-any sexp id-n env store loc-n ek sk #f bk))

(define (expand-expr sexp id-n env store loc-n)
  (expand-expr-or-syntax sexp id-n env store loc-n (lambda (x) x) #f))

(define (expand-exprs sexps id-n env store loc-n)
  (define (expand-one sexp) (expand-expr sexp id-n env store loc-n))
  (map expand-one sexps))

(define (expand-syntax sexp id-n env store loc-n k)
  (expand-any sexp id-n env store loc-n #f k #f #f))

;; For an expression body, returns (ek output).
;; For a syntax body, returns (sk syntax sexp store loc-n).
(define (expand-body sexps id-n env store loc-n ek sk)
  ;; Expand-def expands a definition or begin sequence, adds entries
  ;; to the vds and sds lists of variable and syntax definitons, and
  ;; returns (k vds sds id-n env store loc-n).  If an expression is
  ;; encountered, returns (ek output) instead.
  (define (expand-def sexp vds sds id-n env store loc-n k ek)
    (define (dk builtin sexp id-n store loc-n)
      (let* ((sid (cadr sexp))
	     (id (sid-id sid))
	     (env (extend-env env id loc-n)))
	(define (check def)
	  (if (eqv? id (sid-id (cadr def)))
	      (error "Duplicate internal definitions: " def " and: " sexp)))
	(for-each check sds)
	(if vds (for-each check vds))
	(case builtin
	  ((define-syntax) (k vds (cons sexp sds) id-n env store (+ loc-n 1)))
	  ((define)
	   (or vds (error "Variable definition in a syntax body: " sexp))
	   (let ((store (extend-store store loc-n (make-local-var loc-n sid))))
	     (k (cons sexp vds) sds id-n env store (+ loc-n 1)))))))
    (define (bk sexp id-n store loc-n)
      (let loop ((sexps (cdr sexp)) (vds vds) (sds sds) (id-n id-n)
		 (env env) (store store) (loc-n loc-n) (ek ek))
	(if (null? sexps)
	    (k vds sds id-n env store loc-n)
	    (expand-def (car sexps) vds sds id-n env store loc-n
	      (lambda (id-n store loc-n vds sds)
		(loop (cdr sexps) id-n store loc-n vds sds #f))
	      (and ek (lambda (out)
			(let ((rest (expand-exprs (cdr sexps) id-n env
						  store loc-n)))
			  (ek (make-begin (cons out rest))))))))))
    (expand-any sexp id-n env store loc-n ek #f dk bk))
  (let loop ((first (car sexps)) (rest (cdr sexps))
	     (vds (and ek '())) (sds '()) (id-n id-n)
	     (env env) (store store) (loc-n loc-n))
    (define (finish-body boundary-exp-output)
      (expand-syntax-bindings (map cdr sds) id-n env env store loc-n
	(lambda (store loc-n)
	  (define (iexpand sexp) (expand-expr sexp id-n env store loc-n))
	  (define (expand-vd vd)
	    (list (lookup2 (cadr vd) env store) (iexpand (caddr vd))))
	  (define (make-letrec bindings expr)
	    (if (null? bindings) expr (list 'letrec bindings expr)))
	  (if (and (null? rest) (not (pair? vds)))
	      (expand-expr-or-syntax first id-n env store loc-n ek sk)
	      (ek (make-letrec
		   (map expand-vd (reverse vds))
		   (if (null? rest)
		       (iexpand first)
		       (make-begin (cons boundary-exp-output
					 (map iexpand rest))))))))))
    (if (null? rest)
	(finish-body #f)
	(expand-def first vds sds id-n env store loc-n
	  (lambda (vds sds id-n env store loc-n)
	    (loop (car rest) (cdr rest) vds sds id-n env store loc-n))
	  (and ek finish-body)))))


;; (returns (k outputs store loc-n))
(define (expand-top-level-forms forms store loc-n k)
  (define (finalize store loc-n acc)
    (k (reverse acc) store loc-n))
  ;; expand adds stuff to acc and returns (k store loc-n acc)
  (let expand ((sexps (wrap-vecs forms)) (id-n 0)
	       (store store) (loc-n loc-n) (acc '()) (k finalize))
    (if (null? sexps)
	(k store loc-n acc)
	(let ((rest (cdr sexps)))
	  (define (ek output)
	    (expand rest id-n store loc-n (cons output acc) k))
	  (define (dk builtin sexp id-n* store loc-n)
	    (let* ((tail (cdr sexp))
		   (sid (car tail))
		   (loc (sid-location sid))
		   (init (cadr tail)))
	      (if (eq? builtin 'define)
		  (let* ((expr (expand-expr init id-n* empty-env store loc-n))
			 (var (loc->var loc sid))
			 (acc (cons (list 'define var expr) acc))
			 (store (substitute-in-store store loc var)))
		    (expand rest id-n store loc-n acc k))
		  (expand-syntax init id-n* empty-env store loc-n
		    (lambda (syn error-sexp store loc-n)
		      (let ((store (substitute-in-store store loc syn)))
			(expand rest id-n store loc-n acc k)))))))
	  (define (bk sexp id-n* store loc-n)
	    (expand (cdr sexp) id-n* store loc-n acc
		    (lambda (store loc-n acc)
		      (expand rest id-n store loc-n acc k))))
	  (expand-any (car sexps) id-n empty-env store loc-n ek #f dk bk)))))

;; Compile-syntax-rules:
;; This doesn't do much compiling, it mostly just does verification.
;; Detects all possible errors:
;;   pattern literals list is not a list of identifiers
;;   ellipsis in literals list
;;   rule is not a two-element list
;;   missing pattern keyword (pattern is not a pair whose car is an identifier)
;;   duplicate pattern variable
;;   ellipsis not preceded by a pattern or template.
;;   list or vector pattern with multiple ellipses.
;;   improper list pattern with an ellipsis.
;;   variable instance in template not at sufficient ellipsis depth.
;;   template ellipsis closes no variables.
(define (compile-syntax-rules env ellipsis pattern-lits rules)
  (define (ellipsis? x)
    (and (sid? x)
	 (if ellipsis
	     (eqv? ellipsis (sid-id x))
	     (eq? '... (lookup-sid x env)))))
  (define (check-lit lit)
    (or (sid? lit)
	(error "Non-id: " lit " in literals list: " pattern-lits))
    (if (ellipsis? lit)
	(error "Ellipsis " lit " in literals list: " pattern-lits)))
  (or (list? pattern-lits)
      (error "Pattern literals list is not a list: " pattern-lits))
  (for-each check-lit pattern-lits)
  (let ((pattern-lits (map sid-id pattern-lits)))

    (define (ellipsis-pair? x)
      (and (pair? x) (ellipsis? (car x))))

    (define (check-ellipses pat/tmpl in-template?)
      (define (bad-ellipsis x reason)
	(error (string-append reason ": ")
	       x
	       (if in-template? " in template: " " in pattern: ")
	       pat/tmpl))

      (define (multi-ellipsis-error x)
	(bad-ellipsis x "List or vector pattern with multiple ellipses"))

      (define (ellipsis/tail-error x)
	(bad-ellipsis x "Improper list pattern with an ellipsis"))

      (define (ellipsis-follows x thing)
	(bad-ellipsis x (string-append "Ellipsis following " thing)))
      
      (let ((x (if in-template? pat/tmpl (cdr pat/tmpl))))
	(if in-template?
	    (if (ellipsis? x)
		(ellipsis-follows x "nothing"))
	    (cond ((ellipsis? x)
		   (ellipsis-follows pat/tmpl "a '.'"))
		  ((ellipsis-pair? x)
		   (ellipsis-follows pat/tmpl "the pattern keyword"))))
	(let check ((x x))
	  (cond ((pair? x)
		 (if (ellipsis? (car x)) (ellipsis-follows x "a '('"))
		 (check (car x))
		 (if (ellipsis? (cdr x)) (ellipsis-follows x "a '.'"))
		 (if (ellipsis-pair? (cdr x))
		     (cond ((ellipsis? (cddr x))
			    (ellipsis-follows (cdr x) "a '.'"))
			   ((ellipsis-pair? (cddr x))
			    (ellipsis-follows (cdr x) "an ellipsis"))
			   (in-template? (check (cddr x)))
			   (else (or (list? x) (ellipsis/tail-error x))
				 (for-each (lambda (y)
					     (if (ellipsis? y)
						 (multi-ellipsis-error x))
					     (check y))
				  (cddr x))))
			
		     (check (cdr x))))
		((svector? x)
		 (let ((elts (svector->list x)))
		   (if (ellipsis-pair? elts)
		       (ellipsis-follows x "a '#('")
		       (check elts))))))))

    ;; Returns an alist: ((pat-var . depth) ...)
    (define (make-pat-env pat)
      (let collect ((x (cdr pat)) (depth 0) (l '()))
	(cond ((sid? x)
	       (let ((id (sid-id x)))
		 (cond ((memv id pattern-lits) l)
		       ((assv id l)
			(error "Duplicate pattern var: " x
			       " in pattern: " pat))
		       (else (acons id depth l)))))
	      ((vector? x) (collect (svector->list x) depth l))
	      ((pair? x)
	       (cond ((ellipsis-pair? (cdr x))
		      (collect (car x) (+ 1 depth) (collect (cddr x) depth l)))
		     (else (collect (car x) depth (collect (cdr x) depth l)))))
	      (else l))))

    ;; Checks var depths and returns a list of template literals 
    (define (list-template-lits tmpl pat-env)
      (define (depth-error x)
	(error "Pattern var used at bad depth: " x " in template: " tmpl))
      (define (close-error x)
	(error "Template ellipsis closes no variables: " x
	       " in template: " tmpl))
      (define (finish lits closed?) lits)
      (let collect ((x tmpl) (lits '()) (depth 0) (k finish))
	(cond ((sid? x)
	       (cond ((assv (sid-id x) pat-env)
		      => (lambda (p)
			   (let* ((pat-depth (cdr p))
				  (same-depth? (= depth pat-depth)))
			     (if (and (positive? pat-depth) (not same-depth?))
				 (depth-error x))
			     (k lits same-depth?))))
		     (else (k (cons (sid-id x) lits) #f))))
	      ((vector? x) (collect (svector->list x) lits depth k))
	      ((pair? x)
	       (let ((ellip? (ellipsis-pair? (cdr x))))
		 (collect (car x) lits (if ellip? (+ 1 depth) depth)
		   (lambda (lits car-closed?)
		     (or car-closed? (not ellip?) (close-error x))
		     (collect ((if ellip? cddr cdr) x) lits depth
		       (lambda (lits cdr-closed?)
			 (k lits (or car-closed? cdr-closed?))))))))
	      (else (k lits #f)))))
			 
    ;; Checks rule and returns the list of the template literals.
    (define (check-rule rule)
      (or (list2? rule) (error "Malformed syntax rule: " rule))
      (let ((pat (car rule)) (tmpl (cadr rule)))
	(or (and (pair? pat) (sid? (car pat)))
	    (error "Malformed pattern: " pat))
	(check-ellipses pat #f)
	(check-ellipses tmpl #t)
	(list-template-lits tmpl (make-pat-env pat))))

    ;; Reduce-env: this optional hack cuts down on the clutter when
    ;; examining the store.  Returns an environment with only the
    ;; bindings we need: those of pattern or template literals, and
    ;; those of identifiers named "..." that prevent a "..." from
    ;; being treated as an ellipsis, e.g. in
    ;; (let ((... 1)) ((syntax-rules () ((_) ...)))) => 1.
    (define (reduce-env lits)
      (define (list-dots-ids x ids)
	(cond ((sid? x) (if (eq? '... (sid-location x))
			    (cons (sid-id x) ids)
			    ids))
	      ((vector? x) (list-dots-ids (svector->list x) ids))
	      ((pair? x) (list-dots-ids (car x) (list-dots-ids (cdr x) ids)))
	      (else ids)))
      (let loop ((ids (if ellipsis (list-dots-ids rules lits) lits))
		 (reduced-env '()))
	(if (null? ids)
	    reduced-env
	    (loop (cdr ids)
		  (let ((id (car ids)))
		    (cond ((and (not (assv id reduced-env)) (assv id env))
			   => (lambda (binding) (cons binding reduced-env)))
			  (else reduced-env)))))))

    (let* ((lits (apply append pattern-lits (map check-rule rules)))
	   (env (reduce-env lits)))
	  (cons env (cons ellipsis (cons pattern-lits rules))))))


;; returns (k sexp id-n)
(define (apply-transformer transformer sexp id-n env k)
  (let ((mac-env (car transformer))
	(ellipsis (cadr transformer))
	(pat-literals (caddr transformer))
	(rules (cdddr transformer)))

    (define (pat-literal? id)     (memv id pat-literals))
    (define (not-pat-literal? id) (not (pat-literal? id)))
    (define (ellipsis-pair? x)    (and (pair? x) (ellipsis? (car x))))
    (define (ellipsis? x)
      (and (sid? x)
	   (if ellipsis
	       (eqv? ellipsis (sid-id x))
	       (eq? '... (lookup-sid x mac-env)))))

    ;; List-ids returns a list of the non-ellipsis ids in a
    ;; pattern or template for which (pred? id) is true.  If
    ;; include-scalars is false, we only include ids that are
    ;; within the scope of at least one ellipsis.
    (define (list-ids x include-scalars pred?)
      (let collect ((x x) (inc include-scalars) (l '()))
	(cond ((sid? x) (let ((id (sid-id x)))
			  (if (and inc (pred? id)) (cons id l) l)))
	      ((vector? x) (collect (svector->list x) inc l))
	      ((pair? x)
	       (if (ellipsis-pair? (cdr x))
		   (collect (car x) #t (collect (cddr x) inc l))
		   (collect (car x) inc (collect (cdr x) inc l))))
	      (else l))))
    
    (define (matches? pat)
      (call-with-current-continuation
       (lambda (return)
	 (define (fail-unless x) (if (not x) (return #f)))
	 (let match ((pat pat) (sexp (cdr sexp)))
	   (cond ((sid? pat)
		  (fail-unless (or (not (pat-literal? (sid-id pat)))
				   (and (sid? sexp)
					(eqv? (lookup-sid pat mac-env)
					      (lookup-sid sexp env))))))
		 ((svector? pat)
		  (fail-unless (svector? sexp))
		  (match (svector->list pat) (svector->list sexp)))
		 ((not (pair? pat)) (fail-unless (equal? pat sexp)))
		 ((ellipsis-pair? (cdr pat))
		  (let skip ((p (cddr pat)) (s sexp))
		    (if (pair? p)
			(begin (fail-unless (pair? s))
			       (skip (cdr p) (cdr s)))
			(let match-cars ((sexp sexp) (s s))
			  (if (pair? s)
			      (begin (match (car pat) (car sexp))
				     (match-cars (cdr sexp) (cdr s)))
			      (match (cddr pat) sexp))))))
		 (else (fail-unless (pair? sexp))
		       (match (car pat) (car sexp))
		       (match (cdr pat) (cdr sexp)))))
	 #t)))

    ;; Returns an alist binding pattern variables to parts of the input.
    ;; An ellipsis variable is bound to a list (or a list of lists, etc.).
    (define (make-bindings pat)
      (let collect ((pat pat) (sexp (cdr sexp)) (bindings '()))
	(cond ((and (sid? pat) (not (pat-literal? (sid-id pat))))
	       (acons (sid-id pat) sexp bindings))
	      ((svector? pat)
	       (collect (svector->list pat) (svector->list sexp) bindings))
	      ((not (pair? pat)) bindings)
	      ((ellipsis-pair? (cdr pat))
	       (let skip ((p (cddr pat)) (s sexp))
		 (if (pair? p)
		     (skip (cdr p) (cdr s))
		     (let get-elts ((sexp sexp) (s s) (reversed-elts '()))
		       (if (pair? s)
			   (get-elts (cdr sexp) (cdr s)
				     (cons (car sexp) reversed-elts))
			   (let ((elts (reverse reversed-elts))
				 (vars
				  (list-ids (car pat) #t not-pat-literal?)))
			     (define (collect1 elt)
			       (map cdr (collect (car pat) elt '())))
			     (append (apply map list vars (map collect1 elts))
				     (collect (cddr pat) sexp bindings))))))))
	      (else (collect (car pat) (car sexp)
			     (collect (cdr pat) (cdr sexp) bindings))))))

    ;; Remove duplicates from a list, using eqv?.
    (define (remove-dups l)
      (let loop ((l l) (result '()))
	(if (null? l)
	    result
	    (loop (cdr l)
		  (let ((elt (car l)))
		    (if (memv elt result) result (cons elt result)))))))

    (define (expand-template pat tmpl top-bindings)
      (define tmpl-literals
	(remove-dups (list-ids tmpl #t
			       (lambda (id) (not (assv id top-bindings))))))
      (define ellipsis-vars (list-ids pat #f not-pat-literal?))
      (define (list-ellipsis-vars subtmpl)
	(list-ids subtmpl #t (lambda (id) (memv id ellipsis-vars))))
      (define (expand tmpl bindings)
	(let expand-part ((tmpl tmpl))
	  (cond
	   ((sid? tmpl)
	    (let ((id (sid-id tmpl)))
	      (cond ((assv id bindings) => cdr)
		    ((assv id top-bindings) => cdr)
		    (else
		     (let ((index (+ -1 (length (memv id tmpl-literals))))
			   (location (lookup-sid tmpl mac-env)))
		       (make-sid (sid-name tmpl) (+ id-n index) location))))))
	   ((vector? tmpl)
	    (list->svector (expand-part (svector->list tmpl))))
	   ((pair? tmpl)
	    (if (ellipsis-pair? (cdr tmpl))
		(let ((vars-to-iterate (list-ellipsis-vars (car tmpl))))
		  (define (lookup var) (cdr (assv var bindings)))
		  (define (expand-using-vals . vals)
		    (expand (car tmpl) (map cons vars-to-iterate vals)))
		  (let ((val-lists (map lookup vars-to-iterate)))
		    (if (or (null? (cdr val-lists))
			    (apply = (map length val-lists)))
			(append (apply map expand-using-vals val-lists)
				(expand-part (cddr tmpl)))
			(error "Unequal sequence lengths for pattern vars: "
			       vars-to-iterate " in macro call: " sexp))))
		(cons (expand-part (car tmpl)) (expand-part (cdr tmpl)))))
	   (else tmpl))))
      (k (expand tmpl top-bindings) (+ id-n (length tmpl-literals))))

    (let loop ((rules rules))
      (if (null? rules)
	  (error "No matching rule for macro use: " sexp)
	  (let* ((rule (car rules)) (pat (cdar rule)) (tmpl (cadr rule)))
	    (if (matches? pat)
		(expand-template pat tmpl (make-bindings pat))
		(loop (cdr rules))))))))

(define builtins-store
  (let loop ((bs '(begin define define-syntax if lambda quote set! delay
			 let-syntax letrec-syntax syntax-rules))
	     (store empty-store))
    (if (null? bs)
	store
	(loop (cdr bs) (extend-store store (car bs) (list (car bs)))))))

;; null-prog is the preamble that defines all the standard macros that
;; are in the null-store.  (The "null-" name prefix was chosen to
;; correspond to the name of r5rs's null-environment procedure, even
;; though the null-store is far from empty.)
(define null-prog
  ;; Define-protected-macros defines a set of macros with a private
  ;; set of bindings for some keywords and variables.  If any of the
  ;; keywords or variables are later redefined at top-level, the
  ;; macros will continue to work.  The first argument to
  ;; define-protected-macros is let-syntax or letrec-syntax; if it is
  ;; letrec-syntax, then the macros will also have a private set of
  ;; bindings for one another, and recursive calls made by the macros
  ;; to themselves or to one another will not be affected by later
  ;; top-level redefinitions.
  ;;
  ;; (You'll notice we don't actually bind anything to the name
  ;; define-protected-macros, we just use it as a pattern literal in
  ;; an anonymous transformer.)
  '(((let-syntax
	 ((multi-define
	   (syntax-rules ()
	     ((_ definer (id ...) (init ...))
	      (begin (definer id init) ...))))
	  ;; The private binding for a saved variable is created by a
	  ;; let-syntax, using a dummy syntax as the initializer.  We
	  ;; later assign a value to it using a top-level define (and
	  ;; thus change the status of the binding from keyword to
	  ;; variable).
	  (dummy (syntax-rules ())))
       (syntax-rules (define-protected-macros define-syntax)
	 ((_ (define-protected-macros let/letrec-syntax
	       (saved-kw ...) (saved-var ...)
	       (define-syntax kw syntax)
	       ...)
	     ...)
	  (begin
	    ((let-syntax ((saved-kw saved-kw) ... (saved-var dummy) ...)
	       (let/letrec-syntax ((kw syntax) ...)
		 (syntax-rules ()
		   ((_ top-level-kws top-level-vars)
		    (begin
		      (multi-define define (saved-var ...) top-level-vars)
		      (multi-define define-syntax top-level-kws (kw ...)))))))
	     (kw ...) (saved-var ...))
	    ...))))
     (define-protected-macros letrec-syntax
       (if lambda quote begin define) (eqv?)
       (define-syntax let
	 (syntax-rules ()
	   ((_ ((var init) ...) . body)
	    ((lambda (var ...) . body)
	     init ...))
	   ((_ name ((var init) ...) . body)
	    ((letrec ((name (lambda (var ...) . body)))
	       name)
	     init ...))))
       (define-syntax let*
	 (syntax-rules ()
	   ((_ () . body) (let () . body))
	   ((let* ((var init) . bindings) . body)
	    (let ((var init)) (let* bindings . body)))))
       (define-syntax letrec
	 (syntax-rules ()
	   ((_ ((var init) ...) . body)
	    (let () (define var init) ... (let () . body))))) 
       (define-syntax do
	 (let-syntax ((do-step (syntax-rules () ((_ x) x) ((_ x y) y))))
	   (syntax-rules ()
	     ((_ ((var init step ...) ...)
		 (test expr ...)
		 command ...)
	      (let loop ((var init) ...)
		(if test
		    (begin (if #f #f) expr ...)
		    (begin command ...
			   (loop (do-step var step ...) ...))))))))
       (define-syntax case
	 (letrec-syntax
	     ((compare
	       (syntax-rules ()
		 ((_ key ()) #f)
		 ((_ key (datum . data))
		  (if (eqv? key 'datum) #t (compare key data)))))
	      (case
	       (syntax-rules (else)
		 ((case key) (if #f #f))
		 ((case key (else result1 . results))
		  (begin result1 . results))
		 ((case key ((datum ...) result1 . results) . clauses)
		  (if (compare key (datum ...))
		      (begin result1 . results)
		      (case key . clauses))))))
	   (syntax-rules ()
	     ((_ expr clause1 clause ...)
	      (let ((key expr))
		(case key clause1 clause ...))))))
       (define-syntax cond
	 (syntax-rules (else =>)
	   ((_) (if #f #f))
	   ((_ (else . exps)) (let () (begin . exps)))
	   ((_ (x) . rest) (or x (cond . rest)))
	   ((_ (x => proc) . rest)
	    (let ((tmp x)) (cond (tmp (proc tmp)) . rest)))
	   ((_ (x . exps) . rest)
	    (if x (begin . exps) (cond . rest)))))
       (define-syntax and
	 (syntax-rules ()
	   ((_) #t)
	   ((_ test) (let () test))
	   ((_ test . tests) (if test (and . tests) #f))))
       (define-syntax or
	 (syntax-rules ()
	   ((_) #f)
	   ((_ test) (let () test))
	   ((_ test . tests) (let ((x test)) (if x x (or . tests)))))))
     ;; Prototype-style define is implemented in a
     ;; define-protected-macros with let-syntax scope so that it can
     ;; access the builtin define.  Quasiquote is here too so that it
     ;; can recognize nested uses of itself using a syntax-rules
     ;; literal.  That is, the quasiquote binding that is visible in
     ;; the environment of the quasiquote transformer must be the same
     ;; binding that is visible where quasiquote is used.
     (define-protected-macros let-syntax
       (lambda define quote let) (cons append list vector list->vector)
       (define-syntax define
	 (syntax-rules ()
	   ((_ (var . args) . body)
	    (define var (lambda args . body)))
	   ((_ var init) (define var init))))
       (define-syntax quasiquote
	 (let-syntax
	     ((tail-preserving-syntax-rules
	       (syntax-rules ()
		 ((_ literals
		     ((subpattern ...) (subtemplate ...))
		     ...)
		  (syntax-rules literals
		    ((subpattern ... . tail) (subtemplate ... . tail))
		    ...)))))
	   (define-syntax qq
	     (tail-preserving-syntax-rules
	         (unquote unquote-splicing quasiquote)
	       ((_ ,expr ()) (do-next expr))
	       ((_ `x depth) (qq (x) (depth) make-pair 'quasiquote))
	       ((_ ,x (depth)) (qq (x) depth make-pair 'unquote))
	       ((_ ,@x (depth)) (qq (x) depth make-pair 'unquote-splicing))
	       ((_ (,@x . y) ()) (qq y () make-splice x))
	       ((_ (x . y) depth) (qq x depth qq-cdr y depth))
	       ((_ #(x ...) depth) (qq (x ...) depth make-vector))
	       ((_ x depth) (do-next 'x))))
	   (define-syntax do-next
	     (syntax-rules ()
	       ((_ expr) expr)
	       ((_ expr next-macro . tail) (next-macro expr . tail))))
	   (define-syntax qq-cdr
	     (tail-preserving-syntax-rules ()
	       ((_ car cdr depth) (qq cdr depth make-pair car))))
	   (define-syntax make-pair
	     (tail-preserving-syntax-rules (quote list)
	       ((_ 'y 'x) (do-next '(x . y)))
	       ((_ '() x) (do-next (list x)))
	       ((_ (list . elts) x) (do-next (list x . elts)))
	       ((_ y x) (do-next (cons x y)))))
	   (define-syntax make-vector
	     (tail-preserving-syntax-rules (quote list)
	       ((_ '(x ...)) (do-next '#(x ...)))
	       ((_ (list . elts)) (do-next (vector . elts)))
	       ((_ expr) (scan-conses () expr expr))))
	   ;; Check if expr is a nesting of conses whose last cdr is a
	   ;; quoted list, i.e., convert (cons x1 (cons x2 (cons x3
	   ;; '(x4 x5 x6)))) to (vector x1 x2 x3 'x4 'x5 'x6).
	   ;; Otherwise, use (list->vector expr).
	   (define-syntax scan-conses
	     (tail-preserving-syntax-rules (quote cons)
	       ((_ (elt ...) (cons x y)) (scan-conses (elt ... x) y))
	       ((_ (elt ...) '(x ...) orig) (do-next (vector elt ... 'x ...)))
	       ((_ elts other orig) (do-next (list->vector orig)))))
	   (define-syntax make-splice
	     (tail-preserving-syntax-rules ()
	       ((_ '() x) (do-next x))
	       ((_ y x) (do-next (append x y)))))
	   (syntax-rules () ((_ template) (let () (qq template ()))))))))))

(define null-stuff (expand-top-level-forms null-prog builtins-store 0 list))
(define null-output (car null-stuff))
(define null-store  (cadr null-stuff))
(define null-loc-n  (caddr null-stuff))

(define (expand-program forms)
  (expand-top-level-forms forms null-store null-loc-n
    (lambda (outputs store loc-n) (append null-output outputs))))

;; an mstore is a mutable store.
(define (null-mstore) (cons null-store null-loc-n))

(define (expand-top-level-forms! forms mstore)
  (expand-top-level-forms forms (car mstore) (cdr mstore)
    (lambda (outputs store loc-n)
      (set-car! mstore store)
      (set-cdr! mstore loc-n)
      outputs)))

(define repl-mstore (null-mstore))

;; alexpander-repl: a read-expand-print loop.
;; If called with an argument, resumes a previous session.
;; Top-level vectors are interpreted as directives to the repl:
;;   #(show loc ...) shows values stored in the locations.
;;   #(dump) dumps the whole store.
;;   #(restart) restarts.

(define (alexpander-repl . resume?)
  (define (pp x)
    ;;(write x) (newline)    
    (pretty-print x))
  (define (restart)
    (set! repl-mstore (null-mstore))
    (for-each pp null-output))
  (define (repl)
    (display "expander> ")
    (let ((form (read)))
      (if (not (eof-object? form))
	  (begin
	    (if (vector? form)
		(let ((l (vector->list form)))
		  (case (car l)
		    ((dump) (pp (car repl-mstore)))
		    ((show) (map (lambda (loc)
				   (pp (assv loc (car repl-mstore))))
				 (cdr l)))
		    ((restart) (restart))))
		(for-each pp (expand-top-level-forms! (list form)
						      repl-mstore)))
	    (repl)))))
  (if (null? resume?) (restart))
  (repl))


'(begin
   (define (file->list file)
     (define (f) (let ((x (read))) (if (eof-object? x) '() (cons x (f)))))
     (with-input-from-file file f))

   (define (check-expander filename)
     (let* ((src (file->list filename))
	    (out1 (begin (for-each eval src) (expand-program src))))
       (begin (for-each eval out1) (equal? out1 (expand-program src))))))

'(define-syntax define
   (let-syntax ((old-define define))
     (letrec-syntax
	 ((new-define
	   (syntax-rules ()
	     ((_ (var-or-prototype . args) . body)
	      (new-define var-or-prototype (lambda args . body)))
	     ((_ var expr) (old-define var expr)))))
       new-define)))

'(define-syntax define
   (let-syntax ((old-define define))
     (define-syntax new-define
       (syntax-rules ()
	 ((_ (var-or-prototype . args) . body)
	  (new-define var-or-prototype (lambda args . body)))
	 ((_ var expr) (old-define var expr))))
     new-define))

'(let ((multiplier 2))
   (define ((curried-* x) y) (* x y))
   (map (curried-* multiplier) '(3 4 5)))

;; Example of how a safe letrec could be written if we support macro
;; calls of the forms (SET! KEYWORD DATUM) and just plain KEYWORD.
'
(define-syntax letrec
  (syntax-rules ()
    ((_ ((var init) ...) . body)
     (let ((ready? #f) (var #f) ...)
       (let-syntax
	   ((var (syntax-rules (set!)
		   ((_ set! val) (if ready? (set! var val) (error)))
		   ((_ . args) ((if ready? var (error)) . args))
		   (_ (if ready? var (error)))))
	    ...)
	 (let ((var (let ((tmp init)) (lambda () (set! var tmp))))
	       ...)
	   (set! ready? #t) (var) ...)
	 (let () . body))))))

;; Notes:

;; TODO: revamp error handling, add an error continuation to
;; expand-top-level-forms.

;; Are these legal in r5rs?
;;   (let () (define if 1) (+ if) if) => 1
;;   (let () (define if 1) (set! if 2) if) => 2
;; The 2002 version of the expander didn't allow them.
;;
;; First solution to above problem didn't fix this:
;;   (let () (define if 1) ((let-syntax () if +)) if) => 1
;;
;; 2003-10-05:
;; New semantics:
;;
;;   (let ((a 1) (b 1))
;;     (define a 2)
;;     ((syntax-rules () ((_ m) (define (m) (list a b)))) m)
;;     (define b 2)
;;     (m))
;;   => (1 2)
;;
;;   (let ((a 1) (b 1))
;;     (define a 2)
;;     (define-syntax m (syntax-rules () ((_) (list a b))))
;;     (define b 2)
;;     (m))
;;   => (2 2)


;; Idea for syntax-rules extension to provide automatic gensym sequences:

;; (syntax-rules with-id-sequence ()
;;   ((letrec ((var init) ... (with-id-sequence temp)) . body)
;;    (let ((var 'undefined) ...)
;;      (let ((temp init) ...)
;;        (set! var temp) ... (let () . body)))))

;; (syntax-rules with-ids ()
;;   ((letrec-values (((var ... (with-ids tmp)) init) ... (with-ids thunk))
;;      . body)
;;    (let ()
;;      (begin (define var 'undefined) ...)
;;      ...
;;      (define thunk (call-with-values (lambda () init)
;; 		        (lambda (tmp ...) (lambda () #f (set! var tmp) ...))))
;;      ...
;;      (thunk) ... (let () . body))))

;; (syntax-rules with-ids ()
;;   ((set!-values (var ... (with-ids tmp)) expr)
;;    (call-with-values (lambda () expr)
;;      (lambda (tmp ...) (if #f #f) (set! var tmp) ...))))
