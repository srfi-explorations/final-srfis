(include "html-lib.scm")

(define (format-lambda-list lst)
  (let ((name (car lst))
	(arguments (cdr lst)))
    (<p> (<b> "Procedure: ")
	 (<code> (<a> name: name name)
		 (map (lambda (arg)
			(list " " (cond ((symbol? arg)
					 (<var> arg))
					((keyword? arg)
					 arg)
					(else
					 arg))))
		      arguments)))))

(define (format-global-variable name)
  (<p> (<b> "Variable: ")
       (<code> (<a> name: name name))))
		       
(with-output-to-file
    "srfi-122.html"
  (lambda()
    (html-display
     (list
     (<unprotected> "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
     (<html>
      (<head>
       (<title> "Intervals and Generalized Arrays")
       (<link> href: "http://srfi.schemers.org/srfi.css"
	       rel: "stylesheet"))
      (<body>
       (<h1> "Title")
       (<p> "Intervals and Generalized arrays")
       
       (<h1> "Author")
       (<p> "Bradley J. Lucier")
       
       (<h1> "Status")
       (<p> "This SRFI is currently in " (<em> "draft") "status.  Here is "
(<a> href: "http://srfi.schemers.org/srfi-process.html" "an explanation")
"of each status that a SRFI can hold.  To
provide input on this SRFI, please send email to "
	    (<code> (<a> href: "mailto:srfi minus 122 at srfi dot schemers dot org"
			 "srfi-122@" (<span> class: "nospam") "srfi.schemers.org"))
	    ".
To subscribe to the list, follow "
	    (<a> href: "http://srfi.schemers.org/srfi-list-subscribe.html" "these instructions")
".  You can access previous messages via the mailing list "
	    (<a> href: "http://srfi-email.schemers.org/srfi-122" "archive")".")
       (<ul> (<li> "Received: 2015/7/23")
             (<li> "Draft #1 published: 2015/7/27")))
       
       (<h1> "Abstract")
       (<p> "This SRFI specifies an array mechanism for Scheme. Arrays as defined here are quite general, and benefit from a data type
 called "(<i> 'intervals)", which encapsulate the cross product of non-empty intervals of exact integers. These intervals  specify the domain
information for arrays. An array is then characterized as a mapping from multi-indices of exact integers (i"(<sub>"0")",...,i"(<sub>"d-1")")
contained in an interval to Scheme values. Additionally, specialized variants of arrays are specified to provide portable programs with efficient representations for common use cases.")
       
       (<h1> "Rationale")
       (<p> "An array, as commonly understood, provides a mapping from multi-indices  (i"(<sub>"0")",...,i"(<sub>"d-1")") of exact integers 
in a non-empty, rectangular, d-dimensional interval
[l"(<sub> '0)", u"(<sub> '0)") x [l"(<sub> '1)", u"(<sub> '1)") x ... x [l"(<sub> 'd-1)", u"(<sub> 'd-1)") to Scheme objects.
Thus, two things are necessary to specify an array: an interval and a mapping.")
       (<p> "Since these two things are often sufficient for certain algorithms, we introduce in this SRFI a minimal set of interfaces for dealing with such arrays.")
       (<p> "Specifically, an Array specifies a nonempty, multi-dimensional interval, called its "(<i> "domain")", and a mapping from this domain to (single) Scheme objects.  This mapping is called the "(<i> 'getter)" of the array.")
       (<p> "If this mapping can be changed, the array is said to be "(<i> 'mutable)" and the mutation is effected
by the array's "(<i> 'setter)".  We call an object of this type a Mutable-array.")
       (<p> "In general, we leave the implementation of arrays completely open.  They may be defined simply by closures, or
they may have hash tables or databases behind an implementation.  If the getter and setter functions of a Mutable-array are
defined by accessing and setting elements  of a one-dimensional (heterogeneous or homogeneous) vector that are determined by a one-to-one function from
the domain of the Array into the integers between 0 (inclusive) and the length of the backing-store vector (exclusive),
the array is said to be "(<i> 'fixed)". A Fixed-array is an example of a Mutable-array.")

(<p> "Thus,  we  need the concept of an "(<i> 'indexer)", which is a one-to-one mapping whose domain is an Interval and whose range is
contained in another Interval.  Conceptually, an indexer is itself an Array that returns multiple values.  An
important subset of indexers are affine mappings (linear mappings plus constants) from one domain to another.  We do not
encapsulate indexers, with their domain Interval, range Interval, and multi-valued mapping, into a distinct type.
While we considered the formalized use of non-affine indexers in Fixed-arrays, we restrict indexers in Fixed-arrays to be affine.
Thus our Fixed-arrays are very similar to "
     (<a> href: "#bawden" "Bawden-style arrays")". (If you want to specify a non-affine indexer into a body, it can be done by constructing a Mutable-array.)")
(<p> "The backing store of a Fixed-array, which may be a heterogeneous or homogeneous vector,
is created, accessed, etc., via the components of  objects we call Array-manipulators.  We define their properties below.")
(<p> "The API of this SRFI uses keywords from SRFI-88 and the calling convention from SRFI-89 for optional and keyword arguments (although the implementation defines functions with keyword and optional arguments using DSSSL's notation, not the notation from SRFI-89).")

(<h1> "Examples of application areas")
(<ul>
 (<li> "Many multi-dimensional transforms in signal processing are "(<i> 'separable)", in that that the multi-dimensional transform can be computed by applying one-dimensional transforms in each of the coordinate directions.  Examples of such transforms include the Fast Fourier Transform and the Fast Wavelet Transform.  Each one-dimensional subdomain of the complete domain is called a "(<i> 'pencil)", and the same one-dimensional transform is applied to all pencils in a given direction. This motivates us to define the various procedures *-distinguish-one-axis.")
 (<li> "Many applications have multi-dimensional data that behave differently in different coordinate directions.  For example, one might have a time series of maps, which can be stored in a single three-dimensional array.  Or one might have one-dimensional spectral data assigned to each pixel on a map.  The data cube as a whole is considered three-dimensional "(<i> 'hyperspectral)" data, but for processing the spectra separately one would apply a function to the spectrum at each pixel.  This corresponds to "(<i> 'currying)" arguments in programming languages, so we include such procedures here.")
 (<li> "All arrays are "(<i> 'lazy)" by default, in that we do not compute an array element until it is needed.  So the following code"
       (<pre>
"
(define (vector-field-sequence-ell-infty-ell-1-ell-2-norm p)
  (Array-max (Array-map (lambda (p^k)
                          (Array-average (Array-map Point-length-R^4
                                                    (Array-extract p^k
								   (Array-domain zero-image)))))
                        p)))"
)
       "(with suitable definitions for "(<code> 'Array-max)", "(<code> 'Array-average)", and "(<code> 'Point-length-R^4)") computes the maximum over a number of two-dimensional arrays of the average length of four-vectors in those arrays restricted to the domain of "(<code> "zero-image")", without storing all of the elements of any of the intermediate arrays together.")
       )
 

(<h1> "Issues and Notes")
(<ul>
 (<li> (<b> "Capitalization convention. ") "This SRFI specifies an object and class system without mentioning objects and classes.  Conceptually,
Fixed-arrays are a subclass of Mutable-arrays, which in turn are a subclass of Arrays.  I follow Queinnec in his practice of naming classes beginning with
capital letters in Meroon.")
 (<li> (<b> "Fixed-arrays. ")  "Bawden-style arrays have their elements stored in a heterogeneous or homogeneous
vector.  A single vector can contain the elements of
several conceptually separate arrays.  I needed a name for this type of array implementation, and I think of the
backing-store vector that contains the array elements as \"fixed\", sitting somewhere out of sight.  Thus the name
\"Fixed-array\".  If you can come up with a better name, good.")
 (<li> (<b> "\"object\" vs. \"make-object\". ") "I think of "(<code> 'make-vector)", "(<code> 'make-string)", etc., as low-level memory
allocators with little computation going on behind the scenes.  Instantiating the objects of this SRFI may require non-trivial
computation behind the scenes, so I didn't want to name the instantiation routines make-whatever.")
 (<li> (<b> "Specifying intervals. ")  "I couldn't, for the life of me, figure out the most \"natural\" way to specify Intervals.  Perhaps " #\newline
       (<blockquote> (<code> "(Interval lower-bounds: l0 l1 ... upper-bounds: u0 u1 ...)"))
       "comes closes, but it would take a lot of computation to check properly , and this isn't even a valid DSSSL argument list. " #\newline
       "In the end I decided to just require the lower and upper bound arguments to be vectors.")
 (<li> (<b> (<code>" Interval-lower-bounds->list" )". ") "I think of this routine not as a getter ("(<code> 'Interval-lower-bounds)" would be a getter) but as a converter, like " #\newline
       (<code> 'vector->list)".  I don't want to expose the inner storage mechanism for lower and upper bounds of Intervals, so I don't provide a getter, only these converters.")
 (<li> (<b> "Indexers. ")"Both the arguments new-domain->old-domain to "(<code> 'Fixed-array-share!)" and indexer to "(<code> 'Fixed-array)" are conceptual arrays, " #\newline
       "the first multiple-valued and the second single-valued.")
 (<li> (<b> "Source of function names. ")"The function "(<code> 'Array-curry)" gets its name from the " #\newline
       (<a> href: "http://en.wikipedia.org/wiki/Currying" "curry operator")
       " in programming---we are currying the getter of the Array and keeping careful track of the domains. " #\newline
       "Interval-curry is simply given a parallel name (although it can be thought of as currying the " #\newline
       "characteristic function of the interval,  encapsulated here as "(<code> 'Interval-contains-multi-index?)").  Similarly, " #\newline
       (<code> 'Array-distinguish-one-axis)" makes sense for Arrays, but the parallel function for Intervals is not very natural.")
 (<li> (<b> "Choice of functions on Intervals. ")"The choice of functions for both Arrays and Intervals was motivated almost solely by what I needed for Arrays.  There are " #\newline
       "natural operations on Intervals, like "
       (<blockquote> (<code>"(Interval-cross-product interval1 interval2 ...)"))
       "(the inverse of "(<code> 'Interval-curry)") and"
       (<blockquote> (<code>"(Interval-intersect interval1 interval2 ...)"))
       " which don't seem terribly natural for Arrays.  If you could use these functions in your programs, tell me (windowing systems, etc.?).")
 (<li> (<b> "No empty Intervals or Arrays. ")"Mathematically, a functions f(x) is a relation R, which is a set  of ordered pairs {(x,y)} for x in some domain set X and " #\newline
       "y in some range set Y, for which (x1,y1) and (x2, y2) are in R and x1=x2 implies y1=y2.  (Here we assume that f is onto Y, in other words that Y consists precisely of" #\newline
       "the second coordinates of the ordered pairs in R, and \"=\" means \"identical\", i.e., y1 and y2 are the "(<i> 'same)" objects (in the sense of "#\newline
       (<code> 'eq?)").). The only relation R where X is the empty set is the empty relation consisting " #\newline
       " of no ordered pairs at all.  Since there are no ordered pairs, there are no values in the range set Y, i.e., Y is the empty set.  So the getter of an Array with "#\newline
       "an empty interval as a domain would be able to return no values at all.  So I don't understand constructions that allow Arrays with empty Intervals as domains to return a single value."#\newline
       "Since I don't understand it, I don't allow it.")
       
 )
(<h1> "Specification")
(let ((END ",\n"))
  (<p> "Names defined in this SRFI:")
  (<dl>
   (<dt> "Intervals")
   (<dd> (<a> href: "#Interval" "Interval")END
	 (<a> href: "#Interval?" "Interval?")END
	 (<a> href: "#Interval-dimension" "Interval-dimension")END
	 (<a> href: "#Interval-lower-bound" "Interval-lower-bound")END
	 (<a> href: "#Interval-upper-bound" "Interval-upper-bound")END
	 (<a> href: "#Interval-lower-bounds->list" "Interval-lower-bounds->list")END
	 (<a> href: "#Interval-upper-bounds->list" "Interval-upper-bounds->list")END
	 (<a> href: "#Interval-lower-bounds->vector" "Interval-lower-bounds->vector")END
	 (<a> href: "#Interval-upper-bounds->vector" "Interval-upper-bounds->vector")END
	 (<a> href: "#Interval=" "Interval=")END
	 (<a> href: "#Interval-volume" "Interval-volume")END
	 (<a> href: "#Interval-subset?" "Interval-subset?")END
	 (<a> href: "#Interval-contains-multi-index?" "Interval-contains-multi-index?")END
	 (<a> href: "#Interval-curry" "Interval-curry")END
	 (<a> href: "#Interval-distinguish-one-axis" "Interval-distinguish-one-axis")END
	 (<a> href: "#Interval-for-each" "Interval-for-each")END
	 (<a> href: "#Interval-for-each-serial" "Interval-for-each-serial")END
	 (<a> href: "#Interval-reduce" "Interval-reduce")END
	 (<a> href: "#Interval-reduce" "Interval-reduce-serial")".")
   (<dt> "Dilations")
   (<dd> (<a> href: "#Dilation" "Dilation")END
	 (<a> href: "#Dilation?" "Dilation?")END
	 (<a> href: "#Dilation-dimension" "Dilation-dimension")END
	 (<a> href: "#Dilation-lower-bound" "Dilation-lower-bound")END
	 (<a> href: "#Dilation-upper-bound" "Dilation-upper-bound")END
	 (<a> href: "#Dilation-lower-bounds->list" "Dilation-lower-bounds->list")END
	 (<a> href: "#Dilation-upper-bounds->list" "Dilation-upper-bounds->list")END
	 (<a> href: "#Dilation-lower-bounds->vector" "Dilation-lower-bounds->vector")END
	 (<a> href: "#Dilation-upper-bounds->vector" "Dilation-upper-bounds->vector")END
	 (<a> href: "#Dilation=" "Dilation=")".")
   (<dt> "Arrays")
   (<dd> (<a> href: "#Array" "Array")END
	 (<a> href: "#Array?" "Array?")END
	 (<a> href: "#Array-domain" "Array-domain")END
	 (<a> href: "#Array-getter" "Array-getter")END
	 (<a> href: "#Array-setter" "Array-setter")END
	 (<a> href: "#Array-body" "Array-body")END
	 (<a> href: "#Array-indexer" "Array-indexer")END
	 (<a> href: "#Array-manipulators" "Array-manipulators")END
	 (<a> href: "#Array-map" "Array-map")END
	 (<a> href: "#Array-curry" "Array-curry")END
	 (<a> href: "#Array-distinguish-one-axis" "Array-distinguish-one-axis")END
	 (<a> href: "#Array-for-each" "Array-for-each")END
	 (<a> href: "#Array-reduce" "Array-reduce")".")
   (<dt> "Mutable Arrays")
   (<dd> (<a> href: "#Mutable-array" "Mutable-array")END
	 (<a> href: "#Mutable-array?" "Mutable-array?")END
	 (<a> href: "#Mutable-array-curry" "Mutable-array-curry")END
	 (<a> href: "#Mutable-array-distinguish-one-axis" "Mutable-array-distinguish-one-axis")".")
   (<dt> "Array Manipulators")
   (<dd> (<a> href: "#make-Array-manipulators" "make-Array-manipulators") END
	 (<a> href: "#Array-manipulators-getter" "Array-manipulators-getter") END
	 (<a> href: "#Array-manipulators-setter" "Array-manipulators-setter") END
	 (<a> href: "#Array-manipulators-maker" "Array-manipulators-maker") END
	 (<a> href: "#Array-manipulators-length" "Array-manipulators-length") END
	 (<a> href: "#Array-manipulators-default" "Array-manipulators-default") END
	 (<a> href: "#generic-array-manipulators" "generic-array-manipulators") END
	 (<a> href: "#s8-array-manipulators" "s8-array-manipulators") END
	 (<a> href: "#s16-array-manipulators" "s16-array-manipulators") END
	 (<a> href: "#s32-array-manipulators" "s32-array-manipulators") END
	 (<a> href: "#s64-array-manipulators" "s64-array-manipulators") END
	 (<a> href: "#u1-array-manipulators" "u1-array-manipulators") END
	 (<a> href: "#u8-array-manipulators" "u8-array-manipulators") END
	 (<a> href: "#u16-array-manipulators" "u16-array-manipulators") END
	 (<a> href: "#u32-array-manipulators" "u32-array-manipulators") END
	 (<a> href: "#u64-array-manipulators" "u64-array-manipulators") END
	 (<a> href: "#f32-array-manipulators" "f32-array-manipulators") END
	 (<a> href: "#f64-array-manipulators" "f64-array-manipulators") 
	 ".")
   (<dt> "Indexers")
   (<dd> (<a> href: "#indexer=" "indexer=")
	 ".")
   (<dt> "Fixed Arrays")
   (<dd> (<a> href: "#Fixed-array-default-safe?" "Fixed-array-default-safe?") END
	 (<a> href: "#Fixed-array" "Fixed-array")END
	 (<a> href: "#Fixed-array?" "Fixed-array?")END
	 (<a> href: "#Fixed-array-curry" "Fixed-array-curry")END
	 (<a> href: "#Fixed-array-distinguish-one-axis" "Fixed-array-distinguish-one-axis")END
	 (<a> href: "#Array->Fixed-array" "Array->Fixed-array")END
	 (<a> href: "#Array->Fixed-array-serial" "Array->Fixed-array-serial")END
	 (<a> href: "#Fixed-array-share!" "Fixed-array-share!")
	 "."
	 )))
(<h2> "Intervals")
(<p> "Intervals are sets of all multi-indices
(i"(<sub>"0")",...,i"(<sub>"d-1")")
such that
l"(<sub>"0")"<=i"(<sub>"0")"<u"(<sub>"0")", ..., l"(<sub>"d-1")"<=i"(<sub>"d-1")"<u"(<sub>"d-1")",
where the "(<i>"lower bounds")"
(l"(<sub>"0")",...,l"(<sub>"d-1")")
and the "(<i>"upper bounds")"
(u"(<sub>"0")",...,u"(<sub>"d-1")")
are specified as multi-indices of exact integers.  The positive integer d is the "(<i>"dimension")"
of the interval.  It is required that
l"(<sub>"0")"<u"(<sub>"0")", ..., l"(<sub>"d-1")"<u"(<sub>"d-1")".")

(<h3> "Procedures")
(format-lambda-list '(Interval lower-bounds upper-bounds))
(<p> "Create a new interval; "(<code> (<var>"lower-bounds"))" and "(<code> (<var>"upper-bounds"))"
are nonempty vectors (of the same length) of exact integers that satisfy")
(<blockquote>
 (<code>" (< (vector-ref "(<var>"lower-bounds")" i) (vector-ref "(<var>"upper-bounds")" i))"))
(<p> " for
0<="(<code>"i")"<"(<code>"(vector-length "(<var>"lower-bounds")")")".  It is an error if 
"(<code>(<var>"lower-bounds"))" and "(<code>(<var>"upper-bounds"))" do not satisfy these conditions.")   

(format-lambda-list '(Interval? obj))
(<p> "Returns "(<code> "#t")" if "(<code> (<var>"obj"))" is an interval, and "(<code>"#f")" otherwise.")

(format-lambda-list '(Interval-dimension interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "then "(<code> 'Interval-dimension)" returns "(<code>"(vector-length "(<var>"lower-bounds")")")".  It is an error to call "(<code> 'Interval-dimension)"
if "(<code>(<var>"interval"))" is not an interval.")

(format-lambda-list '(Interval-lower-bound interval i))
(format-lambda-list '(Interval-upper-bound interval i))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "and "(<code>(<var>"i"))" is an exact integer that satisfies")
(<blockquote>
 "0 <= "(<code>(<var>"i"))" < "(<code>"(vector-length "(<var>"lower-bounds")")")",")
(<p> " then "(<code> 'Interval-lower-bound)" returns
"(<code>"(vector-ref "(<var>"lower-bounds")" "(<var>"i")")")" and "(<code> 'Interval-upper-bound)" returns
"(<code>"(vector-ref "(<var>"upper-bounds")" "(<var>"i")")")".  It is an error to call "(<code> 'Interval-lower-bound)" or "(<code> 'Interval-upper-bound)"
if "(<code>(<var>"interval"))" and "(<code>(<var>"i"))" do not satisfy these conditions.")


(format-lambda-list '(Interval-lower-bounds->list interval))
(format-lambda-list '(Interval-upper-bounds->list interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> " then "(<code> 'Interval-lower-bounds->list)" returns "(<code> "(vector->list "(<var>"lower-bounds")")")
     " and  "(<code> 'Interval-upper-bounds->list)" returns "(<code> "(vector->list "(<var>"upper-bounds")")")". It is an error to call
"(<code> 'Interval-lower-bounds->list)" or "(<code> 'Interval-upper-bounds->list)" if "(<code>(<var>"interval"))" does not satisfy these conditions.")

(format-lambda-list '(Interval-lower-bounds->vector interval))
(format-lambda-list '(Interval-upper-bounds->vector interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> " then "(<code> 'Interval-lower-bounds->vector)" returns a copy of "(<code> (<var>"lower-bounds"))
     "  and "(<code> 'Interval-upper-bounds->vector)" returns a copy of "(<code> (<var>"upper-bounds"))". It is an error to call
"(<code> 'Interval-lower-bounds->vector)" or "(<code> 'Interval-upper-bounds->vector)" if "(<code>(<var>"interval"))" does not satisfy these conditions.")


(format-lambda-list '(Interval-volume interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "then "(<code> 'Interval-volume)" returns ")
(<blockquote>
 (<code> "(apply * (map - (Interval-upper-bounds->list "(<var> 'interval)") (Interval-lower-bounds->list "(<var> 'interval)"))"))
(<p> "It is an error to call "(<code> 'Interval-volume)" if "(<code>(<var> 'interval))" does not satisfy this condition.")

(format-lambda-list '(Interval= interval1 interval2))
(<p> "If "(<code>(<var>"interval1"))" and "(<code>(<var>"interval2"))" are intervals built with ")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds1")" "(<var>"upper-bounds1")")"))
(<p> "and")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds2")" "(<var>"upper-bounds2")")"))
(<p> "respectively, then "(<code> 'Interval=)" returns")
(<blockquote>
 (<code> "(and (equal? "(<var> 'lower-bounds1)" "(<var> 'lower-bounds2)") (equal? "(<var> 'upper-bounds1)" "(<var> 'upper-bounds2)"))"))
(<p> "It is an error to call "(<code> 'Interval=)" if "(<code>(<var> 'interval1))" or "(<code>(<var> 'interval2))" do not satisfy this condition.")

(format-lambda-list '(Interval-subset? interval1 interval2))
(<p> "If "(<code>(<var>"interval1"))" and "(<code>(<var>"interval2"))" are intervals of the same dimension built with ")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds1")" "(<var>"upper-bounds1")")"))
(<p> "and")
(<blockquote>
 (<code>"(Interval "(<var>"lower-bounds2")" "(<var>"upper-bounds2")")"))
(<p> "respectively, then "(<code> 'Interval-subset?)" returns")
(<pre>"
(and (equal? (map >= (vector->list lower-bounds1) (vector->list lower-bounds2))
	     (map (lambda (x) #t) (vector->list lower-bounds1)))
     (equal? (map <= (vector->list upper-bounds1) (vector->list upper-bounds2))
	     (map (lambda (x) #t) (vector->list lower-bounds1))))")
     (<p> "It is an error to call "(<code> 'Interval-subset?)" if "(<code>(<var> 'interval1))" or "(<code>(<var> 'interval2))" do not satisfy this condition.")

(format-lambda-list '(Interval-contains-multi-index? interval index-0 ...))
(<p> "If "(<code>(<var> 'interval))" is an Interval with dimension d and "(<code>(<var> 'index-0))", ..., form a multi-index of length d,
then "(<code> 'Interval-contains-multi-index?)" returns "(<code> #t)" if and only if")
(<blockquote>
 (<code> "(Interval-lower-bound "(<var> 'interval)" j) <= "(<var> 'index-j)" < (Interval-upper-bound "(<var> 'interval)" j)"))
(<p>"for 0<= j < d.")
(<p> "It is an error to call "(<code> 'Interval-contains-multi-index?)" if "(<code>(<var> 'interval))" and "(<code>(<var> 'index-0))",..., do not satisfy this condition.")

(format-lambda-list '(Interval-curry interval left-dimension))
(<p> "Conceptually, "(<code> 'Interval-curry)" takes a d-dimensional interval [l"(<sub> '0)", u"(<sub> '0)") x [l"(<sub> '1)", u"(<sub> '1)") x ... x [l"(<sub> 'd-1)", u"(<sub> 'd-1)")" #\newline
"and splits it into two parts")
(<blockquote> "[l"(<sub> '0)", u"(<sub> '0)") x [l"(<sub> '1)", u"(<sub> '1)") x ... x [l"(<sub>"left-dimension - 1")", u"(<sub>"left-dimension - 1")")")
(<p> "and")
(<blockquote>  "[l"(<sub> 'left-dimension)", u"(<sub> 'left-dimension)") x  ... x [l"(<sub> 'd-1)", u"(<sub> 'd-1)")")
(<p> "This function, the inverse of Cartesian products or cross products of intervals, is used to keep track of the domains of curried arrays.")
(<p> "More precisely, if "(<code>(<var> 'interval))" is an Interval and "(<code>(<var> 'left-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'left-dimension)" < (Interval-dimension "(<var> 'interval)")"))
(<p> "then "(<code> 'Interval-curry)" returns two intervals:")
(<pre>"
(values (Interval (vector (Interval-lower-bound interval 0)
			  ...
			  (Interval-lower-bound interval (- left-dimension 1)))
		  (vector (Interval-upper-bound interval 0)
			  ...
			  (Interval-upper-bound interval (- left-dimension 1))))
	(Interval (vector (Interval-lower-bound interval left-dimension)
			  ...
			  (Interval-lower-bound interval (- (Interval-dimension interval) 1)))
		  (vector (Interval-upper-bound interval left-dimension)
			  ...
			  (Interval-upper-bound interval (- (Interval-dimension interval) 1)))))")
(<p> "It is an error to call "(<code> 'Interval-curry)" if its arguments do not satisfy these conditions.")


(format-lambda-list '(Interval-distinguish-one-axis interval index))
(<p> "If "(<code>(<var> 'interval))" is an interval and
"(<code>"(Interval-dimension "(<var> 'interval)")")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (Interval-dimension "(<var> 'interval)")"))
(<p> "then "(<code> 'Interval-distinguish-one-axis)" returns")
(<pre>"
(values (Interval (vector (Interval-lower-bound interval 0)
			  ...         ; leaving out (Interval-lower-bound interval index)
			  (Interval-lower-bound interval (- left-dimension 1)))
		  (vector (Interval-upper-bound interval 0)
			  ...         ; leaving out (Interval-upper-bound interval index)
			  (Interval-upper-bound interval (- left-dimension 1))))
	(Interval (vector (Interval-lower-bound interval index))
		  (vector (Interval-upper-bound interval index))))")
(<p> "It is an error to call "(<code> 'Interval-distinguish-one-axis)" if its arguments do not satisfy these conditions.")


(format-lambda-list '(Interval-for-each f interval))
(format-lambda-list '(Interval-for-each-serial f interval))
(<p> "These routines assume that "(<code>(<var> 'interval))" is an interval and "(<code>(<var> 'f))" is a routine whose domain includes elements of "(<code>(<var> 'interval))".  It is an error to call
"(<code> 'Interval-for-each)" if "(<code>(<var> 'interval))" and "(<code>(<var> 'f))" do not satisfy these conditions.")
(<p> (<code> 'Interval-for-each)" calls "(<code>(<var> 'f))" on each element of "(<code>(<var> 'interval))" in some unspecified order.")
(<p> (<code> 'Interval-for-each-serial)" calls "(<code>(<var> 'f))" on each element of "(<code>(<var> 'interval))" in lexicographical order.")

(format-lambda-list '(Interval-reduce f operator identity interval))
(format-lambda-list '(Interval-reduce-serial f operator identity interval))
(<p> "If "(<code>(<var> 'interval))" is an interval and "(<code>(<var> 'f))" is a routine whose domain includes elements of "(<code>(<var> 'interval))", then
"(<code> 'Interval-reduce-serial)" returns")
(<blockquote>
 (<code> "(... ("
	 (<var> 'operator)
	 " ("
	 (<var> 'operator)
	 " ("
	 (<var>"operator identity ")
	 "("
	 (<var>"f multi-index")
	 (<sub> '1)
	 ")) ("
	 (<var>"f multi-index")
	 (<sub> '2)
	 ")) ("
	 (<var>"f multi-index")
	 (<sub> '3)
	 ")) ...)"))
(<p> "where "(<code> (<var> 'multi-index)(<sub> '1))", "(<code> (<var> 'multi-index)(<sub> '2))", ... are the multi-indices in "(<code> (<var> 'interval))" in lexicographical order.")
(<p> (<code> 'Interval-reduce)" returns the same value when "(<code>(<var> 'operator))" is associative and it doesn't matter in which order "(<code>(<var> 'f))" is evaluated, and
it may assume these properties.")
(<p> "It is an error to call "(<code> 'Interval-reduce)" or "(<code> 'Interval-reduce-serial)" if "(<code>(<var> 'interval))" and "(<code>(<var> 'f))" do not satisfy these conditions.")

(<h2> "Dilations")
(<p> "Dilations are pairs
of "(<i>"lower bounds")"
(l"(<sub>"0")",...,l"(<sub>"d-1")")
and "(<i>"upper bounds")"
(u"(<sub>"0")",...,u"(<sub>"d-1")"), which
are specified as multi-indices of exact integers.  The positive integer d is the "(<i>"dimension")"
of the dilation.  Generally, dilations are used to modify intervals.")

(<h3> "Procedures")
(format-lambda-list '(Dilation lower-bounds upper-bounds))
(<p> "Create a new dilation; "(<code> (<var>"lower-bounds"))" and "(<code> (<var>"upper-bounds"))"
are nonempty vectors (of the same length) of exact integers.  It is an error if 
"(<code>(<var>"lower-bounds"))" and "(<code>(<var>"upper-bounds"))" do not satisfy these conditions.")   

(format-lambda-list '(Dilation? obj))
(<p> "Returns "(<code> "#t")" if "(<code> (<var>"obj"))" is a dilation, and "(<code>"#f")" otherwise.")

(format-lambda-list '(Dilation-dimension dilation))
(<p> "If "(<code>(<var>"dilation"))" is a dilation built with ")
(<blockquote>
 (<code>"(Dilation "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "then "(<code> 'Dilation-dimension)" returns "(<code>"(vector-length "(<var>"lower-bounds")")")".  It is an error to call "(<code> 'Dilation-dimension)"
if "(<code>(<var>"dilation"))" is not a dilation.")

(format-lambda-list '(Dilation-lower-bound dilation i))
(format-lambda-list '(Dilation-upper-bound dilation i))
(<p> "If "(<code>(<var>"dilation"))" is a dilation built with ")
(<blockquote>
 (<code>"(Dilation "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "and "(<code>(<var>"i"))" is an exact integer that satisfies")
(<blockquote>
 "0 <= "(<code>(<var>"i"))" < "(<code>"(vector-length "(<var>"lower-bounds")")")",")
(<p> " then "(<code> 'Dilation-lower-bound)" returns
"(<code>"(vector-ref "(<var>"lower-bounds")" "(<var>"i")")")
"and  "(<code> 'Dilation-upper-bound)" returns
"(<code>"(vector-ref "(<var>"upper-bounds")" "(<var>"i")")")".  It is an error to call "(<code> 'Dilation-lower-bound)"
or "(<code> 'Dilation-upper-bound)" if "(<code>(<var>"dilation"))" and "(<code>(<var>"i"))" do not satisfy these conditions.")


(format-lambda-list '(Dilation-lower-bounds->list dilation))
(format-lambda-list '(Dilation-upper-bounds->list dilation))
(<p> "If "(<code>(<var>"dilation"))" is a dilation built with ")
(<blockquote>
 (<code>"(Dilation "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> " then "(<code> 'Dilation-lower-bounds->list)" returns "(<code> "(vector->list "(<var>"lower-bounds")")")
     " and   "(<code> 'Dilation-upper-bounds->list)" returns "(<code> "(vector->list "(<var>"upper-bounds")")")". It is an error to call
"(<code> 'Dilation-lower-bounds->list)" or "(<code> 'Dilation-upper-bounds->list)" if "(<code>(<var>"dilation"))" does not satisfy these conditions.")

(format-lambda-list '(Dilation-lower-bounds->vector dilation))
(format-lambda-list '(Dilation-upper-bounds->vector dilation))
(<p> "If "(<code>(<var>"dilation"))" is a dilation built with ")
(<blockquote>
 (<code>"(Dilation "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> " then "(<code> 'Dilation-lower-bounds->vector)" returns a copy of "(<code> (<var>"lower-bounds"))
     " and  "(<code> 'Dilation-upper-bounds->vector)" returns a copy of "(<code> (<var>"upper-bounds"))". It is an error to call
"(<code> 'Dilation-lower-bounds->vector)" or "(<code> 'Dilation-upper-bounds->vector)" if "(<code>(<var>"dilation"))" does not satisfy these conditions.")

(format-lambda-list '(Dilation= dilation1 dilation2))
(<p> "If "(<code>(<var>"dilation1"))" and "(<code>(<var>"dilation2"))" are dilations built with ")
(<blockquote>
 (<code>"(Dilation "(<var>"lower-bounds1")" "(<var>"upper-bounds1")")"))
(<p> "and")
(<blockquote>
 (<code>"(Dilation "(<var>"lower-bounds2")" "(<var>"upper-bounds2")")"))
(<p> "respectively, then "(<code> 'Dilation=)" returns")
(<blockquote>
 (<code> "(and (equal? "(<var> 'lower-bounds1)" "(<var> 'lower-bounds2)") (equal? "(<var> 'upper-bounds1)" "(<var> 'upper-bounds2)"))"))
(<p> "It is an error to call "(<code> 'Dilation=)" if "(<code>(<var> 'dilation1))" or "(<code>(<var> 'dilation2))" do not satisfy this condition.")

(format-lambda-list '(Interval-dilate interval dilation))
(<p> "If "(<code>(<var> 'interval))" is an interval with
lower bounds l"(<sub>"0")", ..., l"(<sub>"d-1")" and
upper bounds u"(<sub>"0")", ..., u"(<sub>"d-1")", and
 "(<code>(<var> 'dilation))" is a dilation with
lower bounds L"(<sub>"0")", ..., L"(<sub>"d-1")" and
upper bounds U"(<sub>"0")", ..., U"(<sub>"d-1")", then "
(<code>"(Interval-dilate "(<var> 'interval)" "(<var> 'dilation)")")" returns a new interval with
lower bounds l"(<sub>"0")"+L"(<sub>"0")", ..., l"(<sub>"d-1")"+L"(<sub>"d-1")" and
upper bounds u"(<sub>"0")"+U"(<sub>"0")", ..., u"(<sub>"d-1")"+U"(<sub>"d-1")", as long as this is a
non-empty interval.  It is an error if the arguments do not satisfy these conditions.")
(<p> "Examples:")
(<blockquote>
 (<pre>"
(Interval= (Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(1 1) '#(1 1)))
	   (Interval '#(1 1) '#(101 101))) => #t
(Interval= (Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(-1 -1) '#(1 1)))
	   (Interval '#(-1 -1) '#(101 101))) => #t
(Interval= (Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(0 0) '#(-50 -50)))
	   (Interval '#(0 0) '#(50 50))) => #t
(Interval-dilate (Interval '#(0 0) '#(100 100)) (Dilation '#(0 0) '#(-500 -50))) => error
"))


(<h2> "Arrays")

(<h3> "Procedures")

(format-lambda-list '(Array domain getter))
(<p> "If "(<code>(<var> 'domain))" is an Interval and "(<code>(<var> 'getter))" is a function from
"(<code>(<var> 'domain))" to Scheme objects, then "(<code> 'Array)" returns an array with domain "(<code>(<var> 'domain))"
and getter "(<code>(<var> 'getter))". It is an error to call "(<code> 'Array)" if "(<code>(<var> 'domain))" and "(<code>(<var> 'getter))"
do not satisfy these conditions.")
(<p> "Example: ")
(<pre>"
(define a (Array (Interval '#(1 1) '#(11 11))
		 (lambda (i j)
		   (if (= i j)
		       1
		       0))))")
(<p> "defines an Array for which "(<code> "(Array-getter a)")" returns 1 when i=j and 0 otherwise.")

(format-lambda-list '(Array? obj))
(<p> "Returns "(<code> "#t")" if and only if "(<code>(<var> 'obj))" is an Array.")

(format-lambda-list '(Array-domain array))
(format-lambda-list '(Array-getter array))
(<p> "If "(<code>(<var> 'array))" is an Array built by")
(<blockquote>
 (<code> "(Array "(<var> 'domain)" "(<var> 'getter)")"))
(<p> "then "(<code> 'Array-domain)" returns "(<code>(<var> 'domain))
" and "(<code> 'Array-getter)" returns  "(<code>(<var> 'getter))".
It is an error to call "(<code> 'Array-domain)" or "(<code> 'Array-getter)" if "(<code>(<var> 'array))" is not an Array.")
(<p> "Example: ")
     (<pre>"
(define a (Array (Interval '#(1 1) '#(11 11))
		 (lambda (i j)
		   (if (= i j)
		       1
		       0))))
((Array-getter a) 3 3) => 1
((Array-getter a) 2 3) => 0
((Array-getter a) 11 0) => is an error, which may be signaled")


(format-lambda-list '(Array-map f array #\. arrays))
(<p> "If "(<code>(<var> 'array))", "(<code>"(car "(<var> 'arrays)")")", ... all have the same domain and "(<code>(<var> 'f))" is a function, then "(<code> 'Array-map)"
returns a new Array with the same domain and getter")
(<pre>"
(lambda multi-index
  (apply f (map (lambda (g) (apply g multi-index)) (map Array-getter (cons array arrays)))))")
(<p> "It is an error to call "(<code> 'Array-map)" if its arguments do not satisfy these conditions.")

(format-lambda-list '(Array-curry array outer-dimension))
(<p> "If "(<code>(<var> 'array))" is an Array and "(<code>(<var> 'outer-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'outer-dimension)" < (Interval-dimension (Array-domain "(<var> 'array)"))"))
(<p> "then "(<code> 'Array-curry)" returns")
(<pre>"
(call-with-values
    (lambda () (Interval-curry (Array-domain array) outer-dimension))
  (lambda (outer-interval inner-interval)
    (Array outer-interval
		 (lambda outer-multi-index
		   (Array inner-interval
				(lambda inner-multi-index
				  (apply (Array-getter array) (append outer-multi-index inner-multi-index))))))))")
(<p> "It is an error to call "(<code> 'Array-curry)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" This function curries "(<code>"(Array-getter "(<var> 'array)")")" while keeping track of the
domains of the outer and inner lambdas.")
(<p> "It is expected that "(<code> 'Array-curry)" will specialize the construction of")
(<pre>"
(lambda outer-multi-index
  (Array inner-interval
	       (lambda inner-multi-index
		 (apply (Array-getter array) (append outer-multi-index inner-multi-index)))))")
(<p>"and")
(<blockquote>
 (<code> "(lambda inner-multi-index (apply (Array-getter array) (append outer-multi-index inner-multi-index)))"))

(<p>"Example:")
(<pre> "
(define a (Array (Interval '#(0 0) '#(10 10))
		       list))
((Array-getter a) 3 4)  => (3 4)
(define curried-a (Array-curry a 1))
((Array-getter ((Array-getter curried-a) 3)) 4) => (3 4)")

(format-lambda-list '(Array-distinguish-one-axis array index))
(<p> "If "(<code>(<var> 'array))" is an Array and
"(<code>"(Interval-dimension (Array-domain "(<var> 'interval)"))")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (Interval-dimension (Array-domain "(<var> 'interval)"))"))
(<p> "then "(<code> 'Array-distinguish-one-axis)" returns")
(<pre>"
(call-with-values
    (lambda () (Interval-distinguish-one-axis (Array-domain array) index))
  (lambda (outer-interval inner-interval)
    (Array outer-interval
		 (lambda outer-multi-index
		   (Array inner-interval
				(lambda (m)
				  (apply (Array-getter array) (insert-arg-into-arg-list m outer-index index))))))))")
(<p> "where we define ")
(<pre>"
(define (insert-arg-into-arg-list arg arg-list index)
  (define (helper arg-list i)
    (if (= i 0)
	(cons arg arg-list)
	(cons arg (helper (cdr arg-list) (- i 1)))))
  (helper arg-list index))")

(<p> "It is an error to call "(<code> 'Array-distinguish-one-axis)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" It is expected that "(<code> 'Array-distinguish-one-axis)" will specialize the construction of ")
(<pre>"
(lambda outer-multi-index
  (Array inner-interval
	       (lambda (m)
		 (apply (Array-getter array) (insert-arg-into-arg-list m outer-index index)))))")
(<p>"and")
(<blockquote>
 (<code> "(lambda (m) (apply (Array-getter array) (insert-arg-into-arg-list m outer-index index)))"))

(format-lambda-list '(Array-for-each f array #\. arrays))
(format-lambda-list '(Array-for-each-serial f array #\. arrays))
(<p> "If "(<code>(<var> 'array))", "(<code>"(car "(<var> 'arrays)")")", ... all have the same domain "(<code>(<var> 'domain))" and "(<code>(<var> 'f))" is an appropriate function, then "(<code> 'Array-for-each)"
calls")
(<pre>"
(Interval-for-each  (lambda multi-index
		      (apply f (map (lambda (g) (apply g multi-index)) (map Array-getter (cons array arrays)))))
		    (Array-domain array))")
(<p> "and "(<code> 'Array-for-each-serial)"
calls")
(<pre>"
(Interval-for-each-serial  (lambda multi-index
			     (apply f (map (lambda (g) (apply g multi-index)) (map Array-getter (cons array arrays)))))
			   (Array-domain array))")

(<p> "It is expected that "(<code> 'Array-map)", "(<code> 'Array-for-each)" and "(<code> 'Array-for-each-serial)" will specialize the construction of")
(<pre>"
(lambda multi-index
  (apply f (map (lambda (g) (apply g multi-index)) (map Array-getter (cons array arrays)))))")
(<p> "It is an error to call "(<code> 'Array-for-each)" if its arguments do not satisfy these conditions.")

(format-lambda-list '(Array-reduce operator identity array))
(format-lambda-list '(Array-reduce-serial operator identity array))
(<p> "If "(<code>(<var> 'array))"  is an Array then "(<code> 'Array-reduce)" returns "
     (<code>"(Interval-reduce (Array-getter "(<var>"array")") "(<var>"operator identity")" (Array-domain "(<var> 'array)"))")" and "(<code> 'Array-reduce-serial)" returns "
     (<code>"(Interval-reduce-serial (Array-getter "(<var>"array")") "(<var>"operator identity")" (Array-domain "(<var> 'array)"))")".")

(format-lambda-list '(Array-extract array new-domain))
(<p> "If "(<code>(<var> 'array))" is an Array and "(<code>(<var> 'new-domain))" is an Interval that is a sub-interval of "(<code> "(Array-domain "(<var> 'array)")")", then "(<code> 'Array-extract)" returns a new array")
(<blockquote>
 (<code> "(Array "(<var> 'new-domain)" (Array-getter "(<var>'array)"))"))
							 
(<h2> "Mutable Arrays")
(<h3> "Procedures")
(format-lambda-list '(Mutable-array domain getter setter))
(<p> "Assume that "(<code>(<var> 'domain))" is an Interval of dimension "(<code>(<var> 'n))" and that "(<code>(<var> 'getter))" and
"(<code>(<var> 'setter))" are two routines that satisfy: If ")
(<blockquote>
 (<code>"("(<var> 'i)(<sub> '1)",...,"(<var> 'i)(<sub> 'n)") != ("(<var> 'j)(<sub> '1)",...,"(<var> 'j)(<sub> 'n)")"))
(<p> "are elements of "(<code>(<var> 'domain))" and ")
(<blockquote>
 (<code> "(getter "(<var> 'j)(<sub> '1)" ... "(<var> 'j)(<sub> 'n)") => x"))
(<p> "then \"after\"")
(<blockquote>
 (<code> "(setter v "(<var> 'i)(<sub> '1)" ... "(<var> 'i)(<sub> 'n)")"))
(<p> "we have")
(<blockquote>
 (<code> "(getter "(<var> 'j)(<sub> '1)" ... "(<var> 'j)(<sub> 'n)") => x"))
(<p> "and")
(<blockquote>
 (<code> "(getter "(<var> 'i)(<sub> '1)",...,"(<var> 'i)(<sub> 'n)") => v"))
(<p> "Then "(<code> 'Mutable-array)" builds a Mutable-array with domain "(<code>(<var> 'domain))", getter "(<code>(<var> 'getter))" and
setter "(<code>(<var> 'setter))".  A Mutable-array is an Array.  It is an error to call "(<code> 'Mutable-array)" if its arguments do not satisfy these conditions.")

(format-lambda-list '(Mutable-array? obj))
(<p> "Returns "(<code>"#t")" if and only if "(<code>(<var> 'obj))" is a Mutable-array.  Note: Mutable-arrays are Arrays.")

(format-lambda-list '(Array-setter array))
(<p> "If "(<code>(<var> 'array))" is a Mutable-array built by")
(<blockquote>
 (<code> "(Mutable-array "(<var> 'domain)" "(<var> 'getter)" "(<var> 'setter)")"))
(<p> "then "(<code> 'Array-setter)" returns "(<code>(<var> 'setter))". It is an error to call "(<code> 'Array-setter)"
if "(<code>(<var> 'array))" is not a Mutable-array.")

(<p> "Example: ")
(<pre> "
(define sparse-array
  (let ((domain (Interval '#(0 0) '#(1000000 1000000)))
	(sparse-rows (make-vector 1000000 '())))
    (Mutable-array domain
		   (lambda (i j)
		     (cond ((assv j (vector-ref sparse-rows i))
			    => cdr)
			   (else
			    0.0)))
		   (lambda (v i j)
		     (cond ((assv j (vector-ref sparse-rows i))
			    => (lambda (pair)
				 (set-cdr! pair v)))
			   (else
			    (vector-set! sparse-rows i (cons (cons j v) (vector-ref sparse-rows i)))))))))
((Array-getter sparse-array) 12345 6789)  => 0.
((Array-getter sparse-array) 0 0) => 0.
((Array-setter sparse-array) 1.0 0 0) => undefined
((Array-getter sparse-array) 12345 6789)  => 0.
((Array-getter sparse-array) 0 0) => 1.")
		  
(format-lambda-list '(Mutable-array-curry array outer-dimension))
(<p> "If "(<code>(<var> 'array))" is a Mutable-array and "(<code>(<var> 'outer-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'outer-dimension)" < (Interval-dimension (Array-domain "(<var> 'array)"))"))
(<p> "then "(<code> 'Mutable-array-curry)" returns")
(<pre>"
(call-with-values
    (lambda () (Interval-curry (Array-domain array) outer-dimension))
  (lambda (outer-interval inner-interval)
    (Array outer-interval
	   (lambda outer-multi-index
	     (Mutable-array inner-interval
			    (lambda inner-multi-index
			      (apply (Array-getter array) (append outer-multi-index inner-multi-index)))
			    (lambda (v . inner-multi-index)
			      (apply (Array-setter array) v (append outer-multi-index inner-multi-index))))))))")
(<p> "It is an error to call "(<code> 'Mutable-array-curry)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" This function curries "(<code>"(Array-getter "(<var> 'array)")")"
and "(<code>"(Array-setter "(<var> 'array)")")" while keeping track of the
domains of the outer and inner lambdas.")
(<p> "It is expected that "(<code> 'Mutable-array-curry)" will specialize the construction of")
(<pre>"
(lambda outer-multi-index
  (Mutable-array inner-interval
		       (lambda inner-multi-index
			 (apply (Array-getter array) (append outer-multi-index inner-multi-index)))
		       (lambda (v . inner-multi-index)
			 (apply (Array-setter array) v (append outer-multi-index inner-multi-index)))))")
(<p> "and")
(<blockquote>
 (<code> "(lambda inner-multi-index (apply (Array-getter array) (append outer-multi-index inner-multi-index)))"))
(<p>"and")
(<blockquote>
 (<code> "(lambda (v . inner-multi-index) (apply (Array-setter array) v (append outer-multi-index inner-multi-index)))"))

(format-lambda-list '(Mutable-array-distinguish-one-axis array index))
(<p> "If "(<code>(<var> 'array))" is a Mutable-array and
"(<code>"(Interval-dimension (Array-domain "(<var> 'interval)"))")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (Interval-dimension (Array-domain "(<var> 'interval)"))"))
(<p> "then "(<code> 'Mutable-array-distinguish-one-axis)" returns")
(<pre>"
(call-with-values
    (lambda () (Interval-distinguish-one-axis (Array-domain array) index))
  (lambda (outer-interval inner-interval)
    (array outer-interval
		 (lambda outer-multi-index
		   (Mutable-array inner-interval
					(lambda (m)
					  (apply (Array-getter array) (insert-arg-into-arg-list m outer-index index)))
					(lambda (v m)
					  (apply (Array-setter array) v (insert-arg-into-arg-list m outer-index index))))))))")
(<p> "where we define ")
(<pre>"
(define (insert-arg-into-arg-list arg arg-list index)
  (define (helper arg-list i)
    (if (= i 0)
	(cons arg arg-list)
	(cons arg (helper (cdr arg-list) (- i 1)))))
  (helper arg-list index))")

(<p> "It is an error to call "(<code> 'Mutable-array-distinguish-one-axis)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" It is expected that "(<code> 'Mutable-array-distinguish-one-axis)" will specialize the construction of ")
(<pre>"
(lambda outer-multi-index
  (Mutable-array inner-interval
		       (lambda (m)
			 (apply (Array-getter array) (insert-arg-into-arg-list m outer-index index)))
		       (lambda (v m)
			 (apply (Array-setter array) v (insert-arg-into-arg-list m outer-index index)))))")
(<p>"and")
(<blockquote>
 (<code> "(lambda (m) (apply (Array-getter array) (insert-arg-into-arg-list m outer-index index)))"))
(<p> "and")
(<blockquote>
 (<code> "(lambda (v m) (apply (Array-setter array) v (insert-arg-into-arg-list m outer-index index)))"))

(<h2> "Array Manipulators")
(<p> "Conceptually, an array manipulator is a set of functions to manage the backing store of a Fixed-array.
The functions allow one to make a backing store, to get values from the store and to set new values, to return the length of the store, and to specify a default value for initial elements of the backing store.  Typically, a backing store is a (heterogeneous or homogenous) vector.")
(<h3> "Procedures")

(format-lambda-list '(make-Array-manipulators getter setter checker maker length default))
(<p> "Here we assume the following relationships between the arguments of "(<code> 'make-Array-manipulators)".  Assume that the \"elements\" of
the backing store are of some \"type\", either heterogeneous (all Scheme types) or homogeneous (of some restricted type).")
(<ul>
 (<li> (<code> "("(<var>"maker n")" "(<var> 'value)")")" returns an object containing "(<code>(<var> 'n))" elements of value "(<code>(<var> 'value))".")
 (<li> "If "(<code>(<var> 'v))" is an object created by "
       (<code>"("(<var> "maker n value")")")
       " and  0 <= "(<code>(<var> 'i))" < "(<code>(<var> 'n))", then "(<code> "("(<var>"getter v i")")")" returns the current value of the "(<code>(<var> 'i))"'th element of "(<code>(<var> 'v))", and "(<code> "("(<var> 'checker)" ("(<var>"getter v i")")) => #t")".")
 (<li> "If "(<code>(<var> 'v))" is an object created by "
       (<code>"("(<var> "maker n value")")")
       ",  0 <= "(<code>(<var> 'i))" < "(<code>(<var> 'n))", and "(<code>"("(<var> 'checker)" "(<var> 'val)") => #t")", then "(<code> "("(<var>"setter v i val")")")" sets the value of the "(<code>(<var> 'i))"'th element of  "(<code>(<var> 'v))" to "(<code>(<var> 'val))".")
 (<li> "If "(<code>(<var> 'v))" is an object created by "
       (<code>"("(<var> "maker n value")")")
       " then "(<code> "("(<var>"length v")")")" returns "(<code>(<var> 'n))"."))
(<p> "If the arguments do not satisfy these conditions, then it is an error to call "(<code> 'make-Array-manipulators))
(<p> "Note that we assume that "(<code>(<var> 'getter))" and "(<code>(<var> 'setter))" generally take "(<i> 'O)"(1) time to execute.")

(format-lambda-list '(Array-manipulators-getter m))
(format-lambda-list '(Array-manipulators-setter m))
(format-lambda-list '(Array-manipulators-checker m))
(format-lambda-list '(Array-manipulators-maker m))
(format-lambda-list '(Array-manipulators-length m))
(format-lambda-list '(Array-manipulators-default m))
(<p> "If "(<code>(<var> 'm))" is an object created by")
(<blockquote>
 (<code>"(make-Array-manipulators "(<var> "setter getter checker maker length default")")"))
(<p> " then "
(<code> 'Array-manipulators-getter)" returns "(<code>(<var> 'getter))", "
(<code> 'Array-manipulators-setter)" returns "(<code>(<var> 'setter))", "
(<code> 'Array-manipulators-checker)" returns "(<code>(<var> 'checker))", "
(<code> 'Array-manipulators-maker)" returns "(<code>(<var> 'maker))", and "
(<code> 'Array-manipulators-default)" returns "(<code>(<var> 'default))".  Otherwise, it is an error to call any of these routines.")

(<h3> "Global Variables")
(format-global-variable 'generic-array-manipulators)
(format-global-variable 's8-array-manipulators)
(format-global-variable 's16-array-manipulators)
(format-global-variable 's32-array-manipulators)
(format-global-variable 's64-array-manipulators)
(format-global-variable 'u1-array-manipulators)
(format-global-variable 'u8-array-manipulators)
(format-global-variable 'u16-array-manipulators)
(format-global-variable 'u32-array-manipulators)
(format-global-variable 'u64-array-manipulators)
(format-global-variable 'f32-array-manipulators)
(format-global-variable 'f64-array-manipulators)

(<p> (<code> 'generic-array-manipulators)" is defined by")
(<blockquote>
 (<code> "(define generic-array-manipulators (make-Array-manipulators vector-ref vector-set! (lambda (arg) #t) make-vector vector-length #f))"))
"Furthermore, "(<code> "s"(<var> 'X)"-array-manipulators")" are defined for "(<code>(<var> 'X))"=8, 16, 32, and 64 (which have default values 0 and
manipulate exact integer values between -2"(<sup>(<var> 'X)"-1")" and
2"(<sup> (<var> 'X)"-1")"-1 inclusive),
 "(<code> "u"(<var> 'X)"-array-manipulators")" are defined for "(<code>(<var> 'X))"=1, 8, 16, 32, and 64 (which have default values 0 and manipulate exact integer values between 0 and
2"(<sup> (<var> 'X))"-1 inclusive), and
"(<code> "f"(<var> 'X)"-array-manipulators")" are defined for "(<code>(<var> 'X))"= 32 and 64 (which have default value 0.0 and manipulate 32- and 64-bit floating-point numbers).  Each of these
could be defined simply as generic-array-manipulators, but it is assumed that implementations with homogeneous arrays will give definitions
that either save space, avoid boxing, etc., for the specialized arrays."

(<h2> "Indexers")
(<h3> "Procedures")

(format-lambda-list '(indexer= indexer1 indexer2 interval))
(<p> "If "(<code>(<var> 'indexer1))" and "(<code>(<var> 'indexer2))" are one-to-one affine mappings from "(<code>(<var> 'interval))"
to the interval "(<code>"[0,"(<var> 'K)")")" for some "(<code>(<var> 'K))", then "(<code> 'indexer=)" returns "(<code>"#t")"
if "(<code>(<var> 'indexer1))" and "(<code>(<var> 'indexer2))" take the same values on all elements of "(<code>(<var> 'interval))";
otherwise "(<code> 'indexer=)" returns "(<code>"#f")".  It is an error to call "(<code> 'indexer=)" if its
arguments don't satisfy these conditions.")

(<h2> "Fixed Arrays")
(<h3> "Global Variable")
(format-global-variable 'Fixed-array-default-safe?)
(<p> "Determines whether the setters and getters of Fixed-arrays check their arguments for correctness by default.  Initially it has the value "(<code> "#f")".")
(<h3> "Procedures")
(format-lambda-list '(Fixed-array "domain:" domain "manipulators:" manipulators #\[ "body:" body #\] #\[ "indexer:" indexer #\] #\[ "initializer-value:" initializer-value #\] #\[ "safe?:" safe? #\]))
(<p> "Builds a Fixed-array.  "(<code>(<var> 'domain))" must be an Interval; "(<code>(<var> 'manipulators))" must
 be Fixed-array-manipulators;  if "(<code>(<var> 'body))" is given, it must be of the same type as that returned by
"(<code>"(Fixed-array-manipulators-maker "(<var> 'manipulators)")")"; if "(<code>(<var> 'initializer-value))" is given, it must be storable
in "(<code>(<var> 'body))"; at most one of "(<code>(<var> 'initializer-value))" and "(<code>(<var> 'body))" can be given; if "(<code>(<var> 'indexer))" is given, it must be a one-to-one affine mapping from "(<code>(<var> 'domain))" to
[0,"(<code>"((Fixed-array-manipulators-length "(<var> 'manipulators)") "(<var> 'body)")")"); the variable "(<code>(<var> 'safe?))" determines whether the multi-index arguments to the getter and setter are checked to be in the domain, and whether the value of the setter is storable in the body;
the getter and setter of the result are defined by")
(<pre>"
(lambda (i_0 ... i_n-1)
  ((Fixed-array-manipulators-getter manipulators)
   body
   (indexer i_0 ... i_n-1)))")
(<p> "and")
(<pre>"
(lambda (v i_0 ... i_n-1)
  ((Fixed-array-manipulators-setter manipulators)
   body
   (indexer i_0 ... i_n-1)
   v))")
(<p> "The default values for arguments that are omitted are as follows:")

(<p> "Initializer-value: "(<code>"(Fixed-array-manipulators-default manipulators)"))

(<p> "Body:")
(<pre>"
((Fixed-array-manipulators-maker manipulators)
 (Interval-volume domain)
 initializer-value)")

(<p> "Indexer: The one-to-one mapping of elements of "(<code>(<var> 'domain))" to [0,"(<code>"(Interval-volume "(<var> 'domain)")")") in
lexicographical order.")

(<p> "Safe?: The current value of the global variable "(<code> 'Fixed-array-default-safe?)".")

(format-lambda-list '(Fixed-array? obj))
(<p> "Returns "(<code>"#t")" if "(<code>(<var> 'obj))" is a Fixed-array, and "(<code>"#f")" otherwise. A Fixed-array is a Mutable-array, and hence an Array.")

(format-lambda-list '(Array-body array))
(format-lambda-list '(Array-indexer array))
(format-lambda-list '(Array-manipulators array))
(format-lambda-list '(Array-safe? array))
(<p> (<code>'Array-body)" returns the body of "(<code>(<var> 'array))", "
     (<code>'Array-indexer)" returns the indexer of "(<code>(<var> 'array))", "
     (<code>'Array-manipulators)" returns the manipulators of "(<code>(<var> 'array))", and "
     (<code>'Array-safe?)" is true if and only if the arguments of "(<code> "(Array-getter "(<var> 'array)")")" and "(<code> "(Array-setter "(<var> 'array)")")" are checked for correctness. It is an error to call any of these routines if "(<code>(<var> 'array))" is not a Fixed-array.")

(format-lambda-list '(Fixed-array-share! array new-domain new-domain->old-domain))
(<p> "Constructs a new Fixed-array that shares the body of the Fixed-array "(<code>(<var> 'array))".
Returns an object that is behaviorally equivalent to")
(<pre>"
(Fixed-array domain:       new-domain
	     manipulators: (Fixed-array-manipulators array)
	     body:         (Fixed-array-body array)
	     indexer:      (lambda multi-index
			     (call-with-values
				 (lambda ()
				   (apply new-domain->old-domain multi-index))
			       (Fixed-array-indexer array))))")
(<p> (<code>(<var> 'new-domain->old-domain))" must be an affine one-to-one mapping from "(<code>"(Array-domain "(<var> 'array)")")" to
"(<code>(<var> 'new-domain))".")

(<p> "Note: It is assumed that affine structure of the composition of "(<code>(<var> 'new-domain->old-domain))" and "(<code>"(Fixed-array-indexer "(<var> 'array))" will be used to simplify:")
(<pre>"
(lambda multi-index
  (call-with-values
      (lambda ()
	(apply new-domain->old-domain multi-index))
    (Fixed-array-indexer array)))"
)

(format-lambda-list '(Fixed-array-curry array outer-dimension))
(<p> "It is an error to call "(<code> 'Fixed-array-curry)" unless
 "(<code>(<var> 'array))" is a Fixed-array
and "(<code>(<var> 'outer-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'outer-dimension)" < (Interval-dimension (Fixed-array-domain "(<var> 'array)"))."))
(<p> (<code> 'Fixed-array-curry)" returns")
(<pre>"
(call-with-values
    (lambda () (Interval-curry (Fixed-array-domain array) outer-dimension))
  (lambda (outer-interval inner-interval)
    (Array outer-interval
	   (lambda outer-multi-index
	     (Fixed-array-share! array
				 outer-interval
				 (lambda inner-multi-index
				   (apply values (append outer-multi-index inner-multi-index))))))))")

(<p> (<b> "Notes:")" This function curries "(<code>"(Array-getter "(<var> 'array)")")"
and "(<code>"(Array-setter "(<var> 'array)")")" while keeping track of the
domains of the outer and inner lambdas.")
(<p> "It is expected that "(<code> 'Fixed-array-curry)" will specialize the construction of")
(<blockquote>
 (<code> "(lambda outer-multi-index
		   (Fixed-array-share! array
				       outer-interval
				       (lambda inner-multi-index
					 (apply values (append outer-multi-index inner-multi-index)))))"))

(format-lambda-list '(Fixed-array-distinguish-one-axis array index))
(<p> "It is an error to call "(<code> 'Fixed-array-distinguish-one-axis)" unless
 "(<code>(<var> 'array))" is a Fixed-array and
"(<code>"(Interval-dimension (Fixed-array-domain "(<var> 'interval)"))")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (Interval-dimension (Fixed-array-domain "(<var> 'array)"))"))
(<p> (<code> 'Fixed-array-distinguish-one-axis)" returns")
(<pre>"
(call-with-values
    (lambda () (Interval-distinguish-one-axis (Fixed-array-domain array) index))
  (lambda (outer-interval inner-interval)
    (Array outer-interval
		 (lambda outer-multi-index
		   (Fixed-array-share! array
				       inner-interval
				       (lambda (m) (apply values (insert-arg-into-arg-list m outer-index index))))))))")
(<p> "where we define ")
(<pre>"
(define (insert-arg-into-arg-list arg arg-list index)
  (define (helper arg-list i)
    (if (= i 0)
	(cons arg arg-list)
	(cons arg (helper (cdr arg-list) (- i 1)))))
  (helper arg-list index))")



(<p> "It is expected that "(<code> 'Fixed-array-distinguish-one-axis)" will specialize the construction of ")
(<pre>"
(lambda outer-multi-index
  (Fixed-array-share! array
		      inner-interval
		      (lambda (m) (apply values (insert-arg-into-arg-list m outer-index index)))))")



(format-lambda-list '(Array->Fixed-array array #\[ result-manipulators "generic-array-manipulators" #\] #\[ safe? "#f"#\]))
(format-lambda-list '(Array->Fixed-array-serial array #\[ result-manipulators "generic-array-manipulators" #\] #\[ safe? "#f"#\]))
(<p> "If "(<code>(<var> 'array))" is an Array whose elements can be manipulated by the Fixed-array-manipulators
"(<code>(<var> 'result-manipulators))", then the Fixed-array returned by "(<code> 'Array->Fixed-array)" can be defined by:")
(<pre>"
(let ((result (Fixed-array domain:       (Array-domain array)
			   manipulators: result-manipulators
			   safe?:        safe?)))
  (Interval-for-each (lambda multi-index
		       (apply (Array-setter result) (apply (Array-getter array) multi-index) multi-index))
		     (Array-domain array))
  result)")
(<p> "Otherwise it is an error to call "(<code> 'Array->Fixed-array)".")
(<p> (<code> 'Array->Fixed-array)"  does not specify
the order in which "(<code>"(Array-getter "(<var> 'array)")")" is applied to the multi-indices in "(<code>"(Array-domain "(<var> 'array)")")".")
(<p> "Similarly, the Fixed-array returned by "(<code> 'Array->Fixed-array-serial)" can be defined by:")
(<pre>"
(let ((result (Fixed-array domain:       (Array-domain array)
			   manipulators: result-manipulators
			   safe?:        safe?)))
  (Interval-for-each-serial (lambda multi-index
			      (apply (Array-setter result) (apply (Array-getter array) multi-index) multi-index))
			    (Array-domain array))
  result)")
(<p> "Thus, "(<code> 'Array->Fixed-array-serial)" evaluates "(<code>"(Array-getter "(<var> 'array)")")" to the multi-indices in
"(<code>"(Array-domain "(<var> 'array)")")" in lexicographical order.")


(<h1> "Implementation")
(<p> "We provide an implementation in Gambit-C; the nonstandard techniques used
in the implementation are: DSSSL-style optional and keyword arguments; a
unique object to indicate absent arguments; "(<code>"define-structure")";
and "(<code>"define-macro")".")

(<h1> "Relationship to other SRFIs")
(<p> "Final SRFIs "(<a> href: "#SRFI-25" "25")", "(<a> href: "#SRFI-47" "47")", "(<a> href: "#SRFI-58" "58")", and "(<a> href: "#SRFI-63" "63")" deal with \"Multi-dimensional Array Primitives\", \"Array\", \"Array Notation\",
and \"Homogeneous and Heterogeneous Arrays\", respectively.  Each of these previous SRFIs deal with what we call in this SRFI
Fixed-arrays.  Many of the functions in these previous SRFIs  have corresponding forms in this SRFI.  For example, from SRFI 63, we can
translate: ")
(<dl>
 (<dt> (<code> "(array? obj)"))
 (<dd> (<code> "(Array? obj)"))
 (<dt> (<code> "(Array-rank a)"))
 (<dd> (<code> "(Interval-dimension (Array-domain obj))"))
 (<dt> (<code> "(make-array prototype k1 ...)"))
 (<dd> (<code> "(Fixed-array domain: (Interval (vector 0 ...) (vector k1 ...)) manipulators: manipulators)")".")
 (<dt> (<code> "(make-shared-array array mapper k1 ...)"))
 (<dd> (<code> "(Fixed-array-share! array (Interval (vector 0 ...) (vector k1 ...)) mapper)"))
 (<dt> (<code> "(array-in-bounds? array index1 ...)"))
 (<dd> (<code> "(Interval-contains-multi-index? (Array-domain array) index1 ...)"))
 (<dt> (<code> "(array-ref array k1 ...)"))
 (<dd> (<code> "((Array-getter array) k1 ...)"))
 (<dt> (<code> "(array-set! array obj k1 ...)"))
 (<dd> (<code> "((Array-setter array) obj k1 ...)"))
 )
(<p> "At the same time, this SRFI has some special features:")
(<ul>
 (<li> "Intervals, used as the domains of Arrays in this SRFI, are useful
objects in their own rights, with their own procedures.  We make a sharp distinction between the domains
of Arrays and the Arrays themselves.")
 (<li> "Intervals can have nonzero lower bounds in each dimension.")
 (<li> "Intervals cannot be empty.")
 (<li> "Arrays must have a getter, but may have no setter.  For example, on a system with eight-bit chars, one
can write a function to read greyscale images in the PGM format of the netpbm package as follows.  The  lexicographical
order in Array->Fixed-array-serial guarantees the the correct order of execution of the input procedures:"
(<pre>"
(define make-pgm   cons)
(define pgm-greys  car)
(define pgm-pixels cdr)

(define (read-pgm file)

  (define (read-pgm-object port)
    (skip-white-space port)
    (let ((o (read port)))
      (read-char port) ; to skip the newline or next whitespace
      (if (eof-object? o)
	  (error \"reached end of pgm file\")
	  o)))

  (define (skip-to-end-of-line port)
    (let loop ((ch (read-char port)))
      (if (not (eq? ch #\\newline))
	  (loop (read-char port)))))

  (define (white-space? ch)
    (case ch 
      ((#\\newline #\\space #\\tab) #t)
      (else #f)))

  (define (skip-white-space port)
    (let ((ch (peek-char port)))
      (cond ((white-space? ch) (read-char port) (skip-white-space port))
	    ((eq? ch #\\#) (skip-to-end-of-line port)(skip-white-space port))
	    (else #f))))

  (call-with-input-file
      file
    (lambda (port)
      (let* ((header (read-pgm-object port))
	     (columns (read-pgm-object port))
	     (rows (read-pgm-object port))
	     (greys (read-pgm-object port)))
	(make-pgm greys
		  (Array->Fixed-array-serial
		   (Array
		    (Interval '#(0 0)
			      (vector rows columns))
		    (cond ((or (eq? header 'p5)                                     ;; pgm binary
			       (eq? header 'P5))
			   (if (< greys 256)
			       (lambda (i j)                                        ;; one byte/pixel
				 (char->integer (read-char port)))
			       (lambda (i j)                                        ;; two bytes/pixel, little-endian
				 (let* ((first-byte (char->integer (read-char port)))
					(second-byte (char->integer (read-char port))))
				   (+ (* second-byte 256) first-byte)))))
			  ((or (eq? header 'p2)                                     ;; pgm ascii
			       (eq? header 'P2))
			   (lambda (i j)
			     (read port)))
			  (else
			   (error \"read-pgm: not a pgm file\"))))))))))"))
   (<li> "The set of Fixed-arrays is extensible.  For example, manipulators for Fixed-arrays that contain complex numbers with 32-bit
floating-point real and imaginary parts could be defined using "(<a> href: "#SRFI-4" "SRFI-4")" as:"
(<pre>"
(define c32-array-manipulators
  (make-Array-manipulators (lambda (body i)                                               ;; getter
				   (make-rectangular (f32vector-ref body (* 2 i))
						     (f32vector-ref body (+ (* 2 i) 1))))
				 (lambda (body i obj)                                     ;; setter
				   (f32vector-set! body (* 2 i)       (real-part obj))
				   (f32vector-set! body (+ (* 2 i) 1) (imag-part obj)))
				 (lambda (obj)                                            ;; checker
				   (and (complex? obj)
					(inexact? (real-part obj))
					(inexact? (imag-part obj))))
				 (lambda (n val)                                          ;; maker
				   (let ((l (* 2 n))
					 (re (real-part val))
					 (im (imag-part val)))
				     (let ((result (make-f32vector l)))
				       (do ((i 0 (+ i 2)))
					   ((= i l) result)
					 (f32vector-set! result i re)
					 (f32vector-set! result (+ i 1) im)))))
				 (lambda (body)                                           ;; length
				   (quotient (f32vector-length body) 2))
				 0.+0.i                                                   ;; default
				 ))
(define a (Array->Fixed-array (Array (Interval '#(0 0) '#(10 10))
				     (lambda (i j)
				       (cond ((< i j) 1.0+0.0i)
					     ((< j i) 0.0+1.0i)
					     (else
					      0.0+0.0i))))
			      c32-array-manipulators
			      #t))
((Array-getter a) 2 2) => 0.0+0.0i
((Array-getter a) 2 3) => 1.0+0.0i
((Array-getter a) 3 2) => 0.0+1.0i
((Array-setter a) 2.0+2.0i 2 3) => undefined
((Array-getter a) 2 2) => 0.0+0.0i
((Array-getter a) 2 3) => 2.0+2.0i
((Array-getter a) 3 2) => 0.0+1.0i
((Array-getter a) 10 10) => an error, which is signalled
((Array-setter a) 1+1i 2 2) => an error, which is signalled

"))
)
					
(<h1> "References")
(<ol>
 (<li> (<a> name: 'bawden href: "http://groups-beta.google.com/group/comp.lang.scheme/msg/6c2f85dbb15d986b?hl=en&" "\"multi-dimensional arrays in R5RS?\"")
       ", by Alan Bawden.")
 (<li> (<a> name: 'SRFI-4  href: "http://srfi.schemers.org/srfi-4/"  "SRFI 4:  Homogeneous Numeric Vector Datatypes")", by Marc Feeley.")
 (<li> (<a> name: 'SRFI-25 href: "http://srfi.schemers.org/srfi-25/" "SRFI 25: Multi-dimensional Array Primitives")", by Jussi Piitulainen.")
 (<li> (<a> name: 'SRFI-47 href: "http://srfi.schemers.org/srfi-47/" "SRFI 47: Array")", by Aubrey Jaffer.")
 (<li> (<a> name: 'SRFI-58 href: "http://srfi.schemers.org/srfi-58/" "SRFI 58: Array Notation")", by Aubrey Jaffer.")
 (<li> (<a> name: 'SRFI-63 href: "http://srfi.schemers.org/srfi-63/" "SRFI 63: Homogeneous and Heterogeneous Arrays")", by Aubrey Jaffer."))
))))))
