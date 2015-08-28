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
	(<p> "This SRFI is currently in " (<em> "draft") " status.  Here is "
	     (<a> href: "http://srfi.schemers.org/srfi-process.html" "an explanation")
	     " of each status that a SRFI can hold.  To provide input on this SRFI, please send email to "
	     (<code> (<a> href: "mailto:srfi minus 122 at srfi dot schemers dot org"
			  "srfi-122@" (<span> class: "antispam" "nospam") "srfi.schemers.org"))
	     ".  To subscribe to the list, follow "
	     (<a> href: "http://srfi.schemers.org/srfi-list-subscribe.html" "these instructions")
	     ".  You can access previous messages via the mailing list "
	     (<a> href: "http://srfi-email.schemers.org/srfi-122" "archive")".")
	(<ul> (<li> "Received: 2015/7/23")
	      (<li> "Draft #1 published: 2015/7/27")
	      (<li> "Draft #2 published: 2015/7/31")
	      (<li> "Draft #3 published: 2015/7/31")
	      (<li> "Draft #4 published: 2015/8/??")
	      )
       
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
       (<p> "Specifically, an array specifies a nonempty, multi-dimensional interval, called its "(<i> "domain")", and a mapping from this domain to (single) Scheme objects.  This mapping is called the "(<i> 'getter)" of the array.")
       (<p> "If this mapping can be changed, the array is said to be "(<i> 'mutable)" and the mutation is effected
by the array's "(<i> 'setter)".  We call an object of this type a mutable-array.")
       (<p> "In general, we leave the implementation of arrays completely open.  They may be defined simply by closures, or
they may have hash tables or databases behind an implementation.  If the getter and setter functions of a mutable-array are
defined by accessing and setting elements  of a one-dimensional (heterogeneous or homogeneous) vector that are determined by a one-to-one function from
the domain of the array into the integers between 0 (inclusive) and the length of the backing-store vector (exclusive),
the array is said to be "(<i> 'fixed)". A fixed-array is an example of a mutable-array.")

(<p> "Thus,  we  need the concept of an "(<i> 'indexer)", which is a one-to-one mapping whose domain is an interval and whose range is
contained in another interval.  Conceptually, an indexer is itself an array that returns multiple values.  An
important subset of indexers are affine mappings (linear mappings plus constants) from one domain to another.  We do not
encapsulate indexers, with their domain interval, range interval, and multi-valued mapping, into a distinct type.
While we considered the formalized use of non-affine indexers in fixed-arrays, we restrict indexers in fixed-arrays to be affine.
Thus our fixed-arrays are very similar to "
     (<a> href: "#bawden" "Bawden-style arrays")". (If you want to specify a non-affine indexer into a body, it can be done by constructing a mutable-array.)")
(<p> "The backing store of a fixed-array, which may be a heterogeneous or homogeneous vector,
is created, accessed, etc., via the components of an object we call a storage-class.  We define their properties below.")
(<p> "The API of this SRFI uses keywords from SRFI-88 and the calling convention from SRFI-89 for optional and keyword arguments (although the implementation defines functions with keyword and optional arguments using DSSSL's notation, not the notation from SRFI-89).")

(<h1> "Examples of application areas")
(<ul>
 (<li> "Many multi-dimensional transforms in signal processing are "(<i> 'separable)", in that that the multi-dimensional transform can be computed by applying one-dimensional transforms in each of the coordinate directions.  Examples of such transforms include the Fast Fourier Transform and the Fast Wavelet Transform.  Each one-dimensional subdomain of the complete domain is called a "(<i> 'pencil)", and the same one-dimensional transform is applied to all pencils in a given direction. This motivates us to define the various procedures *-distinguish-one-axis.")
 (<li> "Many applications have multi-dimensional data that behave differently in different coordinate directions.  For example, one might have a time series of maps, which can be stored in a single three-dimensional array.  Or one might have one-dimensional spectral data assigned to each pixel on a map.  The data cube as a whole is considered three-dimensional "(<i> 'hyperspectral)" data, but for processing the spectra separately one would apply a function to the spectrum at each pixel.  This corresponds to "(<i> 'currying)" arguments in programming languages, so we include such procedures here.")
 (<li> "By default, an array computes each array element each time it is needed.  So the following code"
       (<pre>
"
(define (vector-field-sequence-ell-infty-ell-1-ell-2-norm p)
  (array-max (array-map (lambda (p^k)
                          (array-average (array-map Point-length-R^4
                                                    (array-extract p^k
								   (array-domain zero-image)))))
                        p)))"
)
       "(with suitable definitions for "(<code> 'array-max)", "(<code> 'array-average)", and "(<code> 'Point-length-R^4)") computes the maximum over a number of two-dimensional arrays of the average length of four-vectors in those arrays restricted to the domain of "(<code> "zero-image")", without storing all of the elements of any of the intermediate arrays together.")
       )
 

(<h1> "Issues and Notes")
(<ul>
 (<li> (<b> "fixed-arrays. ")  "Bawden-style arrays have their elements stored in a heterogeneous or homogeneous
vector.  A single vector can contain the elements of
several conceptually separate arrays.  I needed a name for this type of array implementation, and I think of the
backing-store vector that contains the array elements as \"fixed\", sitting somewhere out of sight.  Thus the name
\"fixed-array\".  If you can come up with a better name, good.")
 (<li> (<b> "\"object\" vs. \"make-object\". ") "I think of "(<code> 'make-vector)", "(<code> 'make-string)", etc., as low-level memory
allocators with little computation going on behind the scenes.  Instantiating the objects of this SRFI may require non-trivial
computation behind the scenes, so I didn't want to name the instantiation routines make-whatever.")
 (<li> (<b> "Specifying intervals. ")  "I couldn't, for the life of me, figure out the most \"natural\" way to specify intervals.  Perhaps " #\newline
       (<blockquote> (<code> "(interval lower-bounds: l0 l1 ... upper-bounds: u0 u1 ...)"))
       "comes closes, but it would take a lot of computation to check properly , and this isn't even a valid DSSSL argument list. " #\newline
       "In the end I decided to just require the lower and upper bound arguments to be vectors.")
 (<li> (<b> (<code>" interval-lower-bounds->list" )". ") "I think of this routine not as a getter ("(<code> 'interval-lower-bounds)" would be a getter) but as a converter, like " #\newline
       (<code> 'vector->list)".  I don't want to expose the inner storage mechanism for lower and upper bounds of intervals, so I don't provide a getter, only these converters.")
 (<li> (<b> "Indexers. ")"Both the arguments new-domain->old-domain to "(<code> 'fixed-array-share!)" and indexer to "(<code> 'fixed-array)" are conceptual arrays, " #\newline
       "the first multiple-valued and the second single-valued.")
 (<li> (<b> "Source of function names. ")"The function "(<code> 'array-curry)" gets its name from the " #\newline
       (<a> href: "http://en.wikipedia.org/wiki/Currying" "curry operator")
       " in programming---we are currying the getter of the array and keeping careful track of the domains. " #\newline
       "interval-curry is simply given a parallel name (although it can be thought of as currying the " #\newline
       "characteristic function of the interval,  encapsulated here as "(<code> 'interval-contains-multi-index?)").  Similarly, " #\newline
       (<code> 'array-distinguish-one-axis)" makes sense for arrays, but the parallel function for intervals is not very natural.")
 (<li> (<b> "Choice of functions on intervals. ")"The choice of functions for both arrays and intervals was motivated almost solely by what I needed for arrays.  There are " #\newline
       "natural operations on intervals, like "
       (<blockquote> (<code>"(interval-cross-product interval1 interval2 ...)"))
       "(the inverse of "(<code> 'interval-curry)") and"
       (<blockquote> (<code>"(interval-intersect interval1 interval2 ...)"))
       " which don't seem terribly natural for arrays.  If you could use these functions in your programs, tell me (windowing systems, etc.?).")
 (<li> (<b> "No empty intervals or arrays. ")"Mathematically, a functions f(x) is a relation R, which is a set  of ordered pairs {(x,y)} for x in some domain set X and " #\newline
       "y in some range set Y, for which (x1,y1) and (x2, y2) are in R and x1=x2 implies y1=y2.  (Here we assume that f is onto Y, in other words that Y consists precisely of" #\newline
       "the second coordinates of the ordered pairs in R, and \"=\" means \"identical\", i.e., y1 and y2 are the "(<i> 'same)" objects (in the sense of "#\newline
       (<code> 'eq?)").). The only relation R where X is the empty set is the empty relation consisting " #\newline
       " of no ordered pairs at all.  Since there are no ordered pairs, there are no values in the range set Y, i.e., Y is the empty set.  So the getter of an array with "#\newline
       "an empty interval as a domain would be able to return no values at all.  So I don't understand constructions that allow arrays with empty intervals as domains to return a single value."#\newline
       "Since I don't understand it, I don't allow it.")
       
 )
(<h1> "Specification")
(let ((END ",\n"))
  (<p> "Names defined in this SRFI:")
  (<dl>
   (<dt> "Intervals")
   (<dd> (<a> href: "#interval" "interval")END
	 (<a> href: "#interval?" "interval?")END
	 (<a> href: "#interval-dimension" "interval-dimension")END
	 (<a> href: "#interval-lower-bound" "interval-lower-bound")END
	 (<a> href: "#interval-upper-bound" "interval-upper-bound")END
	 (<a> href: "#interval-lower-bounds->list" "interval-lower-bounds->list")END
	 (<a> href: "#interval-upper-bounds->list" "interval-upper-bounds->list")END
	 (<a> href: "#interval-lower-bounds->vector" "interval-lower-bounds->vector")END
	 (<a> href: "#interval-upper-bounds->vector" "interval-upper-bounds->vector")END
	 (<a> href: "#interval=" "interval=")END
	 (<a> href: "#interval-volume" "interval-volume")END
	 (<a> href: "#interval-subset?" "interval-subset?")END
	 (<a> href: "#interval-contains-multi-index?" "interval-contains-multi-index?")END
	 (<a> href: "#interval-curry" "interval-curry")END
	 (<a> href: "#interval-distinguish-one-axis" "interval-distinguish-one-axis")END
	 (<a> href: "#interval-for-each" "interval-for-each")END
	 (<a> href: "#interval-for-each-serial" "interval-for-each-serial")END
	 (<a> href: "#interval-reduce" "interval-reduce")END
	 (<a> href: "#interval-reduce-serial" "interval-reduce-serial") END
	 (<a> href: "#interval-dilate" "interval-dilate")".")
   (<dt> "Arrays")
   (<dd> (<a> href: "#array" "array")END
	 (<a> href: "#array?" "array?")END
	 (<a> href: "#array-domain" "array-domain")END
	 (<a> href: "#array-getter" "array-getter")END
	 (<a> href: "#array-setter" "array-setter")END
	 (<a> href: "#array-body" "array-body")END
	 (<a> href: "#array-indexer" "array-indexer")END
	 (<a> href: "#array-storage-class " "array-storage-class")END
	 (<a> href: "#array-map" "array-map")END
	 (<a> href: "#array-curry" "array-curry")END
	 (<a> href: "#array-distinguish-one-axis" "array-distinguish-one-axis")END
	 (<a> href: "#array-for-each" "array-for-each")END
	 (<a> href: "#array-for-each-serial" "array-for-each-serial")END
	 (<a> href: "#array-reduce" "array-reduce")END
	 (<a> href: "#array-reduce-serial" "array-reduce-serial")".")
   (<dt> "Mutable Arrays")
   (<dd> (<a> href: "#mutable-array" "mutable-array")END
	 (<a> href: "#mutable-array?" "mutable-array?")END
	 (<a> href: "#mutable-array-curry" "mutable-array-curry")END
	 (<a> href: "#mutable-array-distinguish-one-axis" "mutable-array-distinguish-one-axis")".")
   (<dt> "Storage")
   (<dd> (<a> href: "#make-storage-class" "make-storage-class") END
	 (<a> href: "#storage-class-getter" "storage-class-getter") END
	 (<a> href: "#storage-class-setter" "storage-class-setter") END
	 (<a> href: "#storage-class-maker" "storage-class-maker") END
	 (<a> href: "#storage-class-length" "storage-class-length") END
	 (<a> href: "#storage-class-default" "storage-class-default") END
	 (<a> href: "#generic-storage-class" "generic-storage-class") END
	 (<a> href: "#s8-storage-class" "s8-storage-class") END
	 (<a> href: "#s16-storage-class" "s16-storage-class") END
	 (<a> href: "#s32-storage-class" "s32-storage-class") END
	 (<a> href: "#s64-storage-class" "s64-storage-class") END
	 (<a> href: "#u1-storage-class" "u1-storage-class") END
	 (<a> href: "#u8-storage-class" "u8-storage-class") END
	 (<a> href: "#u16-storage-class" "u16-storage-class") END
	 (<a> href: "#u32-storage-class" "u32-storage-class") END
	 (<a> href: "#u64-storage-class" "u64-storage-class") END
	 (<a> href: "#f32-storage-class" "f32-storage-class") END
	 (<a> href: "#f64-storage-class" "f64-storage-class") END
	 (<a> href: "#c64-storage-class" "c64-storage-class") END
	 (<a> href: "#c128-storage-class" "c128-storage-class") 
	 ".")
   (<dt> "Indexers")
   (<dd> (<a> href: "#indexer=" "indexer=")
	 ".")
   (<dt> "Fixed Arrays")
   (<dd> (<a> href: "#fixed-array-default-safe?" "fixed-array-default-safe?") END
	 (<a> href: "#fixed-array" "fixed-array")END
	 (<a> href: "#fixed-array?" "fixed-array?")END
	 (<a> href: "#fixed-array-curry" "fixed-array-curry")END
	 (<a> href: "#fixed-array-distinguish-one-axis" "fixed-array-distinguish-one-axis")END
	 (<a> href: "#array->fixed-array" "array->fixed-array")END
	 (<a> href: "#array->fixed-array-serial" "array->fixed-array-serial")END
	 (<a> href: "#fixed-array-share!" "fixed-array-share!")
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
(format-lambda-list '(interval lower-bounds upper-bounds))
(<p> "Create a new interval; "(<code> (<var>"lower-bounds"))" and "(<code> (<var>"upper-bounds"))"
are nonempty vectors (of the same length) of exact integers that satisfy")
(<blockquote>
 (<code>" (< (vector-ref "(<var>"lower-bounds")" i) (vector-ref "(<var>"upper-bounds")" i))"))
(<p> " for
0<="(<code>"i")"<"(<code>"(vector-length "(<var>"lower-bounds")")")".  It is an error if 
"(<code>(<var>"lower-bounds"))" and "(<code>(<var>"upper-bounds"))" do not satisfy these conditions.")   

(format-lambda-list '(interval? obj))
(<p> "Returns "(<code> "#t")" if "(<code> (<var>"obj"))" is an interval, and "(<code>"#f")" otherwise.")

(format-lambda-list '(interval-dimension interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "then "(<code> 'interval-dimension)" returns "(<code>"(vector-length "(<var>"lower-bounds")")")".  It is an error to call "(<code> 'interval-dimension)"
if "(<code>(<var>"interval"))" is not an interval.")

(format-lambda-list '(interval-lower-bound interval i))
(format-lambda-list '(interval-upper-bound interval i))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "and "(<code>(<var>"i"))" is an exact integer that satisfies")
(<blockquote>
 "0 <= "(<code>(<var>"i"))" < "(<code>"(vector-length "(<var>"lower-bounds")")")",")
(<p> " then "(<code> 'interval-lower-bound)" returns
"(<code>"(vector-ref "(<var>"lower-bounds")" "(<var>"i")")")" and "(<code> 'interval-upper-bound)" returns
"(<code>"(vector-ref "(<var>"upper-bounds")" "(<var>"i")")")".  It is an error to call "(<code> 'interval-lower-bound)" or "(<code> 'interval-upper-bound)"
if "(<code>(<var>"interval"))" and "(<code>(<var>"i"))" do not satisfy these conditions.")


(format-lambda-list '(interval-lower-bounds->list interval))
(format-lambda-list '(interval-upper-bounds->list interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> " then "(<code> 'interval-lower-bounds->list)" returns "(<code> "(vector->list "(<var>"lower-bounds")")")
     " and  "(<code> 'interval-upper-bounds->list)" returns "(<code> "(vector->list "(<var>"upper-bounds")")")". It is an error to call
"(<code> 'interval-lower-bounds->list)" or "(<code> 'interval-upper-bounds->list)" if "(<code>(<var>"interval"))" does not satisfy these conditions.")

(format-lambda-list '(interval-lower-bounds->vector interval))
(format-lambda-list '(interval-upper-bounds->vector interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> " then "(<code> 'interval-lower-bounds->vector)" returns a copy of "(<code> (<var>"lower-bounds"))
     "  and "(<code> 'interval-upper-bounds->vector)" returns a copy of "(<code> (<var>"upper-bounds"))". It is an error to call
"(<code> 'interval-lower-bounds->vector)" or "(<code> 'interval-upper-bounds->vector)" if "(<code>(<var>"interval"))" does not satisfy these conditions.")


(format-lambda-list '(interval-volume interval))
(<p> "If "(<code>(<var>"interval"))" is an interval built with ")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds")" "(<var>"upper-bounds")")"))
(<p> "then "(<code> 'interval-volume)" returns ")
(<blockquote>
 (<code> "(apply * (map - (interval-upper-bounds->list "(<var> 'interval)") (interval-lower-bounds->list "(<var> 'interval)"))"))
(<p> "It is an error to call "(<code> 'interval-volume)" if "(<code>(<var> 'interval))" does not satisfy this condition.")

(format-lambda-list '(interval= interval1 interval2))
(<p> "If "(<code>(<var>"interval1"))" and "(<code>(<var>"interval2"))" are intervals built with ")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds1")" "(<var>"upper-bounds1")")"))
(<p> "and")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds2")" "(<var>"upper-bounds2")")"))
(<p> "respectively, then "(<code> 'interval=)" returns")
(<blockquote>
 (<code> "(and (equal? "(<var> 'lower-bounds1)" "(<var> 'lower-bounds2)") (equal? "(<var> 'upper-bounds1)" "(<var> 'upper-bounds2)"))"))
(<p> "It is an error to call "(<code> 'interval=)" if "(<code>(<var> 'interval1))" or "(<code>(<var> 'interval2))" do not satisfy this condition.")

(format-lambda-list '(interval-subset? interval1 interval2))
(<p> "If "(<code>(<var>"interval1"))" and "(<code>(<var>"interval2"))" are intervals of the same dimension built with ")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds1")" "(<var>"upper-bounds1")")"))
(<p> "and")
(<blockquote>
 (<code>"(interval "(<var>"lower-bounds2")" "(<var>"upper-bounds2")")"))
(<p> "respectively, then "(<code> 'interval-subset?)" returns")
(<pre>"
(and (equal? (map >= (vector->list lower-bounds1) (vector->list lower-bounds2))
	     (map (lambda (x) #t) (vector->list lower-bounds1)))
     (equal? (map <= (vector->list upper-bounds1) (vector->list upper-bounds2))
	     (map (lambda (x) #t) (vector->list lower-bounds1))))")
     (<p> "It is an error to call "(<code> 'interval-subset?)" if "(<code>(<var> 'interval1))" or "(<code>(<var> 'interval2))" do not satisfy this condition.")

(format-lambda-list '(interval-contains-multi-index? interval index-0 ...))
(<p> "If "(<code>(<var> 'interval))" is an interval with dimension d and "(<code>(<var> 'index-0))", ..., form a multi-index of length d,
then "(<code> 'interval-contains-multi-index?)" returns "(<code> #t)" if and only if")
(<blockquote>
 (<code> "(interval-lower-bound "(<var> 'interval)" j) <= "(<var> 'index-j)" < (interval-upper-bound "(<var> 'interval)" j)"))
(<p>"for 0<= j < d.")
(<p> "It is an error to call "(<code> 'interval-contains-multi-index?)" if "(<code>(<var> 'interval))" and "(<code>(<var> 'index-0))",..., do not satisfy this condition.")

(format-lambda-list '(interval-curry interval left-dimension))
(<p> "Conceptually, "(<code> 'interval-curry)" takes a d-dimensional interval [l"(<sub> '0)", u"(<sub> '0)") x [l"(<sub> '1)", u"(<sub> '1)") x ... x [l"(<sub> 'd-1)", u"(<sub> 'd-1)")" #\newline
"and splits it into two parts")
(<blockquote> "[l"(<sub> '0)", u"(<sub> '0)") x [l"(<sub> '1)", u"(<sub> '1)") x ... x [l"(<sub>"left-dimension - 1")", u"(<sub>"left-dimension - 1")")")
(<p> "and")
(<blockquote>  "[l"(<sub> 'left-dimension)", u"(<sub> 'left-dimension)") x  ... x [l"(<sub> 'd-1)", u"(<sub> 'd-1)")")
(<p> "This function, the inverse of Cartesian products or cross products of intervals, is used to keep track of the domains of curried arrays.")
(<p> "More precisely, if "(<code>(<var> 'interval))" is an interval and "(<code>(<var> 'left-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'left-dimension)" < (interval-dimension "(<var> 'interval)")"))
(<p> "then "(<code> 'interval-curry)" returns two intervals:")
(<pre>"
(values (interval (vector (interval-lower-bound interval 0)
			  ...
			  (interval-lower-bound interval (- left-dimension 1)))
		  (vector (interval-upper-bound interval 0)
			  ...
			  (interval-upper-bound interval (- left-dimension 1))))
	(interval (vector (interval-lower-bound interval left-dimension)
			  ...
			  (interval-lower-bound interval (- (interval-dimension interval) 1)))
		  (vector (interval-upper-bound interval left-dimension)
			  ...
			  (interval-upper-bound interval (- (interval-dimension interval) 1)))))")
(<p> "It is an error to call "(<code> 'interval-curry)" if its arguments do not satisfy these conditions.")


(format-lambda-list '(interval-distinguish-one-axis interval index))
(<p> "If "(<code>(<var> 'interval))" is an interval and
"(<code>"(interval-dimension "(<var> 'interval)")")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (interval-dimension "(<var> 'interval)")"))
(<p> "then "(<code> 'interval-distinguish-one-axis)" returns")
(<pre>"
(values (interval (vector (interval-lower-bound interval 0)
			  ...         ; leaving out (interval-lower-bound interval index)
			  (interval-lower-bound interval (- left-dimension 1)))
		  (vector (interval-upper-bound interval 0)
			  ...         ; leaving out (interval-upper-bound interval index)
			  (interval-upper-bound interval (- left-dimension 1))))
	(interval (vector (interval-lower-bound interval index))
		  (vector (interval-upper-bound interval index))))")
(<p> "It is an error to call "(<code> 'interval-distinguish-one-axis)" if its arguments do not satisfy these conditions.")


(format-lambda-list '(interval-for-each f interval))
(format-lambda-list '(interval-for-each-serial f interval))
(<p> "These routines assume that "(<code>(<var> 'interval))" is an interval and "(<code>(<var> 'f))" is a routine whose domain includes elements of "(<code>(<var> 'interval))".  It is an error to call
"(<code> 'interval-for-each)" if "(<code>(<var> 'interval))" and "(<code>(<var> 'f))" do not satisfy these conditions.")
(<p> (<code> 'interval-for-each)" calls "(<code>(<var> 'f))" on each element of "(<code>(<var> 'interval))" in some unspecified order.")
(<p> (<code> 'interval-for-each-serial)" calls "(<code>(<var> 'f))" on each element of "(<code>(<var> 'interval))" in lexicographical order.")

(format-lambda-list '(interval-reduce f operator identity interval))
(format-lambda-list '(interval-reduce-serial f operator identity interval))
(<p> "If "(<code>(<var> 'interval))" is an interval and "(<code>(<var> 'f))" is a routine whose domain includes elements of "(<code>(<var> 'interval))", then
"(<code> 'interval-reduce-serial)" returns")
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
(<p> (<code> 'interval-reduce)" returns the same value when "(<code>(<var> 'operator))" is associative and it doesn't matter in which order "(<code>(<var> 'f))" is evaluated, and
it may assume these properties.")
(<p> "It is an error to call "(<code> 'interval-reduce)" or "(<code> 'interval-reduce-serial)" if "(<code>(<var> 'interval))" and "(<code>(<var> 'f))" do not satisfy these conditions.")


(format-lambda-list '(interval-dilate interval lower-diffs upper-diffs))
(<p> "If "(<code>(<var> 'interval))" is an interval with
lower bounds l"(<sub>"0")", ..., l"(<sub>"d-1")" and
upper bounds u"(<sub>"0")", ..., u"(<sub>"d-1")", and "
(<code>(<var> "lower-diffs"))" is a vector of exact integers L"(<sub>"0")", ..., L"(<sub>"d-1")" and "
(<code>(<var> "upper-diffs"))" is a vector of exact integers U"(<sub>"0")", ..., U"(<sub>"d-1")", then "
(<code>"interval-dilate")" returns a new interval with
lower bounds l"(<sub>"0")"+L"(<sub>"0")", ..., l"(<sub>"d-1")"+L"(<sub>"d-1")" and
upper bounds u"(<sub>"0")"+U"(<sub>"0")", ..., u"(<sub>"d-1")"+U"(<sub>"d-1")", as long as this is a
non-empty interval.  It is an error if the arguments do not satisfy these conditions.")
(<p> "Examples:")
(<blockquote>
 (<pre>"
(interval= (interval-dilate (interval '#(0 0) '#(100 100)) '#(1 1) '#(1 1))
	   (interval '#(1 1) '#(101 101))) => #t
(interval= (interval-dilate (interval '#(0 0) '#(100 100)) '#(-1 -1) '#(1 1))
	   (interval '#(-1 -1) '#(101 101))) => #t
(interval= (interval-dilate (interval '#(0 0) '#(100 100))  '#(0 0) '#(-50 -50))
	   (interval '#(0 0) '#(50 50))) => #t
(interval-dilate (interval '#(0 0) '#(100 100)) '#(0 0) '#(-500 -50)) => error
"))


(<h2> "Arrays")

(<h3> "Procedures")

(format-lambda-list '(array domain getter))
(<p> "If "(<code>(<var> 'domain))" is an interval and "(<code>(<var> 'getter))" is a function from
"(<code>(<var> 'domain))" to Scheme objects, then "(<code> 'array)" returns an array with domain "(<code>(<var> 'domain))"
and getter "(<code>(<var> 'getter))". It is an error to call "(<code> 'array)" if "(<code>(<var> 'domain))" and "(<code>(<var> 'getter))"
do not satisfy these conditions.")
(<p> "Example: ")
(<pre>"
(define a (array (interval '#(1 1) '#(11 11))
		 (lambda (i j)
		   (if (= i j)
		       1
		       0))))")
(<p> "defines an array for which "(<code> "(array-getter a)")" returns 1 when i=j and 0 otherwise.")

(format-lambda-list '(array? obj))
(<p> "Returns "(<code> "#t")" if and only if "(<code>(<var> 'obj))" is an array.")

(format-lambda-list '(array-domain array))
(format-lambda-list '(array-getter array))
(<p> "If "(<code>(<var> 'array))" is an array built by")
(<blockquote>
 (<code> "(array "(<var> 'domain)" "(<var> 'getter)")"))
(<p> "then "(<code> 'array-domain)" returns "(<code>(<var> 'domain))
" and "(<code> 'array-getter)" returns  "(<code>(<var> 'getter))".
It is an error to call "(<code> 'array-domain)" or "(<code> 'array-getter)" if "(<code>(<var> 'array))" is not an array.")
(<p> "Example: ")
     (<pre>"
(define a (array (interval '#(1 1) '#(11 11))
		 (lambda (i j)
		   (if (= i j)
		       1
		       0))))
((array-getter a) 3 3) => 1
((array-getter a) 2 3) => 0
((array-getter a) 11 0) => is an error, which may be signaled")


(format-lambda-list '(array-map f array #\. arrays))
(<p> "If "(<code>(<var> 'array))", "(<code>"(car "(<var> 'arrays)")")", ... all have the same domain and "(<code>(<var> 'f))" is a function, then "(<code> 'array-map)"
returns a new array with the same domain and getter")
(<pre>"
(lambda multi-index
  (apply f (map (lambda (g) (apply g multi-index)) (map array-getter (cons array arrays)))))")
(<p> "It is an error to call "(<code> 'array-map)" if its arguments do not satisfy these conditions.")

(format-lambda-list '(array-curry array outer-dimension))
(<p> "If "(<code>(<var> 'array))" is an array and "(<code>(<var> 'outer-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'outer-dimension)" < (interval-dimension (array-domain "(<var> 'array)"))"))
(<p> "then "(<code> 'array-curry)" returns")
(<pre>"
(call-with-values
    (lambda () (interval-curry (array-domain array) outer-dimension))
  (lambda (outer-interval inner-interval)
    (array outer-interval
		 (lambda outer-multi-index
		   (array inner-interval
				(lambda inner-multi-index
				  (apply (array-getter array) (append outer-multi-index inner-multi-index))))))))")
(<p> "It is an error to call "(<code> 'array-curry)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" This function curries "(<code>"(array-getter "(<var> 'array)")")" while keeping track of the
domains of the outer and inner lambdas.")
(<p> "It is expected that "(<code> 'array-curry)" will specialize the construction of")
(<pre>"
(lambda outer-multi-index
  (array inner-interval
	       (lambda inner-multi-index
		 (apply (array-getter array) (append outer-multi-index inner-multi-index)))))")
(<p>"and")
(<blockquote>
 (<code> "(lambda inner-multi-index (apply (array-getter array) (append outer-multi-index inner-multi-index)))"))

(<p>"Example:")
(<pre> "
(define a (array (interval '#(0 0) '#(10 10))
		       list))
((array-getter a) 3 4)  => (3 4)
(define curried-a (array-curry a 1))
((array-getter ((array-getter curried-a) 3)) 4) => (3 4)")

(format-lambda-list '(array-distinguish-one-axis array index))
(<p> "If "(<code>(<var> 'array))" is an array and
"(<code>"(interval-dimension (array-domain "(<var> 'interval)"))")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (interval-dimension (array-domain "(<var> 'interval)"))"))
(<p> "then "(<code> 'array-distinguish-one-axis)" returns")
(<pre>"
(call-with-values
    (lambda () (interval-distinguish-one-axis (array-domain array) index))
  (lambda (outer-interval inner-interval)
    (array outer-interval
		 (lambda outer-multi-index
		   (array inner-interval
				(lambda (m)
				  (apply (array-getter array) (insert-arg-into-arg-list m outer-index index))))))))")
(<p> "where we define ")
(<pre>"
(define (insert-arg-into-arg-list arg arg-list index)
  (define (helper arg-list i)
    (if (= i 0)
	(cons arg arg-list)
	(cons arg (helper (cdr arg-list) (- i 1)))))
  (helper arg-list index))")

(<p> "It is an error to call "(<code> 'array-distinguish-one-axis)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" It is expected that "(<code> 'array-distinguish-one-axis)" will specialize the construction of ")
(<pre>"
(lambda outer-multi-index
  (array inner-interval
	       (lambda (m)
		 (apply (array-getter array) (insert-arg-into-arg-list m outer-index index)))))")
(<p>"and")
(<blockquote>
 (<code> "(lambda (m) (apply (array-getter array) (insert-arg-into-arg-list m outer-index index)))"))

(format-lambda-list '(array-for-each f array #\. arrays))
(format-lambda-list '(array-for-each-serial f array #\. arrays))
(<p> "If "(<code>(<var> 'array))", "(<code>"(car "(<var> 'arrays)")")", ... all have the same domain "(<code>(<var> 'domain))" and "(<code>(<var> 'f))" is an appropriate function, then "(<code> 'array-for-each)"
calls")
(<pre>"
(interval-for-each  (lambda multi-index
		      (apply f (map (lambda (g) (apply g multi-index)) (map array-getter (cons array arrays)))))
		    (array-domain array))")
(<p> "and "(<code> 'array-for-each-serial)"
calls")
(<pre>"
(interval-for-each-serial  (lambda multi-index
			     (apply f (map (lambda (g) (apply g multi-index)) (map array-getter (cons array arrays)))))
			   (array-domain array))")

(<p> "It is expected that "(<code> 'array-map)", "(<code> 'array-for-each)" and "(<code> 'array-for-each-serial)" will specialize the construction of")
(<pre>"
(lambda multi-index
  (apply f (map (lambda (g) (apply g multi-index)) (map array-getter (cons array arrays)))))")
(<p> "It is an error to call "(<code> 'array-for-each)" if its arguments do not satisfy these conditions.")

(format-lambda-list '(array-reduce operator identity array))
(format-lambda-list '(array-reduce-serial operator identity array))
(<p> "If "(<code>(<var> 'array))"  is an array then "(<code> 'array-reduce)" returns "
     (<code>"(interval-reduce (array-getter "(<var>"array")") "(<var>"operator identity")" (array-domain "(<var> 'array)"))")" and "(<code> 'array-reduce-serial)" returns "
     (<code>"(interval-reduce-serial (array-getter "(<var>"array")") "(<var>"operator identity")" (array-domain "(<var> 'array)"))")".")

(format-lambda-list '(array-extract array new-domain))
(<p> "If "(<code>(<var> 'array))" is an array and "(<code>(<var> 'new-domain))" is an interval that is a sub-interval of "(<code> "(array-domain "(<var> 'array)")")", then "(<code> 'array-extract)" returns a new array")
(<blockquote>
 (<code> "(array "(<var> 'new-domain)" (array-getter "(<var>'array)"))"))
							 
(<h2> "Mutable Arrays")
(<h3> "Procedures")
(format-lambda-list '(mutable-array domain getter setter))
(<p> "Assume that "(<code>(<var> 'domain))" is an interval of dimension "(<code>(<var> 'n))" and that "(<code>(<var> 'getter))" and
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
(<p> "Then "(<code> 'mutable-array)" builds a mutable-array with domain "(<code>(<var> 'domain))", getter "(<code>(<var> 'getter))" and
setter "(<code>(<var> 'setter))".  A mutable-array is an array.  It is an error to call "(<code> 'mutable-array)" if its arguments do not satisfy these conditions.")

(format-lambda-list '(mutable-array? obj))
(<p> "Returns "(<code>"#t")" if and only if "(<code>(<var> 'obj))" is a mutable-array.  Note: mutable-arrays are arrays.")

(format-lambda-list '(array-setter array))
(<p> "If "(<code>(<var> 'array))" is a mutable-array built by")
(<blockquote>
 (<code> "(mutable-array "(<var> 'domain)" "(<var> 'getter)" "(<var> 'setter)")"))
(<p> "then "(<code> 'array-setter)" returns "(<code>(<var> 'setter))". It is an error to call "(<code> 'array-setter)"
if "(<code>(<var> 'array))" is not a mutable-array.")

(<p> "Example: ")
(<pre> "
(define sparse-array
  (let ((domain (interval '#(0 0) '#(1000000 1000000)))
	(sparse-rows (make-vector 1000000 '())))
    (mutable-array domain
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
((array-getter sparse-array) 12345 6789)  => 0.
((array-getter sparse-array) 0 0) => 0.
((array-setter sparse-array) 1.0 0 0) => undefined
((array-getter sparse-array) 12345 6789)  => 0.
((array-getter sparse-array) 0 0) => 1.")
		  
(format-lambda-list '(mutable-array-curry array outer-dimension))
(<p> "If "(<code>(<var> 'array))" is a mutable-array and "(<code>(<var> 'outer-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'outer-dimension)" < (interval-dimension (array-domain "(<var> 'array)"))"))
(<p> "then "(<code> 'mutable-array-curry)" returns")
(<pre>"
(call-with-values
    (lambda () (interval-curry (array-domain array) outer-dimension))
  (lambda (outer-interval inner-interval)
    (array outer-interval
	   (lambda outer-multi-index
	     (mutable-array inner-interval
			    (lambda inner-multi-index
			      (apply (array-getter array) (append outer-multi-index inner-multi-index)))
			    (lambda (v . inner-multi-index)
			      (apply (array-setter array) v (append outer-multi-index inner-multi-index))))))))")
(<p> "It is an error to call "(<code> 'mutable-array-curry)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" This function curries "(<code>"(array-getter "(<var> 'array)")")"
and "(<code>"(array-setter "(<var> 'array)")")" while keeping track of the
domains of the outer and inner lambdas.")
(<p> "It is expected that "(<code> 'mutable-array-curry)" will specialize the construction of")
(<pre>"
(lambda outer-multi-index
  (mutable-array inner-interval
		       (lambda inner-multi-index
			 (apply (array-getter array) (append outer-multi-index inner-multi-index)))
		       (lambda (v . inner-multi-index)
			 (apply (array-setter array) v (append outer-multi-index inner-multi-index)))))")
(<p> "and")
(<blockquote>
 (<code> "(lambda inner-multi-index (apply (array-getter array) (append outer-multi-index inner-multi-index)))"))
(<p>"and")
(<blockquote>
 (<code> "(lambda (v . inner-multi-index) (apply (array-setter array) v (append outer-multi-index inner-multi-index)))"))

(format-lambda-list '(mutable-array-distinguish-one-axis array index))
(<p> "If "(<code>(<var> 'array))" is a mutable-array and
"(<code>"(interval-dimension (array-domain "(<var> 'interval)"))")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (interval-dimension (array-domain "(<var> 'interval)"))"))
(<p> "then "(<code> 'mutable-array-distinguish-one-axis)" returns")
(<pre>"
(call-with-values
    (lambda () (interval-distinguish-one-axis (array-domain array) index))
  (lambda (outer-interval inner-interval)
    (array outer-interval
		 (lambda outer-multi-index
		   (mutable-array inner-interval
					(lambda (m)
					  (apply (array-getter array) (insert-arg-into-arg-list m outer-index index)))
					(lambda (v m)
					  (apply (array-setter array) v (insert-arg-into-arg-list m outer-index index))))))))")
(<p> "where we define ")
(<pre>"
(define (insert-arg-into-arg-list arg arg-list index)
  (define (helper arg-list i)
    (if (= i 0)
	(cons arg arg-list)
	(cons arg (helper (cdr arg-list) (- i 1)))))
  (helper arg-list index))")

(<p> "It is an error to call "(<code> 'mutable-array-distinguish-one-axis)" if its arguments do not satisfy these conditions.")
(<p> (<b> "Notes:")" It is expected that "(<code> 'mutable-array-distinguish-one-axis)" will specialize the construction of ")
(<pre>"
(lambda outer-multi-index
  (mutable-array inner-interval
		       (lambda (m)
			 (apply (array-getter array) (insert-arg-into-arg-list m outer-index index)))
		       (lambda (v m)
			 (apply (array-setter array) v (insert-arg-into-arg-list m outer-index index)))))")
(<p>"and")
(<blockquote>
 (<code> "(lambda (m) (apply (array-getter array) (insert-arg-into-arg-list m outer-index index)))"))
(<p> "and")
(<blockquote>
 (<code> "(lambda (v m) (apply (array-setter array) v (insert-arg-into-arg-list m outer-index index)))"))

(<h2> "Storage classes")
(<p> "Conceptually, a storage-class is a set of functions to manage the backing store of a fixed-array.
The functions allow one to make a backing store, to get values from the store and to set new values, to return the length of the store, and to specify a default value for initial elements of the backing store.  Typically, a backing store is a (heterogeneous or homogenous) vector.")
(<h3> "Procedures")

(format-lambda-list '(make-storage-class getter setter checker maker length default))
(<p> "Here we assume the following relationships between the arguments of "(<code> 'make-storage-class)".  Assume that the \"elements\" of
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
(<p> "If the arguments do not satisfy these conditions, then it is an error to call "(<code> 'make-storage-class))
(<p> "Note that we assume that "(<code>(<var> 'getter))" and "(<code>(<var> 'setter))" generally take "(<i> 'O)"(1) time to execute.")

(format-lambda-list '(storage-class-getter m))
(format-lambda-list '(storage-class-setter m))
(format-lambda-list '(storage-class-checker m))
(format-lambda-list '(storage-class-maker m))
(format-lambda-list '(storage-class-length m))
(format-lambda-list '(storage-class-default m))
(<p> "If "(<code>(<var> 'm))" is an object created by")
(<blockquote>
 (<code>"(make-storage-class "(<var> "setter getter checker maker length default")")"))
(<p> " then "
(<code> 'storage-class-getter)" returns "(<code>(<var> 'getter))", "
(<code> 'storage-class-setter)" returns "(<code>(<var> 'setter))", "
(<code> 'storage-class-checker)" returns "(<code>(<var> 'checker))", "
(<code> 'storage-class-maker)" returns "(<code>(<var> 'maker))", and "
(<code> 'storage-class-default)" returns "(<code>(<var> 'default))".  Otherwise, it is an error to call any of these routines.")

(<h3> "Global Variables")
(format-global-variable 'generic-storage-class)
(format-global-variable 's8-storage-class)
(format-global-variable 's16-storage-class)
(format-global-variable 's32-storage-class)
(format-global-variable 's64-storage-class)
(format-global-variable 'u1-storage-class)
(format-global-variable 'u8-storage-class)
(format-global-variable 'u16-storage-class)
(format-global-variable 'u32-storage-class)
(format-global-variable 'u64-storage-class)
(format-global-variable 'f32-storage-class)
(format-global-variable 'f64-storage-class)
(format-global-variable 'c64-storage-class)
(format-global-variable 'c128-storage-class)

(<p> (<code> 'generic-storage-class)" is defined by")
(<blockquote>
 (<code> "(define generic-storage-class (make-storage-class vector-ref vector-set! (lambda (arg) #t) make-vector vector-length #f))"))
"Furthermore, "(<code> "s"(<var> 'X)"-storage-class")" is defined for "(<code>(<var> 'X))"=8, 16, 32, and 64 (which have default values 0 and
manipulate exact integer values between -2"(<sup>(<var> 'X)"-1")" and
2"(<sup> (<var> 'X)"-1")"-1 inclusive),
 "(<code> "u"(<var> 'X)"-storage-class")" is defined for "(<code>(<var> 'X))"=1, 8, 16, 32, and 64 (which have default values 0 and manipulate exact integer values between 0 and
2"(<sup> (<var> 'X))"-1 inclusive),
"(<code> "f"(<var> 'X)"-storage-class")" is defined for "(<code>(<var> 'X))"= 32 and 64 (which have default value 0.0 and manipulate 32- and 64-bit floating-point numbers), and
"(<code> "c"(<var> 'X)"-storage-class")" is defined for "(<code>(<var> 'X))"= 64 and 128 (which have default value 0.0+0.0i and manipulate complex numbers with, respectively,32- and 64-bit floating-point numbers as real and imaginary parts).  Each of these
could be defined simply as generic-storage-class, but it is assumed that implementations with homogeneous arrays will give definitions
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
(format-global-variable 'fixed-array-default-safe?)
(<p> "Determines whether the setters and getters of fixed-arrays check their arguments for correctness by default.  Initially it has the value "(<code> "#f")".")
(<h3> "Procedures")
(format-lambda-list '(fixed-array "domain:" domain "storage-class:" storage-class #\[ "body:" body #\] #\[ "indexer:" indexer #\] #\[ "initializer-value:" initializer-value #\] #\[ "safe?:" safe? #\]))
(<p> "Builds a fixed-array.  "(<code>(<var> 'domain))" must be an interval; "(<code>(<var> 'storage-class))" must
 be a storage-class;  if "(<code>(<var> 'body))" is given, it must be of the same type as that returned by
"(<code>"(storage-class-maker "(<var> 'storage-class)")")"; if "(<code>(<var> 'initializer-value))" is given, it must be storable
in "(<code>(<var> 'body))"; at most one of "(<code>(<var> 'initializer-value))" and "(<code>(<var> 'body))" can be given; if "(<code>(<var> 'indexer))" is given, it must be a one-to-one affine mapping from "(<code>(<var> 'domain))" to
[0,"(<code>"((storage-class-length "(<var> 'storage-class)") "(<var> 'body)")")"); the variable "(<code>(<var> 'safe?))" determines whether the multi-index arguments to the getter and setter are checked to be in the domain, and whether the value of the setter is storable in the body;
the getter and setter of the result are defined by")
(<pre>"
(lambda (i_0 ... i_n-1)
  ((storage-class-getter storage-class)
   body
   (indexer i_0 ... i_n-1)))")
(<p> "and")
(<pre>"
(lambda (v i_0 ... i_n-1)
  ((storage-class-setter storage-class)
   body
   (indexer i_0 ... i_n-1)
   v))")
(<p> "The default values for arguments that are omitted are as follows:")

(<p> "Initializer-value: "(<code>"(storage-class-default storage-class)"))

(<p> "Body:")
(<pre>"
((storage-class-maker storage-class)
 (interval-volume domain)
 initializer-value)")

(<p> "Indexer: The one-to-one mapping of elements of "(<code>(<var> 'domain))" to [0,"(<code>"(interval-volume "(<var> 'domain)")")") in
lexicographical order.")

(<p> "Safe?: The current value of the global variable "(<code> 'fixed-array-default-safe?)".")

(format-lambda-list '(fixed-array? obj))
(<p> "Returns "(<code>"#t")" if "(<code>(<var> 'obj))" is a fixed-array, and "(<code>"#f")" otherwise. A fixed-array is a mutable-array, and hence an array.")

(format-lambda-list '(array-body array))
(format-lambda-list '(array-indexer array))
(format-lambda-list '(array-storage-class array))
(format-lambda-list '(array-safe? array))
(<p> (<code>'array-body)" returns the body of "(<code>(<var> 'array))", "
     (<code>'array-indexer)" returns the indexer of "(<code>(<var> 'array))", "
     (<code>'array-storage-class)" returns the storage-class of "(<code>(<var> 'array))", and "
     (<code>'array-safe?)" is true if and only if the arguments of "(<code> "(array-getter "(<var> 'array)")")" and "(<code> "(array-setter "(<var> 'array)")")" are checked for correctness. It is an error to call any of these routines if "(<code>(<var> 'array))" is not a fixed-array.")

(format-lambda-list '(fixed-array-share! array new-domain new-domain->old-domain))
(<p> "Constructs a new fixed-array that shares the body of the fixed-array "(<code>(<var> 'array))".
Returns an object that is behaviorally equivalent to")
(<pre>"
(fixed-array domain:        new-domain
	     storage-class: (array-storage-class array)
	     body:          (array-body array)
	     indexer:       (lambda multi-index
			      (call-with-values
				  (lambda ()
				    (apply new-domain->old-domain multi-index))
			        (fixed-array-indexer array))))")
(<p> (<code>(<var> 'new-domain->old-domain))" must be an affine one-to-one mapping from "(<code>"(array-domain "(<var> 'array)")")" to
"(<code>(<var> 'new-domain))".")

(<p> "Note: It is assumed that affine structure of the composition of "(<code>(<var> 'new-domain->old-domain))" and "(<code>"(fixed-array-indexer "(<var> 'array))" will be used to simplify:")
(<pre>"
(lambda multi-index
  (call-with-values
      (lambda ()
	(apply new-domain->old-domain multi-index))
    (fixed-array-indexer array)))"
)

(format-lambda-list '(fixed-array-curry array outer-dimension))
(<p> "It is an error to call "(<code> 'fixed-array-curry)" unless
 "(<code>(<var> 'array))" is a fixed-array
and "(<code>(<var> 'outer-dimension))" is an exact integer that satisfies")
(<blockquote>
 (<code> "0 < "(<var> 'outer-dimension)" < (interval-dimension (fixed-array-domain "(<var> 'array)"))."))
(<p> (<code> 'fixed-array-curry)" returns")
(<pre>"
(call-with-values
    (lambda () (interval-curry (fixed-array-domain array) outer-dimension))
  (lambda (outer-interval inner-interval)
    (array outer-interval
	   (lambda outer-multi-index
	     (fixed-array-share! array
				 outer-interval
				 (lambda inner-multi-index
				   (apply values (append outer-multi-index inner-multi-index))))))))")

(<p> (<b> "Notes:")" This function curries "(<code>"(array-getter "(<var> 'array)")")"
and "(<code>"(array-setter "(<var> 'array)")")" while keeping track of the
domains of the outer and inner lambdas.")
(<p> "It is expected that "(<code> 'fixed-array-curry)" will specialize the construction of")
(<blockquote>
 (<code> "(lambda outer-multi-index
		   (fixed-array-share! array
				       outer-interval
				       (lambda inner-multi-index
					 (apply values (append outer-multi-index inner-multi-index)))))"))

(format-lambda-list '(fixed-array-distinguish-one-axis array index))
(<p> "It is an error to call "(<code> 'fixed-array-distinguish-one-axis)" unless
 "(<code>(<var> 'array))" is a fixed-array and
"(<code>"(interval-dimension (fixed-array-domain "(<var> 'interval)"))")" is greater than one, and")
(<blockquote>
 (<code> "0 <= "(<var> 'index)" < (interval-dimension (fixed-array-domain "(<var> 'array)"))"))
(<p> (<code> 'fixed-array-distinguish-one-axis)" returns")
(<pre>"
(call-with-values
    (lambda () (interval-distinguish-one-axis (fixed-array-domain array) index))
  (lambda (outer-interval inner-interval)
    (array outer-interval
		 (lambda outer-multi-index
		   (fixed-array-share! array
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



(<p> "It is expected that "(<code> 'fixed-array-distinguish-one-axis)" will specialize the construction of ")
(<pre>"
(lambda outer-multi-index
  (fixed-array-share! array
		      inner-interval
		      (lambda (m) (apply values (insert-arg-into-arg-list m outer-index index)))))")



(format-lambda-list '(array->fixed-array array #\[ result-storage-class "generic-storage-class" #\] #\[ safe? "#f" #\]))
(format-lambda-list '(array->fixed-array-serial array #\[ result-storage-class "generic-storage-class" #\[ safe? "#f"#\]))
(<p> "If "(<code>(<var> 'array))" is an array whose elements can be manipulated by the storage-class
"(<code>(<var> 'result-storage-class))", then the fixed-array returned by "(<code> 'array->fixed-array)" can be defined by:")
(<pre>"
(let ((result (fixed-array domain:        (array-domain array)
			   storage-class: result-storage-class
			   safe?:         safe?)))
  (interval-for-each (lambda multi-index
		       (apply (array-setter result) (apply (array-getter array) multi-index) multi-index))
		     (array-domain array))
  result)")
(<p> "Otherwise it is an error to call "(<code> 'array->fixed-array)".")
(<p> (<code> 'array->fixed-array)"  does not specify
the order in which "(<code>"(array-getter "(<var> 'array)")")" is applied to the multi-indices in "(<code>"(array-domain "(<var> 'array)")")".")
(<p> "Similarly, the fixed-array returned by "(<code> 'array->fixed-array-serial)" can be defined by:")
(<pre>"
(let ((result (fixed-array domain:        (array-domain array)
			   storage-class: result-storage-class
			   safe?:         safe?)))
  (interval-for-each-serial (lambda multi-index
			      (apply (array-setter result) (apply (array-getter array) multi-index) multi-index))
			    (array-domain array))
  result)")
(<p> "Thus, "(<code> 'array->fixed-array-serial)" evaluates "(<code>"(array-getter "(<var> 'array)")")" to the multi-indices in
"(<code>"(array-domain "(<var> 'array)")")" in lexicographical order.")


(<h1> "Implementation")
(<p> "We provide an implementation in Gambit-C; the nonstandard techniques used
in the implementation are: DSSSL-style optional and keyword arguments; a
unique object to indicate absent arguments; "(<code>"define-structure")";
and "(<code>"define-macro")".")

(<h1> "Relationship to other SRFIs")
(<p> "Final SRFIs "(<a> href: "#SRFI-25" "25")", "(<a> href: "#SRFI-47" "47")", "(<a> href: "#SRFI-58" "58")", and "(<a> href: "#SRFI-63" "63")" deal with \"Multi-dimensional Array Primitives\", \"Array\", \"Array Notation\",
and \"Homogeneous and Heterogeneous Arrays\", respectively.  Each of these previous SRFIs deal with what we call in this SRFI
fixed-arrays.  Many of the functions in these previous SRFIs  have corresponding forms in this SRFI.  For example, from SRFI 63, we can
translate: ")
(<dl>
 (<dt> (<code> "(array? obj)"))
 (<dd> (<code> "(array? obj)"))
 (<dt> (<code> "(Array-rank a)"))
 (<dd> (<code> "(interval-dimension (array-domain obj))"))
 (<dt> (<code> "(make-array prototype k1 ...)"))
 (<dd> (<code> "(fixed-array domain: (interval (vector 0 ...) (vector k1 ...)) storage-class: storage-class)")".")
 (<dt> (<code> "(make-shared-array array mapper k1 ...)"))
 (<dd> (<code> "(fixed-array-share! array (interval (vector 0 ...) (vector k1 ...)) mapper)"))
 (<dt> (<code> "(array-in-bounds? array index1 ...)"))
 (<dd> (<code> "(interval-contains-multi-index? (array-domain array) index1 ...)"))
 (<dt> (<code> "(array-ref array k1 ...)"))
 (<dd> (<code> "((array-getter array) k1 ...)"))
 (<dt> (<code> "(array-set! array obj k1 ...)"))
 (<dd> (<code> "((array-setter array) obj k1 ...)"))
 )
(<p> "At the same time, this SRFI has some special features:")
(<ul>
 (<li> "Intervals, used as the domains of arrays in this SRFI, are useful
objects in their own rights, with their own procedures.  We make a sharp distinction between the domains
of arrays and the arrays themselves.")
 (<li> "Intervals can have nonzero lower bounds in each dimension.")
 (<li> "Intervals cannot be empty.")
 (<li> "Arrays must have a getter, but may have no setter.  For example, on a system with eight-bit chars, one
can write a function to read greyscale images in the PGM format of the netpbm package as follows.  The  lexicographical
order in array->fixed-array-serial guarantees the the correct order of execution of the input procedures:"
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
		  (array->fixed-array-serial
		   (array
		    (interval '#(0 0)
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
