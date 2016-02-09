
\title{finger-tree}
\author{Kevin Wortman}

\section{Introduction}

This library implements an abstract \emph{2-3 finger tree,} as described in \hyperlink[http://www.staff.city.ac.uk/~ross/papers/FingerTree.html]{Finger Trees: A Simple General-purpose Data Structure} by Ralf Hinze and Ross Paterson.

A finger tree is a \emph{pure} data structure (a.k.a. \emph{immutable} or \emph{persistent}), meaning that operations which take a tree as input leave that tree unchanged and still valid. So a mutation operation, such as adding an element, works by creating and returning a new tree object which shares almost all of its structure with the input tree.

A finger tree stores a collection of \emph{elements} in \emph{left-to-right order}. As discussed below, the left-to-right ordering may be oblivious to element contents (i.e. unsorted), in which case the tree is analogous to a deque where left and right correspond to front and back, respectively. Or, the left-to-right ordering may correspond to increasing sorted order according to some kind of key, in which case the tree is analogous to a binary search tree.

Finger trees are remarkably versatile. Finding, adding, or removing the left or right element takes constant amortized time. Finding, adding, removing, or splitting at an arbitrary ordered element takes logarithmic amortized time. Therefore a finger tree can implement a deque competitive with doubly linked list or a self-resizing vector; a set, bag, or map competitive with a self-balancing binary search tree (e.g. AVL or red-black); or a priority queue competitive with a binary heap.

Finger trees are inherently lazy, meaning that many parts of a tree are promises which are only forced when absolutely necessary. Consequently, the worst-case time efficiency of individual procedure calls is difficult to characterize precisely. We say that a procedure takes \emph{amortized O(X) time} when it contributes at most \emph{O(X)} time to any sequence of finger tree operations. In particular, some of that \emph{O(X)} cost may be incurred immediately, while some of the cost may be deferred to some future procedure call that forces a promise. We say that an operation takes \emph{strictly O(Y) time} when it takes at most \emph{O(Y)} time immediately, with no time cost deferred to future operations.

\section{Measurements and monotone predicates}

Finger tree operations expect that tree elements are \emph{measurable.} Procedures that depend upon measurements take arguments with the following names and required semantics:

\itemlist{
  \item{\var{measure} is a procedure that maps an element object to a measurement of that object.}
  \item{\var{add} is a binary measurement-addition procedure. So for any measurements \var{left} and \var{right}, \scheme{(add left right)} returns the measurement aggregating \var{left} and \var{right}. It is guaranteed that, as implied by the argument names, that all elements contributing to \var{left} are positioned to the left of those elements contributing to \var{right}. This operation must be associative, so e.g. if \scheme{m1}, \scheme{m2}, and \scheme{m3} are measurements, then \scheme{(add m1 (add m2 m3))} must be equivalent to \scheme{(add (add m1 m2) m3)}.}
  \item{\var{zero} is the measurement object corresponding to nothing (no elements at all).}
  \item{\var{pred} is a predicate procedure that takes a measurement and returns an object interpreted as a boolean (so \scheme{#false} or non-\scheme{#false}). The predicate must by \emph{monotone,} meaning that it returns \scheme{#false} for sufficiently small measurements, returns non-\scheme{#false} for sufficiently large measurements, and changes from \scheme{#false} to non-\scheme{#false} exactly once.}
}

This exceedingly abstract measurement scheme helps finger trees achieve their versatility.
\itemlist{
  \item{To obtain an indexed sequence, have \var{measure} return a constant 1, \var{add} be integer addition (e.g. \scheme{+}), and \var{zero} be \scheme{0}.}
  \item{To obtain a set in increasing order, have \var{measure} be the identity function, \var{add} return its \var{right} argument, and \var{zero} be a sentinel value analogous to "negative infinity." Note that you could also have \var{add} return the maximum of its two arguments, but that would actually be overkill since sorted order implies that \var{right} is always greater than \var{left}.}
  \item{To obtain a map in increasing key order, have \var{measure} be a key getter function, and define \var{add} and \var{zero} the same was as in a set.}
  \item{A priority queue is the same as a map, except that "keys" are "priorities." Observe that sorted order guarantees that the minimum-priority element is always at the left and the maximum-priority element is always at the right.}
}

Note that the Hinze and Paterson describe the space of measurements in terms of a \emph{monoid.} We avoid that terminology since it is not universally understood in the Scheme community.

\section{Conventions and preconditions}

The following naming conventions and preconditions apply to all procedures in this library.

It is an error to mutate the internal structure of a finger tree, since finger trees are derived from other trees, so mutating a tree may have unanticipated "ripple effects" on derived trees. This library provides no way of mutating internal structure, so that could only be done with some kind of introspection library.

Mutating an object in a way that changes its measurement invalidates any finger tree containing that object.

Returned finger trees are not necessarily freshly allocated. Indeed, they may be an input finger tree, or part of an input tree.

\var{add}, \var{measure}, \var{pred}, and \var{zero} must represent measurement operations, as described above, which are mutually compatible and also compatible with the finger tree in question.

\var{element} is an object which is, or will be, an element of a tree.

\var{gen} must be a SRFI 121 generator.

\var{k} must be a non-negative integer.

\var{lst} must be a list of elements.

\var{obj} may be any Scheme object.

\var{tree} must be a finger tree.

\section{Whole-tree operations}

\procedure{(make-finger-tree)}
Create and return an empty finger tree. Strictly \emph{O(1)} time.

\procedure{(finger-tree add measure [element ...])}
Create and return a finger tree containing \var{element...}, added in left-to-right (forward) order. Strictly \emph{O(n)} time, where \emph{n} is the number of elements.

\procedure{(finger-tree? obj)}
Returns \scheme{#true} if \var{obj} is a finger tree, or \scheme{#false} otherwise. Strictly \emph{O(1)} time.

\procedure{(finger-tree-empty? tree)}
Returns \scheme{#true} if \var{tree} is empty, or \scheme{#false} if \var{tree} is non-empty. Strictly \emph{O(1)} time.

\procedure{(finger-tree-force tree)}
Force every part of \var{tree}, then return \var{tree}. Ordinarily there is no need to use this procedure, as finger trees automatically force themselves on an efficient as-needed basis. This procedure exists for special cases where it is beneficial to front-load tree operations, for example if a server builds a tree as a preprocessing step before handling requests. Strictly \emph{O(log n)} time.

\section{Sequence operations}

\procedure{(finger-tree-append add measure tree1 tree2 ...)}
Return a finger tree containing the contents of \var{tree1}, \var{tree2}, and so on, appended together. Amortized \emph{O(k log n)} time, where \emph{k} is the number of trees and \emph{n} is the greatest number of elements in any tree.

\procedure{(finger-tree-filter add measure proc tree)}
Return a finger tree containing all the elements of \var{tree} that satisfy predicate \var{proc}. Note that \var{proc} is called on each element, not on each measurement. Strictly \emph{O(n)} time.

\procedure{(finger-tree-fold-left proc seed tree ...)}
The semantics of this procedure are identical to SRFI 1 \scheme{fold}, except of course that the containers are finger trees instead of lists. Strictly \emph{O(n)} time.

\procedure{(finger-tree-fold-right proc seed tree ...)}
The semantics of this procedure are identical to SRFI 1 \scheme{fold-right}, except of course that the containers are finger trees instead of lists. Strictly \emph{O(n)} time.

\procedure{(finger-tree-for-each proc tree ...)}
The semantics of this procedure are identical to SRFI 1 \scheme{for-each}, except of course that the containers are finger trees instead of lists. Strictly \emph{O(n)} time.

\procedure{(finger-tree-length tree)}
Return the number of elements in \var{tree}. Strictly \emph{O(n)} time.

\procedure{(finger-tree-map proc tree ...)}
The semantics of this procedure are identical to SRFI 1 \scheme{map}, except of course that the containers are finger trees instead of lists. The order of elements is preserved, so this procedure is not suitable for measurement-ordered applications such as sorted sets, since in general, mapping elements permutes their order. Strictly \emph{O(n)} time.

\procedure{(finger-tree-reverse add measure tree)}
Return a finger tree containing \var{tree}'s elements in reverse order. Strictly \emph{O(n)} time.

\section{Deque operations}

\procedure{(finger-tree-left tree)}
Return the leftmost element of \var{tree}. It is an error to call this procedure on an empty \var{tree}. Strictly \emph{O(1)} time.

\procedure{(finger-tree-right tree)}
Return the rightmost element of \var{tree}. It is an error to call this procedure on an empty \var{tree}. Strictly \emph{O(1)} time.

\procedure{(finger-tree-add-left add measure tree element)}
Add \var{element} as the new leftmost element of \var{tree}, and return the resulting tree. Amortized \emph{O(1)} time.

\procedure{(finger-tree-add-right add measure tree element)}
Add \var{element} as the new rightmost element of \var{tree}, and return the resulting tree. Amortized \emph{O(1)} time.

\procedure{(finger-tree-remove-left tree)}
Remove the leftmost element of \var{tree}, and return the resulting tree. It is an error to call this procedure on an empty \var{tree}. Amortized \emph{O(1)} time.

\procedure{(finger-tree-remove-right tree)}
Remove the rightmost element of \var{tree}, and return the resulting tree. It is an error to call this procedure on an empty \var{tree}. Amortized \emph{O(1)} time.

\section{Scan and split}

\procedure{(finger-tree-scan add measure pred zero tree match absent)}

Search \var{tree} for the first element whose measurement satisfies \var{pred}. When such a matching element exists, tail-call \scheme{(match element)}; otherwise tail-call \scheme{(absent)}.

\var{pred} must be a monotone predicate, as discussed above.

This procedure is analogous to the search or lookup operation in a search tree. It probes \var{tree} without altering it or creating new objects, so has better constant factors than \scheme{finger-tree-split}.

Strictly \emph{O(log n)} time, assuming that each call to \var{pred} takes \emph{O(1)} time.

\procedure{(finger-tree-split add measure pred zero tree match absent)}

Split \var{tree} around the first element whose measurement satisfies \var{pred}. When such an element exists, tail-call \scheme{(match prefix element suffix)}, where \var{prefix} is a finger tree containing all elements left of \var{element}, \var{element} is the match, and \var{suffix} is a finger tree containing all elements right of \var{element}. Note that \var{prefix} and/or \var{suffix} may be empty. When no match exists, tail-call \scheme{(absent)}.

\var{pred} must be a monotone predicate, as discussed above.

This procedure is strictly more powerful than \scheme{finger-tree-scan}, but since it constructs two new trees, its constant factors are worse. That said, \scheme{finger-tree-split} can be used to implement several essential higher-level operations.

\itemlist{
\item{To insert an element in an ordered tree, split at the location where the new element should be, then reassemble \var{prefix}, \var{element}, the new element, and \var{suffix} with \scheme{finger-tree-append}.}
\item{To delete an element from an ordered tree, split at the element, then reassemble \var{prefix} and \var{suffix} with \scheme{finger-tree-append}, deliberately omitting the deleted element.}
\item{For a predecessor query, split at the element and return the prefix' rightmost element. A successor query is symmetric: return the suffix' leftmost element.}
\item{Partitioning the tree is simply a matter of returning \var{prefix} and \var{suffix}, possibly adding \var{element} to one of the trees depending on how you want the boundary condition to work.}
\item{A range query can be implemented as two successive partition operations.}
}

Strictly \emph{O(log n)} time, assuming that each call to \var{pred} takes \emph{O(1)} time.

\section{Conversion}

\procedure{(generator->finger-tree add measure gen [k])}
Return a finger tree containing the elements of \var{gen}. When \var{k} is given, the first \var{k} elements are taken; it is an error for \var{gen} to have fewer than \var{k} elements. Specifying \var{k} allows the procedure to avoid restructuring the tree as it goes, improving constant factors. When \var{k} is unspecified, the entire contents of \var{gen} is taken, and the tree must restructure itself as it goes, worsening constant factors. Strictly \emph{O(n)} time.

\procedure{(list->finger-tree add measure lst)}
Return a finger tree containing the elements of \var{lst}. This procedure avoids restructuring the tree as it goes, so has fast constant factors. Strictly \emph{O(n)} time.

\procedure{(finger-tree->generator tree)}
Return a generator that yields the elements of \var{tree} in left-to-right order. Strictly \emph{O(1)} time to create the generator, and of course the generator contains n elements.

\procedure{(finger-tree->reverse-generator tree)}
Return a generator that yields the elements of \var{tree} in right-to-left order. Strictly \emph{O(1)} time to create the generator, and of course the generator contains n elements.

\procedure{(finger-tree->list tree)}
Return a freshly-allocated list containing the elements of \var{tree} in left-to-right order. Strictly \emph{O(n)} time.
