
(define-library (finger-tree)
  (import
   (generators)
   (scheme base)
   (scheme case-lambda)
   (scheme cxr)
   (scheme lazy)
   (srfi 1))
  (export
   make-finger-tree
   finger-tree
   finger-tree?
   finger-tree-empty?
   finger-tree-force
   finger-tree-append
   finger-tree-filter
   finger-tree-fold-left
   finger-tree-fold-right
   finger-tree-for-each
   finger-tree-length
   finger-tree-map
   finger-tree-reverse
   finger-tree-left
   finger-tree-right
   finger-tree-add-left
   finger-tree-add-right
   finger-tree-remove-left
   finger-tree-remove-right
   finger-tree-scan
   finger-tree-split
   generator->finger-tree
   list->finger-tree
   finger-tree->generator
   finger-tree->reverse-generator
   finger-tree->list)
  (include "finger-tree.scm"))
