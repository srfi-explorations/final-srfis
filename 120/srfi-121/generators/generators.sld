(define-library (generators)
  (import (scheme base))
  (import (scheme case-lambda))
  (export generator make-iota-generator make-range-generator 
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gcombine gfilter gremove 
          gtake gdrop gtake-while gdrop-while
          gdelete gdelete-neighbor-dups gindex gselect)
  (export generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold)
  (include "generators-impl.scm")
)
