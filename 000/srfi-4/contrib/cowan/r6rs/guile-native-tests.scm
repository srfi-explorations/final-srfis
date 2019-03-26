;;;; Basic tests of (rnrs bytevectors).

(import (rnrs base)
        (rename (rnrs bytevectors) (bytevector-copy! r6rs:bytevector-copy!))
        (rnrs io simple)
        (include))

;; For string tests
(when (defined? 'setlocale)
  (setlocale LC_ALL "C"))
 
(include "r6rs/shared-tests.scm")
