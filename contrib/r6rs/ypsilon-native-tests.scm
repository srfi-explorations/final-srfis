;;;; Basic tests of (r6rs bytevectors).

(import (rnrs base)
        (rename (rnrs bytevectors)
          (utf16->string ypsilon:utf16->string)
          (utf32->string ypsilon:utf32->string)
          (bytevector-copy! r6rs:bytevector-copy!))
        (rnrs io simple)
        (rnrs control)
        (include))

;; Bugs in Ypsilon: utf{16,32}->string requires 2 arguments instead of 1 or 2
 
(define utf16->string
  (case-lambda
    ((string) (ypsilon:utf16->string string (endianness big)))
    ((string endianness) (ypsilon:utf16->string string endianness))))
 
(define utf32->string
  (case-lambda
    ((string) (ypsilon:utf32->string string (endianness big)))
    ((string endianness) (ypsilon:utf32->string string endianness))))
 
(include "r6rs/shared-tests.scm")
