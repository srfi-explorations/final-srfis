;; Note: this library is the same as (rnrs bytevectors (6))
;; except that it exports r6rs:bytevector-copy!
;; instead of bytevector-copy! (which has different argument order
;; in R7RS).

(module (r6rs bytevectors) ()
  (import scheme)
  (import (only (chicken base) include error infinite? nan?))
  (import (only (chicken module) export))
  (import (chicken foreign))
  (import (only (chicken bitwise) bitwise-and bitwise-ior
                                  bitwise-not arithmetic-shift))
  (import utf8)
  (import (rename
            (only (srfi 4) make-u8vector u8vector? u8vector-length
                           u8vector-ref u8vector-set!)
            ; don't rename make-u8vector
            (u8vector? bytevector?)
            (u8vector-length bytevector-length)
            (u8vector-ref bytevector-u8-ref)
            (u8vector-set! bytevector-u8-set!)))

  (export
   endianness
   native-endianness

   bytevector? make-bytevector bytevector-length
   bytevector=?
   bytevector-fill! r6rs:bytevector-copy! bytevector-copy

   bytevector-u8-ref bytevector-s8-ref
   bytevector-u8-set! bytevector-s8-set!
   bytevector->u8-list u8-list->bytevector

   bytevector-uint-ref bytevector-sint-ref
   bytevector-uint-set! bytevector-sint-set!
   bytevector->uint-list bytevector->sint-list
   uint-list->bytevector sint-list->bytevector

   bytevector-u16-ref bytevector-s16-ref
   bytevector-u16-native-ref bytevector-s16-native-ref
   bytevector-u16-set! bytevector-s16-set!
   bytevector-u16-native-set! bytevector-s16-native-set!

   bytevector-u32-ref bytevector-s32-ref
   bytevector-u32-native-ref bytevector-s32-native-ref
   bytevector-u32-set! bytevector-s32-set!
   bytevector-u32-native-set! bytevector-s32-native-set!

   bytevector-u64-ref bytevector-s64-ref
   bytevector-u64-native-ref bytevector-s64-native-ref
   bytevector-u64-set! bytevector-s64-set!
   bytevector-u64-native-set! bytevector-s64-native-set!

   bytevector-ieee-single-native-ref
   bytevector-ieee-single-ref
   bytevector-ieee-double-native-ref
   bytevector-ieee-double-ref
   bytevector-ieee-single-native-set!
   bytevector-ieee-single-set!
   bytevector-ieee-double-native-set!
   bytevector-ieee-double-set!

   string->utf8 string->utf16 string->utf32
   utf8->string utf16->string utf32->string)

  (define exact inexact->exact)
  (define inexact exact->inexact)

  (include "r6rs/bytevectors-impl.scm")
  (include "r6rs/r7rs-shim.scm")
  (include "r6rs/chicken-ieee.scm")

) ; r6rs bytevectors

