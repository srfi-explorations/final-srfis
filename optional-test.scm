(import (scheme base) (scheme char) (srfi-152) (chibi test))

(test-group "srfi-152:optional"
  (test "string-upcase" "ABCDE" (string-upcase "abcde"))
  (test "string-upcase-unicode" "MASS" (string-upcase "Maß"))
  (test "string-downcase" "ABCDE" (string-upcase "abcde"))
  (test "string-downcase-unicode" "hätte" (string-downcase "HÄTTE"))
  (test "string-foldcase" "mass" (string-foldcase "Maß"))
  (test "string-normalize-nfc" "hätte"
    (string-normalize-nfc "ha\\u0308;tte"))
  (test "string-normalize-nfd" "ha\\u0308;tte"
    (string-normalize-nfd "hätte"))
  (test "string-normalize-nfkc" "Ångstrom"
    (string-normalize-nfkc "\u212b;ngstrom"))
  (test "string-normalize-nfkd" "A\\u030a;ngstrom"
    (string-normalize-nfkd "\u212b;ngstrom"))
  (test "string->utf8" #u8(77 97 195 159)
    (string->utf8 "Maß"))
  (test "string->utf16" #u8(0 77 0 97 37 28 1 146)
    (string->utf16 "Maß"))
  (test "string->utf16le" #u8(77 0 97 0 28 37 146 1)
    (string->utf16le "Maß"))
  (test "string->utf16be" #u8(0 77 0 97 37 28 1 146)
    (string->utf16be "Maß"))
  (test "utf8->string" "Maß"
    (utf8->string #u8(77 97 195 159)))
  (test "utf16->string" "Maß"
    (utf16->string #u8(77 0 97 0 28 37 146 1)))
  (test "string->utf16le" "Maß"
    (utf16le->string #u8(0 77 0 97 37 28 1 146)))
  (test "string->utf16be" "Maß"
    (utf16be->string #u8(0 77 0 97 37 28 1 146)))
)
