;;;; Alists for titlecase functions

;;; Assumes that char->integer and integer->char are a subset of Unicode
;;; codepoint mappings rather than some random codes, as R5RS allows
;;; but R[67]RS do not.  It may be necessary to remove some lines if
;;; the codepoints referred to don't correspond to characters present
;;; in the implementation.

;;; These maps are valid from Unicode 5.0 to at least Unicode 8.0
;;; and are expected to be stable for the foreseeable future.

;; Alist mapping titlecase characters to themselves
(define titlecase-chars '(
  (#x01C5 #x01C5) ; LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
  (#x01C8 #x01C8) ; LATIN CAPITAL LETTER L WITH SMALL LETTER J
  (#x01CB #x01CB) ; LATIN CAPITAL LETTER N WITH SMALL LETTER J
  (#x01F2 #x01F2) ; LATIN CAPITAL LETTER D WITH SMALL LETTER Z
  (#x1F88 #x1F88) ; GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
  (#x1F89 #x1F89) ; GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
  (#x1F8A #x1F8A) ; GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
  (#x1F8B #x1F8B) ; GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
  (#x1F8C #x1F8C) ; GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
  (#x1F8D #x1F8D) ; GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
  (#x1F8E #x1F8E) ; GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
  (#x1F8F #x1F8F) ; GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
  (#x1F98 #x1F98) ; GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
  (#x1F99 #x1F99) ; GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
  (#x1F9A #x1F9A) ; GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
  (#x1F9B #x1F9B) ; GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
  (#x1F9C #x1F9C) ; GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
  (#x1F9D #x1F9D) ; GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
  (#x1F9E #x1F9E) ; GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
  (#x1F9F #x1F9F) ; GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
  (#x1FA8 #x1FA8) ; GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
  (#x1FA9 #x1FA9) ; GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
  (#x1FAA #x1FAA) ; GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
  (#x1FAB #x1FAB) ; GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
  (#x1FAC #x1FAC) ; GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
  (#x1FAD #x1FAD) ; GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
  (#x1FAE #x1FAE) ; GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
  (#x1FAF #x1FAF) ; GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
  (#x1FBC #x1FBC) ; GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
  (#x1FCC #x1FCC) ; GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
  (#x1FFC #x1FFC) ; GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
))

;; Alist mapping characters to their single-letter titlecase equivalents
(define title-single-map (append titlecase-chars '(
  (#x01C4 #x01C5) ; LATIN CAPITAL LETTER DZ WITH CARON
  (#x01C6 #x01C5) ; LATIN SMALL LETTER DZ WITH CARON
  (#x01C7 #x01C8) ; LATIN CAPITAL LETTER LJ
  (#x01C9 #x01C8) ; LATIN SMALL LETTER LJ
  (#x01CA #x01CB) ; LATIN CAPITAL LETTER NJ
  (#x01CC #x01CB) ; LATIN SMALL LETTER NJ
  (#x01F1 #x01F2) ; LATIN CAPITAL LETTER DZ
  (#x01F3 #x01F2) ; LATIN SMALL LETTER DZ
)))

;; Alist mapping characters to their multiple-letter titlecase equivalents
(define title-multiple-map (append title-single-map '(
  (#x00DF #x0053 #x0073) ; LATIN SMALL LETTER SHARP S
  (#xFB00 #x0046 #x0066) ; LATIN SMALL LIGATURE FF
  (#xFB01 #x0046 #x0069) ; LATIN SMALL LIGATURE FI
  (#xFB02 #x0046 #x006C) ; LATIN SMALL LIGATURE FL
  (#xFB03 #x0046 #x0066 #x0069) ; LATIN SMALL LIGATURE FFI
  (#xFB04 #x0046 #x0066 #x006C) ; LATIN SMALL LIGATURE FFL
  (#xFB05 #x0053 #x0074) ; LATIN SMALL LIGATURE LONG S T
  (#xFB06 #x0053 #x0074) ; LATIN SMALL LIGATURE ST
  (#x0587 #x0535 #x0582) ; ARMENIAN SMALL LIGATURE ECH YIWN
  (#xFB13 #x0544 #x0576) ; ARMENIAN SMALL LIGATURE MEN NOW
  (#xFB14 #x0544 #x0565) ; ARMENIAN SMALL LIGATURE MEN ECH
  (#xFB15 #x0544 #x056B) ; ARMENIAN SMALL LIGATURE MEN INI
  (#xFB16 #x054E #x0576) ; ARMENIAN SMALL LIGATURE VEW NOW
  (#xFB17 #x0544 #x056D) ; ARMENIAN SMALL LIGATURE MEN XEH
  (#x0149 #x02BC #x004E) ; LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
  (#x0390 #x0399 #x0308 #x0301) ; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
  (#x03B0 #x03A5 #x0308 #x0301) ; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
  (#x01F0 #x004A #x030C) ; LATIN SMALL LETTER J WITH CARON
  (#x1E96 #x0048 #x0331) ; LATIN SMALL LETTER H WITH LINE BELOW
  (#x1E97 #x0054 #x0308) ; LATIN SMALL LETTER T WITH DIAERESIS
  (#x1E98 #x0057 #x030A) ; LATIN SMALL LETTER W WITH RING ABOVE
  (#x1E99 #x0059 #x030A) ; LATIN SMALL LETTER Y WITH RING ABOVE
  (#x1E9A #x0041 #x02BE) ; LATIN SMALL LETTER A WITH RIGHT HALF RING
  (#x1F50 #x03A5 #x0313) ; GREEK SMALL LETTER UPSILON WITH PSILI
  (#x1F52 #x03A5 #x0313 #x0300) ; GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
  (#x1F54 #x03A5 #x0313 #x0301) ; GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
  (#x1F56 #x03A5 #x0313 #x0342) ; GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
  (#x1FB6 #x0391 #x0342) ; GREEK SMALL LETTER ALPHA WITH PERISPOMENI
  (#x1FC6 #x0397 #x0342) ; GREEK SMALL LETTER ETA WITH PERISPOMENI
  (#x1FD2 #x0399 #x0308 #x0300) ; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
  (#x1FD3 #x0399 #x0308 #x0301) ; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
  (#x1FD6 #x0399 #x0342) ; GREEK SMALL LETTER IOTA WITH PERISPOMENI
  (#x1FD7 #x0399 #x0308 #x0342) ; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
  (#x1FE2 #x03A5 #x0308 #x0300) ; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
  (#x1FE3 #x03A5 #x0308 #x0301) ; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
  (#x1FE4 #x03A1 #x0313) ; GREEK SMALL LETTER RHO WITH PSILI
  (#x1FE6 #x03A5 #x0342) ; GREEK SMALL LETTER UPSILON WITH PERISPOMENI
  (#x1FE7 #x03A5 #x0308 #x0342) ; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
  (#x1FF6 #x03A9 #x0342) ; GREEK SMALL LETTER OMEGA WITH PERISPOMENI
  (#x1FB2 #x1FBA #x0345) ; GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
  (#x1FB4 #x0386 #x0345) ; GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
  (#x1FC2 #x1FCA #x0345) ; GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
  (#x1FC4 #x0389 #x0345) ; GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
  (#x1FF2 #x1FFA #x0345) ; GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
  (#x1FF4 #x038F #x0345) ; GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
  (#x1FB7 #x0391 #x0342 #x0345) ; GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
  (#x1FC7 #x0397 #x0342 #x0345) ; GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
  (#x1FF7 #x03A9 #x0342 #x0345) ; GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
)))

;; Alist mapping characters to their multiple-character lowercase equivalents
(define lower-multiple-map '(
  (#x0130 #x0069 #x0307) ; LATIN CAPITAL LETTER I WITH DOT ABOVE
))

;; Alist mapping characters to their multiple-character uppercase equivalents
(define upper-multiple-map '(
  (#x1F80 #x1F08 #x0399) ; GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
  (#x1F81 #x1F09 #x0399) ; GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
  (#x1F82 #x1F0A #x0399) ; GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
  (#x1F83 #x1F0B #x0399) ; GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
  (#x1F84 #x1F0C #x0399) ; GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
  (#x1F85 #x1F0D #x0399) ; GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
  (#x1F86 #x1F0E #x0399) ; GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
  (#x1F87 #x1F0F #x0399) ; GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
  (#x1F90 #x1F28 #x0399) ; GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
  (#x1F91 #x1F29 #x0399) ; GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
  (#x1F92 #x1F2A #x0399) ; GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
  (#x1F93 #x1F2B #x0399) ; GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
  (#x1F94 #x1F2C #x0399) ; GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
  (#x1F95 #x1F2D #x0399) ; GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
  (#x1F96 #x1F2E #x0399) ; GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
  (#x1F97 #x1F2F #x0399) ; GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
  (#x1FA0 #x1F68 #x0399) ; GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
  (#x1FA1 #x1F69 #x0399) ; GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
  (#x1FA2 #x1F6A #x0399) ; GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
  (#x1FA3 #x1F6B #x0399) ; GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
  (#x1FA4 #x1F6C #x0399) ; GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
  (#x1FA5 #x1F6D #x0399) ; GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
  (#x1FA6 #x1F6E #x0399) ; GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
  (#x1FA7 #x1F6F #x0399) ; GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
  (#x1FB3 #x0391 #x0399) ; GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
  (#x1FC3 #x0397 #x0399) ; GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
  (#x1FF3 #x03A9 #x0399) ; GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
))
