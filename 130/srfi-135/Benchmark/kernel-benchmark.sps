;;; Micro-benchmarks for SRFI 135 (Immutable Texts).
;;;
;;; The main purpose of this program is to provide objective
;;; measurements that can be used to decide which of the sample
;;; implementations' three kernels would perform best in any
;;; particular system.
;;;
;;; The kernels implement a small core:
;;;
;;; text?  text-tabulate  text-length  text-ref  subtext  text-concatenate
;;;
;;; The performance of that core determines the performance of
;;; the sample implementations, so a few micro-benchmarks should
;;; suffice:
;;;
;;;     traversal
;;;         English, texts of length 10, 100, 1000
;;;         English, Thucydides Book I
;;;         Greek, Thucydides Book I
;;;     split
;;;         English, Thucydides Book I
;;;         Greek, Thucydides Book I
;;;     join
;;;         English, reversing the split
;;;         Greek, reversing the split
;;;
;;; To provide a basis for comparison, string-ref and the
;;; string-ref/cursors procedure of SRFI 130 are also tested
;;; on traversals, and splits and joins are also tested using
;;; the corresonding procedures of SRFI 130.
;;;
;;; Benchmark results are reported as nanoseconds per character.
;;;
;;; English and Greek texts for Thucydides' History of the
;;; Peloponnesian War, in Unicode, were downloaded from these
;;; sites at Project Gutenberg:
;;;
;;; http://onlinebooks.library.upenn.edu/webbin/gutbook/lookup?num=7142
;;; http://onlinebooks.library.upenn.edu/webbin/gutbook/lookup?num=29833
;;;
;;; The Greek text contains Book I only.  The Crawley translation
;;; is complete, but the end of Book I can be detected by scanning
;;; for a line saying "BOOK II" following a famous sentence about
;;; Pericles.

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme time)
;       (srfi 130)
        (srfi 135))

(cond-expand ((and (not larceny)           ; bug in Larceny v0.99 cond-expand
                   (library (srfi 130)))
              (import (srfi 130)))
             (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read test data from files.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define thucydides1-file "29833-0.txt")
(define crawley-file "7142.txt")

(define (read-file-as-string filename)
  (display "Reading ")
  (display filename)
  (display " as string...\n")
  (call-with-port
   (open-input-file filename)
   (lambda (p)
     (let ((q (open-output-string)))
       (let loop ()
         (let ((s (read-line p)))
           (if (string? s)
               (begin (write-string s q)
                      (newline q)
                      (loop))
               (begin (flush-output-port q)
                      (get-output-string q)))))))))

(define (read-file-as-text filename)
  (display "Reading ")
  (display filename)
  (display " as text...\n")
  (call-with-port
   (open-input-file filename)
   (lambda (p)
     (let loop ((texts '()))
       (let ((s (read-line p)))
         (if (string? s)
             (loop (cons (string->text (string-append s "\n")) texts))
             (textual-concatenate-reverse texts)))))))

(define thucydides1-as-string (read-file-as-string thucydides1-file))
(define thucydides1-as-text   (read-file-as-text   thucydides1-file))
(define crawley-as-string     (read-file-as-string crawley-file))
(define crawley-as-text       (read-file-as-text   crawley-file))

;;; FIXME

(define crawley1-as-text
  (subtext crawley-as-text
           0
           (textual-contains crawley-as-text
                          "BOOK II"
                          (textual-contains crawley-as-text
                                         "Such were the words of Pericles."))))

(define crawley1-as-string
  (substring crawley-as-string 0 (text-length crawley1-as-text)))

(define str10   (substring crawley1-as-string 0 10))
(define str100  (substring crawley1-as-string 0 100))
(define str1000 (substring crawley1-as-string 0 1000))

(define text10   (string->text str10))
(define text100  (string->text str100))
(define text1000 (string->text str1000))

(write (text-length crawley1-as-text))
(display " characters in English input\n")

(write (text-length thucydides1-as-text))
(display " characters in Greek input\n\n")

(display "Nanoseconds per character:\n\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-traversal-benchmark s)
  (define (traverse s)
    (let ((n (string-length s)))
      (let loop ((i 0)
                 (j 0))
        (cond ((= i n) j)
              ((char=? #\space (string-ref s i))
               (loop (+ i 1) (+ j 1)))
              (else (loop (+ i 1) j))))))
  (run-character-benchmark 'string-traversal
                           1
                           (lambda () (traverse s))
                           exact-integer?
                           (string-length s)))

(define (text-traversal-benchmark s)
  (define (traverse s)
    (let ((n (text-length s)))
      (let loop ((i 0)
                 (j 0))
        (cond ((= i n) j)
              ((char=? #\space (text-ref s i))
               (loop (+ i 1) (+ j 1)))
              (else (loop (+ i 1) j))))))
  (run-character-benchmark 'text-traversal
                           1
                           (lambda () (traverse s))
                           exact-integer?
                           (text-length s)))

(define (text-split-benchmark txt)
  (run-character-benchmark 'textual-split
                           1
                           (lambda () (textual-split txt " "))
                           list?
                           (text-length txt)))

(cond-expand ((and (not larceny)           ; bug in Larceny v0.99 cond-expand
                   (library (srfi 130)))

              (define (cursor-traversal-benchmark s)
                (define (traverse s)
                  (let ((n (string-cursor-end s)))
                    (let loop ((i (string-cursor-start s))
                               (j 0))
                      (cond ((string-cursor=? i n) j)
                            ((char=? #\space (string-ref/cursor s i))
                             (loop (string-cursor-next s i) (+ j 1)))
                            (else (loop (string-cursor-next s i) j))))))
                (run-character-benchmark 'string-cursors-traversal
                                         1
                                         (lambda () (traverse s))
                                         exact-integer?
                                         (string-length s)))

              (define (cursor-split-benchmark s)
                (run-character-benchmark 'cursor-split
                                         1
                                         (lambda () (string-split s " "))
                                         list?
                                         (string-length s))))

             (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; and a unary predicate that is true of the
;;; correct results the thunk may return,
;;; runs the benchmark for the number of specified iterations
;;; and returns the execution time in seconds.

(define (run-benchmark name count thunk ok?)
  (let* ((j/s (jiffies-per-second))
         (t0 (current-second))
         (j0 (current-jiffy)))
    (let loop ((i 0)
               (result (if #f #f)))
      (cond ((< i count)
             (loop (+ i 1) (thunk)))
            ((ok? result)
             (let* ((j1 (current-jiffy))
                    (jifs (- j1 j0))
                    (secs (inexact (/ jifs j/s))))
               secs))
            (else
             (display "ERROR: ")
             (display name)
             (display " returned incorrect result: ")
             (write result)
             (newline)
             -1)))))

;;; Given the name of a benchmark,
;;; a suggested number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; a unary predicate that is true of the correct results of that thunk,
;;; and the number of characters processed by calling the thunk once,
;;; runs the benchmark enough to get a timing in excess of one second
;;; and returns the number of seconds per character.

(define (run-character-benchmark name count thunk ok? k)
  (let ((t (max 1e-9 (run-benchmark name count thunk ok?))))
    (if (> t 1.0)
        (/ t k)
        (run-character-benchmark
         name (* 3 count) thunk ok? (* 3 k)))))

;;; Given the name of a benchmark,
;;; a unary procedure that runs the benchmark on its input,
;;; and an input for that procedure,
;;; runs the benchmark and reports its timing.

(define (report-benchmark name proc input)
  (display name)
  (display " : ")
  (let* ((t (proc input))
         (t (exact (round (* 1e9 t)))))
    (write t))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-benchmark "string traversal (10)     "
                  string-traversal-benchmark
                  str10)

(report-benchmark "string traversal (100)    "
                  string-traversal-benchmark
                  str100)

(report-benchmark "string traversal (1000)   "
                  string-traversal-benchmark
                  str1000)

(report-benchmark "string traversal (English)"
                  string-traversal-benchmark
                  crawley1-as-string)

(report-benchmark "string traversal (Greek)  "
                  string-traversal-benchmark
                  thucydides1-as-string)

(report-benchmark "text   traversal (10)     "
                  text-traversal-benchmark
                  text10)

(report-benchmark "text   traversal (100)    "
                  text-traversal-benchmark
                  text100)

(report-benchmark "text   traversal (1000)   "
                  text-traversal-benchmark
                  text1000)

(report-benchmark "text   traversal (English)"
                  text-traversal-benchmark
                  crawley1-as-text)

(report-benchmark "text   traversal (Greek)  "
                  text-traversal-benchmark
                  thucydides1-as-text)

(report-benchmark "text   split     (100)    "
                  text-split-benchmark
                  (subtext crawley1-as-text 1000 1100))

(report-benchmark "text   split     (1000)   "
                  text-split-benchmark
                  (subtext crawley1-as-text 1000 2000))

(report-benchmark "text   split     (10000)  "
                  text-split-benchmark
                  (subtext crawley1-as-text 1000 11000))

(report-benchmark "text   split     (English)"
                  text-split-benchmark
                  crawley1-as-text)

(cond-expand
 ((and (not larceny)           ; bug in Larceny v0.99 cond-expand
       (library (srfi 130)))

  (report-benchmark "cursor traversal (10)     "
                    cursor-traversal-benchmark
                    str10)

  (report-benchmark "cursor traversal (100)    "
                    cursor-traversal-benchmark
                    str100)

  (report-benchmark "cursor traversal (1000)   "
                    cursor-traversal-benchmark
                    str1000)

  (report-benchmark "cursor traversal (English)"
                    cursor-traversal-benchmark
                    crawley1-as-string)

  (report-benchmark "cursor traversal (Greek)  "
                    cursor-traversal-benchmark
                    thucydides1-as-string)

  (report-benchmark "cursor split     (100)    "
                    cursor-split-benchmark
                    (substring crawley1-as-string 1000 1100))

  (report-benchmark "cursor split     (1000)   "
                    cursor-split-benchmark
                    (substring crawley1-as-string 1000 2000))

  (report-benchmark "cursor split     (10000)  "
                    cursor-split-benchmark
                    (substring crawley1-as-string 1000 11000))
#;
  (report-benchmark "cursor split     (English)"
                    cursor-split-benchmark
                    crawley1-as-string))

 (else))

