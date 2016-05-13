;;;; utf8-support.scm -- Unicode support for Chicken
;;
;; Copyright (c) 2004-2009 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USAGE
;;
;; To make your code Unicode aware, just do the following:
;;
;;   (require-extension utf8)
;;   (module mymodule ()
;;     (import utf8)
;;
;;     ... ; unicode-aware code
;;
;;     )
;;
;; then all core, extra, regex and SRFI-13 string operations will be
;; Unicode aware.  string-length will return the number of codepoints,
;; not the number of bytes, string-ref will index by codepoints and
;; return a char with an integer value up to 2^21, regular expressions
;; will match single codepoints rather than bytes and understand Unicode
;; character classes, etc.
;;
;; Strings are still native strings and may be passed to external
;; libraries (either Scheme or foreign) perfectly safely.  Libraries
;; that do parsing invariably do so on ASCII character boundaries and
;; are thus guaranteed to be compatible.  Libraries that reference
;; strings by index would need to be modified with a UTF-8 version.
;; Currently all existing eggs are UTF-8 safe to my knowledge.
;;
;; Alternately, you may import utf8 at the top-level:
;;
;;   ; require modules using byte-semantics
;;   ...
;;   (require-extension utf8)
;;   (import utf8)
;;   ...
;;   ; require modules using utf8-semantics
;;   ...
;;   ; unicode-aware code
;;
;; By importing directly into the top-level, any subsequently loaded
;; code will also use Unicode-aware semantics, even if it was not
;; written with Unicode in mind.  This is more powerful but slightly
;; less safe, since third party units may make assumptions about
;; character ranges or string size.
;;
;;
;; UNICODE CHAR-SETS
;;
;; The default SRFI-14 char-sets are defined using ASCII-only
;; characters, since this is both useful and lighter-weight.  To obtain
;; full Unicode char-set definitions, use the char-set unit:
;;
;;   (require-extension char-set)
;;
;; The following char-sets are provided based on the Unicode properties:
;;
;;   char-set:alphabetic
;;   char-set:arabic
;;   char-set:armenian
;;   char-set:ascii-hex-digit
;;   char-set:bengali
;;   char-set:bidi-control
;;   char-set:bopomofo
;;   char-set:braille
;;   char-set:buhid
;;   char-set:canadian-aboriginal
;;   char-set:cherokee
;;   char-set:common
;;   char-set:cypriot
;;   char-set:cyrillic
;;   char-set:dash
;;   char-set:default-ignorable-code-point
;;   char-set:deprecated
;;   char-set:deseret
;;   char-set:devanagari
;;   char-set:diacritic
;;   char-set:ethiopic
;;   char-set:extender
;;   char-set:georgian
;;   char-set:gothic
;;   char-set:grapheme-base
;;   char-set:grapheme-extend
;;   char-set:grapheme-link
;;   char-set:greek
;;   char-set:gujarati
;;   char-set:gurmukhi
;;   char-set:han
;;   char-set:hangul
;;   char-set:hanunoo
;;   char-set:hebrew
;;   char-set:hex-digit
;;   char-set:hiragana
;;   char-set:hyphen
;;   char-set:id-continue
;;   char-set:id-start
;;   char-set:ideographic
;;   char-set:ids-binary-operator
;;   char-set:ids-trinary-operator
;;   char-set:inherited
;;   char-set:join-control
;;   char-set:kannada
;;   char-set:katakana
;;   char-set:katakana-or-hiragana
;;   char-set:khmer
;;   char-set:lao
;;   char-set:latin
;;   char-set:limbu
;;   char-set:linear-b
;;   char-set:logical-order-exception
;;   char-set:lowercase
;;   char-set:malayalam
;;   char-set:math
;;   char-set:mongolian
;;   char-set:myanmar
;;   char-set:noncharacter-code-point
;;   char-set:ogham
;;   char-set:old-italic
;;   char-set:oriya
;;   char-set:osmanya
;;   char-set:quotation-mark
;;   char-set:radical
;;   char-set:runic
;;   char-set:shavian
;;   char-set:sinhala
;;   char-set:soft-dotted
;;   char-set:sterm
;;   char-set:syriac
;;   char-set:tagalog
;;   char-set:tagbanwa
;;   char-set:tai-le
;;   char-set:tamil
;;   char-set:telugu
;;   char-set:terminal-punctuation
;;   char-set:thaana
;;   char-set:thai
;;   char-set:tibetan
;;   char-set:ugaritic
;;   char-set:unified-ideograph
;;   char-set:uppercase
;;   char-set:variation-selector
;;   char-set:white-space
;;   char-set:xid-continue
;;   char-set:xid-start
;;   char-set:yi
;;
;;
;; BYTE-STRINGS
;;
;; Sometimes you may need access to the original string primitives so
;; you can directly access bytes, such as if you were implementing your
;; own regex library or text buffer and wanted optimal performance.  For
;; these cases we have renamed the original primitives by replacing
;; "string" with "byte-string".  Thus byte-string-length is the length
;; in bytes, not characters, of the strings (the equivalent of Gauche's
;; string-size).  byte-string-set! can corrupt the UTF-8 encoding and
;; should be used sparingly if at all.
;;
;;
;; LOW LEVEL API
;;
;; Direct manipulation of the utf8 encoding is factored away in the
;; utf8-lolevel unit.  This includes an abstract string-pointer API, and
;; an analogous string-pointer implementation for ASCII strings in the
;; string-pointer unit, however as the API is not fixed you use these at
;; your own risk.
;;
;; LIMITATIONS
;;
;; peek-char currently does not have Unicode semantics (i.e. it peeks
;; only a single byte) to avoid problems with port buffering.
;;
;; char-sets are not interchangeable between the existing srfi-14 code
;; and Unicode code (i.e. do not pass a Unicode char-set to an external
;; library that directly uses the old srfi-14).
;;
;;
;; PERFORMANCE
;;
;; string-length, string-ref and string-set! are all O(n) operations as
;; opposed to the usual O(1) since UTF-8 is a variable width encoding.
;; Use of these should be discouraged - it is much cleaner to use the
;; high-level SRFI-13 procedures and string ports.  For examples of how
;; to do common idioms without these procedures look at any string-based
;; code in Gauche.
;;
;; Furthermore, string-set! and other procedures that modify strings in
;; place may invoke gc if the mutated result does not fit within the
;; same UTF-8 encoding size as the original string.  If only mutating
;; 7-bit ASCII strings (or only mutating within fixed encoding sizes
;; such as Cyrillic->Cyrillic) then no gc will occur.
;;
;; string?, string=?, string-append, all R5RS string comparisons, and
;; read-line are unmodified.
;;
;; Regular expression matching will be just as fast except in the case
;; of Unicode character classes (which were not possible before anyway).
;;
;; All other procedures incur zero to minor overhead, but keep the same
;; asymptotic performance.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare
  (no-procedure-checks)
  (bound-to-procedure
    ##sys#become!))

(module
 utf8
 (
  ;; R5RS
  string-length string-ref string-set! make-string string substring
  string->list list->string string-fill! write-char read-char display
  ;; library
  reverse-list->string print print*
  ;; extras
  read-string write-string read-token ->string conc string-chop string-split
  string-translate substring=? substring-ci=? substring-index substring-index-ci
  ;; regexp
  grep regexp string-substitute string-substitute* string-split-fields
  string-match string-match-positions string-match-offsets
  string-search string-search-positions string-search-offsets
  ;; new
  string-set valid-string?
  ;; byte oriented
  make-byte-string byte-substring byte-string-length
  byte-display byte-print byte-print*
  ->byte-string byte-string-split byte-string-translate
  byte-substring=? byte-substring-ci=?
  byte-substring-index byte-substring-index-ci
  read-byte-string
  )

(import (rename (except scheme
                        string string->list list->string string-fill!
                        string-ref string-set! write-char read-char)
                (make-string make-byte-string)
                (substring byte-substring)
                (string-length byte-string-length)
                (display byte-display))
        (rename (except chicken reverse-list->string)
                (print byte-print) (print* byte-print*))
        (rename (except data-structures conc string-chop)
                (->string ->byte-string)
                (string-split byte-string-split)
                (string-translate byte-string-translate)
                (substring=? byte-substring=?)
                (substring-ci=? byte-substring-ci=?)
                (substring-index byte-substring-index)
                (substring-index-ci byte-substring-index-ci))
        (rename (except extras write-string read-token)
                (read-string read-byte-string))
        (rename regex
                (regexp byte-regexp)
                (grep byte-grep)
                (string-search byte-string-search)
                (string-match byte-string-match)
                (string-search-positions byte-string-search-positions)
                (string-match-positions byte-string-match-positions)
                (string-substitute byte-string-substitute)
                (string-substitute* byte-string-substitute*)
                (string-split-fields byte-string-split-fields))
        ports
        (rename utf8-lolevel
                (utf8-string? valid-string?)))

(require-library regex utf8-lolevel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redefine string primitives

(define string-length utf8-string-length)

(define char->string char->utf8-string)

(define (string . args)
  (list->string args))

(define substring utf8-substring)

(define (make-string len . opt)
  (if (pair? opt)
      (let* ((c (car opt))
             (c-i (char->integer c))
             (c-len (ucs-integer->length c-i)))
        (if (<= c-len 1)
            (make-byte-string len c)
            (let* ((size (* len c-len))
                   (res (make-byte-string size)))
              (let loop ((i 0))
                (if (>= i size)
                    res
                    (begin
                      (string-set-at-byte-in-place! res size c-len i c-i)
                      (loop (+ i c-len))))))))
      (make-byte-string len)))

(define string->list utf8-string->list)

(define (list->string ls)
  (string-intersperse (map char->string ls) ""))

(define (string-fill! str c)
  (let* ((size (byte-string-length str))
         (len (string-length str))
         (c-i (char->integer c))
         (c-len (ucs-integer->length c-i))
         (needed (* c-len len)))
    (if (= needed size)
        (let ((c-str (char->string c)))
          (do ((i 0 (+ i c-len)))
              ((= i size) str)
            (string-set-at-byte-in-place! str size len i c-i)))
        (let ((res (make-string len c)))
          (##sys#become! (list (cons str res)))))))

(define string-ref utf8-string-ref)

(define string-set! utf8-string-set!)

(define (string-set s pos val)
  (let* ((size (byte-string-length s))
         (byte (utf8-index->offset s pos))
         (c (string-int-ref s byte))
         (c-len (utf8-start-byte->length c)))
    (string-set-at-byte s size byte c-len val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic I/O

(define write-char write-utf8-char)

(define read-char read-utf8-char)

(define (display x . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (if (char? x) (write-utf8-char x out) (byte-display x out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library

(define (reverse-list->string ls)
  (list->string (reverse ls)))

(define (print . opt)
  (apply byte-print (map (lambda (x) (if (char? x) (char->string x) x)) opt)))

(define (print* . opt)
  (apply byte-print* (map (lambda (x) (if (char? x) (char->string x) x)) opt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O extras

;; this could be optimized by reading bytes while counting characters,
;; instead of counting characters

(define (read-string . opt)
  (let-optionals* opt ((num #f) (in (current-input-port)))
    (if num
        (let loop ((i 0) (acc '()))
          (if (>= i num)
              (list->string (reverse acc))
              (let ((ch (read-char in)))
                (if (eof-object? ch)
                    (loop num acc)
                    (loop (+ i 1) (cons ch acc))))))
        (read-byte-string num in))))

(define (write-string str . opt)
  (let-optionals* opt ((num #f) (out (current-output-port)))
    (if (and num (< num (string-length str)))
        (byte-display (substring str 0 num) out)
        (byte-display str out))))

(define (read-token pred . opt)
  (let ((in (if (pair? opt) (car opt) (current-input-port))))
    (let loop ((acc '()))
      (let ((ch (read-char in)))
        (if (or (eof-object? ch) (not (pred ch)))
            (list->string (reverse acc))
            (loop (cons ch acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string extras

(define (->string x)
  (if (char? x) (char->string x) (->byte-string x)))

(define (conc . args)
  (apply string-append (map ->string args)))

(define (string-chop str len)
  (let ((size (byte-string-length str)))
    (let loop ((i 0) (from 0) (off 0) (acc '()))
      (cond ((>= off size)
             (if (> off from)
                 (reverse (cons (byte-substring str from off) acc))
                 (reverse acc)))
            ((= i len)
             (loop 0 off off (cons (byte-substring str from off) acc)))
            (else
             (loop (+ i 1)
                   from
                   (+ off (utf8-start-byte->length (string-int-ref str off)))
                   acc))))))

(define (string-split str . opt)
  (let-optionals* opt ((delim #f) (keep-empty? #f))
    (if (or (not delim) (ascii-string? delim))
        (byte-string-split str (or delim " \t\n") keep-empty?)
        (let ((delims (string->list delim))
              (join (if keep-empty?
                        (lambda (cur acc)
                          (cons (list->string (reverse cur)) acc))
                        (lambda (cur acc)
                          (if (null? cur)
                              acc
                              (cons (list->string (reverse cur)) acc))))))
          (let loop ((ls (string->list str)) (cur '()) (acc '()))
            (cond ((null? ls)
                   (reverse (join cur acc)))
                  ((memv (car ls) delims)
                   (loop (cdr ls) '() (join cur acc)))
                  (else
                   (loop (cdr ls) (cons (car ls) cur) acc))))))))

(define (string->vector str)
  (list->vector (string->list str)))

(define (string-translate str from . opt)

  ;; Until needed elsewhere
  (define (vector-char-scan vec ch . opts)
    (let-optionals opts ((st 0) (ed (vector-length vec)))
      (let loop ((i st))
        (cond ((= i ed) #f)
              ((char=? ch (vector-ref vec i)) i)
              (else (loop (+ i 1)))))))

  (##sys#check-string str 'string-translate)

  (let ((from (cond ((char? from)   from)
                    ((pair? from)   (list->string from))
                    (else
                     (##sys#check-string from 'string-translate)
                     from)))
        (to (and (pair? opt)
                 (let ((to (car opt)))
                   (cond ((char? to)   to)
                         ((pair? to)   (list->string to))
                         (else
                          (##sys#check-string to 'string-translate)
                          to))))))
    (if (and (if (char? from)
                 (> 128 (char->integer from))
                 (ascii-string? from))
             (if (char? to)
                 (> 128 (char->integer to))
                 (and to (ascii-string? to))))
        (byte-string-translate str from to)
        (let ((from-vec
               (if (char? from) (vector from) (string->vector from)))
              (to-vec
               (and to (if (char? to) (vector to) (string->vector to)))))
          (let ((trans
                 (if to-vec (lambda (i) (display (vector-ref to-vec i)))
                     (lambda _ (void)))))
            (with-output-to-string
              (lambda ()
                (let ((end (sp-last str)))
                  (let lp ((i (sp-first str)))
                    (when (< i end)
                      (let ((c (sp-ref str i)))
                        (cond ((vector-char-scan from-vec c) => trans)
                              (else (display c)))
                        (lp (sp-next str i)))))))))))))

(define (substring=? s1 s2 . opt)
  (let ((s1-len (utf8-string-length s1)) (s2-len (utf8-string-length s2)))
    (let-optionals* opt ((start1 0)
                         (start2 0)
                         (len (min (- s1-len start1) (- s2-len start2))))
      (let ((opt1 (list start1 (+ start1 len)))
            (opt2 (list start2 (+ start2 len))))
        (with-substring-offsets
         (lambda (s1 s1-start s1-end)
           (with-substring-offsets
            (lambda (s2 s2-start s2-end)
              (byte-substring=? s1 s2 s1-start s2-start (- s1-end s1-start)))
            s2 opt2))
         s1 opt1)))) )

(define (substring-ci=? s1 s2 . opt)
  (let ((s1-len (utf8-string-length s1)) (s2-len (utf8-string-length s2)))
    (let-optionals* opt ((start1 0)
                         (start2 0)
                         (len (min (- s1-len start1) (- s2-len start2))))
      (let ((opt1 (list start1 (+ start1 len)))
            (opt2 (list start2 (+ start2 len))))
        (with-substring-offsets
         (lambda (s1 s1-start s1-end)
           (with-substring-offsets
            (lambda (s2 s2-start s2-end)
              (byte-substring-ci=? s1 s2 s1-start s2-start (- s1-end s1-start)))
            s2 opt2))
         s1 opt1)))) )

(define (substring-index which where . opt)
  (let* ((start (if (pair? opt) (utf8-index->offset where (car opt)) 0))
         (res (byte-substring-index which where start)))
    (and res (utf8-offset->index where res))))

(define (substring-index-ci which where . opt)
  (let* ((start (if (pair? opt) (utf8-index->offset where (car opt)) 0))
         (res (byte-substring-index-ci which where start)))
    (and res (utf8-offset->index where res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regexps always enable utf8 unless the 4th arg is explicitly provided

(define (regexp str . opt)
  (let-optionals* opt ((icase? #f) (ispace? #f) (utf8? #t))
    (byte-regexp str icase? ispace? utf8?)))

(define (->rx x)
  (if (regexp? x) x (regexp x)))

(define (opt-off s opt)
  (if (pair? opt) (utf8-index->offset s (car opt)) 0))

(define (grep rx ls)
  (byte-grep (->rx rx) ls))

(define (string-match rx str)
  (byte-string-match (->rx rx) str))

(define (string-match-offsets rx str)
  (byte-string-match-positions (->rx rx) str))

(define (string-search rx str . opt)
  (let* ((start (opt-off str opt))
         (range (if (and (pair? opt) (pair? (cdr opt)))
                    (opt-off str (cdr opt))
                    (- (byte-string-length str) start))))
    (byte-string-search (->rx rx) str start range)))

(define (string-search-offsets rx str . opt)
  (let* ((start (opt-off str opt))
         (range (if (and (pair? opt) (pair? (cdr opt)))
                    (opt-off str (cdr opt))
                    (- (byte-string-length str) start))))
    (byte-string-search-positions (->rx rx) str start range)))

(define (string-split-fields rx str . opt)
  (let-optionals* opt ((mode #t) o2)
    (let ((start (opt-off str o2)))
      (byte-string-split-fields (->rx rx) str mode start))))

(define (string-substitute rx subst str . opt)
  (apply byte-string-substitute (->rx rx) subst str opt))

(define (string-substitute* str smap)
  (byte-string-substitute*
   str (map (lambda (x) (cons (->rx (car x)) (cdr x))) smap)))

;; these could be a lot faster, but you don't want to be working with
;; positions anyway

(define (string-match-positions rx str . opt)
  (let* ((size (byte-string-length str))
         (->pos (lambda (o) (utf8-offset->index str o))))
    (let ((res (apply string-match-offsets rx str opt)))
      (and res (map (lambda (x) (if (pair? x) (map ->pos x) x)) res)))))

(define (string-search-positions rx str . opt)
  (let* ((size (byte-string-length str))
         (->pos (lambda (o) (utf8-offset->index str o))))
    (let ((res (apply string-search-offsets rx str opt)))
      (and res (map (lambda (x) (if (pair? x) (map ->pos x) x)) res)))))

)
