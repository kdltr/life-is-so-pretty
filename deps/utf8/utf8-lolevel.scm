;;;; utf8-lolevel.scm -- encoding utils
;;
;; Copyright (c) 2004-2009 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is an internal library used by the utf8 interface.
;; You probably don't want to use this.
;;
;; Notes:
;;
;; 'pos' and 'index' refer to conceptual utf8 indices (Unicode codepoints).
;; 'off' and 'pointer' refer to actual byte offsets.
;; 'sp-' is a string-pointer function referring to offsets.

;; Uses ##sys#become! since all types are correct at runtime.
;;
;; Assumes string-length, string-ref & string-set! are rewritten by
;; the compiler.

(declare
  (no-argc-checks)
  (no-bound-checks)
  (no-procedure-checks)
  (bound-to-procedure
    ##sys#char->utf8-string ##sys#become!))

(require-library data-structures lolevel)

(module utf8-lolevel
  (
   ;; utils
   string-int-ref string-int-set! ascii-string?
   ;; utf8 encoding
   string-set-at-byte-in-place! string-set-at-byte
   utf8-start-byte->length ucs-integer->length
   utf8-index->offset utf8-offset->index
   utf8-string-ref utf8-string-set! utf8-string-length
   utf8-substring
   utf8-string->list utf8-prev-char utf8-next-char
   make-utf8-string utf8-string?
   with-substring-offsets with-two-substring-offsets
   ;; string-pointers
   make-string-pointer string-pointer? sp-copy
   sp-first sp-last sp-next sp-prev sp-ref sp-ref->string sp-set!
   sp-before sp-after sp-substring
   sp-check? sp-check-lo? sp-check-hi?
   ;; I/O
   read-utf8-char write-utf8-char char->utf8-string
   ;; fast-loop iterator
   in-utf8-string
   )

(import scheme chicken extras lolevel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; utilities which take and return integers
(define (string-int-ref s i)
  (char->integer (string-ref s i)))
(define (string-int-set! s i c)
  (string-set! s i (integer->char c)))

;; determine if a string only has 7-bit ASCII characters
(define (ascii-string? str)
  (let ((limit (string-length str)))
    (let loop ((i 0))
      (or (= i limit)
          (and (> 128 (string-int-ref str i))
               (loop (+ i 1)))))))

;; from SRFI-33, useful in splitting up the bit patterns used to
;; represent unicode values in utf8
(define (extract-bit-field size position n)
  (bitwise-and (bitwise-not (arithmetic-shift -1 size))
               (arithmetic-shift n (- position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indexing utils

;; number of total bytes in a utf8 char given the 1st byte
(define utf8-start-byte->length
  (let ((table '#(
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 0x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 1x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 2x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 3x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 4x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 5x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 6x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 7x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 8x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 9x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; ax
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; bx
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ; cx
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ; dx
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 ; ex
4 4 4 4 4 4 4 4 5 5 5 5 6 6 0 0 ; fx
)))
    (lambda (i) (vector-ref table i))))

(define (ucs-integer->length x)
  (cond
    ((<= x #x7F)       1)
    ((<= x #x7FF)      2)
    ((<= x #xFFFF)     3)
    ((<= x #x1FFFFF)   4)
    (else (error "unicode codepoint out of range:" x))))

(define (utf8-index->offset s pos)
  (if (zero? pos)
    0
    (let ((limit (string-length s)))
      (let loop ((i 0) (count 0))
        (cond
          ((= count pos) i)
          ((>= i limit) (error "index out of range" s pos))
          (else
           (loop (+ i (utf8-start-byte->length (string-int-ref s i)))
                 (+ count 1))))))))

(define (utf8-offset->index s off)
  (let ((limit (string-length s)))
    (let loop ((i 0) (count 0))
      (cond
        ((>= i off) (if (= i off) count (- count 1)))
        ((>= i limit) (error "index out of range" s off))
        (else
         (loop (+ i (utf8-start-byte->length (string-int-ref s i)))
               (+ count 1)))))))

;; return offset of previous char, or #f if at start of string
(define (utf8-prev-char s off)
  (let loop ((i (- off 1)))
    (cond
      ((negative? i) #f)
      ((= #b10000000 (bitwise-and #b11000000 (string-int-ref s i)))
       (loop (- i 1)))
      (else i))))

;; return offset of next char, or #f if at end of string
(define (utf8-next-char s off)
  (let ((limit (string-length s)))
    (and (< off limit)
         (let ((res (+ off (utf8-start-byte->length (string-int-ref s off)))))
           (and (<= res limit) res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redefine string primitives

(define (utf8-substring s start . opt)
  (with-substring-offsets substring s (cons start opt)))

(define (utf8-string-length s)
  (let ((limit (string-length s)))
    (let lp ((i 0) (res 0))
      (if (>= i limit)
          res
          (lp (+ i (utf8-start-byte->length (string-int-ref s i)))
              (+ res 1))))))

(define (with-substring-offsets proc s opt)
  (let* ((start (if (pair? opt) (car opt) 0))
         (b1 (utf8-index->offset s start))
         (opt2 (if (pair? opt) (cdr opt) '())))
    (let ((limit (string-length s)))
      (if (pair? opt2)
        (let ((end (car opt2)))
          (let lp ((b2 b1) (count start))
            (cond
              ((= count end) (proc s b1 b2))
              ((> b2 limit) (error "index out of range" s end))
              (else
               (lp (+ b2 (utf8-start-byte->length (string-int-ref s b2)))
                   (+ count 1))))))
        (proc s b1 limit)))) )

(define (with-two-substring-offsets proc s1 s2 opt)
  (with-substring-offsets
    (lambda (s1 start1 end1)
      (with-substring-offsets
        (lambda (s2 start2 end2)
          (proc s1 s2 start1 end1 start2 end2))
        s2 (if (and (pair? opt) (pair? (cdr opt))) (cddr opt) '())))
    s1 opt) )

(define (utf8-string->list str)
  (let ((limit (string-length str)))
    (let lp ((i 0) (res '()))
      (if (>= i limit)
          (reverse res)
          (lp (+ i (utf8-start-byte->length (string-int-ref str i)))
              (cons (sp-ref str i) res))))))

(define (make-utf8-string len . opt)
  (if (pair? opt)
      (let* ((c (car opt))
             (c-i (char->integer c))
             (c-len (ucs-integer->length c-i)))
        (if (<= c-len 1)
            (make-string len c)
            (let* ((size (* len c-len))
                   (res (make-string size)))
              (let lp ((i 0))
                (if (>= i size)
                    res
                    (begin
                      (string-set-at-byte-in-place! res size c-len i c-i)
                      (lp (+ i c-len))))))))
      (make-string len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessors

(define (sp-ref s off)
  (let* ((c (string-int-ref s off))
         (len (utf8-start-byte->length c))
         (limit (string-length s)))
    (if (<= len 1)
      (integer->char c)
      (let ((end (+ off len)))
        (if (> end limit)
          (error "utf8 trailing char overflow" s off)
          (let loop ((i (+ off 1)) (res (extract-bit-field (- 7 len) 0 c)))
            (if (= i end)
              (integer->char res)
              (loop (+ i 1)
                    (bitwise-ior (arithmetic-shift res 6)
                                 (bitwise-and #b00111111
                                              (string-int-ref s i)))))))))))

(define (sp-ref->string s off)
  (let* ((c (string-int-ref s off))
         (len (utf8-start-byte->length c))
         (limit (string-length s))
         (end (+ off len)))
    (if (> end limit)
      (error "utf8 trailing char overflow" s off)
      (substring s off end))))

(define (utf8-string-ref s pos)
  (sp-ref s (utf8-index->offset s pos)))

(define (string-set-at-byte-in-place! s limit c-len off val-i)
  (let ((end (+ off c-len)))
    (cond
      ((> end limit)
       (error "utf8 trailing char overflow" s off))
      ((<= c-len 1)
       (string-int-set! s off val-i))
      (else
       (let* ((tag (- (expt 2 c-len) 1))
              (tag-shift (arithmetic-shift tag (- 8 c-len)))
              (body (extract-bit-field (- 7 c-len)
                                       (* 6 (- c-len 1))
                                       val-i))
              (b1 (bitwise-ior tag-shift body)))
         (string-int-set! s off b1))
       (let loop ((i 1))
         (unless (= i c-len)
           (let ((b (bitwise-ior
                     #b10000000
                     (extract-bit-field 6 (* 6 (- c-len i 1)) val-i))))
             (string-int-set! s (+ off i) b)
             (loop (+ i 1)))))))))

(define (string-set-at-byte s size byte c-len val)
  (let ((s1 (substring s 0 byte))
        (s2 (char->utf8-string val))
        (s3 (substring s (+ byte c-len) size)))
    (string-append s1 s2 s3)))

(define (sp-set! s off val)
  (let* ((limit (string-length s))
         (c (string-int-ref s off))
         (c-len (utf8-start-byte->length c))
         (val-i (char->integer val))
         (val-len (ucs-integer->length val-i)))
    (if (not (= c-len val-len))
      ;; different size, allocate & become new string
      (let ((res (string-set-at-byte s limit off c-len val)))
        (##sys#become! (list (cons s res))))
      ;; modify in place
      (string-set-at-byte-in-place! s limit c-len off val-i))))

(define (utf8-string-set! s pos val)
  (sp-set! s (utf8-index->offset s pos) val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Pointers

(define (make-string-pointer s . opt)
  (if (pair? opt)
    (let ((pos (car opt)))
      (if (negative? pos)
        (sp-prev s (utf8-prev-char s (string-length s)) (+ pos 1))
        (sp-next s 0 pos)))
    0))

(define (string-pointer? obj)
  (integer? obj))

(define (sp-copy sp) sp)

(define (sp-check-lo? s sp)
  (positive? sp))

(define (sp-check-hi? s sp)
  (< sp (string-length s)))

(define (sp-check? s sp)
  (and (sp-check-lo? s sp) (sp-check-hi? s sp)))

;; returns the next string-pointer, or the string-length (an invalid
;; pointer) otherwise
(define (sp-next s sp . opt)
  (let loop ((i (if (pair? opt) (car opt) 1))
             (sp sp))
    (if (positive? i)
      (let ((res (utf8-next-char s sp)))
        (if res
          (loop (- i 1) res)
          (string-length s)))
      sp)))

(define (sp-prev s sp . opt)
  (let loop ((i (if (pair? opt) (car opt) 1))
             (sp sp))
    (if (positive? i)
      (let ((res (utf8-prev-char s sp)))
        (if res
          (loop (- i 1) res)
          -1))
      sp)))

(define (sp-first s) 0)
(define (sp-last s) (string-length s))

(define (sp-before s sp)
  (substring s 0 sp))

(define (sp-after s sp)
  (substring s sp))

(define (sp-substring s . opt)
  (if (null? opt)
    (substring s 0)
    (apply substring s opt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic I/O

;; now in the core library
(define (char->utf8-string c)
  (##sys#char->utf8-string c))

(define (write-utf8-char c . opt)
  (display (char->utf8-string c)
           (if (pair? opt) (car opt) (current-output-port))))

(define (read-utf8-char . opt)
  (let* ((p (if (pair? opt) (car opt) (current-input-port)))
         (b1 (read-byte p)))
    (if (eof-object? b1)
      b1
      (let ((len (utf8-start-byte->length b1)))
        (if (<= len 1)
          (integer->char b1)
          (let loop ((res (extract-bit-field (- 7 len) 0 b1))
                     (i (- len 1)))
            (if (zero? i)
              (integer->char res)
              (let ((b2 (read-byte p)))
                (cond
                  ((eof-object? b2) b2)
                  ((not (= #b10 (extract-bit-field 2 6 b2)))
                   (error "invalid utf8 sequence"))
                  (else
                   (loop (bitwise-ior (arithmetic-shift res 6)
                                      (bitwise-and #b00111111 b2))
                         (- i 1))))))))))))

(define-syntax char<=
  (syntax-rules ()
    ((char<= a b) (char<=? a b))
    ((char<= a b c) (and (char<=? a b) (char<=? b c)))))

(define (utf8-tail? c)
  (char<= #\x80 c #\xBF))

(define (utf8-2? c0 c1)
  (and (utf8-tail? c1)
       (char<=? #\xC2 c0)))   ;; C0-C1 can only be overlong

(define (utf8-3? c0 c1 c2)
  (and (utf8-tail? c1)
       (utf8-tail? c2)
       (cond ((char=? c0 #\xE0)       ;; check overlong
              (char<=? #\xA0 c1))
             ((char=? c0 #\xED)       ;; check surrogate
              (char<=? c1 #\x9F))
             (else #t))))

(define (utf8-4? c0 c1 c2 c3)
  (and (utf8-tail? c1)
       (utf8-tail? c2)
       (utf8-tail? c3)
       (cond ((char=? c0 #\xF0)       ;; check overlong
              (char<=? #\x90 c1))
             ((char=? c0 #\xF4)       ;; check in range
              (char<=? c1 #\x8F))
             (else
              (char<=? c0 #\xF3)))))  ;; check in range

;; Ensure all codepoints within range, not overlong, are not UTF-16
;; surrogate halves, and have the right number of continuation bytes.
(define (utf8-string? str)
  (let ((len (string-length str)))
    (let loop ((pos 0))
      (or (fx= pos len)
          (let ((c0 (string-ref str pos)))
            (let-syntax
                ((validate
                  (syntax-rules ()
                    ((_ valid? c ... n)
                     (and (fx<= n (fx- len pos))
                          (valid? c0 (string-ref str (fx+ pos c)) ...)
                          (loop (fx+ pos n)))))))
              (if (char<=? c0 #\x7F)
                  (loop (fx+ pos 1))
                  (case (utf8-start-byte->length (char->integer c0))
                    ((2) (validate utf8-2? 1 2))
                    ((3) (validate utf8-3? 1 2 3))
                    ((4) (validate utf8-4? 1 2 3 4))
                    (else #f)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax in-utf8-string
  (syntax-rules ()
    ((in-utf8-string ((var) (str)) next . rest)
     (in-utf8-string ((var off) (str)) next . rest))
    ((in-utf8-string ((var off) (str)) next . rest)
     (next ((tmp str) (lim (sp-last tmp)))
           ((off (sp-first tmp)
                 (fx+ off (utf8-start-byte->length (string-int-ref str off)))))
           ((fx>= off lim))
           ((var (sp-ref tmp off)))
           ()
           . rest))))

)
