;;;; utf8-case-map.scm -- Unicode locale-aware case-mappings
;;
;; Copyright (c) 2004-2010 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Usage:
;;
;;   (utf8-string-upcase str-or-port [locale])
;;   (utf8-string-downcase str-or-port [locale])
;;   (utf8-string-titlecase str-or-port [locale])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare
  (no-bound-checks)
  (no-procedure-checks) )

(module
 utf8-case-map
 (
  char-upcase-single char-downcase-single char-titlecase-single
  char-downcase* char-upcase* char-titlecase*
  utf8-string-upcase utf8-string-downcase utf8-string-titlecase)

(import scheme chicken extras ports posix srfi-4
        utf8-lolevel (except utf8-srfi-14 char-set:hex-digit) unicode-char-sets)

(require-library posix srfi-4 utf8-lolevel utf8-srfi-14 unicode-char-sets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *data-file-path*
  (list "./data" (repository-path)))

(define (find-data-file name)
  (let lp ((ls *data-file-path*))
    (and (pair? ls)
         (let ((path (string-append (car ls) "/" name)))
           (if (file-exists? path)
             path
             (lp (cdr ls)))))))

(define char->ucs char->integer)
(define ucs->char integer->char)

(define read-binary-uint32-le
  ;; files distributed as little-endian in egg
  (lambda (port)
    (let* ((b1 (read-byte port)) (b2 (read-byte port))
           (b3 (read-byte port)) (b4 (read-byte port)))
      (if (eof-object? b4)
          b4
          (bitwise-ior
           b1
           (arithmetic-shift b2 8)
           (arithmetic-shift b3 16)
           (arithmetic-shift b4 24))))))

(define read-binary-uint16-le
  ;; files distributed as little-endian in egg
  (lambda (port)
    (let* ((b1 (read-byte port)) (b2 (read-byte port)))
      (if (eof-object? b2)
          b2
          (bitwise-ior b1 (arithmetic-shift b2 8))))))

;; currently only defined for u16 and u32 vectors
(define (read-block! vec port)
  (cond
    ((u16vector? vec)
     (let ((len (u16vector-length vec)))
       (do ((i 0 (+ i 1)))
           ((= i len))
         (u16vector-set! vec i (read-binary-uint16-le port)))))
    ((u32vector? vec)
     (let ((len (u32vector-length vec)))
       (do ((i 0 (+ i 1)))
           ((= i len))
         (u32vector-set! vec i (read-binary-uint32-le port)))))
    (else
     (error 'read-block! "unsupported type" vec))))

(define (with-string-io* s thunk)
  (with-output-to-string
    (lambda ()
      (with-input-from-port (if (string? s) (open-input-string s) s)
        thunk))))

(define (display-utf8 x)
  (if (char? x)
      (write-utf8-char x)
      (display x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple case conversions

(define *char-case-file-1* "case-map-1.dat")

(define *char-case-table-1*
  (or (condition-case
	  (and-let* ((file (find-data-file *char-case-file-1*))
		     (size (file-size file))
		     (vec (make-u32vector (quotient size 4))))
	    (call-with-input-file file
	      (cut read-block! vec <>) #:binary)
	    vec)
	(var () #f))
      (begin
	(warning "couldn't load case-map-1.dat")
	(make-u32vector 0))))

(define *char-case-count-1*
  (- (quotient (u32vector-length *char-case-table-1*) 4) 1))

(define (char-case-index tab i)
  (if (zero? (u32vector-length tab))
    0
    (do ((j 0 (+ j 4)))
        ((>= (u32vector-ref tab j) i) (quotient j 4)))))

(define (char-case-search tab i off . opt)
  (let-optionals* opt ((lo 0) (hi *char-case-count-1*))
    (and
     (>= hi lo)
     (cond
       ((= i (u32vector-ref tab (* lo 4)))
        (u32vector-ref tab (+ (* lo 4) off)))
       ((= i (u32vector-ref tab (* hi 4)))
        (u32vector-ref tab (+ (* hi 4) off)))
       (else
        (let loop ((a lo) (b hi))
          (if (= a b)
            #f
            (let* ((mid (+ a (quotient (- b a) 2)))
                   (ind (* mid 4))
                   (val (u32vector-ref tab ind)))
              (cond ((< i val) (if (= mid b) #f (loop a mid)))
                    ((> i val) (if (= mid a) #f (loop mid b)))
                    (else (u32vector-ref tab (+ ind off))))))))))))

;; just inline these two indexes for speed
(define *index-2500* (char-case-index *char-case-table-1* #x2500))
(define *index-FF20* (char-case-index *char-case-table-1* #xFF20))

(define (char-map-single-case i off)
  (cond ((< i 128) #f)
        ((< i #x2500)
         (and-let* ((j (char-case-search *char-case-table-1*
                                         i off 0 *index-2500*)))
           (ucs->char j)))
        ((> i #xFF20)
         (and-let* ((j (char-case-search *char-case-table-1*
                                         i off *index-FF20*
                                         *char-case-count-1*)))
           (ucs->char j)))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special casing

(define *char-case-file-2* "case-map-2.dat")

(define *char-case-table-2*
  (or (and-let* ((file (find-data-file *char-case-file-2*)))
	(condition-case
	    (with-input-from-file file read)
	  (var () #f)))
      (begin
	(warning "couldn't load case-map-2.dat")
	'#())))

(define *char-case-length-2* (vector-length *char-case-table-2*))

(define (char-map-multi-case i off)
  (let loop ((a 0) (b *char-case-length-2*))
    (if (= a b)
      #f
      (let* ((mid (+ a (quotient (- b a) 2)))
             (vec (vector-ref *char-case-table-2* mid))
             (val (vector-ref vec 0)))
        (cond ((< i val) (if (= mid b) #f (loop a mid)))
              ((> i val) (if (= mid a) #f (loop mid b)))
              (else (vector-ref vec off)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface

;; returns a single char
(define (char-upcase-single c)
  (let ((i (char->ucs c)))
    (if (< i 128)
      (char-upcase c)
      (or (char-map-single-case i 1) c))))
(define (char-downcase-single c)
  (let ((i (char->ucs c)))
    (if (< i 128)
      (char-downcase c)
      (or (char-map-single-case i 2) c))))
(define (char-titlecase-single c)
  (let ((i (char->ucs c)))
    (if (< i 128)
      (char-upcase c)
      (or (char-map-single-case i 3) c))))

;; may return a char or string
(define (char-downcase* c)
  (or (char-map-multi-case (char->ucs c) 1)
      (char-downcase-single c)))
(define (char-titlecase* c)
  (or (char-map-multi-case (char->ucs c) 2)
      (char-titlecase-single c)))
(define (char-upcase* c)
  (or (char-map-multi-case (char->ucs c) 3)
      (char-upcase-single c)))

(define (lang? opt . args)
  (and (pair? opt)
       (let ((lang (car opt)))
         (and (>= (string-length lang) 2)
              (let lp ((ls args))
                (and (pair? ls)
                     (or (let ((lang2 (car ls)))
                           (and (eqv? (string-ref lang 0)
                                      (string-ref lang2 0))
                                (eqv? (string-ref lang 1)
                                      (string-ref lang2 1))))
                         (lp (cdr ls)))))))))

(define grave-accent (char->utf8-string (ucs->char #x0300)))
(define acute-accent (char->utf8-string (ucs->char #x0301)))
(define tilde-accent (char->utf8-string (ucs->char #x0303)))
(define dot-above (char->utf8-string (ucs->char #x0307)))
(define dotted-capital-i (ucs->char #x0130))
(define dotless-small-i (ucs->char #x0131))
(define dotted-small-i (string-append "i" dot-above))
(define dotted-small-i/grave
  (string-append "i" dot-above grave-accent))
(define dotted-small-i/acute
  (string-append "i" dot-above acute-accent))
(define dotted-small-i/tilde
  (string-append "i" dot-above tilde-accent))
(define small-final-sigma
  (ucs->char #x03C2))
(define small-sigma
  (ucs->char #x03C3))

;; takes an optional locale string
(define (utf8-string-upcase str . opt)
  (with-string-io* str
    (lambda ()
      (if (lang? opt "tr" "az")
        (let loop ((c (read-utf8-char)))
          (unless (eof-object? c)
            (display-utf8
             (if (eqv? c #\i) dotted-capital-i (char-upcase* c)))
            (loop (read-utf8-char))))
        (let loop ((c (read-utf8-char)))
          (unless (eof-object? c)
            (display-utf8 (char-upcase* c))
            (loop (read-utf8-char))))))))

(define (char-downcase-locale c next opt)
  (or
   (case (char->ucs c)
     ;; Final Sigma
     ((#x03A3) (if (and (char? next)
                        (char-set-contains? char-set:greek next))
                 small-sigma
                 small-final-sigma))
     ;; Lithuanian (XXXX add More_Above logic)
     ((#x00CC) (and (lang? opt "lt") dotted-small-i/grave))
     ((#x00CD) (and (lang? opt "lt") dotted-small-i/acute))
     ((#x0128) (and (lang? opt "lt") dotted-small-i/tilde))
     ;; Turkish and Azeri
     ((#x0130) (if (lang? opt "tr" "az") #\i dotted-small-i))
     ((#x0307) (and (lang? opt "tr" "az") ""))
     ((#x0049) (and (lang? opt "tr" "az") dotless-small-i))
     (else #f))
   (char-downcase* c)))

(define (utf8-string-downcase str . opt)
  (with-string-io* str
    (lambda ()
      (let loop ((c (read-utf8-char)))
        (unless (eof-object? c)
          (let ((next (read-utf8-char)))
            (display-utf8 (char-downcase-locale c next opt))
            (loop next)))))))

;; Note: there are some characters which define case mappings (such as
;; the circled latin letters), but which unicode doesn't consider
;; alphabetic.  So the faster and more natural test for the alphabetic
;; property doesn't work, and we somewhat clumsily test whether or not
;; the characters are either upper or lowercase.
;;
;; An alternative approach is to explicitly compare the script property
;; of successive characters and start a new word when that property
;; changes.  So a consecutive string of Greek letters followed
;; immediately by Latin characters would result in the first Greek
;; letter and first Latin character being uppercased, as opposed to just
;; the first Greek letter as we do now.
(define (has-case? c)
  ;;(char-set-contains? char-set:alphabetic c)
  (or (char-set-contains? char-set:uppercase c)
      (char-set-contains? char-set:lowercase c)))

(define (utf8-string-titlecase str . opt)
  (with-string-io* str
    (lambda ()
      (letrec
          ((in-word
            (lambda (c)
              (unless (eof-object? c)
                (let ((next (read-utf8-char)))
                  (display-utf8 (char-downcase-locale c next opt))
                  (if (has-case? c)
                    (in-word next)
                    (out-word next))))))
           (out-word
            (lambda (c)
              (unless (eof-object? c)
                (let ((next (read-utf8-char)))
                  (cond
                    ((has-case? c)
                     (display-utf8
                      (if (eqv? c #\i)
                        (if (lang? opt "tr" "az") dotted-capital-i #\I)
                        (char-titlecase* c)))
                     (in-word next))
                    (else
                     (display-utf8 c)
                     (out-word next))))))))
        (out-word (read-utf8-char))))))

)
