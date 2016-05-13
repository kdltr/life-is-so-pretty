;;;; utf8-srfi-13.scm -- Unicode-aware SRFI-13
;;
;; Copyright (c) 2004-2009 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(declare
  (no-procedure-checks)
  (bound-to-procedure
    ##sys#substring ##sys#become!))

(require-library utf8-lolevel utf8-srfi-14 iset utf8-case-map)

(module
 utf8-srfi-13
 (
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; srfi-13
  ;; predicates
  string-null? string-every string-any
  ;; constructors
  string-tabulate
  ;; selection
  string-copy string-copy! substring/shared
  string-take string-take-right string-drop string-drop-right
  string-pad string-pad-right string-trim string-trim-right string-trim-both
  ;; comparison
  string-compare string-compare-ci string-hash string-ci-hash
  string= string<> string< string> string<= string>=
  string-ci= string-ci<> string-ci< string-ci> string-ci<= string-ci>=
  ;; prefixes & suffixes
  string-prefix? string-prefix-ci? string-prefix-length string-prefix-length-ci
  string-suffix? string-suffix-ci? string-suffix-length string-suffix-length-ci
  ;; searching
  string-index string-index-right string-skip string-skip-right string-count
  string-contains string-contains-ci
  ;; alphabetic case maping
  string-titlecase string-titlecase! string-upcase string-upcase!
  string-downcase string-downcase!
  ;; reverse & append
  string-join string-reverse string-reverse! string-concatenate
  string-concatenate/shared string-append/shared
  string-concatenate-reverse string-concatenate-reverse/shared
  ;; iteration
  string-map string-map! string-fold string-fold-right
  string-unfold string-unfold-right string-for-each string-for-each-index
  ;; replicate & rotate
  xsubstring string-xcopy! string-replace string-tokenize
  ;; filtering & deleting
  string-filter string-delete
  )

(import (except scheme string-copy) chicken data-structures ports
        (except srfi-69 string-hash string-ci-hash)
        utf8-lolevel utf8-srfi-14 iset utf8-case-map)

(define (string-null? s) (equal? s ""))

(define (string-fold kons knil s . opt)
  (let-optionals* opt ((start 0) (end (utf8-string-length s)))
    (let lp ((i start) (b (utf8-index->offset s start)) (acc knil))
      (if (>= i end)
          acc
          (lp (+ i 1)
              (+ b (utf8-start-byte->length (string-int-ref s b)))
              (kons (sp-ref s b) acc))))))

(define (string-fold-right kons knil s . opt)
  (let-optionals* opt ((start 0) (end (utf8-string-length s)))
    (let lp ((i (- end 1))
             (b (utf8-prev-char s (utf8-index->offset s end)))
             (acc knil))
      (if (< i start)
          acc
          (lp (- i 1)
              (utf8-prev-char s b)
              (kons (sp-ref s b) acc))))))

(define (string-unfold p f g seed . opt)
  (let-optionals* opt ((base "")
                       (make-final (lambda (x) "")))
    (let ((out (open-output-string)))
      (display base out) ; base must be a string, so normal display is fine
      (let lp ((seed seed))
        (if (p seed) 
            (display (make-final seed) out)
            (begin
              (write-utf8-char (f seed) out)
              (lp (g seed)))))
      (get-output-string out))))

(define (string-unfold-right p f g seed . opt)
  (let-optionals* opt ((base "")
                       (make-final (lambda (x) "")))
    (let lp ((seed seed) (ans (list base)))
      (if (p seed) 
          (string-intersperse (cons (make-final seed) ans) "")
          (lp (g seed) (cons (char->utf8-string (f seed)) ans))))))

(define (string-map proc s . opt)
  (string-intersperse
   (reverse
    (map char->utf8-string
         (apply string-fold (lambda (c acc) (cons (proc c) acc)) '() s opt)))
   ""))

(define (string-map! proc s . opt)
  (let ((res (apply string-map proc s opt)))
    (##sys#become! (list (cons s res)))
    ;; The SRFI-13 result is unspecified, return the updated string
    ;; anyway for backwards-compatible with earlier versions of this
    ;; library and in the hope of encouraging linear-update APIs.
    res))

(define (string-for-each proc s . opt)
  (apply string-fold (lambda (c acc) (proc c)) #f s opt)
  (if #f #f))

(define (string-for-each-index proc s . opt)
  (let ((start (if (pair? opt) (car opt) 0))
        (end (if (and (pair? opt) (pair? (cdr opt)))
               (cadr opt)
               (utf8-string-length s))))
    (do ((i start (+ i 1)))
        ((= i end))
      (proc i))))

(define (char-predicate x)
  (cond ((procedure? x) x)
        ((char? x) (lambda (c) (eqv? c x)))
        ((char-set? x) (lambda (c) (char-set-contains? x c)))
        (else (error "unknown predicate" x))))

(define (string-any x str . opt)
  (let ((pred (char-predicate x)))
    (call-with-current-continuation
     (lambda (return)
       (apply string-fold
              (lambda (c acc) (cond ((pred c) => return) (else #f)))
              #f str opt)))))

(define (string-every x str . opt)
  (let ((pred (char-predicate x)))
    (call-with-current-continuation
     (lambda (return)
       (apply string-fold
              (lambda (c acc) (cond ((pred c) => identity) (else (return #f))))
              #f str opt)))))

(define (string-tabulate proc len)
  (let ((out (open-output-string)))
    (let lp ((i 0))
      (cond
        ((< i len)
         (write-utf8-char (proc i) out)
         (lp (+ i 1)))))
    (get-output-string out)))

(define (string-copy s . opt)
  (with-substring-offsets (lambda (s start end) (##sys#substring s start end)) s opt))

(define (substring/shared s . opt)
  (with-substring-offsets (lambda (s start end) (##sys#substring s start end)) s opt))

(define (byte-string-copy! target t-off str start end)
  (if (> start t-off)
      (do ((i start (+ i 1))
           (j t-off (+ j 1)))
          ((>= i end))
        (string-set! target j (string-ref str i)))
      (do ((i (- end 1) (- i 1))
           (j (+ -1 t-off (- end start)) (- j 1)))
          ((< i start))
        (string-set! target j (string-ref str i)))))

(define (string-copy! target tstart str . opt)
  (let-optionals* opt ((start 0) (end (utf8-string-length str)))
    (let* ((str (utf8-substring str start end))
           (len (- end start))
           (s-size (string-length str))
           (t-total-size (string-length target))
           (t-off (utf8-index->offset target tstart))
           (t-end-off (utf8-index->offset target (+ tstart len)))
           (t-size (- t-end-off t-off)))
      (if (= s-size t-size)
        (byte-string-copy! target t-off str 0 len)
        (let ((res (string-append
                    (##sys#substring target 0 t-off)
                    str
                    (##sys#substring target t-end-off t-total-size))))
          (##sys#become! (list (cons target res))))))))

(define (string-take s n) (utf8-substring s 0 n))
(define (string-drop s n) (utf8-substring s n))
(define (string-take-right s n) (utf8-substring s (- (utf8-string-length s) n)))
(define (string-drop-right s n) (utf8-substring s 0 (- (utf8-string-length s) n)))

(define (string-pad s len . opt)
  (let-optionals* opt ((ch #\space) (start 0) (end (utf8-string-length s)))
    (let ((diff (- end start)))
      (if (<= len diff)
        (utf8-substring s (- end len) end)
        (string-append (make-utf8-string (- len diff) ch)
                       (utf8-substring s start end))))))

(define (string-pad-right s len . opt)
  (let-optionals* opt ((ch #\space) (start 0) (end (utf8-string-length s)))
    (let ((diff (- end start)))
      (if (<= len diff)
        (utf8-substring s start (+ start len))
        (string-append (utf8-substring s start end)
                       (make-utf8-string (- len diff) ch))))))

(define (string-trim s . opt)
  (let-optionals* opt ((trimmer char-set:whitespace) (start 0) (end #f))
    (let* ((pred (char-predicate trimmer))
           (end-off
            (if end (utf8-index->offset s end) (string-length s))))
      (let lp ((i (utf8-index->offset s start)))
        (if (or (>= i end-off) (not (pred (sp-ref s i))))
            (##sys#substring s i end-off)
            (lp (sp-next s i)))))))

(define (string-trim-right s . opt)
  (let-optionals* opt ((trimmer char-set:whitespace) (start 0) (end #f))
    (let ((pred (char-predicate trimmer))
          (end-off
           (if end (utf8-index->offset s end) (string-length s))))
      (let lp ((i (sp-prev s end-off)) (j end-off))
        (if (or (negative? i) (not (pred (sp-ref s i))))
            (##sys#substring s (utf8-index->offset s start) j)
            (lp (sp-prev s i) i))))))

(define (string-trim-both s . opt)
  (let-optionals* opt ((trimmer char-set:whitespace))
    (string-trim (apply string-trim-right s opt) trimmer)))

;; alas, can't use string-compare3 because the predicates get the
;; index as an argument

(define (string-compare s1 s2 proc< proc= proc> . opt)
  (with-two-substring-offsets
      (lambda (s1 s2 start1 end1 start2 end2)
        (let lp ((i start1) (j start2))
          (cond
            ((>= i end1)
             ((if (>= j end2) proc= proc<)
              (utf8-offset->index s1 i)))
            ((>= j end2)
             (utf8-offset->index s1 i))
            ((char<? (string-ref s1 i) (string-ref s2 i))
             (proc< (utf8-offset->index s1 i)))
            ((char>? (string-ref s1 i) (string-ref s2 i))
             (proc> (utf8-offset->index s1 i)))
            (else
             (lp (+ i 1) (+ j 1))))))
      s1 s2 opt))

(define (string-compare-ci s1 s2 proc< proc= proc> . opt)
  (with-two-substring-offsets
      (lambda (s1 s2 start1 end1 start2 end2)
        (let lp ((i start1) (j start2))
          (cond
            ((>= i end1)
             ((if (>= j end2) proc= proc<)
              (utf8-offset->index s1 i)))
            ((>= j end2)
             (utf8-offset->index s1 i))
            ((char-ci<? (string-ref s1 i) (string-ref s2 i))
             (proc< (utf8-offset->index s1 i)))
            ((char-ci>? (string-ref s1 i) (string-ref s2 i))
             (proc> (utf8-offset->index s1 i)))
            (else
             (lp (+ i 1) (+ j 1))))))
      s1 s2 opt))

(define (make-string-comparator proc pred)
  (lambda (s1 s2 . opt)
    (if (null? opt)
        (pred (proc s1 s2))
        (pred (with-two-substring-offsets
                  (lambda (s1 s2 start1 end1 start2 end2)
                    (proc (##sys#substring s1 start1 end1)
                          (##sys#substring s2 start2 end2)))
                  s1 s2 opt)))))

(define string= (make-string-comparator string-compare3 zero?))
(define string<> (make-string-comparator string-compare3 (complement zero?)))
(define string< (make-string-comparator string-compare3 negative?))
(define string> (make-string-comparator string-compare3 positive?))
(define string<= (make-string-comparator string-compare3 (complement positive?)))
(define string>= (make-string-comparator string-compare3 (complement negative?)))

(define string-ci= (make-string-comparator string-compare3-ci zero?))
(define string-ci<> (make-string-comparator string-compare3-ci (complement zero?)))
(define string-ci< (make-string-comparator string-compare3-ci negative?))
(define string-ci> (make-string-comparator string-compare3-ci positive?))
(define string-ci<= (make-string-comparator string-compare3-ci (complement positive?)))
(define string-ci>= (make-string-comparator string-compare3-ci (complement negative?)))

(define (utf8-substring-length s start . opt)
  (let ((end (if (pair? opt) (car opt) (string-length s))))
    (let lp ((i start) (res 0))
      (if (>= i end)
          res
          (lp (+ i (utf8-start-byte->length (string-int-ref s i)))
              (+ res 1))))))

(define (make-string-fix-length proc)
  (lambda (s1 s2 . opt)
    (with-two-substring-offsets
     (lambda (s1 s2 start1 end1 start2 end2)
       (let ((res (proc s1 s2 start1 start2 end1 end2)))
         (if (zero? res)
             res
             (utf8-substring-length s1 start1 (+ start1 res)))))
     s1 s2 opt)))

(define (byte-string-prefix-length s1 s2 start1 start2 end1 end2)
  (let lp ((i start1) (j start2))
    (cond
      ((>= i end1) (- i start1))
      ((>= j end2) (- j start2))
      ((char=? (string-ref s1 i) (string-ref s2 j))
       (lp (+ i 1) (+ j 1)))
      (else (- i start1)))))

(define (byte-string-prefix-length-ci s1 s2 start1 start2 end1 end2)
  (let lp ((i start1) (j start2))
    (cond
      ((>= i end1) (- i start1))
      ((>= j end2) (- j start2))
      ((char-ci=? (string-ref s1 i) (string-ref s2 j))
       (lp (+ i 1) (+ j 1)))
      (else (- i start1)))))

(define (byte-string-suffix-length s1 s2 start1 start2 end1 end2)
  (let lp ((i (- end1 1)) (j (- end2 1)))
    (cond
      ((< i start1) (- end1 i 1))
      ((< j start2) (- end2 j 1))
      ((char=? (string-ref s1 i) (string-ref s2 j))
       (lp (- i 1) (- j 1)))
      (else (- end1 i 1)))))

(define (byte-string-suffix-length-ci s1 s2 start1 start2 end1 end2)
  (let lp ((i (- end1 1)) (j (- end2 1)))
    (cond
      ((< i start1) (- end1 i 1))
      ((< j start2) (- end2 j 1))
      ((char-ci=? (string-ref s1 i) (string-ref s2 j))
       (lp (- i 1) (- j 1)))
      (else (- end1 i 1)))))

(define string-prefix-length
  (make-string-fix-length byte-string-prefix-length))
(define string-prefix-length-ci
  (make-string-fix-length byte-string-prefix-length-ci))
(define string-suffix-length
  (make-string-fix-length byte-string-suffix-length))
(define string-suffix-length-ci
  (make-string-fix-length byte-string-suffix-length-ci))

(define (make-string-prefix-test proc)
  (lambda (s1 s2 . opt)
    (cond
     ((null? opt)
      (and (<= (string-length s1) (string-length s2)) (proc s1 s2)))
     (else
      (with-two-substring-offsets
       (lambda (s1 s2 start1 end1 start2 end2)
         (let ((s1-len (- end1 start1))
               (s2-len (- end2 start2)))
           (and (<= s1-len s2-len) (proc s1 s2 start1 start2 s1-len))))
       s1 s2 opt)))))

(define string-prefix? (make-string-prefix-test substring=?))
(define string-prefix-ci? (make-string-prefix-test substring-ci=?))

(define (string-suffix? s1 s2 . opt)
  (with-two-substring-offsets
      (lambda (s1 s2 start1 end1 start2 end2)
        (and (>= (- end2 start2) (- end1 start1))
             (let lp ((i (- end1 1)) (j (- end2 1)))
               (or (< i start1)
                   (if (char=? (string-ref s1 i) (string-ref s2 j))
                       (lp (- i 1) (- j 1))
                       #f)))))
      s1 s2 opt))

(define (string-suffix-ci? s1 s2 . opt)
  (with-two-substring-offsets
      (lambda (s1 s2 start1 end1 start2 end2)
        (and (>= (- end2 start2) (- end1 start1))
             (let lp ((i (- end1 1)) (j (- end2 1)))
               (or (< i start1)
                   (if (char-ci=? (string-ref s1 i) (string-ref s2 j))
                       (lp (- i 1) (- j 1))
                       #f)))))
      s1 s2 opt))

(define (make-string-hasher proc)
  (lambda (s . opt)
    (cond
      ((null? opt)
       (proc s))
      ((null? (cdr opt))
       (proc s (car opt)))
      (else
       (with-substring-offsets
           (lambda (s start end)
             (proc (##sys#substring s start end) (car opt)))
           s (cdr opt))))))

(define string-hash
  (make-string-hasher hash))
(define string-ci-hash
  (make-string-hasher (lambda (s) (hash (string-downcase s)))))

(define (with-string-index+offset proc s x . opt)
  (if (equal? s "")
      (proc #f #f)
      (let-optionals* opt ((start 0) (end -1))
        (let ((size (string-length s))
              (pred (char-predicate x)))
          (let lp ((i start) (off (utf8-index->offset s start)))
            (if (or (= i end) (= off size))
                (proc #f #f)
                (let ((ch (sp-ref s off)))
                  (if (pred ch)
                      (proc i off)
                      (lp (+ i 1)
                          (+ off (ucs-integer->length
                                  (char->integer ch))))))))))))

(define (with-string-index+offset-right proc s x . opt)
  (if (equal? s "")
      (proc #f #f)
      (let-optionals* opt ((start 0) (end (utf8-string-length s)))
        (let ((pred (char-predicate x)))
          (if (>= start end)
              (proc #f #f)
              (let lp ((i (- end 1)) (off (utf8-index->offset s (- end 1))))
                (if (< i start)
                    (proc #f #f)
                    (let ((ch (sp-ref s off)))
                      (if (pred ch)
                          (proc i off)
                          (if (zero? i)
                              (lp -1 -1)
                              (lp (- i 1) (utf8-prev-char s off))))))))))))

(define (arg1 a b) a)
;;(define (arg2 a b) b)

(define (string-index s x . opt)
  (apply with-string-index+offset arg1 s x opt))
;; (define (string-offset s x . opt)
;;   (apply with-string-index+offset arg2 s x opt))
(define (string-index-right s x . opt)
  (apply with-string-index+offset-right arg1 s x opt))
;; (define (string-offset-right s x . opt)
;;   (apply with-string-index+offset-right arg2 s x opt))

(define (string-skip s x . opt)
  (apply string-index s (complement (char-predicate x)) opt))
(define (string-skip-right s x . opt)
  (apply string-index-right s (complement (char-predicate x)) opt))

(define (string-count s x . opt)
  (let ((pred (char-predicate x)))
    (apply string-fold (lambda (c sum) (if (pred c) (+ sum 1) sum)) 0 s opt)))

;; cleaner to loop ourselves, but the byte-oriented substring-index
;; uses memcmp directly, so we go out of our way to make use of that,
;; while avoiding substring if at all possible
(define (string-contains s1 s2 . opt)
  (define (return offset index)
    (and offset (+ (utf8-offset->index s1 offset) index)))
  (if (null? opt)
      (return (substring-index s2 s1) 0)
      (let* ((start1-index (car opt))
             (opt (cdr opt))
             (start1 (utf8-index->offset s1 start1-index)))
        (if (null? opt)
            (return (substring-index s2 s1 start1) 0)
            (let* ((end1 (utf8-index->offset s1 (car opt)))
                   (opt (cdr opt))
                   (s2 (if (null? opt)
                           s2
                           (with-substring-offsets ##sys#substring s2 opt))))
              (if (= end1 (string-length s1))
                  (return (substring-index s2 s1 start1) 0)
                  (return (substring-index
                           s2
                           (##sys#substring s1 start1 end1))
                          start1-index)))))))

;; XXXX consider using full unicode case mappings
(define (string-contains-ci s1 s2 . opt)
  (define (return offset index)
    (and offset (+ (utf8-offset->index s1 offset) index)))
  (if (null? opt)
      (return (substring-index s2 s1) 0)
      (let* ((start1-index (car opt))
             (opt (cdr opt))
             (start1 (utf8-index->offset s1 start1-index)))
        (if (null? opt)
            (return (substring-index-ci s2 s1 start1) 0)
            (let* ((end1 (utf8-index->offset s1 (car opt)))
                   (opt (cdr opt))
                   (s2 (if (null? opt)
                           s2
                           (with-substring-offsets ##sys#substring s2 opt))))
              (if (= end1 (string-length s1))
                  (return (substring-index-ci s2 s1 start1) 0)
                  (return (substring-index-ci
                           s2
                           (##sys#substring s1 start1 end1))
                          start1-index)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case mapping

(define string-titlecase utf8-string-titlecase)
(define string-titlecase! utf8-string-titlecase)
(define string-downcase utf8-string-downcase)
(define string-downcase! utf8-string-downcase)
(define string-upcase utf8-string-upcase)
(define string-upcase! utf8-string-upcase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reverse & append

(define (%string-reverse s start end)
  (let* ((len (- end start))
         (res (make-string len)))
    (let lp ((i end)
             (j 0))
      (let ((i2 (utf8-prev-char s i)))
        (if (not i2)
            res
            (let lp2 ((i3 i2) (j j))
              (if (eqv? i3 i)
                  (lp i2 j)
                  (begin
                    (string-set! res j (string-ref s i3))
                    (lp2 (+ i3 1) (+ j 1))))))))))

(define (string-reverse s . opt)
  (with-substring-offsets %string-reverse s opt))

(define (string-reverse! s . opt)
  (with-substring-offsets
   (lambda (s start end)
     (let ((s2 (%string-reverse s start end)))
       (byte-string-copy! s start s2 0 (string-length s2))))
   s opt))

(define (string-join ls . opt)
  (let-optionals* opt ((delim " ")
                       (grammar 'infix))
    (case grammar
      ((infix strict-infix)
       (if (and (eq? grammar 'strict-infix) (null? ls))
           (error "strict-infix grammar requires a non-empty list"))
       (string-intersperse ls delim))
      ((prefix)
       (if (null? ls)
           ""
           (string-intersperse (cons delim (intersperse ls delim)) "")))
      ((suffix)
       (if (null? ls)
           ""
           (string-intersperse (append (intersperse ls delim) (list delim)) "")))
      (else
       (error "unknown string-join grammar" grammar)))))

(define string-append/shared string-append)
(define (string-concatenate ls) (string-intersperse ls ""))
(define (string-concatenate-reverse ls) (string-intersperse (reverse ls) ""))
(define string-concatenate-reverse/shared string-concatenate-reverse)
(define string-concatenate/shared string-concatenate)

(define (xsubstring s1 from . opt)
  (let-optionals* opt ((to1 #f) (start 0) (end (utf8-string-length s1)))
    (let* ((s (utf8-substring s1 start end))
           (len (- end start))
           (to (or to1 (+ from len)))
           (out (open-output-string)))
      (let lp ((i from))
        (cond
          ((< i to)
           (write-utf8-char (utf8-string-ref s (modulo i len)) out)
           (lp (+ i 1)))))
      (get-output-string out))))

(define (string-xcopy target tstart s from . opt)
  (let-optionals* opt ((to1 #f) (start 0) (end (utf8-string-length s)))
    (let ((to (or to1 (+ from (- end start)))))
      (string-append (utf8-substring target 0 tstart)
                     (xsubstring s from to start end)
                     (utf8-substring target
                                (+ tstart (- to from))
                                (utf8-string-length target))))))

(define (string-xcopy! target tstart s from . opt)
  (let ((res (apply string-xcopy target tstart s from opt)))
    (##sys#become! (list (cons target res)))))

(define (string-filter filt s . opt)
  (let ((pred (char-predicate filt)))
    (with-output-to-string
      (lambda ()
        (string-for-each
         (lambda (c) (if (pred c) (write-utf8-char c)))
         (if (pair? opt)
             (apply utf8-substring s opt)
             s))))))

(define (string-delete filt s . opt)
  (let ((pred (char-predicate filt)))
    (apply string-filter (lambda (c) (not (pred c))) s opt)))

(define (string-replace s1 s2 start1 end1 . opt)
  (let ((start1 (utf8-index->offset s1 start1))
        (end1 (utf8-index->offset s1 end1)))
    (with-substring-offsets
     (lambda (s2 start2 end2)
       (string-append (##sys#substring s1 0 start1)
                      (##sys#substring s2 start2 end2)
                      (##sys#substring s1 end1 (string-length s1))))
     s2 opt)))

(define (string-tokenize s . opt)
  (let-optionals* opt ((token-set char-set:graphic)
                       o2)
    (with-substring-offsets
     (lambda (s start end)
       (letrec
           ((out
             (lambda (sp res)
               (cond
                 ((>= sp end)
                  (reverse res))
                 ((char-set-contains? token-set (sp-ref s sp))
                  (in (sp-next s sp) sp res))
                 (else
                  (out (sp-next s sp) res)))))
            (in
             (lambda (sp from res)
               (cond
                 ((>= sp end)
                  (reverse (cons (sp-substring s from end) res)))
                 ((char-set-contains? token-set (sp-ref s sp))
                  (in (sp-next s sp) from res))
                 (else
                  (out (sp-next s sp) (cons (sp-substring s from sp) res)))))))
         (out start '())))
     s o2)))

)
