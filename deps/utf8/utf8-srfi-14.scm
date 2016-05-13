;;;; utf8-srfi-14.scm -- Unicode capable char-sets
;;
;; Copyright (c) 2004-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(declare
  (no-procedure-checks))

(module
 utf8-srfi-14
 (
  ;; srfi-14
  char-set char-set? char-set-copy char-set-hash
  list->char-set list->char-set! string->char-set string->char-set!
  ->char-set char-set->list char-set->string char-set= char-set<= char-set>=
  char-set-empty? char-set-contains? char-set-adjoin char-set-adjoin!
  char-set-delete char-set-delete! char-set-filter char-set-filter!
  char-set-fold char-set-unfold char-set-unfold! char-set-for-each char-set-map
  char-set-every char-set-any char-set-size char-set-count
  char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
  char-set-union! char-set-union char-set-intersection! char-set-intersection
  char-set-difference! char-set-difference char-set-xor! char-set-xor
  char-set-diff+intersection! char-set-diff+intersection
  char-set-complement char-set-complement!
  ucs-range->char-set ucs-range->char-set!
  ;; standard char-sets
  char-set:lower-case char-set:upper-case char-set:title-case
  char-set:letter char-set:digit char-set:letter+digit
  char-set:graphic char-set:printing char-set:whitespace
  char-set:iso-control char-set:punctuation char-set:symbol
  char-set:hex-digit char-set:blank char-set:ascii
  char-set:empty char-set:full
  )

(import scheme chicken (only srfi-69 hash) iset utf8-lolevel)

(require-library srfi-69 utf8-lolevel iset)

(define char-set? iset?)
(define char-set-copy iset-copy)

(define char-set-union iset-union)
(define char-set-difference iset-difference)
(define (char-set-intersection . args)
  (if (null? args)
      char-set:full
      (apply iset-intersection args)))
(define char-set-xor iset-xor)
(define char-set-diff+intersection iset-diff+intersection)

(define (char-set-complement cs)
  (char-set-difference char-set:full cs))

(define char-set-union! iset-union!)
(define char-set-difference! iset-difference!)
(define (char-set-intersection! . args)
  (if (null? args)
      char-set:full
      (apply iset-intersection! args)))
(define char-set-xor! iset-xor!)
(define char-set-diff+intersection! iset-diff+intersection!)

(define char-set-complement! char-set-complement)

(define (char-set . args)
  (list->char-set args))

(define (list->char-set ls . opt)
  (apply list->iset (map char->integer ls) opt))

(define (list->char-set! ls base-is)
  (list->iset! (map char->integer ls) base-is))

(define (string->char-set! str base-cs)
  (let ((end (sp-last str)))
    (let lp ((i (sp-first str)))
      (if (>= i end)
        base-cs
        (let ((c (sp-ref str i)))
          (char-set-adjoin! base-cs c)
          (lp (sp-next str i)))))))

(define (string->char-set str . opt)
  (string->char-set! str (if (pair? opt) (iset-copy (car opt)) (make-iset))))

(define (char-set->list cs)
  (map integer->char (iset->list cs)))

(define (char-set->string cs)
  (let ((out (open-output-string)))
    (char-set-for-each (lambda (ch) (write-utf8-char ch out)) cs)
    (get-output-string out)))

(define char-set= iset=)
(define char-set<= iset<=)
(define char-set>= iset>=)

(define (char-set-hash cs . opt)
  (let ((bound (if (pair? opt) (car opt) 0)))
    (hash cs (if (zero? bound) (expt 2 29) bound))))

(define (char-set-contains? cs c)
  (iset-contains? cs (char->integer c)))

(define char-set-size iset-size)
(define char-set-empty? iset-empty?)

(define (->char-set x)
  (cond ((char-set? x) x)
        ((string? x) (string->char-set x))
        ((char? x) (char-set x))))

(define (char-set-fold kons knil cs)
  (iset-fold (lambda (i acc) (kons (integer->char i) acc)) knil cs))

(define (char-set-unfold f p g seed . opt)
  (apply iset-unfold (lambda (c) (char->integer (f c))) p g seed opt))

(define (char-set-unfold! f p g seed cs)
  (iset-unfold! (lambda (c) (char->integer (f c))) p g seed cs))

(define (char-set-map proc cs)
  (iset-map (lambda (i) (char->integer (proc (integer->char i)))) cs))

(define (char-set-for-each proc cs)
  (iset-for-each (lambda (i) (proc (integer->char i))) cs))

(define (char-set-every pred cs)
  (iset-every (lambda (i) (pred (integer->char i))) cs))

(define (char-set-any pred cs)
  (iset-any (lambda (i) (pred (integer->char i))) cs))

(define (char-set-filter! pred cs base-cs)
  (char-set-for-each
   (lambda (c) (if (pred c) (char-set-adjoin! base-cs c)))
   cs)
  base-cs)

(define (char-set-filter pred cs . opt)
  (if (pair? opt)
    (char-set-filter! pred cs (char-set-copy (car opt)))
    (char-set-filter! pred cs (make-iset))))

(define (char-set-count pred cs)
  (char-set-fold (lambda (c s) (if (pred c) (+ s 1) s)) 0 cs))

(define (char-set-adjoin! cs . args)
  (apply iset-adjoin! cs (map char->integer args)))

(define (char-set-delete! cs . args)
  (apply iset-delete! cs (map char->integer args)))

(define (char-set-adjoin cs . args)
  (apply char-set-adjoin! (char-set-copy cs) args))

(define (char-set-delete cs . args)
  (apply char-set-delete! (char-set-copy cs) args))

(define (ucs-range->char-set! lo hi error? base-cs)
  (iset-union! base-cs (make-iset lo (- hi 1))))

(define (ucs-range->char-set lo hi . opt)
  (let-optionals* opt ((error? #f) (base-is #f))
    (if base-is
      (iset-union base-is (make-iset lo (- hi 1)))
      (make-iset lo (- hi 1)))))

;; cursors

(define char-set-cursor iset-cursor)
(define char-set-cursor-next iset-cursor-next)
(define end-of-char-set? end-of-iset?)

(define (char-set-ref cset cur)
  (integer->char (iset-ref cset cur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default char-sets

(define char-set:lower-case #f)
(define char-set:upper-case #f)
(define char-set:title-case #f)
(define char-set:letter #f)
(define char-set:digit #f)
(define char-set:hex-digit #f)
(define char-set:letter+digit #f)
(define char-set:punctuation #f)
(define char-set:symbol #f)
(define char-set:whitespace #f)
(define char-set:blank #f)
(define char-set:graphic #f)
(define char-set:printing #f)
(define char-set:iso-control #f)
(define char-set:ascii #f)
(define char-set:empty #f)
(define char-set:full (make-iset 0 (- (expt 2 21) 1)))

(set! char-set:lower-case (make-iset 97 122))
(set! char-set:upper-case (make-iset 65 90))
(set! char-set:title-case (make-iset))
(set! char-set:letter
  (char-set-union char-set:lower-case char-set:upper-case))
(set! char-set:digit (make-iset 48 57))
(set! char-set:hex-digit
  (char-set-union char-set:digit (make-iset 65 70) (make-iset 97 102)))
(set! char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))
(set! char-set:punctuation (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}"))
(set! char-set:symbol (string->char-set "$+<=>^`|~"))
(set! char-set:whitespace (iset 9 10 11 12 13 32))
(set! char-set:blank (iset 9 32))
(set! char-set:graphic
  (char-set-union char-set:letter char-set:digit
                  char-set:punctuation char-set:symbol))
(set! char-set:printing
  (char-set-union char-set:graphic char-set:whitespace))
(set! char-set:iso-control
  (let ((s (make-iset 0 31))) (iset-adjoin! s 127) s))
(set! char-set:ascii (make-iset 0 127))
(set! char-set:empty (make-iset))

)
