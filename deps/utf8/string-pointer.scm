;;;; string-pointer.scm -- mimic the interface provided by utf8
;;
;; Copyright (c) 2004-2007 Alex Shinn
;; All rights reserved.
;;
;; BSD-style license: http://www.debian.org/misc/bsd.license

(cond-expand
 (compiling
  (declare
   (fixnum)
   (usual-integrations)
   (export
    make-string-pointer string-pointer? sp-copy
    sp-first sp-last sp-next sp-prev sp-ref sp-set! sp->index
    sp-before sp-after sp-substring
    sp-check? sp-check-lo? sp-check-hi?
    )))
 (else
  ))

(define (make-string-pointer s . opt)
  (let ((i (if (pair? opt) (car opt) 0)))
    (if (negative? i)
      (modulo i (string-length s))
      i)))
(define (string-pointer? x) (integer? x))
(define (sp-copy sp) sp)
(define (sp-next sp . opt) (+ sp (if (pair? opt) (car opt) 1)))
(define (sp-prev sp . opt) (- sp (if (pair? opt) (car opt) 1)))
(define (sp-first s) 0)
(define (sp-last s) (string-length s))
(define (sp-ref s sp) (string-ref s sp))
(define (sp-set! s sp c) (string-set! s sp c))
(define (sp-before s sp) (substring s 0 sp))
(define (sp-after s sp) (substring s sp))
(define (sp-substring s sp1 sp2) (substring s sp1 sp2))
(define (sp->index s sp) sp)
(define (sp-check-lo? s sp) (not (negative? sp)))
(define (sp-check-hi? s sp) (< sp (string-length s)))
(define (sp-check? s sp)
  (and (sp-check-lo? s sp) (sp-check-hi? s sp)))

