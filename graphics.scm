(use sdl2
     cairo
     clojurian-syntax
     miscmacros
     vector-lib)

(include "proto.scm")

(define width 960)
(define height 540)

;; Library

(define (sdl-pixel-format-to-cairo fmt)
  (case (pixel-format-format fmt)
    ((argb8888) CAIRO_FORMAT_ARGB32)
    ((rgb24)    CAIRO_FORMAT_RGB24)
    ((rgb565)   CAIRO_FORMAT_RGB16_565)
    (else
     (error "unsupported pixel format"))))

(define (mouse-button-down-event? ev)
  (and (mouse-button-event? ev)
       (mouse-button-event-state ev)))

(define objects-position
  (make-parameter
   (list->vector
    (list-tabulate num-objects
                   (lambda (_) (list (random width) (random height)))))))

(define (find-object-at x y)
  (vector-index
   (lambda (obj)
     (let ((ox (first obj))
           (oy (second obj)))
       (and (< ox x (+ ox 50))
            (< oy y (+ oy 50)))))
   (objects-position)))

(define *user-choices* '())
(define (register-choice! n)
  (when (and n (< (length *user-choices*) num-choices))
    (set! *user-choices*
      (cons n (delete n *user-choices*)))))

(define (square ctx x y)
  (cairo-rectangle *cairo* x y 50 50))
(define (show-object num)
  (apply cairo-set-source-rgb *cairo*
         (if (member num *user-choices*) '(0 1 0) '(1 0 0)))
  (apply square *cairo* (vector-ref (objects-position) num))
  (cairo-fill *cairo*))

(define (handle-events!)
  (let loop ((ev (poll-event!)))
    (and (mouse-button-down-event? ev)
         (register-choice!
          (find-object-at
           (mouse-button-event-x ev)
           (mouse-button-event-y ev))))
    (and ev (loop (poll-event!)))))

(define (check-user-choices! cont)
  (let ((num-good (interact *user-choices*)))
    (print "Good objects: " num-good)
    (set! *user-choices* '())
    (if (= num-good num-choices)
        (print "You win!")
        (cont))))

;; ===

(set-main-ready!)
(init!)

(define *win*
  (create-window! "Blah" 'undefined 'undefined width height))

(define *screen* (window-surface *win*))

(define *c-surface*
  (cairo-image-surface-create-for-data
   (surface-pixels-raw *screen*)
   (sdl-pixel-format-to-cairo
    (surface-format *screen*))
   width
   height
   (surface-pitch *screen*)))

(define *cairo* (cairo-create *c-surface*))

(let loop ()
  (handle-events!)
  (dotimes (i num-objects) (show-object i))
  (update-window-surface! *win*)
  (if (= (length *user-choices*) num-choices)
      (check-user-choices! loop)
      (loop)))
