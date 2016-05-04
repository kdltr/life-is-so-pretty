(use sdl2
     cairo
     clojurian-syntax
     miscmacros
     vector-lib)

(include "proto.scm")

(define width 960)
(define height 540)

;; Library

(define objects-position
  (make-parameter
   (list->vector
    (list-tabulate num-objects
                   (lambda (i) (list (+ 10 (* i (quotient 320 num-objects))) (- 150 50)))))))

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

(define (square x y)
  (render-fill-rect! *renderer* (make-rect x y 20 50)))
(define (show-object! num)
  (set! (render-draw-color *renderer*)
    (if (member num *user-choices*) (make-color 0 255 0) (make-color 255 0 0)))
  (apply square (vector-ref (objects-position) num)))
(define (show-objects!)
  (dotimes (i num-objects) (show-object! i)))

(define (handle-events!)
  (let loop ((ev (poll-event!)))
    (and (keyboard-event? ev)
         (or (and (eq? (keyboard-event-scancode ev) 'a)
                  (set! *player-left* (keyboard-event-state ev)))
             (and (eq? (keyboard-event-scancode ev) 'd)
                  (set! *player-right* (keyboard-event-state ev)))
             (and (eq? (keyboard-event-scancode ev) 'escape)
                  (exit 0))
             (and (eq? (keyboard-event-scancode ev) 'space)
                  (keyboard-event-state ev)
                  (let ((obj (find-object-at *player-position* (- 150 30))))
                    (set! *user-choices* (cons obj (delete obj *user-choices*)))))))
    (and ev (loop (poll-event!)))))

(define (check-user-choices! cont)
  (let ((num-good (interact *user-choices*)))
    (print "Good objects: " num-good)
    (set! *user-choices* '())
    (if (= num-good num-choices)
        (print "You win!")
        (cont))))

(define *player-left* #f)
(define *player-right* #f)
(define *player-position* 30)

(define (show-player!)
  (set! (render-draw-color *renderer*) (make-color 0 0 255))
  (render-fill-rect! *renderer* (make-rect (round *player-position*) (- 150 60) 30 60)))


;; ===

(set-main-ready!)
(init!)

(define *win*
  (create-window! "Blah" 'undefined 'undefined width height))

(define *renderer*
  (create-renderer! *win* -1 '(accelerated)))
(set! (render-logical-size *renderer*) '(320 180))

(define background-texture
  (create-texture-from-surface *renderer* (load-bmp "background.bmp")))

(let loop ((lt (get-ticks)))
  (let* ((ct (get-ticks))
         (dt (/ (- ct lt) 1000)))
    (handle-events!)
    (set! *player-position*
      (+ *player-position*
         (if *player-left* (- (* 50 dt)) 0)
         (if *player-right* (* 50 dt) 0)))
    (render-copy! *renderer* background-texture)
    (show-objects!)
    (show-player!)
    (render-present! *renderer*)
    (if (= (length *user-choices*) num-choices)
        (check-user-choices! (lambda () (loop ct)))
        (loop ct))))
