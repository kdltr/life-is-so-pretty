(use sdl2
     cairo
     clojurian-syntax
     miscmacros
     vector-lib)

(include "proto.scm")

(define window-width 960)
(define window-height 540)

(define width 320)
(define height 180)

(define object-width 20)
(define object-height 50)

(define player-width 30)
(define player-height 60)
(define player-speed 70)

(define floor-y 150)

(define view-border (+ (/ player-width 2) 10))

;; Library

(define objects-position
  (make-parameter
   (list->vector
    (let loop ((n 0)
               (last-x 10))
      (if (= n num-objects)
          '()
          (cons last-x (loop (add1 n) (+ last-x object-width 10 (random 50)))))))))

(define room-width (+ 50 (vector-ref (objects-position) (sub1 num-objects))))

(define (find-object-at x)
  (vector-index
   (lambda (ox)
     (< ox x (+ ox object-width)))
   (objects-position)))
(define (find-object-at-player)
  (find-object-at
   (+ *player-position* (/ player-width 2))))

(define *user-choices* '())
(define (register-choice! n)
  (when (and n (< (length *user-choices*) num-choices))
    (set! *user-choices*
      (cons n (delete n *user-choices*)))))

(define (square x y)
  (render-fill-rect! *renderer* (make-rect x y object-width object-height)))
(define (show-object! num)
  (set! (render-draw-color *renderer*)
    (if (eq? num (find-object-at-player))
        (make-color 255 0 255)
        (if (member num *user-choices*) (make-color 0 255 0) (make-color 255 0 0))))
  (square (round (- (vector-ref (objects-position) num) *view-position*))
          (- floor-y object-height)))
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
                  (register-choice! (find-object-at-player)))))
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
(define *player-position* (- (/ width 2) (/ player-width 2)))
(define *view-position* 0)

(define (show-player!)
  (set! (render-draw-color *renderer*) (make-color 0 0 255))
  (render-fill-rect! *renderer*
                     (make-rect (round (- *player-position* *view-position*))
                                (- floor-y player-height)
                                player-width
                                player-height)))

(define (move-player! dt)
  (let ((increment (* player-speed dt)))
    (set! *player-position*
      (min (- room-width player-width)
           (max 0 (+ *player-position*
                     (if *player-left* (- increment) 0)
                     (if *player-right* increment 0)))))
    (when (> *player-position* (+ *view-position* (- width player-width view-border)))
      (set! *view-position* (+ *view-position* increment)))
    (when (< *player-position* (+ *view-position* view-border))
      (set! *view-position* (- *view-position* increment)))))

(define (show-background!)
  (render-copy! *renderer*
                background-texture
                #f
                (make-rect (round (- 0 *view-position*)) 0 width height)))

;; ===

(set-main-ready!)
(init!)

(set-hint! 'render-vsync "0")
(set-hint! 'render-scale-quality "0")

(define *win*
  (create-window! "Blah" 'undefined 'undefined window-width window-height))

(define *renderer*
  (create-renderer! *win* -1 '(accelerated)))
(set! (render-logical-size *renderer*) (list width height))

(define background-texture
  (create-texture-from-surface *renderer* (load-bmp "background.bmp")))

(let loop ((lt (get-ticks)))
  (let* ((ct (get-ticks))
         (dt (/ (- ct lt) 1000)))
    (handle-events!)
    (move-player! dt)
    (set! (render-draw-color *renderer*) (make-color 255 255 255))
    (render-clear! *renderer*)
    (show-background!)
    (show-objects!)
    (show-player!)
    (render-present! *renderer*)
    (if (= (length *user-choices*) num-choices)
        (check-user-choices! (lambda () (loop ct)))
        (loop ct))))
