(use sdl2
     (prefix sdl2-image img:)
     cairo
     clojurian-syntax
     miscmacros
     vector-lib
     utf8)

(include "proto.scm")

(define window-width 1920)
(define window-height 1080)

(define width 320)
(define height 180)

(define object-width 20)
(define object-height 50)

(define player-speed 70)
(define player-width 23)
(define player-height 79)

(define view-border (+ (/ player-width 2) 40))

(define floor-y 150)

;;

(set-main-ready!)
(init!)
(img:init!)

(set-hint! 'render-vsync "0")
(set-hint! 'render-scale-quality "0")

(define *win*
  (create-window! "Blah" 'undefined 'undefined window-width window-height '(fullscreen)))

(define *renderer*
  (create-renderer! *win* -1 '(accelerated)))
(set! (render-logical-size *renderer*) (list width height))


;; Library

(include "text.scm")

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

(define player-left-texture
  (create-texture-from-surface *renderer* (img:load "player-left.png")))
(define player-right-texture
  (create-texture-from-surface *renderer* (img:load "player-right.png")))
(define *player-left* #f)
(define *player-right* #f)
(define *player-position* (- (/ width 2) (/ player-width 2)))
(define *player-texture* player-right-texture)
(define *view-position* 0)

(define (show-player!)
  (render-copy! *renderer*
                *player-texture*
                #f
                (make-rect (round (- *player-position* *view-position*))
                                (- floor-y player-height)
                                player-width
                                player-height)))

(define (move-player! dt)
  (let* ((factor (* player-speed dt))
         (increment (+ (if *player-left* (- factor) 0)
                       (if *player-right* factor 0))))
    (set! *player-texture*
      (cond ((< increment 0) player-left-texture)
            ((> increment 0) player-right-texture)
            (else *player-texture*)))
    (set! *player-position*
      (min (- room-width player-width) (max 0 (+ *player-position* increment))))
    (when (> *player-position* (+ *view-position* (- width player-width view-border)))
      (set! *view-position* (+ *view-position* factor)))
    (when (< *player-position* (+ *view-position* view-border))
      (set! *view-position* (- *view-position* factor)))))

;; ===

(let loop ((lt (get-ticks)))
  (let* ((ct (get-ticks))
         (dt (/ (- ct lt) 1000)))
    (handle-events!)
    (move-player! dt)
    (set! (render-draw-color *renderer*) (make-color 255 255 255))
    (render-clear! *renderer*)
    (show-objects!)
    (show-player!)
    (show-text! 10 10 "Bonjour, je suis depressif! Aidez-moi a mourir. ☹")
    (show-text! 10 24 "neRUSiten audietn aursiet ndptenrasptued tpnersta")
    (render-present! *renderer*)
    (if (= (length *user-choices*) num-choices)
        (check-user-choices! (lambda () (loop ct)))
        (loop ct))))
