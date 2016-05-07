(use sdl2
     (prefix sdl2-image img:)
     cairo
     clojurian-syntax
     miscmacros
     vector-lib
     utf8
     srfi-1)

(include "helpers")
(include "proto.scm")

(define window-width 960)
(define window-height 540)

(define width 320)
(define height 180)

(define object-width 20)
(define object-height 50)

(define player-speed 150)
(define player-width 23)
(define player-height 79)

(define view-border (+ (/ player-width 2) 40))

(define ceiling-y 30)
(define floor-y 150)

;;

(set-main-ready!)
(init!)
(img:init! '(png))

(set-hint! 'render-vsync "0")
(set-hint! 'render-scale-quality "0")

(define *win*
  (create-window! "Blah" 'undefined 'undefined window-width window-height '()))

(define *renderer*
  (create-renderer! *win* -1 '(accelerated)))
(set! (render-logical-size *renderer*) (list width height))


;; Library

(include "objects")
(include "text.scm")

(define room-width
  (let* ((last-obj (last scene))
         (pos (scene-filler-position last-obj))
         (tex (scene-filler-texture last-obj)))
    (+ pos (texture-w tex))))

(define *user-choices* '())
(define (register-choice! obj)
  (when (and obj (scene-object? obj) (< (length *user-choices*) num-choices))
    (let* ((num (scene-object-number obj))
           (already-checked? (member num *user-choices*)))
      (unless already-checked?
        (let ((text (random-elt (vector-ref objects-texts num))))
          (let loop ((ev (poll-event!)))
            (reset-movement!)
            (show-formated-text! text)
            (render-present! *renderer*)
            (unless (and (keyboard-event? ev) (keyboard-event-state ev) (eq? 'space (keyboard-event-scancode ev)))
              (loop (poll-event!)))))
        (set! *user-choices*
          (cons num (delete num *user-choices*)))))))

(define (show-object! obj)
  (let* ((x (scene-object-position obj))
         (num (scene-object-number obj))
         (user-x (round (- x *view-position*)))
         (name (vector-ref objects-name num))
         (tex (vector-ref objects-texture num))
         (len (fold (lambda (s n) (max n (string-length s))) 0 (string-split name "\n")))
         (text-x (round (+ user-x (/ (texture-w tex) 2) (- (/ (* character-width len) 2)))))
         (text-y (+ ceiling-y 20)))
    (render-copy! *renderer*
                  tex
                  #f
                  (make-rect user-x 0 (texture-w tex) (texture-h tex)))
    (when (and (eq? obj (find-object-at-player))
               (not (member num *user-choices*)))
      (show-boxed-text! text-x text-y name small-box))))

(define (show-filler! flr)
  (let* ((x (scene-filler-position flr))
         (tex (scene-filler-texture flr)))
    (render-copy! *renderer*
                  tex
                  #f
                  (make-rect (round (- x *view-position*)) 0 (texture-w tex) (texture-h tex)))))

(define (show-scene!)
  (for-each
   (lambda (elm)
     (cond ((scene-filler? elm) (show-filler! elm))
           ((scene-object? elm) (show-object! elm))))
   scene))

(define player-left-texture
  (create-texture-from-surface *renderer* (img:load "player-left.png")))
(define player-right-texture
  (create-texture-from-surface *renderer* (img:load "player-right.png")))
(define *player-left* #f)
(define *player-right* #f)
(define *player-interacting* #f)
(define *player-position* (- (/ width 2) (/ player-width 2)))
(define *player-texture* player-right-texture)
(define *view-position* 0)

(define (reset-movement!)
  (set! *player-left* #f)
  (set! *player-right* #f)
  (set! *player-interacting* #f))

(define (find-object-at-player)
  (find-object-at
   (+ *player-position* (/ player-width 2))))

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
      (min (- room-width player-width 55) (max 56 (+ *player-position* increment))))
    (when (> *player-position* (+ *view-position* (- width player-width view-border)))
      (set! *view-position* (+ *view-position* factor)))
    (when (< *player-position* (+ *view-position* view-border))
      (set! *view-position* (- *view-position* factor)))))

;; ===

(define (handle-events!)
  (let loop ((ev (poll-event!)))
    (and (keyboard-event? ev)
         (or (and (eq? (keyboard-event-scancode ev) 'a)
                  (set! *player-left* (keyboard-event-state ev)))
             (and (eq? (keyboard-event-scancode ev) 'd)
                  (set! *player-right* (keyboard-event-state ev)))
             (and (eq? (keyboard-event-scancode ev) 'escape)
                  (exit 0))
             (and (eq? (keyboard-event-scancode ev) 'r)
                  (keyboard-event-state ev)
                  (set! scene (random-scene)))
             (and (eq? (keyboard-event-scancode ev) 'space)
                  (set! *player-interacting* (keyboard-event-state ev)))))
    (and ev (loop (poll-event!)))))

(define (check-user-choices! cont)
  (let ((num-good (interact *user-choices*)))
    (print "Good objects: " num-good)
    (set! *user-choices* '())
    (if (= num-good num-choices)
        (print "You win!")
        (cont))))

(let loop ((lt (get-ticks)))
  (let* ((ct (get-ticks))
         (dt (/ (- ct lt) 1000)))
    (handle-events!)
    (move-player! dt)
    (when *player-interacting*
      (register-choice! (find-object-at-player)))
    (set! (render-draw-color *renderer*) (make-color 0 0 0))
    (render-clear! *renderer*)
    (show-scene!)
    (show-player!)
    (render-present! *renderer*)
    (if (= (length *user-choices*) num-choices)
        (check-user-choices! (lambda () (loop ct)))
        (loop ct))))
