(use sdl2
     (prefix sdl2-image img:)
     clojurian-syntax
     miscmacros
     vector-lib
     utf8
     srfi-1
     (only color color->L*C*h L*C*h->color color->sRGB sRGB->color)
     (prefix sdl2-mixer mix:)
     posix
     anaphora)

(include "helpers")
(include "proto.scm")

(define window-width 960)
(define window-height 540)

(define width 320)
(define height 180)

(define object-width 20)
(define object-height 50)

(define player-speed 100)
(define player-width 40)
(define player-height 79)

(define view-border (+ (/ player-width 2) 33))

(define ceiling-y 30)
(define floor-y 150)

;;

(set-main-ready!)
(init!)
(img:init! '(png))
(mix:init!)
(mix:open-audio!)

(set-hint! 'render-vsync "1")
(set-hint! 'render-scale-quality "0")

;; (define dm (make-display-mode))
;; (SDL_GetDesktopDisplayMode 0 dm)

;; (define win (create-window! "Crepes-party-hard-yolo-swag 2015"
;;                             'undefined 'undefined
;;                             (display-mode-w dm)
;;                             (display-mode-h dm)
;;                             '(fullscreen)))

(define *win*
  (create-window! "Life Is So Pretty" 'undefined 'undefined window-width window-height '(fullscreen-desktop)))

(define *renderer*
  (create-renderer! *win* -1 '(accelerated)))
(set! (render-logical-size *renderer*) (list width height))


;; Library

(include "resources")
(include "objects")
(include "text")

(define room-width
  (let* ((last-obj (last scene))
         (pos (scene-filler-position last-obj))
         (tex (scene-filler-texture last-obj)))
    (+ pos (texture-w tex))))

(define *user-choices* '())

(define (turn-finished?)
  (= (length *user-choices*) num-choices))

(define (allowed-interaction? obj)
  (and (scene-object? obj)
       (not (member (scene-object-number obj) *user-choices*))
       obj))

(define (register-choice! obj)
  (set! *user-choices* (cons (scene-object-number obj) *user-choices*)))

(define (show-object! obj ending)
  (let* ((x (scene-object-position obj))
         (num (scene-object-number obj))
         (user-x (floor (- x *view-position*)))
         (name (vector-ref objects-name num))
         (tex (vector-ref objects-texture num))
         (len (fold (lambda (s n) (max n (string-length s))) 0 (string-split name "\n")))
         (text-x (floor (+ user-x (/ (texture-w tex) 2) (- (/ (* character-width len) 2)))))
         (text-y (+ ceiling-y 20)))
    (render-copy! *renderer*
                  tex
                  #f
                  (make-rect user-x 0 (texture-w tex) (texture-h tex)))
    (when (and (eq? obj (find-object-at-player))
               (not (member num *user-choices*))
               (not (turn-finished?))
               (not ending))
      (show-boxed-text! text-x text-y name small-box))))

(define (show-filler! flr ending)
  (let* ((x (scene-filler-position flr))
         (tex (scene-filler-texture flr))
         (user-x (floor (- x *view-position*))))
    (render-copy! *renderer*
                  tex
                  #f
                  (make-rect user-x 0 (texture-w tex) (texture-h tex)))
    (when (and (eq? tex bed-texture)
               (turn-finished?)
               (<= x *player-position* (+ x (texture-w tex))))
      (show-boxed-text! (floor (+ user-x (/ (texture-w tex) 2) (/ (- (* character-width 3)) 2)))
                        (+ ceiling-y 20) "Bed" small-box))
    (when (and (eq? ending 'good)
               (eq? tex door-texture)
               (<= x *player-position* (+ x (texture-w tex))))
      (show-boxed-text! (floor (+ user-x (/ (texture-w tex) 2) (/ (- (* character-width 3)) 2)))
                        (+ ceiling-y 20) "Door" small-box))
    (when (and (eq? ending 'bad)
               (eq? tex window-texture)
               (<= x *player-position* (+ x (texture-w tex))))
      (show-boxed-text! (floor (+ user-x (/ (texture-w tex) 2) (/ (- (* character-width 3)) 2)))
                        (+ ceiling-y 20) "Window" small-box))))

(define (show-scene! #!optional (ending #f))
  (for-each
   (lambda (elm)
     (cond ((scene-filler? elm) (show-filler! elm ending))
           ((scene-object? elm) (show-object! elm ending))))
   scene))

(define *player-left* #f)
(define *player-right* #f)
(define *player-interacting* #f)
(define *player-position* (- (/ width 2) (/ player-width 2)))
(define *player-clock* 0)
(define *player-frames* player-right-frames)
(define *frame-orientation* 'right)
(define *view-position* 0)

(define (reset-movement!)
  (set! *player-left* #f)
  (set! *player-right* #f)
  (set! *player-clock* 0)
  (set! *player-interacting* #f))

(define (find-object-at-player)
  (find-object-at
   (+ *player-position* (/ player-width 2))))

(define (find-filler-at-player)
  (find-filler-at
   (+ *player-position* (/ player-width 2))))

(define (show-player!)
  (render-copy! *renderer*
                (cdar *player-frames*)
                #f
                (make-rect (floor (- *player-position* *view-position*))
                                (- floor-y player-height)
                                player-width
                                player-height)))

(define (move-player! dt)
  (let* ((factor (* player-speed dt))
         (clk (+ *player-clock* dt))
         (frame (car *player-frames*))
         (time (car frame))
         (increment (+ (if *player-left* (- factor) 0)
                       (if *player-right* factor 0))))
    (if (>= clk time)
        (begin (set! *player-frames* (cdr *player-frames*))
               (set! *player-clock* 0))
        (set! *player-clock* clk))
    (when (and *player-left* (eq? *frame-orientation* 'right))
      (set! *frame-orientation* 'left)
      (set! *player-frames* player-left-frames)
      (set! *player-clock* 0.3))
    (when (and *player-right* (eq? *frame-orientation* 'left))
      (set! *frame-orientation* 'right)
      (set! *player-frames* player-right-frames)
      (set! *player-clock* 0.3))
    (when (not (or *player-left* *player-right*))
      (set! *player-frames* (if (= (car frame) 0.3) *player-frames* (cdr *player-frames*)))
      (set! *player-clock* 0.250))
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

(define (default-game-state dt)
  (move-player! dt)
  (show-scene!)
  (show-player!)
  (cond ((turn-finished?)
         end-turn-game-state)
        ((and *player-interacting* (allowed-interaction? (find-object-at-player))) =>
         (cut make-interaction-game-state <>))
        (else
         default-game-state)))

(define (end-turn-game-state dt)
  (move-player! dt)
  (show-scene!)
  (show-player!)

  (if *player-interacting*
      (cond ((aand (find-filler-at-player) (eq? (scene-filler-texture it) bed-texture))
             (make-dream-game-state))
            ((find-object-at-player)
             (let ((phrase (random-elt end-turn-phrases)))
               (reset-movement!)
               (rec (state dt)
                    (show-scene!)
                    (show-player!)
                    (show-formated-text! phrase)
                    (if *player-interacting*
                        (begin (reset-movement!) end-turn-game-state)
                        state))))
            (else
             end-turn-game-state))
      end-turn-game-state))

(define (make-interaction-game-state obj)
  (let* ((num (scene-object-number obj))
         (texts (vector-ref objects-texts num))
         (text (random-elt texts)))
    (reset-movement!)
    (register-choice! obj)
    (rec (state dt)
         (show-scene!)
         (show-player!)
         (show-formated-text! text)
         (if *player-interacting*
             (begin (reset-movement!) default-game-state)
             state))))

(define (make-dream-game-state)
  (reset-movement!)
  (let* ((good-objects (interact *user-choices*))
         (dreams (take (permutation dream-textures) good-objects))
         (nightmares (take (permutation nightmare-textures) (- num-choices good-objects)))
         (items (permutation (append dreams nightmares)))
         (bubbles-clock 0)
         (bed-clock 0)
         (frames sleep-frames))
    (let ((factor (/ (max life 0) max-life)))
      (mix:volume-chunk! dark-ambiance-sound (inexact->exact (round (* (- 1 factor) 90))))
      (mix:volume-chunk! empty-room-sound (inexact->exact (round (* factor 128)))))
    (rec (state dt)
         (let* ((frame (car frames))
                (clk (+ bed-clock dt))
                (time (car frame))
                (tex (cdr frame)))
           (render-copy! *renderer* tex)
           (if (>= clk time)
               (begin (set! bed-clock 0)
                      (set! frames (cdr frames)))
               (set! bed-clock clk)))
         (render-copy! *renderer* (car items) #f (make-rect 0 (floor (* 2 (sin (* bubbles-clock 3)))) width height))
         (set! bubbles-clock (+ bubbles-clock dt))
         (if *player-interacting*
             (if (null? (cdr items))
                 (begin (reset-movement!)
                        (set! *user-choices* '())
                        (cond ((= good-objects num-choices)
                               (mix:volume-chunk! dark-ambiance-sound 0)
                               (mix:volume-chunk! empty-room-sound 128)
                               (make-last-action-state 'good door-texture))
                              ((<= life 0)
                               (make-last-action-state 'bad window-texture))
                              (else
                               default-game-state)))
                 (begin
                   (reset-movement!)
                   (set! bubbles-clock 0)
                   (set! items (cdr items))
                   state))
             state))))

(define (make-last-action-state ending clickable-texture)
  (rec (state dt)
       (move-player! dt)
       (show-scene! ending)
       (show-player!)
       (let* ((obj (find-filler-at-player))
              (tex (and obj (scene-filler-texture obj))))
         (if (and *player-interacting*
                  (eq? tex clickable-texture))
             (lambda (dt) (exit 0))
             state))))

(define (introduction-game-state dt)
  (show-formated-text! "Welcome…\nPress A and D to move around and space to interact with your environment.\nPress escape at anytime to give up.")
  (if *player-interacting*
      (begin (reset-movement!) default-game-state)
      introduction-game-state))


(define (main-loop state lt)
  (let* ((ct (get-ticks))
         (dt (/ (- ct lt) 1000)))
    (render-present! *renderer*)
    (set! (render-draw-color *renderer*) (make-color 0 0 0))
    (render-clear! *renderer*)
    (handle-events!)
    (main-loop (state dt) ct)))


;; Init

(mix:volume-chunk! empty-room-sound 128)
(mix:volume-chunk! dark-ambiance-sound 0)
(mix:play-channel! 0 empty-room-sound -1)
(mix:play-channel! 1 dark-ambiance-sound -1)

(main-loop introduction-game-state (get-ticks))
