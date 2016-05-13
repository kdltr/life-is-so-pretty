;; The contents of this demo file are made available under the CC0 1.0
;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;; http://creativecommons.org/publicdomain/zero/1.0/


;;; This is a demo program which loads and displays an image file.
;;;
;;; This program demonstrates the following concepts:
;;;
;;; - Initializing SDL
;;; - Loading an image file using sdl2-image
;;; - Creating, configuring, and resizing a window
;;; - Rotating and flipping a surface
;;; - Filling a surface with a color
;;; - Blitting one surface onto another
;;; - Updating the window surface
;;; - A basic event loop that responds to user input.


(define (usage)
  (print #<#EOF
Usage:
  #(program-name) IMAGE-PATH

Controls:
  - R, L: Rotate image clockwise / counter-clockwise
  - X, Y: Flip image on X / Y axis
  - Esc, or close button: Quit
EOF
))

(when (null? (command-line-arguments))
  (usage)
  (exit 1))

(define image-path (car (command-line-arguments)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INITIALIZATION

(use (prefix sdl2 sdl2:)
     (prefix sdl2-image img:)
     miscmacros)

;; Initialize SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video))

;; Schedule quit! to be automatically called when the program exits.
(on-exit sdl2:quit!)

;; Ensure that quit! will be called even if an unhandled exception
;; reaches the top level.
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))



(define *image* (make-parameter #f))

;;; Attempt to load the image. Print an error message and exit if
;;; loading fails.
(condition-case
 (*image* (img:load image-path))
 (e (sdl2)
    (printf "Error: Could not load image file: ~S" image-path)
    (print-error-message e)
    (exit 1)))


(define window
  (sdl2:create-window!
   image-path 'undefined 'undefined
   (sdl2:surface-w (*image*)) (sdl2:surface-h (*image*))))

(define window-surf (sdl2:window-surface window))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; USER ACTIONS

(define (rotate-image! turns)
  (replace-image! (sdl2:rotate-surface-90 (*image*) turns))
  (resize-window-to-fit!)
  (redraw!))

(define (flip-image! flip-x? flip-y?)
  (replace-image! (sdl2:flip-surface (*image*) flip-x? flip-y?))
  (redraw!))


(define (replace-image! new-image)
  (sdl2:free-surface! (*image*))
  (*image* new-image))

(define (resize-window-to-fit!)
  (sdl2:window-size-set!
   window (list (sdl2:surface-w (*image*)) (sdl2:surface-h (*image*))))
  (set! window-surf (sdl2:window-surface window)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REDRAW

(define (redraw!)
  (when (has-alpha-mask? (*image*))
    (draw-checkerboard! window-surf 10 lite-gray dark-gray))
  (sdl2:blit-surface! (*image*) #f window-surf #f)
  (sdl2:update-window-surface! window))

(define (has-alpha-mask? surf)
  (positive? (sdl2:pixel-format-amask (sdl2:surface-format surf))))

(define lite-gray (sdl2:make-color 200 200 200))
(define dark-gray (sdl2:make-color 150 150 150))

(define (draw-checkerboard! surf grid-size color1 color2)
  (sdl2:fill-rect! surf #f color1)
  (let ((width  (sdl2:surface-w surf))
        (height (sdl2:surface-h surf))
        (rect   (sdl2:make-rect 0 0 grid-size grid-size)))
    (do ((row 0 (add1 row)))
        ((>= (* row grid-size) width))
      (do ((column 0 (add1 column)))
          ((>= (* column grid-size) height))
        (when (or (and (odd? row) (even? column))
                  (and (even? row) (odd? column)))
          (sdl2:rect-set! rect (* row grid-size) (* column grid-size))
          (sdl2:fill-rect! surf rect color2))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN LOOP / EVENT HANDLING

(define (main-loop)
  ;; An event struct that will be overwritten by sdl2:wait-event!.
  ;; Passing an existing event struct to sdl2:wait-event! is optional,
  ;; but it reduces the amount of garbage, because sdl2:wait-event!
  ;; allocates a new event if none is passed. Since there may be many
  ;; events handled per second, that small bit of garbage can add up.
  (define event (sdl2:make-event))

  (redraw!)

  ;; Create a continuation that can be called to exit the main loop.
  (let/cc exit-main-loop!
    ;; Loop forever (until exit-main-loop! is called).
    (while #t
      ;; Wait for the next event, then handle it.
      (handle-event (sdl2:wait-event! event) exit-main-loop!))))


(define (handle-event ev exit-main-loop!)
  (case (sdl2:event-type ev)
    ;; Window exposed, etc.
    ((window)
     (sdl2:update-window-surface! window))

    ;; User requested app quit (e.g. clicked the close button).
    ((quit)
     (exit-main-loop! #t))

    ;; Keyboard key pressed
    ((key-down)
     (case (sdl2:keyboard-event-sym ev)
       ;; Escape quits the program
       ((escape)
        (exit-main-loop! #t))
       ;; R rotates the image by 90 degrees clockwise.
       ((r)
        (rotate-image! 1))
       ;; L rotates the image by 90 degrees counter-clockwise.
       ((l)
        (rotate-image! -1))
       ;; X flips the image on the X axis
       ((x)
        (flip-image! #t #f))
       ;; Y flips the image on the Y axis
       ((y)
        (flip-image! #f #t))))))


(main-loop)
