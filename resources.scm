;; sounds

(define empty-room-sound
  (mix:load-wav "empty-room.wav"))

(print (get-error))

(define dark-ambiance-sound
  (mix:load-wav "dark-ambiance.wav"))

(print (get-error))


;; images

(define (color:scale-chroma color factor)
  (let* ((LCh (color->L*C*h color))
         (chroma (* (cadr LCh) factor)))
    (L*C*h->color (cons (car LCh) (cons chroma (cddr LCh))))))

(define (desaturate-color color)
  (let* ((l (color->list color))
         (c (take l 3))
         (alpha (last-pair l)))
    (apply make-color
           (append
            (color->sRGB
             (color:scale-chroma
              (sRGB->color c)
              0.5))
            alpha))))

(define (desaturate source)
  (identity source)
  #;
  (let* ((w (surface-w source))
  (h (surface-h source))
  (result (make-surface w h 32)))
  (dotimes (i w)
  (dotimes (j h)
  (set! (surface-ref result i j)
  (desaturate-color (surface-ref source i j)))))
  result))


(define player-left-surface (desaturate (img:load "player-left.png")))
(define player-left-texture (create-texture-from-surface *renderer* player-left-surface))

(define player-right-surface (desaturate (img:load "player-right.png")))
(define player-right-texture (create-texture-from-surface *renderer* player-right-surface))

(define window-surface* (desaturate (img:load "window.png")))
(define window-texture (create-texture-from-surface *renderer* window-surface*))

(define door-surface (desaturate (img:load "door.png")))
(define door-texture (create-texture-from-surface *renderer* door-surface))

(define bed-surface (desaturate (img:load "bed.png")))
(define bed-texture (create-texture-from-surface *renderer* bed-surface))

(define computer-surface (desaturate (img:load "computer.png")))
(define computer-texture (create-texture-from-surface *renderer* computer-surface))

(define plush-surface (desaturate (img:load "plush.png")))
(define plush-texture (create-texture-from-surface *renderer* plush-surface))

(define bookcase-surface (desaturate (img:load "bookcase.png")))
(define bookcase-texture (create-texture-from-surface *renderer* bookcase-surface))

(define telephone-surface (desaturate (img:load "telephone.png")))
(define telephone-texture (create-texture-from-surface *renderer* telephone-surface))

(define radio-surface (desaturate (img:load "radio.png")))
(define radio-texture (create-texture-from-surface *renderer* radio-surface))

(define television-surface (desaturate (img:load "television.png")))
(define television-texture (create-texture-from-surface *renderer* television-surface))

(define sofa-surface (desaturate (img:load "sofa.png")))
(define sofa-texture (create-texture-from-surface *renderer* sofa-surface))

(define meds-surface (desaturate (img:load "meds.png")))
(define meds-texture (create-texture-from-surface *renderer* meds-surface))

(define pants-surface (desaturate (img:load "pants.png")))
(define pants-texture (create-texture-from-surface *renderer* pants-surface))

(define clock-surface (desaturate (img:load "clock.png")))
(define clock-texture (create-texture-from-surface *renderer* clock-surface))

(define pizza-surface (desaturate (img:load "pizza.png")))
(define pizza-texture (create-texture-from-surface *renderer* pizza-surface))

(define crack-surface (desaturate (img:load "crack.png")))
(define crack-texture (create-texture-from-surface *renderer* crack-surface))

(define plug-surface (desaturate (img:load "plug.png")))
(define plug-texture (create-texture-from-surface *renderer* plug-surface))


;; dreams

(define sleep-textures
  (map (lambda (f) (create-texture-from-surface *renderer* (img:load f))) (sort (glob "sleep*") string<?)))

(define dream-textures
  (map (lambda (f) (create-texture-from-surface *renderer* (img:load f))) (glob "dream*")))

(define nightmare-textures
  (map (lambda (f) (create-texture-from-surface *renderer* (img:load f))) (glob "nightmare*")))



(define all-textures
  (list window-texture door-texture bed-texture computer-texture plush-texture bookcase-texture
        telephone-texture radio-texture television-texture sofa-texture meds-texture
        player-left-texture player-right-texture
        pants-texture clock-texture pizza-texture crack-texture plug-texture))


(define all-fillers
  (list pants-texture pizza-texture crack-texture clock-texture plug-texture))

(define sleep-frames
  (circular-list
   (cons 2.0 (first sleep-textures))
   (cons 1.0 (second sleep-textures))
   (cons 1.5 (third sleep-textures))
   (cons 0.5 (second sleep-textures))))
