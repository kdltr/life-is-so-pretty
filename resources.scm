(define small-box
  (create-texture-from-surface *renderer* (img:load "assets/small-box.png")))
(define big-box
  (create-texture-from-surface *renderer* (img:load "assets/big-box.png")))
(define text-font
  (create-texture-from-surface *renderer* (img:load "assets/font.png")))


;; sounds

(define empty-room-sound
  (mix:load-wav "assets/empty-room.wav"))

(print (get-error))

(define dark-ambiance-sound
  (mix:load-wav "assets/dark-ambiance.wav"))

(print (get-error))


;; images


(define desaturate identity)

(define (textures-from l)
  (map
   (lambda (f)
     (create-texture-from-surface *renderer* (img:load f)))
   l))


(define player-left-textures (textures-from (sort (glob "assets/player-left*") string<?)))
(define player-right-textures (textures-from (sort (glob "assets/player-right*") string<?)))


(define window-surface* (desaturate (img:load "assets/window.png")))
(define window-texture (create-texture-from-surface *renderer* window-surface*))

(define door-surface (desaturate (img:load "assets/door.png")))
(define door-texture (create-texture-from-surface *renderer* door-surface))

(define bed-surface (desaturate (img:load "assets/bed.png")))
(define bed-texture (create-texture-from-surface *renderer* bed-surface))

(define computer-surface (desaturate (img:load "assets/computer.png")))
(define computer-texture (create-texture-from-surface *renderer* computer-surface))

(define plush-surface (desaturate (img:load "assets/plush.png")))
(define plush-texture (create-texture-from-surface *renderer* plush-surface))

(define bookcase-surface (desaturate (img:load "assets/bookcase.png")))
(define bookcase-texture (create-texture-from-surface *renderer* bookcase-surface))

(define telephone-surface (desaturate (img:load "assets/telephone.png")))
(define telephone-texture (create-texture-from-surface *renderer* telephone-surface))

(define radio-surface (desaturate (img:load "assets/radio.png")))
(define radio-texture (create-texture-from-surface *renderer* radio-surface))

(define television-surface (desaturate (img:load "assets/television.png")))
(define television-texture (create-texture-from-surface *renderer* television-surface))

(define sofa-surface (desaturate (img:load "assets/sofa.png")))
(define sofa-texture (create-texture-from-surface *renderer* sofa-surface))

(define meds-surface (desaturate (img:load "assets/meds.png")))
(define meds-texture (create-texture-from-surface *renderer* meds-surface))

(define pants-surface (desaturate (img:load "assets/pants.png")))
(define pants-texture (create-texture-from-surface *renderer* pants-surface))

(define clock-surface (desaturate (img:load "assets/clock.png")))
(define clock-texture (create-texture-from-surface *renderer* clock-surface))

(define pizza-surface (desaturate (img:load "assets/pizza.png")))
(define pizza-texture (create-texture-from-surface *renderer* pizza-surface))

(define crack-surface (desaturate (img:load "assets/crack.png")))
(define crack-texture (create-texture-from-surface *renderer* crack-surface))

(define plug-surface (desaturate (img:load "assets/plug.png")))
(define plug-texture (create-texture-from-surface *renderer* plug-surface))

(define dildo-surface (desaturate (img:load "assets/dildo.png")))
(define dildo-texture (create-texture-from-surface *renderer* dildo-surface))


;; dreams

(define sleep-textures (textures-from (sort (glob "assets/sleep*") string<?)))
(define dream-textures (textures-from (glob "assets/dream*")))
(define nightmare-textures (textures-from (glob "assets/nightmare*")))



(define all-textures
  (append
   player-left-textures player-right-textures
   sleep-textures dream-textures nightmare-textures
   (list window-texture door-texture bed-texture computer-texture plush-texture bookcase-texture
         telephone-texture radio-texture television-texture sofa-texture meds-texture
         pants-texture clock-texture pizza-texture crack-texture plug-texture)))

(define (color-mod-all-textures! mod)
  (for-each
   (lambda (tex)
     (set! (texture-color-mod tex) mod))
   all-textures))


(define all-fillers
  (list pants-texture pizza-texture crack-texture clock-texture plug-texture))

(define sleep-frames
  (circular-list
   (cons 1.8 (first sleep-textures))
   (cons 1.0 (second sleep-textures))
   (cons 1.2 (third sleep-textures))
   (cons 0.2 (second sleep-textures))))

(define player-left-frames
  (circular-list
   (cons 0.30 (first player-left-textures))
   (cons 0.25 (second player-left-textures))
   (cons 0.30 (first player-left-textures))
   (cons 0.25 (third player-left-textures))))

(define player-right-frames
  (circular-list
   (cons 0.30 (first player-right-textures))
   (cons 0.25 (second player-right-textures))
   (cons 0.30 (first player-right-textures))
   (cons 0.25 (third player-right-textures))))
