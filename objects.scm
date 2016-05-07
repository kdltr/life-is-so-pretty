(define-record scene-object number position)
(define-record scene-filler texture position)


;; fillers

(define window-texture
  (create-texture-from-surface *renderer* (img:load "window.png")))
(define door-texture
  (create-texture-from-surface *renderer* (img:load "door.png")))


;; proper objects

(define bed-texture
  (create-texture-from-surface *renderer* (img:load "bed.png")))
(define computer-texture
  (create-texture-from-surface *renderer* (img:load "computer.png")))
(define plush-texture
  (create-texture-from-surface *renderer* (img:load "plush.png")))
(define bookcase-texture
  (create-texture-from-surface *renderer* (img:load "bookcase.png")))
(define telephone-texture
  (create-texture-from-surface *renderer* (img:load "telephone.png")))
(define radio-texture
  (create-texture-from-surface *renderer* (img:load "radio.png")))
(define television-texture
  (create-texture-from-surface *renderer* (img:load "television.png")))
(define sofa-texture
  (create-texture-from-surface *renderer* (img:load "sofa.png")))
(define meds-texture
  (create-texture-from-surface *renderer* (img:load "meds.png")))


(define objects-texture
  (vector computer-texture
          plush-texture
          bookcase-texture
          telephone-texture
          radio-texture
          television-texture
          sofa-texture
          meds-texture))

(define objects-name
  (vector "computer"
          "plush"
          "bookcase"
          "phone"
          "HiFi\nsystem"
          "television"
          "sofa"
          "meds"))

(define objects-texts
  (vector '("You watch some porn.\nYou take some pleasure at it."
            "You spend some time trashtalking some My Little Pony fans, and post hateful comments under a young broadcaster's videos.")
          '("You savagely punch your poor teddy bear.\nYou feel a little guilty.\n\"…Why did I do this?\""
            "You deeply hug your teddy bear. The softeness of his fabric makes you feel safe for a couple of minutes.")
          '("You take some random book in the shelf.\n\"Les Fleurs du Mal\" from Charles Baudelaire.\nIt feels like he's talking directely to your heart with his poetry.")
          '("The phone is ringing. You answer it. It's some random classmate.\n\"Hi? Yeah. Whatever. Thanks for calling, I guess… Bye.\""
            "The phone is ringing. You pick it up. It's an advertiser who sells windows. You hang up immediately and sigh.")
          '("Some music is playing. You move your head in rhythm.\n\"I like this song so much!\""
            "You switch on the radio. It's filled with informations from the world. The world seems collapsing. Peapole are starving, wars are starting, nature is diying, and politics are doing nothing about it. You turn it off, you don't care anyway.")
          '("You play some random anime.\n\"Well, that is cool! I can't wait for the next episode to come!\""
            "You switch on the TV and play to video games. You die many times.\n\"Fuck this shit ! I'm tired of loosing... Well. Next time can only be a better run !\"")
          '("You lay down and spend some time doing nothing…"
            "You spend some time daydreaming to a better life, where everything is better.")
          '("You stare at the box of medicine.\n\"Do I Really need to take these?\""
            "You stare at the box of meds.\n\"I'm pretty sure it's ineffective…\""
            "You eat one of the pills\n\"…Why didn't they do it in strawberry taste…?\""
            "You water your plant. At least, it didn't bother you. It lays here, doing nothing but eat oxygen and light.")))

(define (random-object)
  (random (vector-length objects-name)))

#;
(define scene
  (let loop ((n 0)
             (last-x 10))
    (if (= n num-objects)
        '()
        (let* ((obj (random-object))
               (tex (vector-ref objects-texture obj)))
          (cons (make-scene-object obj last-x)
                (loop (add1 n) (+ last-x (texture-w tex))))))))

(define scene
  (cons*
   (make-scene-filler window-texture 0)
   (let loop ((rest (permutation (iota (vector-length objects-name))))
              (last-x (texture-w window-texture)))
     (if (null? rest)
         (list (make-scene-filler door-texture last-x))
         (cons
          (make-scene-object (car rest) last-x)
          (loop (cdr rest) (+ last-x (texture-w (vector-ref objects-texture (car rest))))))))))

(define (find-object-at x)
  (find
   (lambda (o)
     (and (scene-object? o)
      (let ((ox (scene-object-position o))
            (ow (texture-w (vector-ref objects-texture (scene-object-number o)))))
        (and (< ox x (+ ox ow))
             (scene-object-number o)))))
   scene))

