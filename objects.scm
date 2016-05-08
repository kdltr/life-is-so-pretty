(define-record scene-object number position)
(define-record scene-filler texture position)

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
  (vector "Computer"
          "Plush"
          "Bookcase"
          "Phone"
          "HiFi"
          "Television"
          "Sofa"
          "Meds"))

(define objects-texts
  (vector '("You watch some porn.\nYou take some pleasure at it."
            "You spend some time trashtalking some My Little Pony fans, and post hateful comments under a young broadcaster's videos."
            "Tu trouves la vidéo d'un petit chaton trop mignon qui se casse la gueule. Ca te fais bien marrer. Tu perds un temps fou à en regarder d'autres."
            "Tu joues à un jeu dans lequel tu incarnes une créature avec des pouvoirs surnaturels, qui monte avec un ami, une équipe de secours pour sauver les autres créatures qui ont des problèmes.")

          
          '("You savagely punch your poor teddy bear.\nYou feel a little guilty.\n\"…Why did I do this?\""
            "You deeply hug your teddy bear. The softeness of his fabric makes you feel safe for a couple of minutes."
            "Tu enfouies ton visage dans la peluche et hurles à pleins poumons pendant de longues secondes.")

          
          '("You take some random book in the shelves.\n\"Les Fleurs du Mal\" from Charles Baudelaire.\nIt feels like he's talking directely to your heart with his poetry."
            "Tu ouvres un manga appellé \"Fullmetal Alchemist.\" Il parle d'un garçon capable de façonner la matière comme il le désire, qui se bat contre des êtres artificiels. \"Est ce que cela serait possible en vrai ?\""
            "Tu t'évades en lisant \"Treasure Island\" de Robert Louis Stevenson, dont l'histoire te fais rêver de liberté, d'alcool, de femmes et de trésors millénaires.")

          
          '("The phone is ringing. You answer it. It's some random classmate.\n\"Hi? Yeah. Whatever. Thanks for calling, I guess… Bye.\""
            "The phone is ringing. You pick it up. It's an advertiser who sells windows. You hang up immediately and sigh.")

          
          '("Some music is playing. You move your head in rhythm.\n\"That's a nice song…\""
            "You switch on the radio. It's filled with informations from the world. The world seems collapsing. Peapole are starving, wars are starting, nature is diying, and politics are doing nothing about it. You turn it off, you don't care anyway.")

          
          '("You play some random anime.\n\"Well, that is cool! I can't wait for the next episode to come!\""
            "You switch on the TV and play to video games. You die many times.\n\"Fuck this shit ! I'm tired of loosing... Well. Next time can only be a better run !\""
            "Tu regardes un anime dans lequel le personnage principal devient invisible aux yeux de tous. Malgré tous ses efforts, il lui est impossible de communiquer avec quiconque. Quelques larmes coulent sur tes joues."
            "Tu joues à un jeu vidéo dans lequelle une fille est capable de remonter le temps pour changer ce qu'elle veut dans sa vie, sauver ses amis et même faire le bien autour d'elle.")

          
          '("You lay down and spend some time doing nothing…"
            "You spend some time daydreaming to a better life, where everything is better.")

          
          '("You stare at the box of medicine.\n\"Do I Really need to take these?\""
            "You stare at the box of meds.\n\"I'm pretty sure it's ineffective…\""
            "You eat one of the pills\n\"…Why didn't they do it in strawberry taste…?\""
            "You water your plant. At least, it didn't bother you. It lays here, doing nothing but eat oxygen and light.")))

(define (random-scene)
  (cons*
   (make-scene-filler window-texture 0)
   (let loop ((rest (permutation (iota (vector-length objects-name))))
              (last-x (texture-w window-texture))
              (place-bed? #t))
     (cond ((or (and (null? rest) place-bed?)
                (and place-bed? (eq? 0 (random 10))))
            (cons (make-scene-filler bed-texture last-x)
                  (loop rest (+ last-x (texture-w bed-texture)) #f)))
           ((null? rest)
            (list (make-scene-filler door-texture last-x)))
           (else
            (cons
             (make-scene-object (car rest) last-x)
             (loop (cdr rest) (+ last-x (texture-w (vector-ref objects-texture (car rest)))) place-bed?)))))))

(define scene (random-scene))

(define (find-object-at x)
  (find
   (lambda (o)
     (and (scene-object? o)
          (let ((ox (scene-object-position o))
                (ow (texture-w (vector-ref objects-texture (scene-object-number o)))))
            (and (< ox x (+ ox ow))
                 (scene-object-number o)))))
   scene))

(define (find-filler-at x)
  (find
   (lambda (o)
     (and (scene-filler? o)
          (let ((ox (scene-filler-position o))
                (ow (texture-w (scene-filler-texture o))))
            (and (< ox x (+ ox ow))
                 o))))
   scene))
