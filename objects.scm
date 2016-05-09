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
            "You spend some time trashtalking some My Little Pony fans. You also obnoxiously comment videos from a young broadcaster."
            "You come across upon a video showing a cute kitten falling flat on his face, it makes you laugh.\nYou spend an awful lot of time watching others."
            "You start up a game in which you play a little creature gifted with supernatural powers that, with a friend, puts up a rescue team to help other creatures in danger.")

          
          '("You savagely punch your poor teddy bear.\nYou feel a bit guilty.\n\"…What have I done?\""
            "You deeply hug your teddy bear. The softeness of the fabric makes you feel safe for a couple of minutes."
            "You burrow your face in the plush and cry out in despair for long minutes.")

          
          '("You take some random book in the shelves.\n\"Les Fleurs du Mal\" from Charles Baudelaire.\nIt feels like his poetry is talking directely to your heart."
            ;; "Tu ouvres un manga appellé \"Fullmetal Alchemist.\" Il parle d'un garçon capable de façonner la matière comme il le désire, qui se bat contre des êtres artificiels. \"Est ce que cela serait possible en vrai ?\""
            "You escape for a while by reading \"Treasure Island\" by Robert Louis Stevenson.\nThis story sparks off some dreams about freedom, alcohol and thousands-years-old treasures.")

          
          '("The phone is ringing. You answer it. It's some random classmate.\n\"Hi? Yeah. Whatever. Thanks for calling, I guess… Bye.\""
            "The phone is ringing. You pick it up. It's a dealer who sells windows. You hang up immediately and sigh.")

          
          '("Some music is playing. You move your head in rhythm.\n\"That's a nice song…\""
            "You switch on the radio. It's filled with informations from the world. It sounds like it's collapsing. People are starving, wars are breaking out, nature is diying. Politics are not doing anything about it. You turn it off, you don't care anyway.")

          
          '("You watch some random anime.\n\"Well, that is cool! I can't wait for the next episode to come out!\""
            "You switch on the TV and play video games.\nYou die many times.\n\"Fuck this shit! I'm tired of loosing…\""
            "You watch an anime where the main character becomes invisible. Despite his best efforts, he's unable to communicate with anyone. A few tears roll on your cheeks."
            "You fire up a game. It's about a girl able to rewind time to change anything she wants in her life, save her friends and do good around her.")

          
          '("You lay down and spend some time doing nothing…"
            "You spend some time daydreaming about a better life, where everything is better.")

          
          '("You stare at the box of medicine.\n\"Do I Really need to take these?\""
            "You stare at the box of meds.\n\"I'm pretty sure they're ineffective…\""
            "You eat one pill.\n\"Why doesn't it taste like strawberry…?\""
            "You water your plant. At least it doesn't bother you. It just lays there, doing nothing but eat oxygen and light.")))

(define end-turn-phrases
  '("It's late, I should go to bed."
    "I'm tired now…"
    "Now is not a good time…"
    "Maybe I can try that twomorrow."))

(define (random-scene)
  (cons*
   (make-scene-filler window-texture 0)
   (let loop ((rest (permutation (iota (vector-length objects-name))))
              (fillers-left (permutation all-fillers))
              (last-x (texture-w window-texture))
              (place-bed? #t)
              (place-dildo? #t))
     (cond ((or (and (null? rest) place-bed?)
                (and place-bed? (zero? (random 10))))
            (cons (make-scene-filler bed-texture last-x)
                  (loop rest fillers-left (+ last-x (texture-w bed-texture)) #f place-dildo?)))
           ((null? rest)
            (list (make-scene-filler door-texture last-x)))
           ((and place-dildo? (zero? (random 200)))
            (cons (make-scene-filler dildo-texture last-x)
                  (loop rest fillers-left (+ last-x (texture-w dildo-texture)) place-bed? #f)))
           ((and (not (null? fillers-left)) (zero? (random 5)))
            (cons (make-scene-filler (car fillers-left) last-x)
                  (loop rest (cdr fillers-left) (+ last-x (texture-w (car fillers-left))) place-bed? place-dildo?)))
           (else
            (cons
             (make-scene-object (car rest) last-x)
             (loop (cdr rest) fillers-left (+ last-x (texture-w (vector-ref objects-texture (car rest)))) place-bed? place-dildo?)))))))

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
