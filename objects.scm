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
  (vector '("You watch some porn. You take some pleasure at it.")
          '("You savagely punch your poor teddy bear. You feel a little guilty.\n\"…Why did I do this?\"")
          '("You take some random book in the shelf. \"Les Fleurs du Mal\" from Charles Baudelaire. It feels like he's talking directely to your heart whith his poetry.")
          '("The phone is ringing. You answer it. It's some random classmate.\n\"Hi? Yeah. Whatever. Thanks for calling, I guess… Bye.\"")
          '("Some music is playing. You move your head in rhythm.\n\"I like this song so much!\"")
          '("You play some random anime.\n\"Well, that is cool! I can't wait for the next episode to come!\"")
          '("You lay down and spend some time doing nothing…")
          '("You stare at the box of medicine.\n\"Do I Really need to take these?\"")))

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

