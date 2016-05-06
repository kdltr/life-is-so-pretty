(define-record scene-object number position)
(define-record scene-filler texture position)

(define window-texture
  (create-texture-from-surface *renderer* (img:load "window.png")))
(define door-texture
  (create-texture-from-surface *renderer* (img:load "door.png")))

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


(define objects-texture
  (vector bed-texture
          computer-texture
          plush-texture
          bookcase-texture
          telephone-texture
          radio-texture
          television-texture
          sofa-texture))

(define objects-name
  (vector "bed"
          "computer"
          "plush"
          "bookcase"
          "phone"
          "radio"
          "television"
          "sofa"))

(define (random-object)
  (random (vector-length objects-name)))

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
   (let loop ((rest (permutation (iota num-objects)))
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

