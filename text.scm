
(define small-box
  (create-texture-from-surface *renderer* (img:load "small-box.png")))
(define big-box
  (create-texture-from-surface *renderer* (img:load "big-box.png")))
(define text-font
  (create-texture-from-surface *renderer* (img:load "font.png")))
(define character-width 6)
(define character-height 14)

(define first-capital 0)
(define first-normal (+ first-capital 26))
(define first-number (+ first-normal 26))
(define first-special (+ first-number 10))

(define (char-encoding c)
  (let ((n (char->integer c)))
    (cond ((<= (char->integer #\A) n (char->integer #\Z))
           (+ first-capital (- n (char->integer #\A))))
          ((<= (char->integer #\a) n (char->integer #\z))
           (+ first-normal (- n (char->integer #\a))))
          ((= n (char->integer #\space))
           first-special)
          ((= n (char->integer #\,))
           (+ first-special 1))
          ((= n (char->integer #\.))
           (+ first-special 2))
          ((= n (char->integer #\:))
           (+ first-special 3))
          ((= n (char->integer #\;))
           (+ first-special 4))
          ((= n (char->integer #\'))
           (+ first-special 5))
          ((= n (char->integer #\"))
           (+ first-special 6))
          ((= n (char->integer #\())
           (+ first-special 7))
          ((= n (char->integer #\)))
           (+ first-special 8))
          ((= n (char->integer #\[))
           (+ first-special 9))
          ((= n (char->integer #\]))
           (+ first-special 10))
          ((= n (char->integer #\!))
           (+ first-special 11))
          ((= n (char->integer #\?))
           (+ first-special 12))
          ((= n (char->integer #\…))
           (+ first-special 13))
          ((= n (char->integer #\-))
           (+ first-special 14))
          ((= n (char->integer #\+))
           (+ first-special 15))
          ((= n (char->integer #\_))
           (+ first-special 16))
          ((= n (char->integer #\☺))
           (+ first-special 17))
          ((= n (char->integer #\☹))
           (+ first-special 18))
          ((= n (char->integer #\|))
           (+ first-special 19))
          (else (sub1 (/ (texture-w text-font) character-width))))))

(define (show-char! x y char font)
  (let* ((char-number (char-encoding char)))
    (render-copy! *renderer*
                  font
                  (make-rect (* char-number character-width) 0 character-width character-height)
                  (make-rect x y character-width character-height))))

(define (show-text! x y str #!optional (font text-font) (color #f))
  (when color
    (set! (texture-color-mod font) color))
  (let loop ((chars (string->list str))
             (x x))
    (unless (null? chars)
      (show-char! x y (car chars) font)
      (loop (cdr chars) (+ x character-width)))))

(define (show-boxed-text! x y str box-font #!optional (color '(0 0 0)))
  (let* ((box-x (- x character-width))
         (box-y (- y character-height))
         (lines (string-split str "\n"))
         (max-len (fold (lambda (s n) (max (string-length s) n)) 0 lines)))
    (show-lines!
     box-x box-y
     `(,(string-append "A" (make-string max-len #\B) "C")
       ,@(map (lambda (_) (string-append "D" (make-string max-len #\E) "F")) lines)
       ,(string-append "G" (make-string max-len #\H) "I"))
     box-font)
    (show-lines! x y lines text-font color)))

(define (show-lines! x y lines #!optional (font text-font) (color #f))
  (let loop ((rest lines)
             (y y))
    (unless (null? rest)
      (show-text! x y (car rest) font color)
      (loop (cdr rest) (+ y character-height)))))

(define (show-formated-text! str)
  (show-boxed-text! 20 (+ ceiling-y 20) str big-box))
