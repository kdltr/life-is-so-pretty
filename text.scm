
(define small-box
  (create-texture-from-surface *renderer* (img:load "small-box.png")))
(define font
  (create-texture-from-surface *renderer* (img:load "font.png")))
(define character-width 6)
(define character-height 12)

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
          ((= n (char->integer #\"))
           (+ first-special 5))
          ((= n (char->integer #\())
           (+ first-special 6))
          ((= n (char->integer #\)))
           (+ first-special 7))
          ((= n (char->integer #\[))
           (+ first-special 8))
          ((= n (char->integer #\]))
           (+ first-special 9))
          ((= n (char->integer #\!))
           (+ first-special 10))
          ((= n (char->integer #\?))
           (+ first-special 11))
          ((= n (char->integer #\…))
           (+ first-special 12))
          ((= n (char->integer #\-))
           (+ first-special 13))
          ((= n (char->integer #\+))
           (+ first-special 14))
          ((= n (char->integer #\_))
           (+ first-special 15))
          ((= n (char->integer #\☺))
           (+ first-special 16))
          ((= n (char->integer #\☹))
           (+ first-special 17))
          ((= n (char->integer #\|))
           (+ first-special 18))
          (else (sub1 (/ (texture-w font) character-width))))))

(define (show-char! x y char)
  (let* ((char-number (char-encoding char)))
    (render-copy! *renderer*
                  font
                  (make-rect (* char-number character-width) 0 character-width character-height)
                  (make-rect x y character-width character-height))))

(define (show-text! x y str #!optional (color '(0 0 0)))
  (set! (texture-color-mod font) color)
  (let loop ((chars (string->list str))
             (x x))
    (unless (null? chars)
      (show-char! x y (car chars))
      (loop (cdr chars) (+ x character-width)))))

(define (show-boxed-text! x y str #!optional (color '(0 0 0)))
  (let ((box-x (- x character-width))
        (box-y (- y 5)))
    (render-copy! *renderer*
                  small-box
                  (make-rect 0 0 character-width (texture-h small-box))
                  (make-rect box-x box-y character-width (texture-h small-box)))
    (dotimes (i (string-length str))
      (render-copy! *renderer*
                    small-box
                    (make-rect character-width 0 character-width (texture-h small-box))
                    (make-rect (+ box-x (* (add1 i) character-width))
                               box-y
                               character-width
                               (texture-h small-box))))
    (render-copy! *renderer*
                  small-box
                  (make-rect (- (texture-w small-box) character-width)
                             0
                             character-width
                             (texture-h small-box))
                  (make-rect (+ x (* (string-length str) character-width))
                             box-y
                             character-width
                             (texture-h small-box))))
  (show-text! x y str color))
