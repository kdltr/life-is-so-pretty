(use srfi-1)

(define num-objects 8)
(define num-choices 4)
(define max-life 12)

(define (choose-good-objects n)
  (unless (= n 0)
    (let ((obj (random num-objects)))
      (if (vector-ref choices obj)
          (choose-good-objects n)
          (begin
            (vector-set! choices obj #t)
            (choose-good-objects (sub1 n)))))))

(define (interact l)
  (if (not (= (length l) num-choices))
      'nope
      (count
       identity
       (map
        (lambda (n)
          (if (vector-ref choices n)
              #t
              (begin (set! life (sub1 life))
                     #f)))
        l))))

(define life max-life)
(define choices (make-vector num-objects #f))
(choose-good-objects num-choices)
