(define (random-elt l)
  (list-ref l (random (length l))))

(define (permutation l)
  (if (null? l)
      '()
      (let ((elt (random-elt l)))
        (cons elt (permutation (delete elt l))))))
