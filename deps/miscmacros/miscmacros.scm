;;;; miscmacros.scm

(module miscmacros
  (modify-location
   let/cc until
   repeat while repeat* if* while*
   dotimes push! pop! inc! dec! exchange! modify!
   begin0
   define-optionals define-parameter define-enum
   ignore-values ignore-errors
   ecase
   define-syntax-rule
   cond-list)

  (import scheme)
  ;; No effect -- caller must import these manually.
  (import (only chicken
                when unless handle-exceptions let-optionals make-parameter
                add1 sub1))

;;; Modify locations, T-like:

;; syntax-case implementation -- unused
;;   (define-syntax (modify-location x)
;;     (syntax-case x ()
;;       ((_ (loc ...) proc)
;;        (with-syntax (((tmp ...) (generate-temporaries #'(loc ...))))
;;          #'(let ((tmp loc) ...)
;;              (proc (lambda () (tmp ...))
;;                    (lambda (x) (set! (tmp ...) x)))) ) )
;;       ((_ loc proc)
;;        #'(proc (lambda () loc)
;;                (lambda (x) (set! loc x)) ) ) ) )

  (define-syntax modify-location
    (lambda (f r c)
      (##sys#check-syntax 'modify-location f '(_ _ _))
      (let ((loc (cadr f))
            (proc (caddr f))
            (%lambda (r 'lambda))
            (%set! (r 'set!))
            (%let (r 'let))
            (x (r 'x)))                 ; a temporary
        (if (atom? loc)
            `(,proc (,%lambda () ,loc)
                    (,%lambda (,x) (,%set! ,loc ,x)))
            (let ((tmps (map (lambda _ (r (gensym))) loc)))
              `(,%let ,(map list tmps loc)
                      (,proc (,%lambda () ,tmps)
                             (,%lambda (,x) (,%set! ,tmps ,x)))))))))

;; evaluates body with an explicit exit continuation
;;
  (define-syntax let/cc
    (syntax-rules ()
      ((let/cc k e0 e1 ...)
       (call-with-current-continuation
        (lambda (k) e0 e1 ...)))))

;; loop while expression false
;;
  (define-syntax until
    (syntax-rules ()
      ((until test body ...)
       (let loop ()
         (unless test
           body ...
           (loop))))))

  (define-syntax repeat
    (syntax-rules ()
      ((repeat n body ...)
       (let loop ((i n))
         (when (< 0 i)
           body ...
           (loop (sub1 i)))))))

  (define-syntax while
    (syntax-rules ()
      ((while test body ...)
       (let loop ()
         (if test
             (begin
               body ...
               (loop)))))))

;; repeat*, if*, while*: versions which break hygiene to assign to 'it'
(define-syntax repeat*
  (lambda (f r c)
    (##sys#check-syntax 'repeat* f '(_ _ . _))
    (let ((loop (r 'loop))
          (n (cadr f))
          (body (cddr f)))
      `(,(r 'let) ,loop ((it ,n))
        (,(r 'when) (,(r '<) 0 it)
         ,@body
         (,loop (,(r '-) it 1)))))))

(define-syntax if*
  (lambda (f r c)
    (##sys#check-syntax 'if* f '(_ _ _ . _))
    (let ((x (cadr f))
          (y (caddr f))
          (z (cdddr f))
          (var (r 'var)))
      `(,(r 'let) ((,var ,x))
        (,(r 'if) ,var
         (,(r 'let) ((it ,var))
          ,y)
         ,@z)))))

(define-syntax while*
  (lambda (f r c)
    (##sys#check-syntax 'while* f '(_ _ . _))
    (let ((test (cadr f))
          (body (cddr f)))
      `(,(r 'let) ,(r 'loop) ()
        (,(r 'if*) ,test
         (,(r 'begin)
          ,@body
          (,(r 'loop)) ))))))

;; repeat body n times, w/ countup n bound to v
  (define-syntax dotimes
    (syntax-rules ()
      ((dotimes (v n) body ...)
       (dotimes (v n (begin)) body ...))
      ((dotimes (v n f) body ...)
       (let loop ((v 0) (nv n))
         (if (< v nv)
             (begin
               body ...
               (loop (add1 v) nv))
             f)))))

  (define-syntax push!
    (syntax-rules ()
      ((push! x loc)
       (modify-location loc
                        (lambda (get set)
                          (set (cons x (get))))))))

  (define-syntax pop!
    (syntax-rules ()
      ((pop! loc)
       (modify-location loc
                        (lambda (get set)
                          (let* ((var (get))
                                 (var2 (car var)))
                            (set (cdr var))
                            var2))))))

  (define-syntax inc!
    (syntax-rules ()
      ((inc! loc val)
       (modify-location loc
                        (lambda (get set)
                          (let ((new (+ (get) val)))
			    (set new)
			    new))))
      ((inc! loc) (inc! loc 1))))

  (define-syntax dec!
    (syntax-rules ()
      ((dec! loc val)
       (modify-location loc
                        (lambda (get set)
                          (let ((new (- (get) val)))
			    (set new)
			    new))))
      ((dec! loc) (dec! loc 1))))

  (define-syntax exchange!
    (syntax-rules ()
      ((exchange! x y)
       (modify-location
        x
        (lambda (get1 set1)
          (modify-location
           y
           (lambda (get2 set2)
             (let ((tmp (get1)))
               (set1 (get2))
               (set2 tmp)))))))))

  (define-syntax modify!
    (syntax-rules ()
      ((modify! loc proc)
       (modify-location loc
                        (lambda (get set)
                          (set (proc (get))))))))

  (define-syntax begin0
    (syntax-rules ()
      ((_ e0 e1 ...)
       (##sys#call-with-values
        (lambda () e0)
        (lambda var
          (begin
            e1 ...
            (apply ##sys#values var)))))))

  (define-syntax define-optionals
    (lambda (f r c)
      (let ((vars (cadr f))
            (args (caddr f)))
        (##sys#check-syntax 'define-optionals f '(_ #(#(_ 2 2) 1) _))
        `(,(r 'begin)
          ,@(map (lambda (b) `(,(r 'define) ,(car b) #f)) vars)
          ,(let ([aliases (map (lambda (b) (r (car b))) vars)])
             `(,(r 'let-optionals) ,args
               ,(map (lambda (b a) (cons a (cdr b))) vars aliases)
               ,@(map (lambda (b a) `(,(r 'set!) ,(car b) ,a)) vars aliases) ) ) ))) )

  (define-syntax define-parameter
    (syntax-rules ()
      ((define-parameter name value guard)
       (define name (make-parameter value guard)))
      ((define-parameter name value)
       (define name (make-parameter value)))
      ((define-parameter name)
       (define name (make-parameter (void))))))

  (define-syntax ignore-values
    (syntax-rules ()
      ((ignore-values exp)
       (##sys#call-with-values (lambda () exp)
                               (lambda _ (##sys#void))))))

  (define-syntax ignore-errors
    (syntax-rules ()
      ((ignore-errors body ...)
       (handle-exceptions _ #f body ...))))

;;; The following is courtesy of Alex Shinn:

  (define-syntax define-enum
    (lambda (f r c)
      (define (enumerate vars)
        (let loop ((n 0) (enums '()) (vars vars))
          (if (null? vars)
              (reverse enums)
              (let ((n (if (pair? (car vars))
                           (cadar vars)
                           n)))
                (loop (+ n 1)
                      (cons n enums)
                      (cdr vars))))))
      (##sys#check-syntax 'define-enum f '(_ _ _ . _))
      (let ((->int (cadr f))
            (->sym (caddr f))
            (vars (cdddr f)))
        (let ((ints (enumerate vars))
              (vars (map (lambda (v) (if (pair? v) (car v) v)) vars)))
          `(,(r 'begin)
            ,@(map (lambda (x i)
                     `(,(r 'define-constant) ,x ,i))
                   vars ints)
            (,(r 'define) (,->int ,(r 'sym))
             (,(r 'case) ,(r 'sym)
              ,@(map (lambda (x i)
                       `((,x) ,i))
                     vars ints)
              (,(r 'else) #f)))
            (,(r 'define) (,->sym ,(r 'int))
             (,(r 'case) ,(r 'int)
              ,@(map (lambda (x i)
                       `((,i) ',x))
                     vars ints)
              (,(r 'else) #f))))))))

(define-syntax define-syntax-rule
  (er-macro-transformer
   (lambda (x r c)
     (if (or (not (pair? (cdr x)))
	     (not (pair? (cadr x)))
	     (not (symbol? (caadr x))))
	 (syntax-error "invalid argument syntax")
	 (let ((name (caadr x))
	       (args (cdadr x)))
	   `(,(r 'define-syntax) ,name
	     (,(r 'syntax-rules) ()
	      ((_ . ,(cdadr x)) ,@(cddr x)))))))))

(define-syntax-rule (ecase val clauses ...)
  (case val
    clauses ...
    (else (error "no valid case" val))))

(define-syntax cond-list
  (syntax-rules ()
    ((cond-list) (list))
    ((cond-list (c e) rest ...)
     (let ((tail (cond-list rest ...)))
       (if c
           (cons e tail)
           tail)))))

)
