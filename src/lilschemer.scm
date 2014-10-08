(define atom?
  (lambda (x)
    (not (or (pair? x) 
             (null? x)))))

(define lat?
  (lambda (x)
    (cond ((null? x) #t)
          ((atom? (car x)) (lat? (cdr x)))
          (else #f))))

(define member?
  (lambda (x l)
    (cond
     ((null? l) #f)
     (else (or (eq? (car l) x)
               (member? x (cdr l)))))))

(define rember
  (lambda (x l)
    (cond
     ((null? l) (quote ()))
     ((eq? (car l) x) (cdr l))
     (else (cons (car l) (rember x (cdr l)))))))

(define multirember
  (lambda (x l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((eq? (car l) x) (multirember x (cdr l)))
       (else (cons (car l) (multirember x (cdr l)))))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertRight
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((eq? (car l) old) (cons old (cons new (cdr l))))
       (else (cons (car l) (insertRight new old (cdr l)))))))))

(define multiInsertRight
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((eq? (car l) old) (cons old (cons new (multiInsertRight new old (cdr l)))))
       (else (cons (car l) (multiInsertRight new old (cdr l)))))))))
            
(define insertLeft
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((eq? (car l) old) (cons new (cons old (cdr l))))
       (else (cons (car l) (insertLeft new old (cdr l)))))))))

(define multiInsertLeft
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((eq? (car l) old) (cons new (cons old (multiInsertLeft new old (cdr l)))))
       (else (cons (car l) (multiInsertLeft new old (cdr l)))))))))

(define subst
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((eq? (car l) old) (cons new (cdr l)))
       (else (cons (car l) (subst new old (cdr l)))))))))

(define multiSubst
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((eq? (car l) old) (cons new (multiSubst new old (cdr l))))
       (else (cons (car l) (multiSubst new old (cdr l)))))))))

(define subst2
  (lambda (new old1 old2 l)
    (cond
     ((null? l) (quote ()))
     (else
      (cond
       ((or (eq? (car l) old1) (eq? (car l) old2)) (cons new (cdr l)))
       (else (cons (car l) (subst2 new old1 old2 (cdr l)))))))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define addtup
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (+ (car l) (addtup (cdr l)))))))

(define tup+
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     ((null? l2) l1)
     (else (cons (+ (car l1) (car l2)) (tup+ (cdr l1) (cdr l2)))))))

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (+ 1 (length (cdr l)))))))

(define pick
  (lambda (n l)
    (cond
     ((= 0 (- n 1)) (car l))
     (else (pick (- n 1) (cdr l))))))

(define rempick
  (lambda (n l)
    (cond
     ((= 0 (- n 1)) (cdr l))
     (else (cons (car l) (rempick (- n 1) (cdr l)))))))

(define rempick2
  (lambda (n l)
    (cond
     ((is-one? n) (cdr l))
     (else (cons (car l) (rempick2 (- n 1) (cdr l)))))))

(define no-nums
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cond
            ((number? (car l)) (no-nums (cdr l)))
            (else (cons (car l) (no-nums (cdr l)))))))))

(define all-nums
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cond
            ((number? (car l)) (cons (car l) (all-nums (cdr l))))
            (else (all-nums (cdr l))))))))

(define equiv-atoms
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2) #f))
     (else (eq? a1 a2)))))

(define occurs
  (lambda (x l)
    (cond
     ((null? l) 0)
     (else (cond
            ((equiv-atoms x (car l)) (+ 1 (occurs x (cdr l))))
            (else (occurs x (cdr l))))))))

(define is-one?
  (lambda (x)
    (= x 1)))
