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
