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
