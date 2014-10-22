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

(define rember2
  (lambda (x l)
    (cond
     ((null? l) (quote ()))
     ((equal? x (car l)) (cdr l))
     (else (cons (car l) (rember2 x (cdr l)))))))

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

(define rember*
  (lambda (x l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((equiv-atoms x (car l)) (rember* x (cdr l)))
                       (else (cons (car l) (rember* x (cdr l))))))
     (else (cons (rember* x (car l)) (rember* x (cdr l)))))))

(define insertRight*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((equiv-atoms old (car l)) (cons old (cons new (insertRight* new old (cdr l)))))
                       (else (cons (car l) (insertRight* new old (cdr l))))))
     (else (cons (insertRight* new old (car l)) (insertRight* new old (cdr l)))))))

(define insertLeft*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((equiv-atoms old (car l)) (cons new (cons old (insertLeft* new old (cdr l)))))
                       (else (cons (car l) (insertLeft* new old (cdr l))))))
     (else (cons (insertLeft* new old (car l)) (insertLeft* new old (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((equiv-atoms old (car l)) (cons new (subst* new old (cdr l))))
                       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define occurs*
  (lambda (x l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
                       ((equiv-atoms x (car l)) (+ 1 (occurs* x (cdr l))))
                       (else (+ 0 (occurs* x (cdr l))))))
     (else (+ (occurs* x (car l)) (occurs* x (cdr l)))))))

(define member*
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (equiv-atoms x (car l)) (member* x (cdr l))))
     (else (or (member* x (car l)) (member* x (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define eqlist
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2)) (eqlist (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (x y)
    (cond
     ((and (atom? x) (atom? y)) (equiv-atoms x y))
      ((or (atom? x) (atom? y)) #f)
      (else (eqlist x y)))))

(define numbered?
  (lambda (x)
    (cond
     ((atom? x) (number? x))
     (else (and (numbered? (car x))
                (numbered? (car (cdr (cdr x)))))))))

; For simplicity, we'll only consider raising a number by a number >= 0
; For raising a number to a power < 0, treat as raising to 0th power
(define raise
  (lambda (x y)
    (cond
     ((or (= y 0)
          (< y 0))
      1)
     ((= y 1) x)
     (else (* x (raise x (- y 1)))))))

(define valid-operator?
  (lambda (op)
    (or (eq? (quote +) op)
        (eq? (quote *) op)
        (eq? (quote -) op)
        (eq? (quote raise) op))))

(define operator
  (lambda (x)
    (car (cdr x))))

; This also only works for superior lisps (like Racket)
; This allows us to evaluate arithmetic expressions that look like '(1 + 4)
; This doesn't allow us to deal with actual lisp prefix notation style expressions like (+ 1 4)
(define value
  (lambda (x)
    (cond
     ((atom? x) x)
     ((valid-operator? (operator x))
      ((eval (operator x))
       (value (car x))
       (value (car (cdr (cdr x))))))
     (else 0))))

(define operator-2
  (lambda (x)
    (car x)))

(define first-sub-expr
  (lambda (x)
    (car (cdr x))))

(define second-sub-expr
  (lambda (x)
    (car (cdr (cdr x)))))

; This version evalues standard prefix notation things like (+ 1 4)
(define value-2
  (lambda (x)
    (cond
     ((atom? x) x)
     ((valid-operator? (operator-2 x))
      ((eval (operator-2 x))
       (value-2 (first-sub-expr x))
       (value-2 (second-sub-expr x)))))))

; Works for MIT scheme (not as 'cool')
(define (value-3 x)
  (define (first x) (car x))
  (define (second x) (car (cdr (cdr x))))
  (define (operator x) (car (cdr x)))
  (cond
      ((atom? x) x)
      ((eq? (operator x) (quote +))
       (+ (value-3 (first x))
          (value-3 (second x))))
      ((eq? (operator x) (quote *))
       (* (value-3 (first x))
          (value-3 (second x))))
      ((eq? (operator x) (quote raise))
       (raise (value-3 (first x))
              (value-3 (second x))))))

; Prefix notation version that works for MIT scheme
(define (value-4 x)
  (define (first x) (car (cdr x)))
  (define (second x) (car (cdr (cdr x))))
  (define (operator x) (car x))
  (cond
     ((atom? x) x)
     ((eq? (operator x) (quote +))
      (+ (value-4 (first x))
         (value-4 (second x))))
     ((eq? (operator x) (quote *))
      (* (value-4 (first x))
         (value-4 (second x))))
     ((eq? (operator x) (quote raise))
      (raise (value-4 (first x))
             (value-4 (second x))))))

(define (sero? x)
  (null? x))

(define (paren-add1 x)
  (cons (quote ()) x))

(define (paren-sub1 x)
  (cdr x))

(define set?
  (lambda (x)
    (cond
     ((null? x) #t)
     ((member? (car x) (cdr x)) #f)
     (else
      (set? (cdr x))))))

(define makeset
  (lambda (x)
    (cond
     ((null? x) ())
     (else (cons (car x) (makeset (multirember (car x) (cdr x))))))))

(define subset?
  (lambda (x y)
    (cond
     ((null? x) #t)
     ((and (member? (car x) y)
           (subset? (cdr x) y)))
     (else #f))))
