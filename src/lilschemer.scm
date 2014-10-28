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

(define eqset?
  (lambda (x y)
    (and (subset? x y)
         (subset? y x))))

(define intersect?
  (lambda (x y)
    (cond
     ((null? x) #f)
     (else
      (or (member? (car x) y)
          (intersect? (cdr x) y))))))

(define intersect
  (lambda (x y)
    (cond
     ((null? x) ())
     ((member? (car x) y) (cons (car x) (intersect (cdr x) y)))
     (else (intersect (cdr x) y)))))

(define union
  (lambda (x y)
    (cond
     ((null? x) y)
     ((member? (car x) y) (union (cdr x) y))
     (else
      (cons (car x) (union (cdr x) y))))))

(define a-pair?
  (lambda (x)
    (cond
     ((or (null? x) ;if it's zero items
          (atom? x) ;if it's only one item
          (null? (cdr x)) ;if it's a list of only one item
          (not (null? (cdr (cdr x))))) #f)
     (else #t))))

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (car (cdr x))))

(define third
  (lambda (x)
    (car (cdr (cdr x)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (build
             (second (car rel))
             (first (car rel)))
            (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

; use revpair
(define revrel2
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (revpair (car rel))
            (revrel2 (cdr rel)))))))

(define seconds
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (second (car rel)) (seconds (cdr rel)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))

(define remberf
  (lambda (test? x l)
    (cond
     ((null? l) (quote ()))
     ((test? x (car l)) (cdr l))
     (else (cons (car l) (remberf test? x (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (k)
      (eq? a k))))

(define rember-f
  (lambda (test?)
    (lambda (x l)
      (cond
       ((null? l) (quote ()))
       ((test? x (car l)) (cdr l))
       (else (cons (car l) ((rember-f test?) x (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       (else
        (cond
         ((test? (car l) old) (cons new (cons old (cdr l))))
         (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       (else
        (cond
         ((test? (car l) old) (cons old (cons new (cdr l))))
         (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))))

(define insert-before
  (lambda (new old l)
    (cons new (cons old (cdr l)))))

(define insert-after
  (lambda (new old l)
    (cons old (cons new (cdr l)))))

(define insert-g
  ;test? defines how we compare
  ;insert is the funciton that defines how we insert
  (lambda (test? insert)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       (else
        (cond
         ((test? (car l) old) (insert new old l))
         (else (cons (car l) ((insert-g test? insert) new old (cdr l))))))))))

(define subst-f (insert-g equal? (lambda (new old l) (cons new (cdr l)))))

(define multirember-f
  (lambda (test?)
    (lambda (x l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) x) ((multirember-f test?) x (cdr l)))
       (else
        (cons (car l)
              ((multirember-f test?) x (cdr l))))))))

(define multiremberT
  (lambda (eq?-x l)
    (cond
     ((null? l) (quote ()))
     ((eq?-x (car l)) (multiremberT eq?-x (cdr l)))
     (else
      (cons (car l)
            (multiremberT eq?-x (cdr l)))))))

(define multirember&co
  (lambda (a l col)
    (cond
     ((null? l) (col (quote ()) (quote ())))
     ((eq? (car l) a)
      (multirember&co a (cdr l) (lambda (newl seen) (col newl (cons (car l) seen)))))
     (else
      (multirember&co a (cdr l) (lambda (newl seen) (col (cons (car l) newl) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiInsertLR
  (lambda (new oldL oldR l)
    (cond
     ((null? l) (quote ()))
     ((eq? (car l) oldL) (cons new (cons oldL (multiInsertLR new oldL oldR (cdr l)))))
     ((eq? (car l) oldR) (cons oldR (cons new (multiInsertLR new oldL oldR (cdr l)))))
     (else
      (cons (car l) (multiInsertLR new oldL oldR (cdr l)))))))

(define multiInsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiInsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (+ L 1) R))))
      ((eq? (car lat) oldR)
       (multiInsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L (+ R 1)))))
      (else
        (multiInsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat)
                                 L R)))))))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* (car l) p) s))))
       (else (evens-only*&co (cdr l) (lambda (newl p s) (col newl p (+ (car l) s)))))))
     (else
      (evens-only*&co (car l) (lambda (newl p s) (evens-only*&co (cdr l) (lambda (newl2 p2 s2) (col (cons newl newl2) (* p p2) (+ s s2))))))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn) (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

(define pick
  (lambda (num lat)
    (cond
     ((= num 1) (car lat))
     (else (pick (- num 1) (cdr lat))))))

(define shift
  (lambda (p)
    (define (first x) (car x))
    (define (second x) (cadr x))
    (build (first (first p))
          (build (second (first p))
                (second p)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (length* (first pora))
         (length* (second pora)))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define almost-fibonacci
  (lambda (f)
    (lambda (n)
      (cond
       ((= n 0) 1)
       ((= n 1) 1)
       (else (+ (f (- n 1))
                (f (- n 2))))))))

(define fibonacci (Y almost-fibonacci))

(define almost-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

; Y combinator allows to have recursion without having a function look like it's calling itself
(define factorial (Y almost-factorial))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help
       name
       (cdr names)
       (cdr values)
       entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry
       name
       (car table)
       (lambda (name)
         (lookup-in-entry
          name
          (cadr table)
          table-f)))))))
