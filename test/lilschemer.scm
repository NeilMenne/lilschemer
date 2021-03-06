(load "lib/test-manager/load.scm")
(load "src/lilschemer.scm")
(in-test-group
  Chapter1
  (define-test (lists-are-not-atoms)
    (define l (list 1 2 3))
    (assert-false (atom? l) "Lists are not atoms")
  )
  (define-test (arbitrary-eval)
    (assert-true (atom? 'example) "Nailed it!")
  )
  (define-test (numbers-are-atoms)
    (assert-true (atom? 3) "Integer numbers are atoms")
    (assert-true (atom? 3.5) "Decimal numbers are atoms")
    (assert-true (atom? 2/3) "Fractional numbers are atoms")
  )
  (define-test (strings-are-atoms)
    (assert-true (atom? "test") "Strings are atoms")
  )
  (define-test (symbols-are-atoms)
    (assert-true (atom? 'a) "Arbitrary symbols are atoms")
  )
)

(in-test-group
  Chapter2
  (define-test (list-of-atoms)
    (define l '(1 2 3))
    (assert-true (lat? l) "List is only composed of atoms")
  )
  (define-test (list-of-atoms-2)
    (define l '(thing1 thing2 thing3))
    (assert-true (lat? l) "List is only composed of atoms")
  )
  (define-test (list-of-not-just-atoms)
    (define l '(1 2 '(3 4)))
    (assert-false (lat? l) "Lists that aren't only atoms return false")
  )
  (define-test (member-of-list)
    (define l '(3 2 1))
    (assert-true (member? 1 l) "1 is a member of l")
  )
)

(in-test-group
  Chapter3:Rember
  (define-test (removes-occurrence)
    (define l '(1 2 3))
    (define out-l '(1 2))
    (assert-equals out-l (rember 3 l) "Removes the only occurrence")
  )
  (define-test (removes-occurrence-two)
    (define l '(thing3 thing2 thing3))
    (define out-l '(thing2 thing3))
    (assert-equals out-l (rember 'thing3 l) "Removes only the first occurrence")
  )
  (define-test (removes-only-first-occurrence)
    (define l '(1 1 1))
    (define out-l '(1 1))
    (assert-equals out-l (rember 1 l) "Removes the first occurrence")
  )
  (define-test (does-no-harm)
    (define l '(1 2 3))
    (assert-equals l (rember 4 l) "Doesn't do anything if the item doesn't exist")
  )
  (define-test (removes-all-occurrences)
    (define l '(1 3 1 4 1))
    (define out-l '(3 4))
    (assert-equals out-l (multirember 1 l) "Multirember removes all occurrences of the atom")
  )
)

(in-test-group
  Chapter3:Firsts
  (define-test (gets-first-from-each-s-expression)
    (define l (list (list 1 2 3) (list 4 5 6) (list 6 7 8)))
    (define out-l (list 1 4 6))
    (assert-equals out-l (firsts l) "Gets the first from each of the nested s-expressions")
  )
  (define-test (gets-first-from-each-arbitrary-s-expression)
    (define l (list (list 'thing 'other) (list 'thing2 'other2) (list 'thing3 'other3)))
    (define out-l (list 'thing 'thing2 'thing3))
    (assert-equals out-l (firsts l) "Gets the first from each of the nested s-expressions")
  )
)

(in-test-group
  Chapter3:Inserts
  (define-test (inserts-to-the-right-of-another-s-expression)
    (define l (list (quote thing1) (quote thing3)))
    (define out-l (list (quote thing1) (quote thing2) (quote thing3)))
    (assert-equals out-l (insertRight (quote thing2) (quote thing1) l) "Inserts to the right of the old atom")
  )
  (define-test (inserts-to-the-right)
    (define l '(1 2 4))
    (define out-l '(1 2 3 4))
    (assert-equals out-l (insertRight 3 2 l) "Inserts the new atom after the old one")
  )
  (define-test (inserts-to-the-left)
    (define l '(1 3 4))
    (define out-l '(1 2 3 4))
    (assert-equals out-l (insertLeft 2 3 l) "Inserts the new atom to the left of the old one")
  )
  (define-test (inserts-to-the-left-s-expression)
    (define l (list (quote thing2) (quote thing3)))
    (define out-l (list (quote thing1) (quote thing2) (quote thing3)))
    (assert-equals out-l (insertLeft (quote thing1) (quote thing2) l) "Inserts the new atom to the left")
  )
  (define-test (substitutes)
    (define l '(1 5 3))
    (define out-l '(1 2 3))
    (assert-equals out-l (subst 2 5 l) "Replaces the old atom with the new atom")
  )
  (define-test (substitutes-for-the-first-of-two)
    (define l '(1 5 3 6))
    (define out-l '(1 2 3 6))
    (assert-equals out-l (subst2 2 5 6 l) "Replaces the first old atom with the new atom")
  )
  (define-test (substitutes-for-the-second-of-two)
    (define l '(1 6 3 5))
    (define out-l '(1 2 3 5))
    (assert-equals out-l (subst2 2 5 6 l) "Replaces the second old atom with the new atom")
  )
)

(in-test-group
  Chapter3:MultiInsert
  (define-test (inserts-to-the-right-of-each-occurrence)
    (define l '(1 3 1 3 1))
    (define out-l '(1 2 3 1 2 3 1 2))
    (assert-equals out-l (multiInsertRight 2 1 l) "Inserts to the right of each occurrence of an atom")
  )
  (define-test (inserts-to-the-left-of-each-occurrence)
    (define l '(1 3 1 3 1))
    (define out-l '(1 2 3 1 2 3 1))
    (assert-equals out-l (multiInsertLeft 2 3 l) "Inserts to the left of each occurrence of an atom")
  )
  (define-test (replaces-each-occurrence)
    (define l '(1 3 1 3))
    (define out-l '(1 2 1 2))
    (assert-equals out-l (multiSubst 2 3 l) "Replaces the each occurrence of the old atom with the new atom")
  )
)

(in-test-group
  Chapter4:BasicMath
  (define-test (add1-does-that)
    (define x 4)
    (assert-equals 5 (add1 4) "Adds 1")
    (assert-equals 5 (add1 x) "Adds 1")
  )
  (define-test (sub1-does-that)
    (define x 4)
    (assert-equals 3 (sub1 4) "Subtracts 1")
    (assert-equals 3 (sub1 x) "Subtracts 1")
  )
  (define-test (addtup-does-that)
    (define t '(1 2 3))
    (assert-equals 6 (addtup t) "Adds all of the numbers in the tuple")
  )
  (define-test (tupplus-does-its-job)
    (define t1 '(1 2 3))
    (define t2 '(3 2 1))
    (assert-equals '(4 4 4) (tup+ t1 t2) "Adds each of the members of the tuple to the equivalent member")
  )
  (define-test (tupplus-does-its-job-for-unbalanced-tuples)
    (define t1 '(1 2 3))
    (define t2 '(3 2 1 4))
    (assert-equals '(4 4 4 4) (tup+ t1 t2) "Should handle unbalanced tuples too")
  )
  (define-test (length-does-its-job)
    (define l '(1 2 3))
    (assert-equals 3 (length l) "Counts the number of atoms in a list")
  )
  (define-test (pick-does-its-job)
    (define l '(5 4 3 2 1))
    (assert-equals 1 (pick 5 l) "Picking the 5th atom should return 1")
  )
  (define-test (rempick-does-its-job)
    (define l '(5 4 6 2 1))
    (assert-equals '(5 4 2 1) (rempick 3 l) "Pick all except pick")
  )
  (define-test (no-nums-does-its-job)
    (define l '(5 "the" 4 "answer" 4 3))
    (assert-equals '("the" "answer") (no-nums l) "Only the non-numeric atoms remain")
  )
  (define-test (all-nums-does-its-job)
    (define l '(5 "the" 4 "answer" 4 3))
    (assert-equals '(5 4 4 3) (all-nums l) "Only the numeric atoms remain")
  )
  (define-test (equiv-atoms-does-its-job)
    (assert-true (equiv-atoms 2 2) "numbers of the same value are the same")
    (define x 2)
    (assert-true (equiv-atoms x 2) "numbers and atoms that share the same value are the same")
    (assert-false (equiv-atoms x 3) "numbers and atoms that don't share the same value are different")
    (assert-false (equiv-atoms 3 2) "numbers that are different are in fact different")
    (assert-true (equiv-atoms "test" 3) "Numbers and other kinds of atoms are different")
  )
  (define-test (occurs-does-its-job)
    (define l '(1 1 1 1))
    (assert-equals 4 (occurs 1 l) "counts the number of occurrences properly")
  )
  (define-test (is-one-does-its-job)
    (assert-true (is-one? 1) "the number one is one")
    (define x 1)
    (assert-true (is-one? x) "atoms with a value of one are one")
    (assert-false (is-one? 3) "other values are not one")
  )
  (define-test (rempick2-does-its-job)
    (define l '(5 4 3 2 1))
    (assert-equals '(5 3 2 1) (rempick2 2 l) "remove second atom")
  )
)

(in-test-group
  Chapter5:ISee*s
  (define-test (removes-all-occurrences)
    (define l (list 1 3 (list 1) 4 (list 4 1)))
    (define out-l (list 3 (list ) 4 (list 4)))
    (assert-equals out-l (rember* 1 l) "rember* removes all occurrences of the atom")
  )
  (define-test (inserts-to-the-right-of-all-occurrences)
    (define l (list 1 3 (list 1) 4 (list 4 1)))
    (define out-l (list 1 2 3 (list 1 2) 4 (list 4 1 2)))
    (assert-equals out-l (insertRight* 2 1 l) "insertRight* inserts to the right of all occurrences")
  )
  (define-test (counts-all-occurrences)
    (define l (list 1 3 (list 1) 4 (list 4 1)))
    (assert-equals 3 (occurs* 1 l) "occurs* counts all occurrences properly")
  )
  (define-test (replaces-all-occurrences)
    (define l (list 1 3 (list 1) 4 (list 4 1)))
    (define out-l (list 2 3 (list 2) 4 (list 4 2)))
    (assert-equals out-l (subst* 2 1 l) "replace all occurrences")
  )
  (define-test (inserts-to-the-left-of-all-occurrences)
    (define l (list 2 3 (list 2) 4 (list 4 2)))
    (define out-l (list 1 2 3 (list 1 2) 4 (list 4 1 2)))
    (assert-equals out-l (insertLeft* 1 2 l) "insertRight* inserts to the right of all occurrences")
  )
  (define-test (membership-is-properly-determined)
    (define l (list 1 2 (list 1) 3 (list 4 2)))
    (assert-true (member* 3 l) "properly identifies top-level occurrences")
    (assert-true (member* 4 l) "properly identifies nested occurrences")
  )
  (define-test (leftmost-works-as-intended)
    (define l (list 1 2 (list 3) 4))
    (assert-equals 1 (leftmost l) "finds the leftmost thing")
    (define l2 (list (list 1 2) 3 4 (list 4 5)))
    (assert-equals 1 (leftmost l) "finds the leftmost thing even if it's in a list")
  )
  (define-test (eqlist-works-as-intended)
    (define l1 (list 1 2 (list 3) 4))
    (define l2 (list 1 2 (list 4) 3))
    (assert-true (eqlist l1 l1) "identical lists are identical")
    (assert-false (eqlist l1 l2) "non-identical lists aren't identical")
  )
  (define-test (equals?-works-as-intended)
    (define x (list 1 2 (list 3) 4))
    (assert-true (equal? x x) "identical lists are equal")
    (define y (list 1 2 (list 4) 3))
    (assert-false (equal? x y) "non-identical lists aren't identical")
    (assert-false (equal? x 3) "atoms vs lists aren't identical")
  )
  (define-test (rember2-removes-whole-s-expressions)
    (define x (list 1 2 (list 3 4) 5))
    (assert-equal (list 1 2 5) (rember2 (list 3 4) x) "remove the s-expression not just the atom")
  )
)

(in-test-group
  Chapter6:Shadows
  (define-test (numbered?-works)
    (assert-true (numbered? 2) "numbers are numbers")
    (assert-true (numbered? '(1 + 3)) "arithmetic expressions are cool too")
  )
  (define-test (value-3-works)
    (assert-equals 9 (value-3 '(8 + 1)) "add")
    (assert-equals 10 (value-3 '(5 * 2)) "multiply")
    (assert-equals 8 (value-3 '(2 raise 3)) "raise")
  )
  (define-test (value-4-works)
    (assert-equals 8 (value-4 '(* 4 2)) "multiply prefix style")
    (assert-equals 9 (value-4 '(+ (* 4 2) 1)) "recurses properly")
    (assert-equals 8 (value-4 '(raise 2 3)) "prefix notation does its thing")
  )
  (define-each-test
    (assert-true (sero? '()) "sero? works as expected")
    (assert-equals '(()) (paren-add1 '()) "add1 works for this method")
    (assert-equals '(() ()) (paren-add1 '(())) "try add1 for non-zero")
    (assert-equals '(()) (paren-sub1 '(() ())) "sub1 works")
  )
)

(in-test-group
 Chapter7:FriendsAndRelations
 (define-each-test
   ; set? tests
   (assert-true (set? '(a b c)) "No repeating members")
   (assert-false (set? '(a b c a)) "Has repeating members")
   (assert-true (set? '()) "No members is okay too")
   (assert-true (set? '(1 2 3)) "Works for numbers")
   (assert-false (set? '(1 2 3 1)) "False when duplicate numbers exist")
 )
 (define-test (makeset-works)
   (define l '(a b c d a b c e))
   (define out-l '(a b c d e))
   (assert-equal out-l (makeset l) "makes a unique set as expected")
 )
 (define-test (subset?-works)
   (assert-true (subset? '(a) '(a b c)) "subset works")
   (assert-true (subset? '() '(a b c)) "null is always a subset")
   (assert-false (subset? '(d) '(a b c)) "false when it should be false")
 )
 (define-test (eqset?-works)
   (assert-true (eqset? '(a b c) '(a b c)) "equivalence")
   (assert-true (eqset? '(c a b) '(a b c)) "order doesn't matter")
   (assert-false (eqset? '(d a b) '(a b c)) "they're different")
 )
 (define-test (intersect?-works)
   (assert-true (intersect? '(a b c) '(b d e)) "intersection of a single atom")
   (assert-false (intersect? '(a c d) '(b e f)) "properly finds no intersection")
 )
 (define-test (intersect-works)
   (assert-equals '(a b) (intersect '(a c b) '(b d e a f)) "intersection found")
   (assert-equals '() (intersect '(a b c) '(d e f)) "no intersection available")
 )
 (define-test (union-works)
   (assert-equals '(a b d e) (union '(a b) '(d e)) "union is union")
   (assert-equals '(a b d e) (union '(a b d e) '()) "all of set 1")
   (assert-equals '(a b e d) (union '() '(a b e d)) "all of set 2")
 )
 (define-each-test
   (assert-false (a-pair? 1) "it's an atom")
   (assert-false (a-pair? '(a)) "it's a list of only one atom")
   (assert-false (a-pair? '(a b c)) "it's more than two")
   (assert-true (a-pair? '(() 1)) "empty lists count")
   (assert-true (a-pair? '(a b)) "it's a pair!")
 )
 (define-test (revrel-works)
   (assert-equal '((a 8) (pie pumpkin) (sick got)) (revrel '((8 a) (pumpkin pie) (got sick))) "revrel")
   (assert-equal '((a 8) (pie pumpkin) (sick got)) (revrel2 '((8 a) (pumpkin pie) (got sick))) "revrel2")
 )
 (define-test (second-sucks) ;song reference, the function doesn't suck
   (assert-equal '(raisin prune grape) (seconds '((grape raisin) (plum prune) (stewed grape))) "seconds found")
 )
 (define-test (fullfun?-works)
   (assert-true (fullfun? '((grape raisin) (plum prune) (stewed grape))) "fullfun? properly identifies things")
   (assert-false (fullfun? '((grape raisin) (plum raisin))) "returns false when it should")
 )
 (define-test (one-to-one?-works)
   (assert-true (one-to-one? '((grape raisin) (plum prune) (stewed grape))) "one-to-one? though different, also properly identifies things")
   (assert-false (one-to-one? '((grape raisin) (plum raisin))) "returns false when it should")
 )
)

(in-test-group
 Chapter8:LambdatheUltimate
 (define-test (remberf-test)
   (assert-equal '(jelly beans are good) (remberf eq? 'not '(jelly beans are not good)) "eq? works as a test function")
   (assert-equal '(1 2 3) (remberf equal? 4 '(1 2 3 4)) "remberf can be passed any test function")
 )
 (define-each-test
   (assert-true ((eq?-c 'salad) 'salad) "intro to functions returning functions")
   (assert-equal '(jelly beans are good) ((rember-f eq?) 'not '(jelly beans are not good)) "rember-f 2.0")
   (assert-equal '(1 2 3) ((rember-f equal?) 4 '(1 2 3 4)) "pass a different function")
   (assert-equal '(1 2 3 4) ((insertL-f equal?) 3 4 '(1 2 4)) "the new insertL")
   (assert-equal '(1 2 3 4) ((insertR-f equal?) 4 3 '(1 2 3)) "the new insertR")
   (assert-equal '(1 2 3 4) ((insert-g equal? insert-before) 3 4 '(1 2 4)) "insert left as a result of insert-g")
   (assert-equal '(1 2 3 4) ((insert-g equal? (lambda (new old l) (cons old (cons new (cdr l))))) 4 3 '(1 2 3)) "insert right with anon fn")
 )
 (define-test (substitutes)
    (define l '(1 5 3))
    (define out-l '(1 2 3))
    (assert-equals out-l (subst-f 2 5 l) "Replaces the old atom with the new atom")
  )
 (define-test (removes-all-occurrences)
    (define l '(1 3 1 4 1))
    (define out-l '(3 4))
    (assert-equals out-l ((multirember-f eq?) 1 l) "Multirember removes all occurrences of the atom")
  )
 (define-test (removes-all-occurrences)
    (define l '(1 3 1 4 1))
    (define out-l '(3 4))
    (assert-equals out-l (multiremberT (eq?-c 1) l) "Multirember removes all occurrences of the atom")
 )
 (define-each-test
   (assert-true (multirember&co 'tuna '() a-friend) "This is a friendlier example")
   (assert-false (multirember&co 'tuna '(tuna) a-friend) "getting more complicated")
   (assert-equal 3 (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend) "the number of things that aren't tuna")
   (assert-equal '(1 2 3 4 2) (multiInsertLR 2 3 4 '(1 3 4)) "multi insert left and right")
   (assert-equal 2 (multiInsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (x y z) z)) "multiInsertLR&co")
   (assert-false (even? 9) "even test")
   (assert-equal '((2 8) 10 (() 6) 2) (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) "evens only recursive")
   (assert-equal 38 (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) (lambda (x y z) z)) "give me the sum of the odds")
   (assert-equal (* 2 8 10 6 2) (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) (lambda (x y z) y)) "give me the product of the evens")
 )
)
(in-test-group
 Chapter9:Again_Again_and_Again
 (define-each-test
   (assert-equal 3 (pick 2 (list 1 3 2)) "pick the second item")
   (assert-equal 4 (pick 1 (list 4 3 2)) "pick first item")
   (assert-equal 3 (pick 4 (list 4 5 2 3 1)) "pick the third item")
 )
 (define-each-test
   (assert-true (looking 'answer '(6 2 4 answer 5 7 3)) "finds the answer")
   (assert-false (looking 'answer '(6 2 other answer 5 7 3)) "can't find the answer")
 )
 (define-each-test
   (assert-equal '(a (b c)) (shift '((a b) c)) "shift")
   (assert-equal '(a (b (c d))) (shift '((a b) (c d))) "shift part 2")
 )
 (define-each-test
   (assert-equal 5 (length* '((1 2) (3 (4 5)))) "length* can count pairs")
   (assert-equal 1 (length* 'atom) "atoms count as 1")
 )
 (define-each-test
   (assert-equal 120 (factorial 5) "Verify that the Y-combinator gives us the proper recursion")
   (assert-equal 144 (fibonacci 11) "Y combinator to implement the fibonacci sequence")
 )
)
(in-test-group
 Chapter10:WhatIsTheValueOfAllThis?
 (define-test (lookup-tests)
   ; return false when name isn't found
   (define entry-function (lambda (name) #f))
   (define entries '((notme findme meeither) (whocares answer doesntmatter)))
   (assert-equal 'answer (lookup-in-entry 'findme entries entry-function) "finds the answer")
   (assert-false (lookup-in-entry 'unfindable entries entry-function) "no can find")
 )
 (define-test (lookup-in-table-tests)
   (define table-function (lambda (name) #f))
   (define table '(((entree dessert) (spaghetti spumoni))
                   ((appetizer entree beverage) (food tastes good))))
   (assert-equal 'spaghetti (lookup-in-table 'entree table table-function) "first things first")
   (assert-equal 'good (lookup-in-table 'beverage table table-function) "will look in second if necessary")
   (assert-false (lookup-in-table 'dne table table-function) "falls back on the provided function")
 )
)
