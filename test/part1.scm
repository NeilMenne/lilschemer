(load "src/part1.scm")
(in-test-group
  Chapter1
  (define-test (lists-are-not-atoms)
    (define l (list 1 2 3))
    (assert-false (atom? l) "Lists are not atoms")
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
)

(in-test-group
  Chapter3:Inserts
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
