CL-LC provides list comprehensions, and other “cataphoric” macros,
using a syntax suggested by Christopher Riesbeck and an
implementation taken from Mario Latendresse, “Simple and Efficient
Compilation of List Comprehensions in Common Lisp.”

# Syntax

A list comprehension consists of an expression, whose results will be
collected, followed by a list of filters and generators.

Generators are expressions with a keyword as their second argument.

     (lc:list-of x (x :in xs))
     ≡ (mapcar #'identity xs)

The binding of a generator can use destructuring:

     (list-of key ((key . value) :in alist))

Generators can be made parallel simply by enclosing them in a list.

    (list-of (list x y)
      ((x :in xs) (y :in ys)))
    ≡ (list-of (list x y)
       ((x y) :in (mapcar #'list xs ys)))

Filters are ordinary expressions that filter the results of each
generator:

     (list-of x (x :in (iota 10)) (evenp x))
     => (2, 4, 6, 8, 10)

Acceptable generators are:

1. Lists, with `:in`.

2. Vectors, with `:across`.

3. The arithmetic subclauses of `loop`, e.g.

     `(i :from 0 :to 10 :by 2)`

4. `:on' to bind the successive tails of a list.

5. Direct bindings with `:=` and, optionally, `:then`.

     `(x := y :then z)`

6. Multiple values, again with `:='.

     `(x y z := (values 1 2 3))`

7. `:over', to bind the items of a hash table.

    `((key value) :over table)`

# Other macros

    count-of
    ≡ (count-if #'identity (list-of ...))

    any-of
    ≡ (some #'identity (list-of ...))

    all-of
    ≡ (every #'identity (list-of ...))

    none-of
    ≡ (notany #'identity (list-of ...))

    sum-of
    ≡ (reduce #'+ (list-of ...))

    max-of
    ≡ (reduce #'max (list-of ...))

    min-of
    ≡ (reduce #min (list-of ...))

# For

`for` is a cross between a list comprehension and `do`. It does no
accumulating or reducing; it just binds variables.

    (let ((pairs '()))
      (for ((x :in '(a b c))
            (y :in '(1 2 3)))
        (push (cons x y) pairs))
      (nreverse pairs))
    => ‘((A . 1) (A . 2) (A . 3) (B . 1) (B . 2) (B . 3) (C . 1) (C . 2) (C . 3))
