CL-LC provides list comprehensions, and other “cataphoric” macros,
using a syntax suggested by Christopher Riesbeck and an
implementation taken from Mario Latendresse, “Simple and Efficient
Compilation of List Comprehensions in Common Lisp.”

# Syntax

A list comprehension consists of an expression, whose results will be
collected, followed by a list of filters and generators.

Generators are expressions with a keyword as their second argument.

     (lc:list-of x (for x in xs))
     ≡ (mapcar #'identity xs)

The binding of a generator can use destructuring:

     (list-of key (for (key . value) in alist))

Generators can be made parallel simply by enclosing them in a list.

    (list-of (list x y)
      ((for x in xs)
       (for y in ys)))
    ≡ (list-of (list x y)
       (for (x y) in (mapcar #'list xs ys)))

Filters are ordinary expressions that filter the results of each
generator:

     (list-of x (for x in (iota 10)) (evenp x))
     => (2, 4, 6, 8, 10)

You may use `if`, `when`, and `unless` as syntactic sugar:

     (list-of x (for x in (iota 10)) if (evenp x))
     => (2, 4, 6, 8, 10)

     (list-of x (for x in (iota 10)) unless (evenp x))
     => (1, 3, 5, 7, 9)

Generators can be any `for` clause understood by Iterate, including
user-defined ones. We provide an additional driver, `(for .. over
...)`, which allow iterating over any sequence.


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

    product-of
    ≡ (reduce #'* (list-of ...))

    max-of
    ≡ (reduce #'max (list-of ...))

    min-of
    ≡ (reduce #min (list-of ...))

# For

`for` is a cross between a list comprehension and `do`. It does no
accumulating or reducing; it just binds variables.

    (let ((pairs '()))
      (do-for ((for x in '(a b c))
               (for y in '(1 2 3))
               (<= y 2))
        (push (cons x y) pairs))
      (nreverse pairs))
    => ‘((A . 1) (A . 2) (B . 1) (B . 2) (C . 1) (C . 2))
