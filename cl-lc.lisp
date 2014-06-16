;;;; cl-lc.lisp

(in-package #:cl-lc)

;;; "cl-lc" goes here. Hacks and glory await!

(defun parse-generator (exp)
  (match exp
    ((list (list key value) :over hash)
     `(:for ,key :being :each :hash-key :of ,hash :using (:hash-value ,value)))
    ((list* var (and gen (type keyword)) rest)
     `(:for ,var ,gen ,@rest))
    ((list* (and gen (list _ (type keyword) _))
            gens)
     (mappend #'parse-generator (cons gen gens)))
    ((and list (type list))
     (let ((tail (member := list)))
       (if (not tail)
           (fail)
           (let ((vars (ldiff list tail))
                 (form (cdr tail)))
             `(:with ,@(intersperse :and vars)
               :for nil = (setf (values ,@vars)
                                (progn ,@form)))))))
    (otherwise exp)))

(defun generator? (exp)
  (not (eq exp (parse-generator exp))))

(defun ensure-head (qs)
  (if (generator? (car qs))
      (values (caar qs) qs)
      (values (car qs) (cdr qs))))

(defmacro lc (qs &optional (reducer :nconc) (accumulator :collect) (test :if))
  (multiple-value-bind (head qualifiers)
      (ensure-head qs)
    `(loop :repeat 1 ,@(lcrec head qualifiers reducer accumulator test))))

(defun lcrec (head qualifiers reducer accumulator test)
  (if (null qualifiers)
      `(,accumulator ,head)
      (destructuring-bind (q &rest qs)
          qualifiers
        (if (generator? q)
            `(,reducer
              (loop ,@(parse-generator q)
                    ,@(lcrec head qs reducer accumulator test)))
            `(,test ,q ,@(lcrec head qs reducer accumulator test))))))

(defmacro defcomp (name &rest args)
  (let ((documentation (and (stringp (car args)) (pop args))))
    (destructuring-bind (&key reducer accumulator (test :if))
        args
      `(defmacro ,name (exp &body exps)
         ,@(unsplice documentation)
         `(lc ,(cons exp exps) ,',reducer ,',accumulator ,',test)))))

(defcomp list-of
  "A list comprehension.

A list comprehension consists of an expression, whose results will be
collected, followed by a list of filters and generators.

Generators are expressions with a keyword as their second argument.
     (list-of x (x :in xs))
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

1. Lists, with `:in'.

2. Vectors, with `:across'.

3. `loop's arithmetic subclauses, e.g.

     (i :from 0 :to 10 :by 2)

4. `:on' to bind the successive tails of a list.

5. Direct bindings with `:=' and, optionally, `:then'.

     (x := y :then z)

6. Multiple values, again with `:='.

     (x y z := (values 1 2 3))

7. `:over', to bind the items of a hash table.
     ((key value) :over table)"
  :reducer :nconc
  :accumulator :collect)

(defcomp count-of
  "Like a list comprehension but, instead of collecting the results,
count them if they are non-nil."
  :reducer :sum
  :accumulator :count)

(defcomp any-of
  "Like a list comprehension but, as soon as any result is non-nil,
stop evaluating and return it from the whole form."
  :reducer :return
  :accumulator :return
  :test :if)

(defcomp all-of
  "Like a list comprehension but, as soon as any result is nil, stop
evaluating and return `nil' from the whole form."
  :reducer :always
  :accumulator :do
  :test :always)

(defcomp none-of
  "Like a list comprehension but, as soon as any result is non-nil,
stop evaluating and return `nil' from the whole form."
  :reducer :always
  :accumulator :do
  :test :never)

(defcomp sum-of
  "Like a list comprehension but, instead of collecting the results,
sum them."
  :reducer :sum
  :accumulator :sum)

(defcomp max-of
  "Like a list comprehension but, instead of collecting the results,
track and return the maximum."
  :reducer :maximize
  :accumulator :maximize)

(defcomp min-of
  "Like a list comprehension but, instead of collecting the results,
track and return the minimum."
  :reducer :minimize
  :accumulator :minimize)

(defmacro for ((&rest qs) &body head)
  "Imperative macro for list comprehension–like iteration.

QS are like the filters and generators of a list comprehension; BODY
is like its expression. No reducing or accumulating is done."
  `(lc ,(cons `(block nil (tagbody (return (progn ,@head)))) qs) :do :do :if))
