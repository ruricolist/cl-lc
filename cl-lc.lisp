;;;; cl-lc.lisp

(in-package #:cl-lc)

;;; "cl-lc" goes here. Hacks and glory await!

(eval-and-compile
  (def iterate (find-package :iterate))

  (def for (find-symbol "FOR" iterate)))

(defun parse-generator (exp)
  (match exp
    ((list (list key value) :in-hashtable hash)
     `((,for (,key ,value) in-hashtable ,hash)))
    ((list var (and _ (eql :over)) seq)
     `((,for ,var over ,seq)))
    ((list* var (and gen (type keyword)) rest)
     (flet ((keyword->iterate (keyword)
              (or (find-symbol (string keyword) iterate)
                  (error "No such driver: ~a" gen))))
       `((,for ,var
           ,(keyword->iterate gen)
           ,@(loop for expr in rest
                   if (keywordp expr)
                     collect (keyword->iterate expr)
                   else collect expr)))))
    ((list* (and gen (list _ (type keyword) _))
            gens)
     (mappend #'parse-generator (cons gen gens)))
    ((and list (type list))
     (let ((tail (member := list)))
       (if (not tail)
           (fail)
           (let ((vars (ldiff list tail))
                 (form (cdr tail)))
             `((,for (values ,@vars) = ,@form))))))
    (otherwise exp)))

(defun generator? (exp)
  (not (eq exp (parse-generator exp))))

(defun ensure-head (qs)
  (if (generator? (car qs))
      (values (caar qs) qs)
      (values (car qs) (cdr qs))))

(defmacro lc (qs &optional (accumulator 'collect))
  (with-gensyms (outer)
    (multiple-value-bind (head qualifiers)
        (ensure-head qs)
      `(iterate ,outer (repeat 1)
         ,(lcrec head qualifiers accumulator outer)))))

(defun lcrec (head qualifiers accumulator outer)
  (if (null qualifiers)
      (if (listp accumulator)
          `(in ,outer ,(substitute head '_ accumulator))
          `(in ,outer (,accumulator ,head)))
      (destructuring-bind (q &rest qs)
          qualifiers
        (if (generator? q)
            `(iterate ,@(parse-generator q)
               ,(lcrec head qs accumulator outer))
            `(if ,q ,(lcrec head qs accumulator outer))))))

(defmacro defcomp (name &rest (accumulator &optional documentation))
  `(defmacro ,name (exp &body exps)
     ,@(unsplice documentation)
     `(lc ,(cons exp exps) ,',accumulator)))

(defmacro seq-dispatch (seq &body (list vector sequence))
  (declare (ignorable sequence))
  #+(or sbcl acbl)
  (once-only (seq)
    `(cond ((listp ,seq) ,list)
           ((arrayp ,seq) ,vector)
           (t ,sequence)))
  #-(or sbcl abcl)
  `(if (listp ,seq)
       ,list
       ,vector))

(defmacro-driver (#.for var over seq)
  (with-gensyms (gseq idx)
    (let ((for (if generate 'generate for)))
      `(progn
         (with ,gseq = ,seq)
         (with ,idx = 0)
         (declare (type alexandria:array-length ,idx))
         (,for ,var next
           (seq-dispatch ,gseq
             (if ,gseq
                 (pop ,gseq)
                 (terminate))
             (progn
               (unless (< ,idx (length ,gseq))
                 (terminate))
               (aref ,gseq
                     (prog1 ,idx
                       (incf ,idx))))
             (progn
               (unless (< ,idx (length ,gseq))
                 (terminate))
               (elt ,gseq
                    (prog1 ,idx
                      (incf ,idx))))))))))

(defcomp list-of collect
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

3. Sequences of any kind, with `:over'.

4. `loop's arithmetic subclauses, e.g.

     (i :from 0 :to 10 :by 2)

5. `:on' to bind the successive tails of a list.

6. Direct bindings with `:=' and, optionally, `:then'.

     (x := y :then z)

7. Multiple values, again with `:='.

     (x y z := (values 1 2 3))

8. `:in-hashtable', to bind the items of a hash table.
     ((key value) :in-hashtable table)")

(defcomp count-of count
  "Like a list comprehension but, instead of collecting the results,
count them if they are non-nil.")

(defcomp any-of thereis
  "Like a list comprehension but, as soon as any result is non-nil,
stop evaluating and return it from the whole form.")

(defcomp all-of always
  "Like a list comprehension but, as soon as any result is nil, stop
evaluating and return `nil' from the whole form.")

(defcomp none-of never
  "Like a list comprehension but, as soon as any result is non-nil,
stop evaluating and return `nil' from the whole form.")

(defcomp sum-of sum
  "Like a list comprehension but, instead of collecting the results,
sum them.")

(defcomp max-of maximize
  "Like a list comprehension but, instead of collecting the results,
track and return the maximum.")

(defcomp min-of minimize
  "Like a list comprehension but, instead of collecting the results,
track and return the minimum.")

(defcomp product-of multiply
  "Like a list comprehension but, instead of collecting the results
  into a list, multiply them together.")

(defmacro reduction (fn expr &body exprs)
  "Like a list comprehension, but reduce the results using FN."
  `(lc ,(cons (gensym) exprs) (reducing ,expr by ,fn)))

(defmacro for ((&rest qs) &body head)
  "Imperative macro for list comprehension–like iteration.

QS are like the filters and generators of a list comprehension; BODY
is like its expression. No reducing or accumulating is done."
  `(lc ,(cons `(block nil (tagbody (return (progn ,@head)))) qs) progn))
