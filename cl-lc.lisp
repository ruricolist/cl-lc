;;;; cl-lc.lisp

(in-package #:cl-lc)

;;; "cl-lc" goes here. Hacks and glory await!

(defun parse-generator (exp)
  (match exp
    ((list* (eql 'for) _)
     (list exp))
    ((list* (list* (eql 'for) _) _)
     exp)
    (otherwise nil)))

(defun generator? (exp)
  (parse-generator exp))

(defun ensure-head (qs)
  (if (generator? (car qs))
      (values (caar qs) qs)
      (values (car qs) (cdr qs))))

(defmacro lc (qs &optional (accumulator 'collect))
  (let ((qs (handle-inline-conditions qs)))
    (with-gensyms (outer)
      (multiple-value-bind (head qualifiers)
          (ensure-head qs)
        `(iterate ,outer (repeat 1)
           ,(lcrec head qualifiers accumulator outer))))))

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

(defun handle-inline-conditions (qs &optional acc)
  (match qs
    ((list* 'for gen qs)
     (handle-inline-conditions qs (cons gen acc)))
    ((list* (or 'when 'if) test qs)
     (handle-inline-conditions qs (cons test acc)))
    ((list* 'unless test qs)
     (handle-inline-conditions qs (cons `(not ,test) acc)))
    ((list* q qs)
     (handle-inline-conditions qs (cons q acc)))
    ((list)
     (nreverse acc))))

(defmacro defcomp (name &rest (accumulator &optional documentation))
  `(defmacro ,name (exp &body exps)
     ,@(and documentation (list documentation))
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

(defmacro-driver (for var over seq)
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

(defmacro do-for ((&rest qs) &body head)
  "Imperative macro for list comprehension–like iteration.

QS are like the filters and generators of a list comprehension; BODY
is like its expression. No reducing or accumulating is done."
  `(block nil
     (lc ,(cons `(block nil (tagbody (return (progn ,@head)))) qs) progn)))

(defmacro dict-of (exp &rest exps)
  "Like a list comprehension, but collect the results into a new 'equal
hash table instead. Each key and value should be returned as separate
values."
  (with-unique-names (dict k v)
    `(let ((,dict (make-hash-table :test 'equal)))
       (do-for ,exps
         ;; Use multiple-value-call instead of multiple-value-bind to
         ;; ensure two values are returned.
         (multiple-value-call
             (lambda (,k ,v)
               (setf (gethash ,k ,dict) ,v))
           ,exp))
       ,dict)))
