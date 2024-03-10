(defpackage :cl-lc.test
  (:use :cl :alexandria :cl-lc :fiveam :iterate)
  (:shadowing-import-from :iterate :for))
(in-package :cl-lc.test)

(def-suite cl-lc)
(in-suite cl-lc)

(defun run-tests ()
  (run! 'cl-lc))

(def-test paper-example ()
  (let ((i50000 (iota 50000)))
    (is (equal
         (loop repeat 1
               nconc (loop for x in i50000
                           nconc
                           (loop for y in '(a b c)
                                 if (> 0 x) collect x)))
         (list-of x (for x in i50000) (for y in '(a b c)) (> 0 x))))))

(def-test identity-lc ()
  (let ((xs (iota 10)))
    (is (equal
         (list-of x (for x in xs))
         (iter (for x in xs) (collect x))))))

(def-test destructuring-lc ()
  (is (equal '(1 2 3)
             (list-of
                 key
               (for (key . nil) in '((1 . a) (2 . b) (3 . c)))))))

(def-test parallel-lc ()
  (let ((xs (iota 10))
        (ys (reverse (iota 10))))
    (is (equal
         (list-of (list x y)
           ((for x in xs)
            (for y in ys)))
         (mapcar #'list xs ys)))))

(def-test simple-filter ()
  (is (equal
       (remove-if-not #'evenp (iota 10))
       (list-of x (for x in (iota 10)) (evenp x)))))

(def-test filter-with-if-sugar ()
  (is (equal
       (remove-if-not #'evenp (iota 10))
       (list-of x (for x in (iota 10)) if (evenp x)))))

(def-test filter-with-when-sugar ()
  (is (equal
       (remove-if-not #'evenp (iota 10))
       (list-of x (for x in (iota 10)) when (evenp x)))))

(def-test filter-with-unless-sugar ()
  (is (equal
       (remove-if #'evenp (iota 10))
       (list-of x (for x in (iota 10)) unless (evenp x)))))

(def-test test-do-for ()
  (is (equal
       (let ((pairs '()))
         (do-for ((for x in '(a b c))
                  (for y in '(1 2 3))
                  (<= y 2))
           (push (cons x y) pairs))
         (nreverse pairs))
       '((A . 1) (A . 2) (B . 1) (B . 2) (C . 1) (C . 2)))))

(def-test test-dict-of ()
  (is (set-equal '((a . 1) (b . 2))
                 (hash-table-alist
                  (dict-of (values k v)
                           (for (k . v) in '((a . 1) (b . 2)))))
                 :test #'equal)))
