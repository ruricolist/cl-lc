;;;; package.lisp

(defpackage #:cl-lc
  (:use #:cl #:alexandria #:optima #:iterate)
  (:shadow #:for)
  (:nicknames #:lc)
  (:export #:list-of
           #:sum-of #:product-of
           #:max-of #:min-of
           #:any-of #:all-of #:none-of
           #:count-of
           #:for))
