;;;; package.lisp

(uiop:define-package #:cl-lc
  (:use #:cl #:alexandria #:optima #:iterate)
  (:use-reexport :iterate)
  (:nicknames #:lc)
  (:export #:list-of
           #:sum-of #:product-of
           #:max-of #:min-of
           #:any-of #:all-of #:none-of
           #:count-of
           #:over
           #:do-for))
