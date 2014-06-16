;;;; package.lisp

(defpackage #:cl-lc
  (:use #:cl #:alexandria #:serapeum #:optima)
  (:nicknames #:lc)
  (:export #:list-of
           #:sum-of #:max-of #:min-of
           #:any-of #:all-of #:none-of
           #:count-of
           #:for))
