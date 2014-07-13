;;;; package.lisp

(defpackage #:cl-lc
  (:use #:cl #:alexandria #:serapeum #:optima #:iterate)
  (:shadow #:for)
  (:shadowing-import-from #:iterate #:in #:collecting #:summing)
  (:nicknames #:lc)
  (:export #:list-of
           #:sum-of #:product-of
           #:max-of #:min-of
           #:any-of #:all-of #:none-of
           #:count-of
           #:for))
