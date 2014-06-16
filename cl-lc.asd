;;;; cl-lc.asd

(asdf:defsystem #:cl-lc
  :serial t
  :description "List comprehensions"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria #:serapeum #:optima)
  :components ((:file "package")
               (:file "cl-lc")))
