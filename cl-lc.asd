;;;; cl-lc.asd

(asdf:defsystem "cl-lc"
  :serial t
  :description "List comprehensions"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("alexandria" "optima" "iterate")
  :in-order-to ((test-op (test-op "cl-lc/test")))
  :components ((:file "package")
               (:file "cl-lc")))

(asdf:defsystem "cl-lc/test"
  :serial t
  :description "Test list comprehensions"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("cl-lc" "fiveam")
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :cl-lc.test :run-tests)))
