#|
  This file is a part of bookshops project.
|#

(asdf:defsystem "bookshops-test"
  :defsystem-depends-on ("prove-asdf")
  :author "vindarel"
  :license "GPL3"
  :depends-on ("bookshops"
               "prove")
  :components ((:module "tests"
                :components
                ((:file "test-utils")
                 (:test-file "bookshops"))))
  :description "Test system for bookshops"

  ;; :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c))
  )
