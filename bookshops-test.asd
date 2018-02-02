#|
  This file is a part of bookshops project.
|#

(defsystem "bookshops-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("bookshops"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "bookshops"))))
  :description "Test system for bookshops"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
