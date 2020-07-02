#|
  This file is a part of bookshops project.
|#

(asdf:defsystem "bookshops-test"
  :author "vindarel"
  :license "GPL3"
  :depends-on ("bookshops"
               "prove"
               "mito"
               "sxql"
               "parachute")
  :components ((:module "tests"
                :components
                ((:file "test-utils")
                 (:file "test-bookshops")
                 (:file "test-contacts")
                 (:file "test-authentication"))))
  :description "Test system for bookshops"

  ;; :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c))
  )
