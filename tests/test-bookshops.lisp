(defpackage bookshops-test
  (:shadow :search)
  (:use :cl
        :bookshops
        :prove)
  (:import-from :bookshops.models
                :book
                :title))
(in-package :bookshops-test)

;; NOTE: To run this test file, execute `(asdf:test-system :bookshops)' in your Lisp.

(plan nil)

(subtest "Simple creation and access"
  (let ((book (make-instance 'book :title "Antigone")))
    (ok book "creation")
    (is (title book) "Antigone" "title access"))
  )

(finalize)
