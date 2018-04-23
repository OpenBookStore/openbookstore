(defpackage bookshops-test
  (:shadow :search)
  (:use :cl
        :bookshops
        :prove)
  (:import-from :bookshops.models
                :book
                :make-book
                :save-book
                :title
                :isbn
                :quantity
                :find-by)
  (:import-from :bookshops-test.utils
                :with-empty-db))
(in-package :bookshops-test)

;; NOTE: To run this test file, execute `(asdf:test-system :bookshops)' in your Lisp.

(plan nil)

(subtest "Simple creation and access"
  (let ((book (make-instance 'book :title "Antigone")))
    (ok book "creation")
    (is (title book) "Antigone" "title access"))
  )

(subtest "Creation and DB save"
  (with-empty-db
    (let ((bk (make-book :title "in-test")))
      (save-book bk)

      (is (quantity bk)
          1
          "The quantity is 1 after adding to the DB."))))

(defvar fixtures nil)

(defun fixtures-init ()
  (setf fixtures (list (make-book :title "test"
                                  :isbn "9782710381419"))))

(subtest "Add a book that already exists"
  (fixtures-init)
  (with-empty-db
    (let* ((bk (first fixtures))
           (same-bk (make-book :title "different title"
                               :isbn (isbn bk))))
      (save-book bk)
      (save-book same-bk)
      (is (quantity (find-by :isbn (isbn bk)))
          2)
      )))

(finalize)
