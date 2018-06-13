(defpackage bookshops-test
  (:shadow :search)
  (:use :cl
        :bookshops
        :mito
        :prove)
  (:import-from :bookshops.models
                :book
                :make-book
                :save-book
                :create-book
                :title
                :isbn
                :quantity
                :find-by
                :make-place
                :save-place
                :create-place
                :default-place
                :add-to)
  (:import-from :bookshops-test.utils
                :with-empty-db))
(in-package :bookshops-test)

(plan nil)

(defvar *books* nil)
(defvar *places* nil)

(subtest "Simple creation and access"
  (let ((book (make-instance 'book :title "Antigone")))
    (ok book "creation")
    (is (title book) "Antigone" "title access"))
  )

(defmacro with-book-fixtures (&body body)
  "Create some books in DB."
  `(progn
     (setf *books* (list (create-book :title "test"
                                      :isbn "9782710381419")))
     ,@body))

(defmacro with-place-fixtures (&body body)
  "Create some places in DB."
  `(progn
     (setf *places* (list (create-place "place 1")
                          (create-place "place 2")))
     ,@body))

(subtest "Creation and DB save"
  (with-empty-db
    (with-place-fixtures
      (let ((bk (make-book :title "in-test")))
        (save-book bk)
        (is (quantity bk)
            0
            "book creation ok, quantity 0.")))))

(subtest "Add a book that already exists"
  (with-empty-db
    (with-book-fixtures
      (with-place-fixtures
        (let* ((bk (first *books*))
               (same-bk (make-book :title "different title"
                                   :isbn (isbn bk))))
          (is (object-id (save-book same-bk))
              (object-id bk)
              "saving a book that already exists doesn't create a new one.")
          )))))

(subtest "Create a default place"
  (with-empty-db
    (is (type-of (default-place))
        'bookshops.models::place
        "we create a default place if there is none.")))

(subtest "quantity"
  (with-empty-db
    (with-book-fixtures
      (with-place-fixtures

        (add-to (first *places*) (first *books*) :quantity 2)
        (is 2
            (quantity (first *books*))
            "quantity of a book")
        (is 2
            (quantity (first *places*))
            "quantity of a place")
        (add-to (second *places*) (first *books*))
        (is 3
            (quantity (first *books*))
            "quantity of a book accross two places")
        (is 1
            (quantity (second *places*))
            "quantity in second place")))))

(subtest "Places"
  (with-empty-db
    (with-book-fixtures
      (with-place-fixtures
        (is (add-to (first *places*) (first *books*))
            1
            "add-to")))))

(finalize)
