(defpackage bookshops-test
  (:shadow :search)
  (:use :cl
        :bookshops
        :bookshops.models
        :mito
        :sxql
        :prove
        ;; :parachute
        )
  (:import-from :bookshops-test.utils
                :with-empty-db))
(in-package :bookshops-test)

(plan nil)

(subtest "Simple creation and access"
  (let ((book (make-instance 'book :title "Antigone")))
    (ok book "creation")
    (is (title book) "Antigone" "title access"))
  )

(defmacro with-fixtures (&body body)
  "Create books and places."
  `(progn
     (defvar *books* nil)
     (defvar *places* nil)
     (setf *books* (list (create-book :title "test"
                                      :isbn "9782710381419")
                         (create-book :title "book 2"
                                      :isbn "978xxxxxxxxxx")))
     ;; places
     (setf *places* (list (create-place "place 1")
                          (create-place "place 2")))
     (log:info "--- fixture adding ~a of id ~a~&" (first *books*) (object-id (first *books*)))
     (add-to (first *places*) (first *books*))
     (add-to (second *places*) (second *books*) :quantity 2)

     ,@body))

(subtest "add to places"
  ;; Our fixtures work.
  (with-empty-db
    (with-fixtures
        (is (quantity (first *books*))
            1
            "add-to")
      (is (quantity (second *books*))
          2
          "add many")
      (is (quantity (second *places*))
          2
          "quantity of books in a place"))))

(subtest "Creation and DB save"
  (with-empty-db
    (with-fixtures
        (let ((bk (make-book :title "in-test")))
          (log:info "~&-- qty book not saved: ~a~&" (quantity bk))
          (save-book bk)
          (log:info "~&-- qty: ~a~&" (quantity bk))
          (is (quantity bk)
              0
              "book creation ok, quantity 0.")))))

(subtest "Add a book that already exists"
  (with-empty-db
    (with-fixtures
        (let* ((bk (first *books*))
               (same-bk (make-book :title "different title"
                                   :isbn (isbn bk))))
          (is (object-id (save-book same-bk))
              (object-id bk)
              "saving a book that already exists doesn't create a new one.")
          ))))

(subtest "Create a default place"
  (with-empty-db
    (is (type-of (default-place))
        'bookshops.models::place
        "we create a default place if there is none.")))


(subtest "Deleting cards"
  (with-empty-db
    (with-fixtures
        (is (count-dao 'place-copies)
            2)
        (delete-obj (first *books*))
        (is (quantity (first *books*))
            0)
        (is (count-dao 'place-copies)
            1
            "deleting a book"))))

(subtest "Delete a place"
  (with-empty-db
    (with-fixtures
        (delete-obj (first *places*))
        (is 1 (count-dao 'place)
            "deleting a place")
        ;; delete a list of objects
        (delete-objects (append *books* *places*))
        (is 0 (count-dao 'place))
        (is 0 (count-dao 'book)
            "deleting a list of objects"))))


;; With Parachute: interactive reports on errors.
#|
(define-test delete
  (with-empty-db
    (with-book-fixtures
      (with-place-fixtures
        ;; delete a book.
        (is = 1 (quantity (first *books*)))
        (is = 2 (count-dao 'place-copies))
        (delete-obj (first *books*))
        (is = 0 (quantity (first *books*)))
        (is = 1 (count-dao 'place-copies))
        ;; delete a place.
        (is = 2 (count-dao 'place))
        (delete-obj (first *places*))
        (is = 1 (count-dao 'place))
        ;; delete a list of objects.
        (delete-objects (append *books* *places*))
        (is = 0 (count-dao 'place))
        (is = 0 (count-dao 'book))))))

(test 'delete :report 'interactive)

(test 'delete)
|#

(finalize)
