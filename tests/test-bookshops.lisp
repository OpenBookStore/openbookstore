(defpackage bookshops-test
  (:shadow :search)
  (:use :cl
        :bookshops
        :openbookstore.models
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

(defparameter *books* nil)
(defparameter *places* nil)
(defparameter *contacts* nil)

(defmacro with-fixtures (&body body)
  "Create books and places."
  `(progn
     (setf *books* (list (create-book :title "title1"
                                      :title-ascii "title1"
                                      :authors "author1"
                                      :authors-ascii "author1"
                                      :isbn "9782710381419")
                         (create-book :title "title2"
                                      :title-ascii "title2"
                                      :authors "author2"
                                      :authors-ascii "author2"
                                      :isbn "9784567890123")))
     ;; contacts
     (setf *contacts* (list (create-contact "first contact")))
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
        'openbookstore.models::place
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

(subtest "search books with permutations of keywords"
  (with-empty-db
    (with-fixtures
      (let ((res (openbookstore.models:find-book :query "title1")))
        (is (length res)
            1
            "search one keyword")
        (is (title (first res))
            "title1"
            "titles match"))

      (let ((res (openbookstore.models:find-book :query "title1 author1"))
            (res2 (openbookstore.models:find-book :query "author1 title1"))
            (res3 (openbookstore.models:find-book :query "title1 author2"))
            (res4 (openbookstore.models:find-book :query "author2 title1"))
            (res5 (openbookstore.models:find-book :query "titl"))
            (res6 (openbookstore.models:find-book :query "titl author2")))

        (is (length res)
            1
            "search with title + author")
        (is (length res2)
            1
            "search with author + title")
        (is (length res3)
            0
            "title1 + author2 gives no results")
        (is (length res4)
            0
            "author2 + title1 gives no results")
        (is (length res5)
            2
            "search common title")
        (is (length res6)
            1
            "narrow keyword search by and-ing each keyword")))))

;; (finalize) ;; done in the last test file (test-contacts).
