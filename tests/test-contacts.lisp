(in-package :bookshops-test)

(subtest "Create a contact"
  ;; Our fixtures work.
  (with-empty-db
    (is (object-id (create-contact "contact test"))
        1
        "create a contact")))

(subtest "Lend books, see loans"
  (with-empty-db
    (with-fixtures
      (ok (lend (first *books*) (first *contacts*))
          "Lend")
      ;; error:
      ;; (ok (loans)
      ;;     "Loans")
      )))

(finalize)
