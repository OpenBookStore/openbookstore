
(defpackage bookshops-test.utils
  (:use :cl)
  (:import-from :bookshops.models
                :*db*
                :*db-name*
                :connect
                :ensure-tables-exist
                :migrate-all)
  (:export :with-empty-db))

(in-package bookshops-test.utils)


(defun random-string (length)
  ;; thanks hacrm.
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length
                  collect (aref chars (random (length chars))))
            'string)))

(defmacro with-empty-db (&body body)
  "Run `body` with a new temporary DB.
  "
  `(let* ((*random-state* (make-random-state t))
          (prefix (concatenate 'string
                               (random-string 8)
                               "/")))
     (uiop:with-temporary-file (:pathname name :prefix prefix)
       (let* ((*db-name* name)
              (*db* (connect)))
         (ensure-tables-exist)
         (migrate-all)
         ,@body)))
  ;; Despite the let, we must re-connect to our DB.
  ;; If body was the last element evaluated, we'd see the migration traces.
  (connect))
