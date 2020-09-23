
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
  "Run `body` with a new temporary DB."
  `(let* ((*random-state* (make-random-state t))
          (prefix (concatenate 'string
                               (random-string 8)
                               "/"))
          (connection *db*))
     (uiop:with-temporary-file (:pathname name :prefix prefix)
       (let* ((*db-name* name))
         (connect)
         ;; catch anything to always re-connect to our real db.
         ;XXX: a verbose or debug parameter would be nice.
         ;XXX: see *mito-migration-logger-stream* to use with
         ; with-sql-logging
         (with-output-to-string (s)
           (let ((*standard-output* s))
             (ensure-tables-exist)
             (migrate-all)))
         ,@body

         (setf mito.connection:*connection* connection)))))
