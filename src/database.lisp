(in-package :bookshops.models)
;;
;; DB connection, migrations.
;;

(defparameter *tables* '(book
                         place
                         place-copies
                         contact
                         contact-copies
                         basket
                         basket-copies
                         user
                         role
                         user-role
                         role-copy
))

(defun connect (&optional (db-name *db-name*))
  "Connect to the DB."
  ;; also use mito:*connection*
  (log:debug "connecting to ~a~&" *db-name*) (force-output)
  (setf *db* (mito:connect-toplevel :sqlite3 :database-name db-name)))

(defun ensure-tables-exist ()
  (unless mito::*connection*
    (connect))
  (mapcar #'mito:ensure-table-exists *tables*))

(defun migrate-all ()
  "Migrate the Book table after we changed the class definition."
  (mapcar #'mito:migrate-table *tables*))

(defun bootstrap-base-roles ()
  (define-role :visitor ())
  (define-role :vendor (:visitor))
  (define-role :editor (:vendor))
  (define-role :admin (:editor))
  t)
