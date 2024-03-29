(in-package :openbookstore.models)
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
                         sell
                         sold-cards
                         shelf
                         payment-method
))

(defun connect (&optional (db-name (db-name)))
  "Connect to the DB."
  ;; also use mito:*connection*
  (log:debug "connecting to ~a~&" (db-name)) (force-output)
  (setf *db* (mito:connect-toplevel :sqlite3 :database-name db-name)))

(defun ensure-tables-exist ()
  (unless mito::*connection*
    (connect))
  (mapcar #'mito:ensure-table-exists *tables*))

(defun migrate-all ()
  "Migrate the Book table after we changed the class definition."
  ;; We'd rather use a "with-connexion" style to drop the connection afterwards.
  ;; Use mito:*auto-migration-mode* to nil to delete migration intermediate tables used with sqlite3.
  (unless mito::*connection*
    (connect))
  (mapcar #'mito:migrate-table *tables*))

(defun bootstrap-base-roles ()
  (define-role :visitor ())
  (define-role :vendor (:visitor))
  (define-role :editor (:vendor))
  (define-role :admin (:editor))
  t)

(defun initialize-database ()
  (ensure-tables-exist)
  (bootstrap-base-roles))
