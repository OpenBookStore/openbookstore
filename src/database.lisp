(in-package :bookshops.models)
;;
;; DB connection, migrations.
;;

(defun connect ()
  "Connect to the DB."
  ;; also use mito:*connection*
  (log:debug "connecting to ~a~&" *db-name*) (force-output)
  (setf *db* (connect-toplevel :sqlite3 :database-name *db-name*)))

(defun ensure-tables-exist ()
  (mapcar #'ensure-table-exists '(book
                                  place
                                  place-copies
                                  contact
                                  contact-copies)))

(defun migrate-all ()
  "Migrate the Book table after we changed the class definition."
  (mapcar #'mito:migrate-table '(book
                                 place
                                 place-copies
                                 contact
                                 contact-copies)))
