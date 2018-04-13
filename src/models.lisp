(defpackage bookshops.models
  (:use :cl
        :mito
        :cl-ansi-text)
  (:export :main
           :connect
           :ensure-tables-exist
           ;; book accessors
           :book
           :make-book
           :editor
           :find-book
           :title
           :authors
           :price
           :print-book
           :print-book-details
           :count-book
           :quantity
           ;; book methods
           :save-book
           :quantity-of
           ;; utils
           :erase-metaclass-from))
(in-package :bookshops.models)

#|
Usage:

(connect)

(make-book :title "antigone" :datasource "xxx")

(save-book *)

(find-dao 'book)
;; => #<Book antigone>

|#

(defparameter *db-name* (asdf:system-relative-pathname :bookshops "db.db"))

(defparameter *db* nil
  "DB connection object, returned by (connect).")

;;
;; DB connection, migrations.
;;

(defun connect ()
  "Connect to the DB."
  ;; also use mito:*connection*
  (setf *db* (connect-toplevel :sqlite3 :database-name *db-name*)))

(defun ensure-tables-exist ()
  (ensure-table-exists 'book))

(defun migrate-all ()
  "Migrate the Book table after we changed the class definition."
  (mito:migrate-table 'book))

;;
;; DB tables definition.
;;

;; col-types:
;; varchar, text, integer, serial, bigserial, binary,
;; timestamp, (or ... :null), relationship.

(defclass book ()
  ;; "Book class. Use make-book to create an object
  ;; (do not export and use book directly, use make-book).
  ;;
  ;; After modification, run (migrate-all)
  ;;
  ;; - create a date: (local-time:now)
  ;; "
  ((datasource :accessor datasource :initarg :datasource
               ;; how to use a variable for 128 ?
               ;; we get datasource VARCHAR(+varchar-length+) NOT NULL,
               :col-type (or (:varchar 128) :null))
   (title :accessor title :initarg :title
          :col-type (:varchar 128))
   (price :accessor price :initarg :price
          :col-type (or :integer :null))
   (date-publication :accessor date-publication :initarg :date-publication
                     :col-type (or (:varchar 128) :null))
   (editor :accessor editor :initarg :editor
           :col-type (or (:varchar 128) :null))
   (authors :accessor authors :initarg :authors
                                        ;TODO: relationship
            :col-type (or (:varchar 128) :null))
   (quantity :accessor quantity
             :initform 0
             :col-type (or :integer :null)))
  (:metaclass dao-table-class))

(defmethod print-object ((book book) stream)
  (print-unreadable-object (book stream :type t)
    (with-accessors ((title title))
        book
      (format stream "~a" title))))

(defun print-book (book &optional (stream t))
  "Print to stream a user-readable output."
  ;; xxx: print as a nice table.
  (format stream "~2@a- ~30a ~15a ~15a x ~a~&"
          (object-id book)
          (blue (title book))
          (or (authors book) "")
          (or (price book) "")
          (cl-ansi-text:red (prin1-to-string (quantity-of book)))))

(defun print-book-details (pk)
  (let ((bk (find-dao 'book :id pk)))
    (if bk
        (progn
          (format t "~a x ~a~&" (blue (title bk)) (quantity-of bk))
          (format t "~t~a~&" (authors bk))
          (format t "~t~a~&" (price bk)))
        (format t "There is no such book with id ~a~&" pk))))

(defun make-book (&key title authors editor date-publication price datasource)
  "Create a Book instance. If given author or authors, create Author
  instance(s) if they don't already exist in DB.
  "
  (make-instance 'book
                 :datasource datasource
                 :title title
                 :authors authors
                 :editor editor
                 :price price
                 :date-publication date-publication))

(defun save-book (book)
  "Save this book in DB."
  ;; logging
  (handler-case
      (let ((new (insert-dao book)))
        (incf (quantity new))
        (save-dao new)
        new)
    (error (c) (format t "Oops, an unexpected error happened:~&~a~&" c))))

(defun find-book (&optional title)
  "Return a list of book objects."
  (select-dao 'book))

(defun count-book ()
  ""
  (count-dao 'book))

(defun quantity-of (book)
  ;; Use a wrapper around the quantity accessor, for future additions.
  (quantity book))

(defclass author ()
  ((name :accessor name :initarg :name
         :col-type (:varchar 128)))
  (:metaclass dao-table-class))

(defmethod print-object ((author author) stream)
  (print-unreadable-object (author stream :type t)
    (format stream "~a" (slot-value author 'name))))

;;
;; utils
;;
(defun erase-metaclass-from (class)
  "Needed to change the metaclass, e.g. add mito."
  ;; https://stackoverflow.com/questions/38811931/how-to-change-classs-metaclass
  (setf (find-class class) nil))
