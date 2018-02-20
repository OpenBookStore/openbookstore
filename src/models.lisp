(defpackage bookshops.models
  (:use :cl
        :mito)
  (:export :main
           ;; book accessors
           :book
           :editor
           :title
           :authors
           :price
           ;; utils
           :erase-metaclass-from))
(in-package :bookshops.models)


(defconstant +varchar-length+ 128)

(defparameter *db-name* (asdf:system-relative-pathname :bookshops "db.db"))

(defun connect ()
  "Connect to the DB."
  (connect-toplevel :sqlite3 :database-name *db-name*))

;; col-types:
;; varchar, text, integer, serial, bigserial, binary,
;; timestamp, (or ... null), relationship.


(defclass book ()
  ((datasource :accessor datasource :initarg :datasource
               :col-type (:varchar +varchar-length+))
   (title :accessor title :initarg :title
          :col-type (:varchar +varchar-length+))
   (price :accessor price :initarg :price
          :col-type :integer)
   (date-publication :accessor date-publication :initarg :date-publication
                     :col-type :timestamp)
   (editor :accessor editor :initarg :editor
           :col-type (:varchar +varchar-length+))
   (authors :accessor authors :initarg :authors
            ;TODO: relationship
            :col-type (:varchar +varchar-length+)))
  (:metaclass dao-table-class))

(defmethod print-object ((book book) stream)
  (print-unreadable-object (book stream :type t)
    (with-accessors ((title title)
                     (price price)
                     (authors authors))
        book
      (format stream "~a, ~a" title authors))))

(defun create-book (&key title author authors editor)
  "Create a Book instance. If given author or authors, create Author
  instance(s) if they don't already exist in DB.
  "
  )

(defclass author ()
  ((name :accessor name :initarg :name
         :col-type (:varchar +varchar-length+)))
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
