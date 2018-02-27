(defpackage bookshops.models
  (:use :cl
        :mito)
  (:export :main
           ;; book accessors
           :book
           :make-book
           :editor
           :title
           :authors
           :price
           ;; utils
           :erase-metaclass-from))
(in-package :bookshops.models)


(defparameter *db-name* (asdf:system-relative-pathname :bookshops "db.db"))

(defun connect ()
  "Connect to the DB."
  (connect-toplevel :sqlite3 :database-name *db-name*))

;; col-types:
;; varchar, text, integer, serial, bigserial, binary,
;; timestamp, (or ... :null), relationship.


(defclass book ()
  ((datasource :accessor datasource :initarg :datasource
               ;; how to use a variable for 128 ?
               ;; we get datasource VARCHAR(+varchar-length+) NOT NULL,
               :col-type (:varchar 128))
   (title :accessor title :initarg :title
          :col-type (:varchar 128))
   (price :accessor price :initarg :price
          :col-type :integer)
   (date-publication :accessor date-publication :initarg :date-publication
                     :col-type :timestamp)
   (editor :accessor editor :initarg :editor
           :col-type (:varchar 128))
   (authors :accessor authors :initarg :authors
            ;TODO: relationship
            :col-type (or (:varchar 128) :null)))
  (:metaclass dao-table-class))

(defmethod print-object ((book book) stream)
  (print-unreadable-object (book stream :type t)
    (with-accessors ((title title))
        book
      (format stream "~a" title))))

(defun make-book (&key title authors editor date-publication price)
  "Create a Book instance. If given author or authors, create Author
  instance(s) if they don't already exist in DB.
  "
  (make-instance 'book
                 :title title
                 :authors authors
                 :editor editor
                 :price price
                 :date-publication date-publication))

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
