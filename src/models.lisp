(defpackage bookshops.models
  (:use :cl)
  (:export :main
           ;; book accessors
           :book
           :editor
           :title
           :authors
           :price))
(in-package :bookshops.models)

(defclass book ()
  ((datasource :accessor datasource :initarg :datasource)
   (title :accessor title :initarg :title)
   (price :accessor price :initarg :price)
   (date-publication :accessor date-publication :initarg :date-publication)
   (editor :accessor editor :initarg :editor)
   (authors :accessor authors :initarg :authors)))

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
  ((name :accessor name :initarg :name)))

(defmethod print-object ((author author) stream)
  (print-unreadable-object (author stream :type t)
    (format stream "~a" (slot-value author 'name))))
