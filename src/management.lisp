(defpackage bookshops.management
  (:use :cl
        :bookshops.utils
        :mito)
  (:import-from :bookshops.models
                :find-book
                :price)
  (:export :parse-prices)
  (:documentation "Commands to work on the database (clean-up,...). Needs cl-ppcre."))

(in-package :bookshops.management)


(defun parse-prices (&key dry-run)
  "For all books, if its price is a string, extract a number from it
   and set the price.

   If :dry-run is true, don't make changes and only print output."
  (unless (find :CL-PPCRE *features*)
    (error "We need cl-ppcre"))
  (let ((books (find-book)))
    (dolist (bk books)
      (format t "price of ~a: ~a~&" bk (price bk))
      (if (numberp (price bk))
          (format t "price is a number: ~a~&" (price bk))
          (progn
            (let ((new-price (ppcre:scan-to-strings "[0-9]+.[0-9]+" (price bk))))
              (format t "scan: ~a~&" new-price)
              (unless dry-run
                (setf (price bk) new-price)
                (save-dao bk))))))))

(defun total-price ()
  "Sum the prices of all books in the DB."
  (format t "summing prices… supposing the data scrapers return a number.~&")
  ;; SQL query ?
  (reduce #'+ (find-book) :key #'price))
