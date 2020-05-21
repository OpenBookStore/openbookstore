(defpackage bookshops.management
  (:use :cl
        :mito)
  (:import-from :bookshops.models
                :find-book
                :price)
  (:export :parse-prices)
  (:documentation "Commands to work on the database (clean-up,...). Depends on models and needs cl-ppcre."))

(in-package :bookshops.management)


(defun parse-prices (&key dry-run)
  "For all books, if its price is a string, extract a number from it
   and set the price.

   If :dry-run is true, don't make changes and only print output."
  (let ((books (find-book)))
    (dolist (bk books)
      (format t "price of ~a: ~a~&" bk (price bk))
      (if (numberp (price bk))
          (format t "price is a number: ~a~&" (price bk))
          (progn
            (let ((new-price (extract-float (price bk))))
              (format t "scan: ~a~&" new-price)
              (unless dry-run
                (setf (price bk) new-price)
                (save-dao bk))))))))

(defun total-price ()
  "Sum the prices of all books in the DB."
  (format t "summing prices… supposing the data scrapers return a number.~&")
  ;; SQL query ?
  (reduce #'+ (find-book) :key #'price))

(defun create-ascii-slots ()
  "For all cards, create and save the ascii representations of: title, authors, publisher.
  (needs to migrate the table).
  To use manually when necessary."
  (time
   (loop for card in (bookshops.models::find-book)
      for i from 0
      for title-ascii = (asciify (title card))
      for authors-ascii = (asciify (authors card))
      for publisher-ascii = (asciify (publisher card))
      do (format t "- ~a asciify card ~a, \"~a\"~&"
                 i (object-id card) (str:shorten 40 (title card)))
      do (setf (bookshops.models::title-ascii card) title-ascii)
      do (setf (bookshops.models::authors-ascii card) authors-ascii)
      do (setf (bookshops.models::publisher-ascii card) publisher-ascii)
      do (save-dao card))))


(defun cleanup-prices ()
  "When the currency symbol appears in the price, remove it.
  This should be useful during development of the datasources."
  (loop for card in (select-dao 'book
                      (where (:like :price "%€%")))
     ;; XXX: we should run 1 SQL query to update all fields.
     do (setf (price card)
              (str:trim (str:replace-all "€" "" (price card))))
     do (save-dao card)))
