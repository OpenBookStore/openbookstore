(defpackage bookshops.management
  (:use :cl
        :mito)
  (:import-from :bookshops.models
                :find-book
                :price
                :ensure-integer)
  (:import-from :bookshops.utils)
  (:export :parse-prices)
  (:local-nicknames (:models :bookshops.models)
                    (:utils :bookshops.utils))
  (:documentation "Commands to work on the database (clean-up,...). Depends on models and needs cl-ppcre.

- parse-prices
  If some prices were saved as a string, turn them into numbers.
  NB: might need an upgrade to save them as integers.

- cleanup-prices

- total-price
  Compute the sum of all prices.

- create-ascii-slots

- cards-prices-to-integers
  For all books, ensure their price is an integer (the price as cents).
  Required on Oct, 22, when we changed the DB model to only accept prices as integers.

"))

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
            (let ((new-price (utils:extract-float (price bk))))
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
   (loop for card in (models::find-book)
      for i from 0
      for title-ascii = (utils:asciify (models::title card))
      for authors-ascii = (utils:asciify (models::authors card))
      for publisher-ascii = (utils:asciify (models::publisher card))
      do (format t "- ~a asciify card ~a, \"~a\"~&"
                 i (object-id card) (str:shorten 40 (models::title card)))
      do (setf (models::title-ascii card) title-ascii)
      do (setf (models::authors-ascii card) authors-ascii)
      do (setf (models::publisher-ascii card) publisher-ascii)
      do (save-dao card))))


(defun cleanup-prices ()
  "When the currency symbol appears in the price (currently only €), remove it.
  This should be useful during development of the datasources."
  (loop for card in (select-dao 'book
                      (sxql:where (:like :price "%€%")))
     ;; XXX: we should run 1 SQL query to update all fields.
     do (setf (price card)
              (str:trim (str:replace-all "€" "" (price card))))
     do (save-dao card)))

(defun cards-prices-to-integers ()
  "If the price of a book is a float, transform it to an integer.

  This can create rounding errors: if our price was badly saved as a float,
  it could get rounding errors: 14.9499998092651 instead of 14.95,
  and we will save 14.94 instead of 14.95.
  That's OK for now (Oct, 2022), the only users are developers."
  (let ((books (select-dao 'models::book
                 (sxql:where (:not-null :price)))))
    (loop for card in books
       for price = (price card)
       for new = (ensure-integer (* 100 price))
       do (format t "~&~a results." (length books))
       do (format t "~&converting price of ~a (~a): ~a -> ~a" (models::title card) (models::isbn card) price new)
       do (setf (price card) new)
       do (save-dao card)
       do (format t " OK~&"))))
