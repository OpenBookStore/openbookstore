(in-package :openbookstore.models)

(export '(find-baskets))

(defclass basket ()
  ((name
    :initarg :name
    :accessor name
    :col-type (:varchar 128)))
  (:metaclass mito:dao-table-class)
  (:documentation "A list of cards, independent from the stock (DB).
Adding and deleting cards of baskets doesn't modify the stock."))

(defclass basket-copies ()
  ((book
    :accessor basket-copies-book
    :initarg :book
    :col-type book)
   (basket
    :accessor basket-copies-basket
    :initarg :basket
    :col-type basket)
   (quantity
    :accessor basket-copies-quantity
    :initform 0
    :col-type (or (:integer) :null)))
  (:metaclass mito:dao-table-class)
  (:documentation "Intermediate table between books and baskets.
Specifies the quantity in the given basket."))

(defmethod print-object ((obj basket) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name obj))))

(defun basket-books (basket)
  (mapcar #'basket-copies-book (mito:select-dao 'basket-copies
                                 (sxql:where (:= :basket basket)))))
(defun create-basket (name)
  "Create a `basket' object in the DB."
  (mito:create-dao 'basket :name name))

(defun find-baskets (&optional name)
  (if name
      (error 'not-implemented)
      (mito:select-dao 'basket)))

(defun print-basket (basket &key (stream t))
  "Print the name of the basket and its number of books.
With details, print the list of books."
  (format stream "~2a - ~40a~t x~3a/ ~3a~&"
          (mito:object-id basket)
          (name basket)
          (length (basket-books basket)) ;XXX: not length, count.
          (reduce #'+ (mapcar #'quantity (basket-books basket))))
  (when *print-details*
    (let ((books (basket-books basket)))
      (if books
          (mapcar #'print-book books)
          (format stream "This basket is empty.~&")))))
