(in-package :bookshops.models)

(defclass basket ()
  ((name
    :initarg :name
    :accessor name
    :col-type (:varchar 128)))
  (:metaclass dao-table-class)
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
  (:metaclass dao-table-class)
  (:documentation "Intermediate table between books and baskets.
Specifies the quantity in the given basket."))

(defmethod print-object ((obj basket) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a"
            (name obj))))

(defun create-basket (name)
  "Create a `basket' object in the DB."
  (create-dao 'basket :name name))

(defun find-baskets (&optional name)
  (if name
      (error 'not-implemented)
      (select-dao 'basket)))
