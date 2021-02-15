(in-package :bookshops.models)


(defclass sell ()
  ((cancelled
    :accessor sell-cancelled
    :initarg :cancelled
    :initform nil
    :type :bool
    :col-type :boolean
    :documentation "If the sale has been cancelled")

   (client
    :accessor sell-client
    :initarg :client
    :initform nil
    :col-type (or (:varchar 128) :null) ;TODO: should be link to client table?
    :documentation "Name of customer"))

  (:metaclass mito:dao-table-class)
  (:documentation "Represents a sale on a specific date of a number of books to a client."))

(defclass sold-cards ()
  ((card
    :col-type book)

   (sell
    :col-type sell)

   (quantity
    :initform 0
    :col-type :integer
    :documentation "Quantity of this book sold in this transaction. Can be a negative number.")

   (price
    :accessor sold-price
    :initarg :price
    :initform nil
    :col-type (or :float :null)))

  (:metaclass mito:dao-table-class)
  (:documentation "A many-to-many connection table representing books sold. Card field is a link to the book that was sold. Sell field is a link to the transaction in which it was sold."))

(defclass payment-method ()
  ((sell
    :col-type sell)

   (name
    :accessor payment-name
    :initarg :name
    :initform nil
    :col-type (or (:varchar 128) :null))

   (count-for-revenue
    :accessor count-for-revenue
    :initarg :count-for-revenue
    :initform t
    :col-type :boolean
    :documentation "If false, show this transaction in the history but don't count it in the revenue."))

   (:metaclass mito:dao-table-class)
   (:documentation "Payment method that was used to complete a Sell."))



