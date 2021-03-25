(in-package :bookshops.models)


(defclass sell ()
  ((cancelled
    :accessor sell-cancelled
    :initarg :cancelled
    :initform nil
    :type :bool
    :col-type :boolean
    :documentation "If the sale has been cancelled")

   (date
    :accessor date
    :initarg :date
    :type :date-stamp
    :col-type :datestamp
    :documentation "Date of the sale")

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
    :accessor card
    :col-type book)

   (sell
    :col-type sell)

   (quantity
    :accessor quantity
    :initform 0
    :col-type :integer
    :documentation "Quantity of this book sold in this transaction. Can be a negative number.")

   (sold-price
    :accessor sold-price
    :initarg :sold-price
    :initform nil
    :col-type (or :float :null))

   (current-price
    :accessor current-price
    :initarg :current-price
    :initform nil
    :col-type (or :float :null)
    :documentation "Book list price at time of sale")
   )

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

(defun make-sale (&key books payment client date)
  (let* ((sale (make-instance 'sell :client client :date date))
         (payment (make-instance 'payment-method :sell sale :name payment))
         (bookobjs (mapcar (lambda (book)
                             (let ((card (find-by :id (access book :id))))
                               (unless card
                                 (error "Book not found!"))
                               (make-instance
                                'sold-cards
                                :current-price (price card)
                                :sold-price (access book :price)
                                :quantity (access book :quantity)
                                :card card
                                :sell sale)))
                           books)))
    (mito:save-dao sale)
    (mito:save-dao payment)  ;XXX: why save the payment? This leaked threw a PR review.
    (mapc #'mito:save-dao bookobjs)
    (dolist (sold bookobjs)
      (add-to (default-place)
              (card sold)
              :quantity (- (quantity sold))))
    sale))

(defun find-sell (&key (order :asc))
  "Find sell objects."
  ;TODO: search soldcards, group by sell_id, colorize.
  (mito:select-dao 'sell
    (sxql:order-by `(,order :created-at))))

(defun find-soldcards (&key (order :asc) (limit 400))
  "Find soldcards objects (sells details)."
  ;TODO: views to show by month and day, with no limit..
  (mito:select-dao 'sold-cards
    (sxql:order-by `(,order :created_at))
    (sxql:limit limit)))
