(in-package :openbookstore.models)


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
    :documentation "Name of customer")

   (payment-method-id
    :accessor payment-method-id
    :initarg :payment-method-id
    :initform -1
    :col-type :integer
    :documentation "ID used for this payment method. We currently don't store all the payment methods in a table because: they will quite change depending on the user, they might change in time, we are not so interested about them. We want to compute totals per payment method and show totals in the UI.")

   (payment-method-name
    :accessor payment-method-name
    :initarg :payment-method-name
    :initform ""
    :col-type (or (:varchar 128) :null)
    :documentation "Name of the payment method used for this transaction. NOTE: we should allow up to 3 payment methods."))

  (:metaclass mito:dao-table-class)
  (:documentation "Represents a sale on a specific date of a number of books to a client."))

(defmethod print-object ((obj sell) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "#~a"
            (or
             ;; let's be defensive!
             (ignore-errors (mito:object-id obj))
             "<error getting object ID>"))))

(defclass sold-cards ()
  ((card
    :accessor card
    :col-type book)

   (sell
    :accessor sell
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

(defmethod print-object ((obj sold-cards) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((card card))
        obj
      (format stream "#~a for card ~a (#~a)"
              (mito:object-id obj)
              (or
               (ignore-errors (title card))
               "<error getting card's title !>")
              (or
               (ignore-errors (mito:object-id card))
               "<error getting the card ID!>")))))

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

(defun make-sale (&key books payment client date payment-method-id payment-method-name)
  "Create one sell with the given books (list of alist with ID, PRICE, QUANTITY).
  For each book, create a SOLD-CARD row.

  Only BOOKs is required.

  Example:

  (make-sale :books '(((:ID . 45) (:QUANTITY . 2) (:PRICE . 13.72))
          ((:ID . 15) (:QUANTITY . 2) (:PRICE . 41))))

  "
  (let* ((date (or date (local-time:now)))
         (sale (make-instance 'sell :client client :date date
                              :payment-method-id payment-method-id
                              :payment-method-name payment-method-name))
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

#+(or)
(make-sale :books '(((:ID . 45) (:QUANTITY . 2) (:PRICE . 13.72))
                    ((:ID . 15) (:QUANTITY . 2) (:PRICE . 41))))

(defun find-sell (&key (order :asc) (limit 400) max-date min-date)
  "Find sell objects.

  Return: a list of SELL objects

  ORDER: :asc or :desc
  LIMIT: maximum objects to return (defaults to 400).

  MIN-DATE: a local-time date. Find the sells from and including this date.
  MAX-DATE: find sells up to and including this date."
  ;; TODO: search soldcards, group by sell_id, colorize.
  (mito:select-dao 'sell
                   (when min-date
                     (sxql:where (:>= :created-at min-date)))
                   (when max-date
                     (sxql:where (:<= :created-at
                                      ;; need to add one day to include today
                                      (local-time:timestamp+ max-date 1 :day))))
                   (sxql:order-by `(,order :created-at))
                   (sxql:limit limit)))

(defun find-soldcards (&key (order :asc) (limit 400) sell sell-id)
  "Find soldcards objects (sells details).

  If SELL or SELL-ID is not nil, filter by the sell ID.

  Limit results to LIMIT (last 400 transactions by default).

  Return: a list of SOLD-CARDS objects.

  Example:

  (find-soldcards :order :desc :sell-id 19)"
  ;; TODO: views to show by month and day, with no limit..
  (mito:select-dao 'sold-cards
                   (when sell-id
                     (sxql:where (:= :sell-id sell-id)))
                   (when sell
                     (sxql:where (:= :sell sell)))
                   (sxql:order-by `(,order :created_at))
                   (sxql:limit limit)))

(defun group-sells-and-soldcards (&key (order :asc) (limit 400) min-date max-date)
  "Find soldcard objects between MIN-DATE and MAX-DATE (both including).
  The goal is to show sells in the history: one line per sold card, but they should be grouped by sells.
  We want to show the sell ID only once.

  Return a list of dict with:
  - sell-id
  - sell object
  - created-ad: date of the sell
  - soldcard: the soldcard objects (it's about 1 sold item), which contains the price sold, the quantity sold, etc.
  - first-sell-item: used for display. We want to show the sell ID and date only for the first soldcard of this sell.
  - item-group: T or NIL, used for display (different colors for each sell)."
  ;; NOTE: we don't have a backlink from a SELL to its SOLD-CARDS ?
  (let ((sells (find-sell :order order :limit limit :min-date min-date :max-date max-date)))
    (alexandria:flatten
     (loop for sell in sells
        for sell-id = (mito:object-id sell)
        for item-group = t then (not item-group) ;; used to color each group of sells.
        collect (loop for soldcard in (find-soldcards :sell-id (mito:object-id sell))
                   for first-sell-item = t then nil
                   collect (dict :sell-id sell-id
                                 :first-sell-item first-sell-item
                                 :item-group item-group
                                 :sell sell
                                 :created-at (mito:object-created-at sell)
                                 :soldcard soldcard))))))

#+(or)
(group-sells-and-soldcards :min-date (utils:yesterday) :max-date (local-time:today))
