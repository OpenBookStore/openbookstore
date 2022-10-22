(in-package :bookshops.models)

(defun data2books (data)
  "Transform search results to a list of book objects."
  (loop for elt in data
     collect (make-book
              :title (str:sentence-case
                      (access elt :title))
              :isbn (access elt :isbn)
              :authors (str:title-case
                        (access elt :authors))
              :price (access elt :price)
              :details-url (access elt :details-url)
              :cover-url (access elt :cover-url)
              :publisher (str:title-case
                          (access elt :publisher))
              :date-publication (access elt :date-publication)
              :datasource (access elt :datasource)
              )))

(defun check-in-stock (data &key shelf-id)
  "DATA is not a list of book objects (but currently a hash-table).
  Add an IN-STOCK field by looking up there ISBN.

  Set the book shelf (Receive page). Ideally, we would not modify it if it already has a shelf,
AND we would send a notification to the user."
  ;; 1 query to get the ones in stock.
  (setf data (alexandria:ensure-list data))
  (let* ((in-stock (mito:select-dao 'book
                     (sxql:where
                      (:in :isbn (print (remove-if #'null
                                                   (mapcar (lambda (it)
                                                             (access it :isbn))
                                                           data)))))))
         (shelf (when shelf-id
                  (bookshops.models::find-shelf-by :id shelf-id))))
    ;; Add :in-stock to matches only.
    (loop for book in (print in-stock)
       for elt = (find (isbn book) data
                       :key (lambda (it) (access it :isbn))
                       :test #'string-equal)
       ;; do (format t "book isbn: ~a, elt found: ~a~&, quantity in stock: ~a" (isbn book) elt (quantity book))
       do (setf (access elt :in-stock)
                (quantity book))
       do (setf (access elt :id)
                (mito:object-id book))

       ;; Set the shelf.
       ;; In next iterations: don't change it if it is already set,
       ;; AND display a message to te user.
       do (if shelf
              (progn
                (log:info "--- this book already has a shelf. Btw, our shelf is " shelf)
                (setf (shelf book)
                      shelf)
                (mito:save-dao book)
                (setf (access elt :shelf)
                      shelf))
              (progn
                (log:info "--- No shelf. Set it to " shelf)
                (setf (shelf book)
                      shelf)
                (mito:save-dao book)
                (setf (access elt :shelf)
                      shelf)))
         )
    data))

(defun ensure-integer (number)
  "Return this number as an integer (TRUNCATE and discard decimals).
  If it isn't a number, return 0.

  Typically, for a price that is parsed as a float, NUMBER should be
  the price x 100, and we return it as an integer.

  - number: float

  Return: an integer or 0."
  (if (and number
           (not (equalp number 0))
           (numberp number))
      (truncate number)
      (progn
        ;; log or warning? Both!
        (log:warn "Could not parse ~s to an integer price." number)
        0)))

;; usage:
#+(or)
(progn
  (assert (= 1495 (ensure-integer (* 100 14.95))))
  (assert (= 0 (ensure-integer "foo"))))
