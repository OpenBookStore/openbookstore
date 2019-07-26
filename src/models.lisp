(defpackage bookshops.models
  (:use :cl
        :mito
        :sxql
        :cl-ansi-text
        :log4cl)

  (:export :connect
           :ensure-tables-exist
           ;; book accessors
           :book
           :make-book
           :editor
           :find-book
           :find-by
           :find-existing
           :find-book-noisbn
           :last-books
           :title
           :authors
           :cover-url
           :isbn
           :price
           :print-book
           :print-book-details
           :count-book
           :quantity
           ;; book methods
           :save-book
           :create-book
           :quantity
           :set-quantity
           :delete-books
           :delete-obj
           :delete-objects
           ;; places
           :place
           :place-copies
           :make-place
           :create-place
           :current-place
           :save-place
           :print-place
           :find-places
           :find-place-by
           :default-place
           :add-to
           :remove-from
           :move
           :*current-place*
           ;; utils
           :print-quantity-red-green
           :negative-quantities
           :erase-metaclass-from))

(in-package :bookshops.models)

#|
Usage:

(connect)

(make-book :title "antigone" :datasource "xxx")

(save-book *)

(find-dao 'book)
;; => #<Book antigone>

|#

(defparameter *db-name* (asdf:system-relative-pathname :bookshops "db.db"))

(defparameter *db* nil
  "DB connection object, returned by (connect).")

(defvar *current-place* nil
  "The current place we manipulate the books from.")

(setf str:*ellipsis* "(…)")
;;
;; DB connection, migrations.
;;

(defun connect ()
  "Connect to the DB."
  ;; also use mito:*connection*
  (log:debug "connecting to ~a~&" *db-name*) (force-output)
  (setf *db* (connect-toplevel :sqlite3 :database-name *db-name*)))

(defun ensure-tables-exist ()
  (mapcar #'ensure-table-exists '(book
                                  place
                                  place-copies
                                  contact
                                  contact-copies)))

(defun migrate-all ()
  "Migrate the Book table after we changed the class definition."
  (mapcar #'mito:migrate-table '(book
                                 place
                                 place-copies
                                 contact
                                 contact-copies)))

;;
;; DB tables definition.
;;

;; col-types:
;; varchar, text, integer, serial, bigserial, binary,
;; timestamp, (or ... :null), relationship.

(defclass book ()
  ;; "Book class. Use make-book to create an object
  ;; (do not export and use book directly, use make-book).
  ;;
  ;; After modification, run (migrate-all)
  ;;
  ;; - create a date: (local-time:now)
  ;; "
  ((datasource
    :accessor datasource
    :initarg :datasource
    ;; how to use a variable for 128 ?
    ;; we get datasource VARCHAR(+varchar-length+) NOT NULL,
    :col-type (or (:varchar 128) :null)
    :documentation "The source (website) we took bibliographic information from.")

   (title
    :initarg :title
    :accessor title
    :col-type (:varchar 128))

   (isbn
    :accessor isbn
    :initarg :isbn
    :col-type (or (:varchar 128) :null))

   (price
    :initarg :price
    :accessor price
    ;; we don't default it to 0 (nil denotes a missing field),
    ;; and it might be useful for other objects.
    :col-type (or :integer :null))

   (date-publication
    :accessor date-publication
    :initarg :date-publication
    :col-type (or (:varchar 128) :null))

   (editor
    :accessor editor :initarg :editor
    :col-type (or (:varchar 128) :null))

   (authors
    :accessor authors
    :initarg :authors                   ;TODO: relationship
    :col-type (or (:varchar 128) :null))

   (cover-url
    :accessor cover-url
    :initarg :cover-url
    :col-type (or (:varchar 1024) :null)))
  (:metaclass dao-table-class)
  (:documentation "A book represents the book entity, not the physical object.
     It may not have an isbn.
     A book is stored in one or many places."))

(defgeneric title (obj)
  (:documentation "Title of a book.")
  (:method (obj)
    ;; nothing (for erronous print-objects to work).
    ))

(defmethod title ((book book))
  (slot-value book 'title))

(defgeneric price (obj)
  (:documentation "Return the price of the current object. Return 0 if nil."))

(defmethod price ((book book))
  (cond
    ;; should not happen now with the initform to 0.
    ((null (slot-value book 'price))
     0)
    (t (slot-value book 'price))))

(defmethod (setf price) (price (book book))
  "Set the price of this book."
  (setf (slot-value book 'price) price))

(defmethod print-object ((book book) stream)
  (print-unreadable-object (book stream :type t)
    (with-accessors ((title title))
        book
      (format stream "~a" title))))

(defclass place ()
  ((name
    ;; accessor as generic, for the intermediate class too.
    :initarg :name
    :col-type (:varchar 128)))
  (:metaclass dao-table-class)
  (:documentation "Where the books are stored.
    It could be a temporary place, like a stand, or a friend's. We are allowed to sell from certain
    places, not from others."))

(defgeneric name (obj)
  (:documentation "Name of this object.")
  (:method (obj)
    ;; nothing
    ))

(defmethod name ((place place))
  (slot-value place 'name))

(defmethod (setf name) (val (place place))
  (setf (slot-value place 'name) val))

;; Intermediate table for the book <-> place many-to-many relationship.
(defclass place-copies ()
  ((book
    :accessor place-copies-book
    :initarg :book
    :col-type book)
   (place
    :accessor place-copies-place
    :initarg :place
    :col-type place)
   (quantity
    :accessor place-copies-quantity
    :initform 0
    :col-type (or (:integer) :null)))
  (:metaclass dao-table-class)
  (:documentation "Intermediate table between books and places.
    Specifies the quantity in the given place."))

(defmethod print-object ((pc place-copies) stream)
  (print-unreadable-object (pc stream :type t)
    (format stream "book \"~a\" in \"~a\", x~a"
            (str:prune 20 (title pc))
            (name (place-copies-place pc))
            (place-copies-quantity pc))))

(defmethod title ((it place-copies))
  (if (place-copies-book it)
      (title (place-copies-book it))
      "<no book>"))

(defmethod name ((it place-copies))
  (name (place-copies-place it)))

(defmethod price ((place place))
  (reduce #'+ (mapcar #'price (place-books place))))

(defun make-place (name)
  "Create a Place object in the DB."
  (make-instance 'place :name name))

(defun save-place (place)
  (insert-dao place))

(defun create-place (name)
  (create-dao 'place :name name))

(defun default-place ()
  "Return the default place (the first created one by default).
   If none exist, create one."
  (when (mito.connection:connected-p)
    ;; Check the connection because of setf *current-place* in commands package.
    ;; Or initialize it elsewhere.
    (if (= 0 (count-dao 'place))
        (create-place "home")
        (first (select-dao 'place (order-by (:asc :id)))))))

(defun current-place ()
  "Return the current place, set it with the default one if needed."
  ;; since it wasn't initialized, see above.
  (or *current-place*
      (setf *current-place* (default-place))))

(defun find-places (&optional query)
  "If query (list of strings), return places matching this name. Otherwise, return all places."
  (if query
      (progn
        ;; xxx should be same interface as find-book
        (unless (consp query)
          (setf query (cons query nil)))
        (select-dao 'place
          (where (:like :name (str:concat "%" (str:join "%" query) "%")))))
      (select-dao 'place)))

(defun find-place-by (key val)
  "Find a place by key. "
  (when val
    (find-dao 'place key val)))

(defmethod print-object ((place place) stream)
  (print-unreadable-object (place stream :type t)
    (format stream "~a" (name place))))

(defparameter *print-details* nil
  "Print some lists with details.")

(defun print-place (place &key (stream t) (details *print-details*))
  "Print the name of the place and its number of books.
   If :details is t, print a paginated list of its books."
  (format stream "~2a - ~40a~t x~3a/ ~3a total: ~3a~&"
          (object-id place)
          (name place)
          (length (place-books place))
          (reduce #'+ (mapcar #'quantity (place-books place)))
          (price place))
  (when details
    (format stream "~a~&" (mapcar #'print-book (place-books place)))))

(defmethod print-obj ((obj place) &optional (stream t))
  (print-place obj :stream stream))

(defun place-books (place)
  (mapcar #'place-copies-book (select-dao 'place-copies
                                (where (:= :place place)))))

(defun book-places (bk)
  (mapcar #'place-copies-place (select-dao 'place-copies
                                 (where (:= :book bk)))))

(defun add-to (place bk &key (quantity 1))
  "Add the given book to this place.
   Return the quantity. nil means it is not present."
  (assert bk)
  (assert place)
  (unless (object-id bk)
    (error "The book ~a is not saved in DB." bk))
  (let ((existing (find-dao 'place-copies :place place :book bk))
        place-copy)
    (if existing
        (progn
          (log:info "The book ~a exists in ~a." bk place)
          (incf (place-copies-quantity existing) quantity)
          (save-dao existing)
          (quantity existing))
        (progn
          (log:info "~a doesn't exist in ~a yet, let's add it.~&" bk place)
          (setf place-copy (make-instance 'place-copies
                                          :place place
                                          :book bk
                                          :quantity quantity))
          (insert-dao place-copy)
          (quantity place-copy)))))

(defun remove-from (place bk &key (quantity 1))
  "Remove the given book from this place.
   Return the quantity.
   If the book was never in the original place, don't remove it. Otherwise, it can end in a negative quantity."
  (unless (object-id bk)
    (error "The book ~a is not saved in the DB." bk))
  (let ((existing (find-dao 'place-copies :place place :book bk))
        qty)
    (if existing
        (progn
          (setf qty (quantity existing))
          (setf (quantity existing) (decf qty quantity))
          (save-dao existing)
          ;; (when (minusp qty)
          ;;   (format t "~a" (red (format nil "mmh, you now have a negative stock of \"~a\"" (title bk)))))
          qty)
        (progn
          (format t "Will not remove the book ~a from ~a, it doesn't exist there.~&" bk place)
          nil))))

(defun print-quantity-red-green (qty &optional (stream nil))
  "If qty is > 0, print in green. If < 0, in red."
  (cond
    ((= 0 qty)
     (format stream "~a" qty))
    ((< qty 0)
     (red (prin1-to-string qty)))
    ((> qty 0)
     (green (prin1-to-string qty)))))

(defun print-book (book &optional (stream t))
  "Print to stream a user-readable output."
  ;; xxx: print as a nice table.
  ;; ~30a = substring 20 + ansi colors markers.
  (format stream "~&~2@a- ~40a ~40a ~15a x ~3@a~&"
          (prin1-to-string (object-id book))
          (blue (str:prune 30 (title book)))
          (str:prune 40 (or (authors book) ""))
          (str:prune 15 (or (format nil "~a" (price book))
                            ""))
          (print-quantity-red-green (quantity book))))

(defmethod print-obj ((obj book) &optional (stream t))
  (print-book obj stream))

(defun print-book-repartition (bk)
  "Print a list of places where this book exists with its quantity.
   bk: book object or id (int).

   Example output:

   1  - default place                              x1
   2  - second place                               x-2
   "
  (when (integerp bk)
    (setf bk (find-dao 'book :id bk)))
  (let ((bk-places (select-dao 'place-copies
                     (where (:= :book bk)))))
    (if bk-places
        (progn
          (format t "----------~&")
          (format t "In places:~%")
          (format t "----------~%")

          (mapc (lambda (it)
                  (format t "~2a - ~40a ~t x~a~&"
                          (object-id (place-copies-place it))
                          (name (place-copies-place it))
                          (print-quantity-red-green (quantity it))))
                bk-places))
        (format t "~%This book is not registered in any place.~&"))))

(defun print-book-details (bk)
  (when (integerp bk)
    (setf bk (find-dao 'book :id bk)))
  (if bk
      (progn
        (format t "~a x ~a~&" (blue (title bk)) (quantity bk))
        (format t "~t~a~&" (authors bk))
        (format t "~tisbn: ~a~&" (isbn bk))
        (format t "~t~a~&" (price bk))
        (format t "~tcover: ~a~&" (cover-url bk))

        (print-book-repartition bk))
      (format t "There is no such book with id ~a~&" bk)))

(defun make-book (&key title isbn authors cover-url editor date-publication price datasource)
  "Create a Book instance. If given author or authors, create Author
  instance(s) if they don't already exist in DB.
  "
  (make-instance 'book
                 :datasource datasource
                 :cover-url cover-url
                 :title title
                 :isbn isbn
                 :authors authors
                 :editor editor
                 :price price
                 :date-publication date-publication))

(defun create-book (&key title isbn)
  (create-dao 'book
              :title title
              :isbn isbn))

(defun save-book (book)
  "Save this book in DB. If it already exists, return the existing book. Otherwise, return the new one."
  ;; logging
  (let ((existing (find-by :isbn (isbn book))))
    (if existing
        (progn
          (log:info "book of isbn " (isbn book) " is already in stock.")
          (save-dao existing)
          existing)
        (progn
          (let ((new (insert-dao book)))
            (log:info "creating new book")
            (save-dao new)
            new)))))

(defun find-by (key val)
  "Find by slot. Example: (find-by :isbn xxx). Return only the first matching result."
  (when val
    (find-dao 'book key val)))

(defun find-existing (bk)
  "bk: a book object. Check in the DB if it already exists. Return a book."
  (when bk
    (let ((existing (find-by :isbn (isbn bk))))
      (if existing
          existing
          bk))))

(defun find-book (&optional query)
  "Return a list of book objects. If a query string is given, filter by title."
  (if query
      (select-dao 'book
        (where (:like :title (str:concat "%" query "%"))))
      (select-dao 'book)))

(defun last-books ()
  "Newest first."
  (reverse
   (select-dao 'book
     (order-by :created-at))))

(defun find-book-noisbn ()
  (select-dao 'book
    (where (:is-null :isbn))))

(defun count-book ()
  ""
  (count-dao 'book))

(defgeneric quantity (obj)
  (:documentation "Quantity of the given book, or the number of books in the given place."))

(defmethod quantity ((book book))
  "Sum of the quantities in all places."
  (if (object-id book)
      (let ((place-copies (select-dao 'place-copies
                            (where (:= :book book)))))
        (reduce #'+ (mapcar #'place-copies-quantity place-copies)))
      ;; if book not saved in db.
      0))

(defmethod quantity ((place place))
  "Quantity of books in this place."
  (reduce #'+ (mapcar #'place-copies-quantity (select-dao 'place-copies
                                                (where (:= :place place))))))

(defmethod quantity ((pc place-copies))
  (place-copies-quantity pc))

;; (defmethod (setf quantity) (val (pc place-copies))
;;   (setf (place-copies-quantity pc) val))

(defun set-quantity (book nb)
  "Set the quantity of this book into the default place."
  (assert (numberp nb))
  (add-to (default-place) book :quantity nb))

(defgeneric (setf quantity) (val obj))

(defmethod (setf quantity) (val (obj place-copies))
  ;TODO: TEST !! just wrote it to remove a compiler warning. see line
  ; see           (setf (quantity existing) (decf qty quantity))
  ;; in remove-from
  (assert (numberp val))
  (add-to (default-place) obj :quantity val))

;;
;; Authors
;;

(defclass author ()
  ((name :accessor name :initarg :name
         :col-type (:varchar 128)))
  (:metaclass dao-table-class))

(defmethod print-object ((author author) stream)
  (print-unreadable-object (author stream :type t)
    (format stream "~a" (slot-value author 'name))))

;;
;; Delete
;;
(defun delete-matching (kw)
  "Delete the books whose titles match kw."
  (delete-books (find-book kw)))

(defun delete-books (bklist)
  "Delete this list of books."
  (mapcar #'mito:delete-dao bklist))

(defgeneric delete-obj (obj)
  (:method (obj)
    (let ((place-copies (select-dao 'place-copies
                          (where (:= :book obj)))))
      (mapcar #'delete-dao place-copies)
      (delete-dao obj))))

(defmethod delete-obj ((place place))
  (let ((place-copies (select-dao 'place-copies
                        (where (:= :place place)))))
    (mapcar #'delete-dao place-copies)
    (delete-dao place)))

(defun delete-objects (objlist)
  (mapcar #'delete-obj objlist))

;;
;; Move from place to place
;;
;; use current-place, new command "inside <place>" ?
(defun move (bk to &key (quantity 1) (from (current-place)))
  "Move a book from the actual place (the default one) to another one.
   If :from is specified, move from this place."
  (log:info from (object-id from)
            to (object-id to))
  (if (= (object-id from) (object-id to))
      (format t (_ "No need to move this book from and to the same place (~a).~&") (name to))
      (progn
        (if (remove-from from bk :quantity quantity)
            (progn
              (add-to to bk :quantity quantity)
              (format t "Moved ~a cop~:@p of '~a' from ~a to ~a.~&"
                      quantity (title bk) (name from) (name to)))))))

;;
;; Some stats, observing the stock.
;;
(defun negative-quantities ()
  "Return a list of place-copies where the book's quantity is negative."
  (select-dao 'place-copies
    (where (:< :quantity 0))))

;;
;; utils
;;
(defun erase-metaclass-from (class)
  "Needed to change the metaclass, e.g. add mito."
  ;; https://stackoverflow.com/questions/38811931/how-to-change-classs-metaclass
  (setf (find-class class) nil))
