
(in-package :bookshops.models)

(export '(*print-details*))

#|
Usage:

(connect)

(make-book :title "antigone" :datasource "xxx")

(save-book *)

(find-dao 'book)
;; => #<Book antigone>

Design notes
============

A book's title, authors and publisher fields also get a -ascii
equivalent (without accentuated letters). The ascii field is used for
searches. This method was thought the most portable.

|#


(defparameter *db-name* (asdf:system-relative-pathname :bookshops "db.db"))

(defparameter *db* nil
  "DB connection object, returned by (connect).")

(defvar *current-place* nil
  "The current place we manipulate the books from.")

(setf str:*ellipsis* "(…)")
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
    :initform nil
    :type string
    ;; how to use a variable for 128 ?
    ;; we get datasource VARCHAR(+varchar-length+) NOT NULL,
    :col-type (or (:varchar 128) :null)
    :documentation "The source name (website) we took bibliographic information from.")

   (details-url
    :accessor details-url
    :initarg :details-url
    :initform nil
    :type string
    :col-type (or (:varchar 128) :null)
    :documentation "Link to the book's page on its datasource.")

   (title
    :accessor title
    :initarg :title
    :initform nil
    :type string
    :col-type (:varchar 128))

   (title-ascii
    :accessor title-ascii
    :initform nil
    :type string
    :col-type (or (:varchar 128) :null)
    :documentation "The title, normalized to not contain accentuated letters or special characters. To search a card by title we'll normalize the search query and search against this column. This is the most portable solution.")

   (isbn
    :accessor isbn
    :initarg :isbn
    :initform nil
    :type string
    :col-type (or (:varchar 128) :null))

   (price
    :accessor price
    :initarg :price
    ;; we don't default it to 0 (nil denotes a missing field),
    ;; and it might be useful for other objects.
    :initform nil
    :type float
    :col-type (or :integer :null))

   (date-publication
    :accessor date-publication
    :initarg :date-publication
    :initform nil
    :col-type (or (:varchar 128) :null))

   (publisher
    :accessor publisher
    :initarg :publisher
    :initform nil
    :type string
    :col-type (or (:varchar 128) :null))

   (publisher-ascii
    :accessor publisher-ascii
    :initform nil
    :type string
    :col-type (or (:varchar 128) :null))

   (authors
    :accessor authors
    :initarg :authors                   ;TODO: relationship
    :initform nil
    :col-type (or (:varchar 128) :null))

   (authors-ascii
    :accessor authors-ascii
    :initform nil
    :col-type (or (:varchar 128) :null)
    :documentation "Normalized representation of authors, without accentuated letters.")

   (cover-url
    :accessor cover-url
    :initarg :cover-url
    :initform nil
    :type string
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

(defmethod (setf title) :after (val (book book))
  "After setting a book's title, set also its title ASCII representation (used for searches)."
  ;; note: obviously these setf methods are not called if we use slot-value directly.
  ;: XXX: here tests will be welcome to catch other cases where they would not be called.
  (log:debug "updating title-ascii too")
  (setf (title-ascii book)
        (bookshops.utils::asciify (slot-value book 'title))))

(defmethod (setf authors) :after (val (book book))
  (log:debug "updating authors-ascii too")
  (setf (authors-ascii book)
        (bookshops.utils::asciify (slot-value book 'authors))))

(defmethod (setf publisher) :after (val (book book))
  (log:debug "updating publisher-ascii too")
  (setf (publisher-ascii book)
        (bookshops.utils::asciify (slot-value book 'publisher))))

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
    :initform nil
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
    :initform nil
    :col-type book)
   (place
    :accessor place
    :initarg :place
    :initform nil
    :col-type place)
   (quantity
    :accessor place-copy-quantity
    :initform 0
    :col-type (or (:integer) :null)))
  (:metaclass dao-table-class)
  (:documentation "Intermediate table between books and places.
    Specifies the quantity in the given place."))

(defmethod print-object ((pc place-copies) stream)
  (print-unreadable-object (pc stream :type t)
    (with-slots (quantity) pc
      (format stream "book \"~a\" in \"~a\", x~a"
              (str:prune 20 (title pc))
              (name (place pc))
              quantity))))

(defmethod title ((it place-copies))
  (if (place-copies-book it)
      (title (place-copies-book it))
      "<no book>"))

(defmethod name ((it place-copies))
  (name (place it)))

(defmethod price ((place place))
  (reduce #'+ (mapcar #'price (place-books place))))

(defun make-place (name)
  "Create a Place object (don't save in the DB)."
  (make-instance 'place :name name))

(defun save-place (place)
  (insert-dao place))

(defun create-place (name)
  "Create and save a `place' object in DB."
  (create-dao 'place :name name))

(defun default-place ()
  "Return the default place (the first created one by default).
   If none exist, create one."
  ;TODO: do that once at startup and bind a variable.
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
          (reduce #'+ (mapcar #'place-copy-quantity (place-books place)))
          (price place))
  (when details
    (format stream "~a~&" (mapcar #'print-book (place-books place)))))

(defmethod print-obj ((obj place) &optional (stream t))
  (print-place obj :stream stream))

(defun place-books (place)
  (mapcar #'place-copies-book (select-dao 'place-copies
                                (where (:= :place place)))))

(defun book-places (bk)
  (mapcar #'place (select-dao 'place-copies
                    (where (:= :book bk)))))

(defun book-places-quantities (bk)
  "Return the intermediate objects place-copies, so than we can know how
  many copies of this book are in what places."
  (select-dao 'place-copies
    (where (:= :book bk))))

(defun add-to (place bk &key (quantity 1))
  "Add the given book to this place.
   Return the quantity. nil means it is not present."
  (assert place)
  (assert bk)
  (unless (object-id bk)
    (error "The book ~a is not saved in DB." bk))
  (let ((existing (find-dao 'place-copies :place place :book bk))
        place-copy)
    (if existing
        (progn
          (log:info "The book ~a exists in ~a." bk place)
          (incf (place-copy-quantity existing) quantity)
          (save-dao existing)
          (quantity existing))
        (progn
          (log:info "~a doesn't exist in ~a yet, let's add it.~&" bk place)
          (setf place-copy (make-instance 'place-copies
                                          :place place
                                          :book bk
                                          :quantity quantity))
          (insert-dao place-copy)
          (quantity bk)))))

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

(defun print-quantity-red-green (qty)
  "If qty is > 0, print in green. If < 0, in red."
  ;; Always use colors, for the strings to be the same length. Helps in calcuting the padding.
  (cond
    ((= 0 qty)
     (white (prin1-to-string qty)))
    ((< qty 0)
     (red (prin1-to-string qty)))
    ((> qty 0)
     (green (prin1-to-string qty)))))

(defun print-book (book &optional (stream t))
  "Print to stream a user-readable output."
  ;; xxx: print as a nice table.
  ;; ~30a = substring 20 + ansi colors markers.
  ;; ~12@a = justify on the right, count colors markers.
  (format stream "~&~2@a- ~40a ~40a ~8@a x~12@a~&"
          (prin1-to-string (object-id book))
          (blue (str:prune 30 (title book)))
          (str:prune 40 (or (authors book) ""))
          (str:prune 15  (format nil "~$" (price book)))
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
                          (object-id (place it))
                          (name (place it))
                          (print-quantity-red-green (quantity it))))
                bk-places))
        (format t "~%This book is not registered in any place.~&"))))

(defun print-book-details (bk)
  (cond
    ((integerp bk)
     (setf bk (find-dao 'book :id bk)))
    ((stringp bk)
     ;; What with two similar titles?
     ;; Mito takes the first one.
     (setf bk (find-dao 'book :title bk))))
  (if bk
      (progn
        (format t "~a x ~a~&" (blue (title bk)) (quantity bk))
        (format t "~t~a~&" (authors bk))
        (format t "~tisbn: ~a~&" (isbn bk))
        (format t "~t~a~&" (price bk))
        (format t "~tcover: ~a~&" (cover-url bk))

        (print-book-repartition bk))
      (format t "There is no such book with id ~a~&" bk)))

(defun make-book (&key title isbn authors details-url cover-url publisher
                    date-publication price datasource)
  "Create a Book instance.
  Authors are saved as a string, not as related objects."
  (check-type price float)
  (make-instance 'book
                 :datasource datasource
                 :details-url details-url
                 :cover-url cover-url
                 :title title
                 :title-ascii (bookshops.utils::asciify title)
                 :isbn isbn
                 :authors authors
                 :authors-ascii (bookshops.utils::asciify authors)
                 :publisher publisher
                 :publisher-ascii (bookshops.utils::asciify publisher)
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
  "Find a book by slot. Example: (find-by :isbn xxx). Return only the first matching result."
  (when val
    (find-dao 'book key val)))

(defun find-existing (bk)
  "bk: a book object. Check in the DB if it already exists. Return a book."
  (when bk
    (let ((existing (find-by :isbn (isbn bk))))
      (if existing
          existing
          bk))))

(defun find-book (&key query (order :asc))
  "Return a list of book objects. If a query string is given, filter by the ascii title."
  (select-dao 'book
    (when query
      (where (:or (:like :title-ascii (str:concat "%" query "%"))
                  (:like :authors-ascii (str:concat "%" query "%")))))
    (order-by `(,order :created-at))))

(defun last-books (&key (order :asc))
  ""
  (select-dao 'book
    (order-by `(,order :created-at))))

(defun find-book-noisbn ()
  (select-dao 'book
    (where (:is-null :isbn))))

(defun count-book ()
  ""
  (count-dao 'book))

(defgeneric quantity (obj)
  (:documentation "Quantity of the given book, or the number of books in the given place."))

(defun total-quantities ()
  "Total number of cards in all places.
  Sums all quantities."
  (let ((places (find-places)))
    (loop for place in places
       sum (bookshops.models:quantity place))))

(defmethod quantity ((book book))
  "Sum of the quantities in all places."
  (if (object-id book)
      (let ((place-copies (select-dao 'place-copies
                            (where (:= :book book)))))
        (reduce #'+ (mapcar #'place-copy-quantity place-copies)))
      ;; if book not saved in db.
      0))

(defmethod (setf quantity) (val (book book))
  (error "Please add or remove copies from a place instead of setting the quantity directly."))

(defmethod quantity ((place place))
  "Quantity of books in this place."
  (reduce #'+ (mapcar #'place-copy-quantity (select-dao 'place-copies
                                              (where (:= :place place))))))

(defmethod quantity ((pc place-copies))
  ;; XXX: test the change from quantity of place to place-copy-quantity
  (place-copy-quantity pc))

;; (defmethod (setf quantity) (val (pc place-copies))
;;   (setf (quantity pc) val))

(defun set-quantity (book nb)
  "Set the quantity of this book into the default place."
  (assert (numberp nb))
  (add-to (default-place) book :quantity nb))

(defgeneric (setf quantity) (val obj))

(defmethod (setf quantity) (val (obj place-copies))
  (error "Please use proper methods to add copies to a place, or use the accessor instead."))

;;
;; Authors
;;

;; Class currently unused. Authors are stored as strings in books.
;; Actually, even the professional books DB (at least FEL à la demande)
;; doesn't represent authors objects.
;; And, throughout the application, the focused, most important object is the book.
;; We little need to get all books of an author (and in that case, we can make a search, see ABStock).

;; (defclass author ()
;;   ((name :accessor name
;;          :initarg :name
;;          :initform nil
;;          :col-type (:varchar 128)))
;;   (:metaclass dao-table-class))

;; (defmethod print-object ((author author) stream)
;;   (print-unreadable-object (author stream :type t)
;;     (format stream "~a" (slot-value author 'name))))

;;
;; Delete
;;
(defun delete-matching (kw)
  "Delete the books whose titles match kw."
  (delete-books (find-book :query kw)))

(defun delete-books (bklist)
  "Delete this list of books."
  (mapcar #'mito:delete-dao bklist))

(defun delete-books-without-authors (&key simulate)
  "Delete books without authors."
  (let ((to-delete (select-dao 'book
                     (where (:is-null :authors)))))
    (log:info "deleting ~a books with no authors" (length to-delete))
    (unless simulate
      (mapcar #'delete-dao to-delete))))

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
