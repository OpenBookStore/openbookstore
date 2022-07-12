
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
    :type (or string null)
    ;; how to use a variable for 128 ?
    ;; we get datasource VARCHAR(+varchar-length+) NOT NULL,
    :col-type (or (:varchar 128) :null)
    :documentation "The source name (website) we took bibliographic information from.")

   (details-url
    :accessor details-url
    :initarg :details-url
    :initform nil
    :type (or string null)
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
    :type (or string null)
    :col-type (or (:varchar 128) :null)
    :documentation "The title, normalized to not contain accentuated letters or special characters. To search a card by title we'll normalize the search query and search against this column. This is the most portable solution.")

   (isbn
    :accessor isbn
    :initarg :isbn
    :initform nil
    :type (or string null)
    :col-type (or (:varchar 128) :null))

   (price
    :accessor price
    :initarg :price
    ;; we don't default it to 0 (nil denotes a missing field),
    ;; and it might be useful for other objects.
    :initform nil
    :type (or integer float null) ;; integer: for compatibility. Otherwise, Mito is strict about float and fails.
    :col-type (or :float :null))  ;; (or :integer :float :null) fails in tests

   (date-publication
    :accessor date-publication
    :initarg :date-publication
    :initform nil
    :col-type (or (:varchar 128) :null))

   (publisher
    :accessor publisher
    :initarg :publisher
    :initform nil
    :type (or string null)
    :col-type (or (:varchar 128) :null))

   (publisher-ascii
    :accessor publisher-ascii
    :initform nil
    :type (or string null)
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

   (shelf
    :accessor shelf
    :initform nil
    :col-type (or :null shelf)
    :documentation "Shelf")

   (cover-url
    :accessor cover-url
    :initarg :cover-url
    :initform nil
    :type (or string null)
    :col-type (or (:varchar 1024) :null)))
  (:metaclass mito:dao-table-class)
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
        (utils::asciify (slot-value book 'title))))

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
    (with-accessors ((title title) (shelf shelf))
        book
      (format stream "~a. SHELF: ~a" title (name shelf)))))

(defun pprint-books (results)
  "Pretty print this list of books, as rows."
  (let ((i (length results)))
    (mapcar (lambda (it)
              (format t "~2@a- ~a, ~a~t~$ ~tstock: x~a~&"
                      i
                      (blue (title it))
                      (authors it)
                      (price it)
                      (print-quantity-red-green (quantity it)))
              (format t "~t ed: ~a, ~a~&"
                      (publisher it)
                      (isbn it))
              (decf i))
            (reverse results))))

(defclass place ()
  ((name
    ;; accessor as generic, for the intermediate class too.
    :initarg :name
    :initform nil
    :col-type (:varchar 128)))
  (:metaclass mito:dao-table-class)
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
  (:metaclass mito:dao-table-class)
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
  (mito:insert-dao place))

(defun create-place (name)
  "Create and save a `place' object in DB."
  (mito:create-dao 'place :name name))

(defun default-place ()
  "Return the default place (the first created one by default).
   If none exist, create one."
  ;TODO: do that once at startup and bind a variable.
  (when (mito.connection:connected-p)
    ;; Check the connection because of setf *current-place* in commands package.
    ;; Or initialize it elsewhere.
    (if (= 0 (mito:count-dao 'place))
        (create-place "home")
        (first (mito:select-dao 'place (sxql:order-by (:asc :id)))))))

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
        (mito:select-dao 'place
          (sxql:where (:like :name (str:concat "%" (str:join "%" query) "%")))))
      (mito:select-dao 'place)))

(defun find-place-by (key val)
  "Find a place by key. "
  (when val
    (mito:find-dao 'place key val)))

(defmethod print-object ((place place) stream)
  (print-unreadable-object (place stream :type t)
    (format stream "~a" (name place))))

(defparameter *print-details* nil
  "Print some lists with details.")

(defun print-place (place &key (stream t) (details *print-details*))
  "Print the name of the place and its number of books.
   If :details is t, print a paginated list of its books."
  (format stream "~2a - ~40a~t x~3a/ ~3a total: ~3a~&"
          (mito:object-id place)
          (name place)
          (length (place-books place))
          (reduce #'+ (mapcar #'place-copy-quantity (place-books place)))
          (price place))
  (when details
    (format stream "~a~&" (mapcar #'print-book (place-books place)))))

(defmethod print-obj ((obj place) &optional (stream t))
  (print-place obj :stream stream))

(defun place-books (place)
  (mapcar #'place-copies-book (mito:select-dao 'place-copies
                                (sxql:where (:= :place place)))))

(defun book-places (bk)
  (mapcar #'place (mito:select-dao 'place-copies
                    (sxql:where (:= :book bk)))))

(defun book-places-quantities (bk)
  "Return the intermediate objects place-copies, so than we can know how
  many copies of this book are in what places."
  (mito:select-dao 'place-copies
    (sxql:where (:= :book bk))))

(defun add-to (place bk &key (quantity 1))
  "Add the given book to this place.
   Return the quantity. nil means it is not present."
  (assert place)
  (assert bk)
  (unless (mito:object-id bk)
    (error "The book ~a is not saved in DB." bk))
  (let ((existing (mito:find-dao 'place-copies :place place :book bk))
        place-copy)
    (if existing
        (progn
          (log:info "The book ~a exists in ~a." bk place)
          (incf (place-copy-quantity existing) quantity)
          (mito:save-dao existing)
          (quantity existing))
        (progn
          (log:info "~a doesn't exist in ~a yet, let's add it.~&" bk place)
          (setf place-copy (make-instance 'place-copies
                                          :place place
                                          :book bk
                                          :quantity quantity))
          (mito:insert-dao place-copy)
          (quantity bk)))))

(defun remove-from (place bk &key (quantity 1))
  "Remove the given book from this place.
   Return the quantity.
   If the book was never in the original place, don't remove it. Otherwise, it can end in a negative quantity."
  (unless (mito:object-id bk)
    (error "The book ~a is not saved in the DB." bk))
  (let ((existing (mito:find-dao 'place-copies :place place :book bk))
        qty)
    (if existing
        (progn
          (setf qty (quantity existing))
          (setf (quantity existing) (decf qty quantity))
          (mito:save-dao existing)
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
     (cl-ansi-text:white (prin1-to-string qty)))
    ((< qty 0)
     (cl-ansi-text:red (prin1-to-string qty)))
    ((> qty 0)
     (cl-ansi-text:green (prin1-to-string qty)))))

(defun print-book (book &optional (stream t))
  "Print to stream a user-readable output."
  ;; xxx: print as a nice table.
  ;; ~30a = substring 20 + ansi colors markers.
  ;; ~12@a = justify on the right, count colors markers.
  (format stream "~&~2@a- ~40a ~40a ~8@a x~12@a~&"
          (prin1-to-string (mito:object-id book))
          (cl-ansi-text:blue (str:prune 30 (title book)))
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
    (setf bk (mito:find-dao 'book :id bk)))
  (let ((bk-places (mito:select-dao 'place-copies
                     (sxql:where (:= :book bk)))))
    (if bk-places
        (progn
          (format t "----------~&")
          (format t "In places:~%")
          (format t "----------~%")

          (mapc (lambda (it)
                  (format t "~2a - ~40a ~t x~a~&"
                          (mito:object-id (place it))
                          (name (place it))
                          (print-quantity-red-green (quantity it))))
                bk-places))
        (format t "~%This book is not registered in any place.~&"))))

(defun print-book-details (bk)
  (cond
    ((integerp bk)
     (setf bk (mito:find-dao 'book :id bk)))
    ((stringp bk)
     ;; What with two similar titles?
     ;; Mito takes the first one.
     (setf bk (mito:find-dao 'book :title bk))))
  (if bk
      (progn
        (format t "~a x ~a~&" (cl-ansi-text:blue (title bk)) (quantity bk))
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
                 :price (utils:ensure-float price)
                 :date-publication date-publication))

(defun create-book (&rest initargs)
  "Create a book and save it. CAUTION: you must provide title-ascii and authors-ascii.
  Or use make-book and save-book."
  (unless (member :title-ascii initargs)
    (warn "create-book: you probably forgot to give title-ascii."))
  (unless (member :authors-ascii initargs)
    (warn "create-book: you probably forgot to give authors-ascii."))
  ;;
  (apply #'mito:create-dao 'book initargs))

(defun save-book (book)
  "Save this book in DB. If it already exists, return the existing book. Otherwise, return the new one."
  ;; logging
  (let ((existing (find-by :isbn (isbn book))))
    (if existing
        (progn
          (log:info "book of isbn " (isbn book) " is already in stock.")
          (mito:save-dao existing)
          existing)
        (progn
          (let ((new (mito:insert-dao book)))
            (log:info "creating new book")
            (mito:save-dao new)
            new)))))

(defun find-by (key val)
  "Find a book by slot. Example: (find-by :isbn xxx). Return only the first matching result."
  (when val
    (mito:find-dao 'book key val)))

(defun update-book (book bk)
  "Update and save the book object with this bk data."
  (loop for slot in '(:price :date-publication :publisher :details-url :datasource
                      :cover-url)
     if (not (equal (access book slot) (access bk slot)))
     do (log:debug "updating ~a~&" slot)
     do (setf (access book slot)
              (access bk slot))
     do (mito:save-dao book))
  book)

(defun find-existing (bk &key update)
  "bk: a book object. Check in the DB if it already exists.
  Update its fields.
  Return a book object."
  (when bk
    (let ((existing (find-by :isbn (isbn bk))))
      (if existing
          (if update
              (update-book existing bk)
              existing)
          bk))))

(defun %build-sxql-words-query (query)
  "Return an expression for Mito's select-dao :when (using SxQL).
  Used to compose queries in find-book.

  We want to search the words of the query in both the title and the authors field.

(%build-sxql-words-query \"hello world love\")
;; =>
(:AND
 (:OR (:LIKE :TITLE-ASCII \"%hello%\") (:LIKE :AUTHORS-ASCII \"%hello%\"))
 (:OR (:LIKE :TITLE-ASCII \"%world%\") (:LIKE :AUTHORS-ASCII \"%world%\")))
 (:OR (:LIKE :TITLE-ASCII \"%love%\") (:LIKE :AUTHORS-ASCII \"%love%\")))
"
  (if query
      `(:and
        ,@(loop for word in (str:words query)
             :collect `(:or (:like :title-ascii ,(str:concat "%" word "%"))
                            (:like :authors-ascii ,(str:concat "%" word "%")))))
      '()))

(defun %build-sxql-shelf-query (shelf)
  "If shelf is not NIL, return an expression for Mito's select-dao (using SxQL).
  We search by the object ID."
  ;; I was having a difficulty extending an existing macro.
  ;; Instead of writing a bigger one, decomposing the query building made it easier.
  (if shelf
      `(:= :shelf_id ,(mito:object-id shelf))
      '()))

(defun %merge-queries (q1 q2)
  "Merge two SxQL queries to one, joined by a AND.
  One query can be NIL but one query must not be NIL, or else the SQL query will fail. Please do your checks beforehand.

  Example:

  (%merge-queries '(:= :SHELF_ID 1) '())
  ;; => (:= :SHELF_ID 1)

"
  (cond
    ((null q1)
     q2)
    ((null q2)
     q1)
    (t
     `(:and ,q1 ,q2))))

(defun find-book (&key query
                    shelf
                    (order :desc) (limit 50))
  "Return a list of book objects.
  If a query string is given, filter by the ascii title and authors.

  Filter also by:

  - shelf (object)

  Parameters:

  - order: :desc (default) or :asc
  - limit: 50"
  (let ((shelf-query (%build-sxql-shelf-query shelf))
        (words-query (%build-sxql-words-query query)))
    (mito:select-dao 'book
      (when (or query shelf)
        (sxql:where
         (%merge-queries shelf-query words-query)))
      (sxql:limit limit)
      (sxql:order-by `(,order :created-at)))))

(defun last-books (&key (order :asc))
  ""
  (mito:select-dao 'book
    (sxql:order-by `(,order :created-at))))

(defun find-book-noisbn ()
  (mito:select-dao 'book
    (sxql:where (:is-null :isbn))))

(defun count-book ()
  ""
  (mito:count-dao 'book))

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
  (if (mito:object-id book)
      (let ((place-copies (mito:select-dao 'place-copies
                            (sxql:where (:= :book book)))))
        (reduce #'+ (mapcar #'place-copy-quantity place-copies)))
      ;; if book not saved in db.
      0))

(defmethod (setf quantity) (val (book book))
  (error "Please add or remove copies from a place instead of setting the quantity directly."))

(defmethod quantity ((place place))
  "Quantity of books in this place."
  (reduce #'+ (mapcar #'place-copy-quantity (mito:select-dao 'place-copies
                                              (sxql:where (:= :place place))))))

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
  (let ((to-delete (mito:select-dao 'book
                     (sxql:where (:is-null :authors)))))
    (log:info "deleting ~a books with no authors" (length to-delete))
    (unless simulate
      (mapcar #'mito:delete-dao to-delete))))

(defgeneric delete-obj (obj)
  (:method (obj)
    (let ((place-copies (mito:select-dao 'place-copies
                          (sxql:where (:= :book obj)))))
      (mapcar #'mito:delete-dao place-copies)
      (mito:delete-dao obj))))

(defmethod delete-obj ((place place))
  (let ((place-copies (mito:select-dao 'place-copies
                        (sxql:where (:= :place place)))))
    (mapcar #'mito:delete-dao place-copies)
    (mito:delete-dao place)))

(defun delete-objects (objlist)
  (mapcar #'delete-obj objlist))

;;
;; Move from place to place
;;
;; use current-place, new command "inside <place>" ?
(defun move (bk to &key (quantity 1) (from (current-place)))
  "Move a book from the actual place (the default one) to another one.
   If :from is specified, move from this place."
  (log:info from (mito:object-id from)
            to (mito:object-id to))
  (if (= (mito:object-id from) (mito:object-id to))
      (format t (utils:_ "No need to move this book from and to the same place (~a).~&")
              (name to))
      (when (remove-from from bk :quantity quantity)
        (add-to to bk :quantity quantity)
        (format t "Moved ~a cop~:@p of '~a' from ~a to ~a.~&"
                quantity (title bk) (name from) (name to)))))

;;
;; Some stats, observing the stock.
;;
(defun negative-quantities ()
  "Return a list of place-copies where the book's quantity is negative."
  (mito:select-dao 'place-copies
    (sxql:where (:< :quantity 0))))

;;
;; utils
;;
(defun erase-metaclass-from (class)
  "Needed to change the metaclass, e.g. add mito."
  ;; https://stackoverflow.com/questions/38811931/how-to-change-classs-metaclass
  (setf (find-class class) nil))

;;
;; useful type definitions
;;

(defun list-of-type-p (type list)
  (and (listp list)
       (every (a:rcurry #'typep type) list)))

(defun list-of-books-p (list)
  (list-of-type-p 'book list))

(deftype list-of-books ()
  `(satisfies list-of-books-p))

(defun book-hash-p (obj)
  (and (hash-table-p obj)
       (let ((keys (a:hash-table-keys obj)))
         (every (lambda (x) (member x keys)) '(:title :isbn)))))

(deftype book-hash ()
  `(satisfies book-hash-p))

(defun list-of-search-results-p (list)
  (list-of-type-p 'book-hash  list))

(deftype list-of-search-results ()
  `(satisfies book-hash-p))
