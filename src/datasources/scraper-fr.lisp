(defpackage bookshops.datasources.scraper-fr
  (:use :cl
        :cl-ansi-text
        :parse-float
        :log4cl)
  (:export :books)
  (:documentation "Search for books by ISBN or keywords on a french website. Return a list of hash-tables. Don't create Book objects from bookshops.models here."))

(in-package :bookshops.datasources.scraper-fr)

;; This scraper does not use the base-scraper class and methods (which was created later).

;; Print hash-tables readably (used for debug logs).
(serapeum:toggle-pretty-print-hash-table t)

(defparameter *french-search* "http://www.librairie-de-paris.fr/listeliv.php?MOTS={QUERY}&SUPPORT=&RECHERCHE=simple&TRI=&DISPOCHE=&RAYONS=&LIVREANCIEN=2&CSR="
  "French source of books. The {query} string will be replaced by the list
  of '+' separated search keywords.")

(defparameter *url-base* "http://www.librairie-de-paris.fr"
  ;; no trailing / for now plz.
  "Base url.")

(defparameter *datasource* *french-search*
  "The default data source.")

(defvar *last-results* nil
  "List of last results by `books` (objects).")

(defparameter *debug* nil "xxx: just use (log:debug)")

;; Conditions.
(define-condition parse-warning (simple-warning)
  ((data :initarg :data
         :initform nil
         :accessor parse-warning-data
         :documentation "The element we are trying to parse (ex: the price).")
   (message :initarg :message
            :initform ""
            :accessor parse-warning-message))
  (:report (lambda (c stream)
             (format stream "scraper warning: ~a" (parse-warning-message c)))))

(define-condition price-parse-warning (parse-warning)
  ()
  (:report (lambda (c stream)
             (declare (ignorable c))
             (format stream "scraper warning"))))

;; bad: copied from utils.lisp.
(defun extract-float (s)
  "Extract a double float from the given string.

  Use a double float to delay floating point errors.
  A simple (* 100 9.90), to turn a price into an integer, is not 990.
  Using a double is OK.

  Return: a double float, or NIL on error."
  (check-type s string)
  (ignore-errors
    ;; The regexp should be enough for this scraper, we parse a known HTML.
    ;; It will need to be adapted for other scrapers.
    ;; parse-float has :junk-allowed t, but a regexp is a bit more solid.
    ;; Example:
    ;; (parse-float:parse-float "price 9.90 euros" :junk-allowed t)
    ;; => NIL
    (parse-float (ppcre:scan-to-strings "-?\\d+.?\\d*" s)
                 :type 'double-float)))
#+(or)
(progn
  (assert (= 9.90d0 (extract-float "9.90 €")))
  (assert (= 990 (* 100 9.90d0))))      ;; try 9.90: rounding error.

(defun get-url (url)
  "Http get this url.
   Function mocked in unit tests."
  (log:info url)
  (dex:get url))

(defun parse (request)
  "Parse with plump, return a plump node.
   Function mocked in unit test."
  (plump:parse request))

(defun select (selector parsed)
  "Find nodes with CSS selector from a plump-parsed node."
  (lquery:$ selector parsed))

(lquery:define-lquery-list-function elt0 (vector)
  (elt vector 0))

(defun node-selector-to-text (selector node &key selector2)
  " Take a CSS selector (str), a plump node, extract and clean the result."
  (declare (ignorable selector2))
  (let* ((nodes (clss:select selector node))
         res
         txt)
    (setf nodes (coerce nodes 'list))
    (when (not (null nodes))
      (setf res (first nodes))
      (setf txt (plump:text res))
      (str:trim txt))))

(defmacro with-log-error ((name) &body body)
  `(handler-case
       (progn
         ,@body)
     (error (c) (format *error-output* "could not parse ~a: ~a." ,name c))))

(defun parse-title (node)
  (with-log-error (:title)
    (node-selector-to-text  ".livre_titre" node)))

(defun parse-authors (node)
  (with-log-error (:authors)
    (lquery:$ node ".livre_auteur a"
              (attr :title)
              (elt0))))

(defun ensure-integer (number)
  "Return this number as an integer (TRUNCATE and discard decimals).
  If it isn't a number, return 0.

  Typically, for a price that is parsed as a float, NUMBER should be
  the price in cents (x 100), and we return it as an integer.

  - number: float

  Return: an integer or 0."
  ;; warn: copied to model-utils.lisp. The scraper module should stay completely independant of
  ;; the application code.
  (if (and number
           (not (equalp number 0))
           (numberp number))
      (truncate number)
      (progn
        ;; log or warning? Both!
        (log:warn "Could not parse ~s to an integer price." number)
        (signal (make-condition 'parse-warning
                                :message (format nil "fr scraper: could not transform ~S to an integer. Returning 0." number)
                                :data number))
        0)))

;; usage:
#+(or)
(progn
  (assert (= 1495 (ensure-integer (* 100 14.95))))
  (handler-case
      (assert (= 0 (ensure-integer "foo")))
    (warning (c)
      (princ c)
      (print (parse-warning-data c)))))

(defun parse-price (node)
  "Extract the price. `node': plump node."
  (let ((price (extract-float (node-selector-to-text ".item_prix" node))))
    (let ((res (ensure-integer (* 100 price))))
      (log:info "Price ~s is parsed as: ~s~&" price res)
      res)
    ))

(defun parse-publisher (node)
  (with-log-error (:publisher)
    (node-selector-to-text ".editeur" node)))

(defun parse-isbn (node)
  (str:trim (first (last (str:lines
                          (node-selector-to-text ".editeur-collection-parution" node))))))

(defun parse-publication-date (node)
  (with-log-error (:publication-date)
    (node-selector-to-text ".MiseEnLigne" node)))

(defun parse-cover-url (node)
  (with-log-error (:cover)
    (lquery:$ node ".zone_image img"
              (attr "data-original")
              (elt0))))

(defun parse-details-url (node)
  "Extract the url to the book online information.
  https://www.librairie-de-paris.fr/livre/9782742720682-antigone-henry-bauchau/"
  (with-log-error (:details-url)
    (str:concat *url-base*
                (lquery:$ node ".livre_titre a" (attr :href) (elt0)))))

(defun make-book (&key title isbn datasource cover-url authors details-url
                    price publisher date-publication)
  "Create a hash-table with these slots.
  Don't rely on the models in the data scrapers.

  tip: access the fields easily with the access library."
  (serapeum:dict :title title
            :isbn isbn
            :datasource datasource
            :cover-url cover-url
            :authors authors
            :details-url details-url
            :price price
            :publisher publisher
            :date-publication date-publication))

(defun book-info (node)
  "Takes a plump node and returns a list of book objects with: title, authors, price, publisher, date of publication, etc."
  (let ((title (parse-title node))
        (authors (parse-authors node))
        (price (parse-price node))
        (publisher  (parse-publisher node))
        (date-publication (parse-publication-date node))
        (isbn (parse-isbn node))
        (cover-url (parse-cover-url node))
        (details-url (parse-details-url node))
        bk)

    ;; Every field of this dict must be present in search.html
    ;; for the add-or-create and quick-add-stock forms, where we save
    ;; the book to our DB.
    (setf bk (make-book :title title
                        :isbn isbn
                        :datasource "fr:librairiedeparis"
                        :cover-url cover-url
                        :authors authors
                        :details-url details-url
                        :price price
                        :publisher publisher
                        :date-publication date-publication))
    bk))

(defun build-url (query &key (source *datasource*) (encode t))
  "Build the search url with the query terms in it.
  Encode the search terms (if `:encode' is true, the default).

  - query: a str (possibly many words).

  Return the url (a str).
  "
  (let* ((words (str:words query))
         (joined (str:join "+" words))
         (encoded? (if encode
                       (quri:url-encode joined)
                       joined)))
    (str:replace-all "{QUERY}" encoded? source)))

(defparameter *last-parsing-res* nil "for debug pursposes.")

(defun books (query)
  "From a search query (str), return a list of book objects (with a title, a price, a date-publication, authors,...)."
  (let* ((url (build-url query))
         (req (get-url url))
         (parsed (parse req))
         ;; one node
         ;: XXX: clss can be replaced by lQuery.
         (node (clss:select ".resultsList" parsed))
         ;; direct children:
         (res (clss:select "> li" node)))
    (setf *last-parsing-res* (coerce res 'list))
    (setf *last-results* (map 'list #'book-info res))))
