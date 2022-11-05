(defpackage bookshops.datasources.base-scraper
  (:use :cl
        :cl-ansi-text
        :parse-float
        :log4cl)
  (:import-from :serapeum
                :dict)
  (:import-from :access
                :access)
  (:export :books)
  (:documentation "Search for books by ISBN or keywords on a website. Return a list of hash-tables. Don't create Book objects from bookshops.models here."))

(in-package :bookshops.datasources.base-scraper)

;; For Argentina:
;;
;; - cuspide.com: looks complete enough. Doesn't show the ISBN inline in the search results, but it appears in the book's details URL, we can extract it.
;;   - doesn't show the publisher in search results, it appears on the book's page.
;;
;; - https://www.libreriadonquijote.com.ar : no ISBN.
;;
;; - https://mercadolibre.com/

;; Print hash-tables readably (used for debug logs).
(serapeum:toggle-pretty-print-hash-table t)

(defclass base-scraper ()
  ((datasource-name :initarg :datasource-name
                    :initform ""
                    :documentation "The human-readable name of this data source.")

   (url-base :initarg :url-base
             :initform ""
             :accessor url-base
             :type string
             :documentation "The base URL of the target website, sans trailing /.
  Example: http://www.librairie-de-paris.fr")
   (url-search :initarg :url-search
               :initform ""
               :accessor url-search
               :type string
               :documentation "The URL, with query placeholders, that runs a search on the distant website. Placeholders include:
  - {QUERY} : most of the times, must be replaced with '+' separated search terms. Example: 'lisp+book'

  Example: http://www.librairie-de-paris.fr/listeliv.php?MOTS={QUERY}&SUPPORT=&RECHERCHE=simple&TRI=&DISPOCHE=&RAYONS=&LIVREANCIEN=2&CSR=")
   (%last-results :initform (list)
                  :type list
                  :documentation "For dev purposes. Contains the results of the last search.")

   ;; Now some internal data, CSS selectors to parse the results.
   (css-container :initform "#ctl00_ContentPlaceHolder1_cotenedorLibros"
                  :initarg :css-container
                  :documentation "A CSS selector that wraps the books. Must return 1 node (or 0).")
   (css-elements :initarg :css-elements
                 :initform ".libro"
                 :documentation "A CSS selector that finds all book elements, once we have the node from css-container.")

   ;; Now find all possible book data:
   (css-title :initarg :css-title
              :initform ".md-datos h1"
              :documentation "Inside each element found by css-card, a CSS selector to find the title.")
   (css-author :initarg :css-author
               :initform ".autor a"
               :documentation "CSS selector to find the author(s) inside each card found by css-card.")
   (css-price :initarg :css-price
              :initform ".precio")

   (css-isbn :initarg :css-isbn
             :initform ".isbn"
             :documentation "Important field, it is required.")

   (css-cover-url :initarg :css-cover-url
                  :initform "figure img[src]"
                  :documentation "CSS selector to find the book's cover URL. Sometimes it is full URL, sometimes it is a relative URL, to concatenate to the url-base.")

   (css-details-url :initarg :css-details-url
                  :initform "figure a[href]"
                  :documentation "CSS selector to find the book's details page URL. Often, it is a relative result so we have to concatenate url-base to this result.")

   ;; TODO: editorial, publication-date

   (css-currency :initarg :css-currency
                 :initform ".precio span"
                 :documentation "Optional. Set the default currency for this scraper with CURRENCY.")
   (currency-symbol :initarg :currency-symbol
                    :initform "AR$"
                    :documentation "The default currency symbol. Example: AR$ 1.500")
   ))

(defmethod print-object ((obj base-scraper) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((url-base url-base))
            obj
          (format stream "Base scraper for ~a" url-base))))

(defun make-base-scraper (name &key url-base url-search)
  (make-instance 'base-scraper
                 :datasource-name name
                 :url-search url-search
                 :url-base url-base))

;;;
;;; Conditions.
;;;
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

(defun get-url (url)
  "Http GET this url.
   Function mocked in unit tests."
  (log:info url)
  (dex:get url))

(function-cache:defcached get-url-with-cache (url)
  "Http GET this url.
  Cache results"
  (log:info url)
  (dex:get url))

;; (defun parse (request)
;;   "Parse with plump, return a plump node.
;;    Function mocked in unit test."
;;   (plump:parse request))

(defmethod parse ((scraper base-scraper) request)
  "Parse with Plump."
  (declare (ignorable scraper))
  (plump:parse request))

(defun select (selector parsed)
  "Find nodes with CSS selector from a plump-parsed node."
  (lquery:$ selector parsed))

(lquery:define-lquery-list-function elt0 (vector)
  (elt vector 0))

(defun node-selector-to-text (selector node &key selector2)
  "Take a CSS selector (str), a plump node, extract and clean the result."
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
     (error (c)
       (format *error-output* "could not parse ~a: ~a." ,name c)
       nil)))

(defmethod parse-title (scraper node)
  (with-log-error (:title)
    (node-selector-to-text (slot-value scraper 'css-title) node)))

(defmethod parse-authors (scraper node)
  (with-log-error (:authors)
    ;; slot-value is overriden by lquery, can't use it inside $
    (let ((css (slot-value scraper 'css-author)))
      (str:trim
       (lquery:$ node css (elt0) (text))))))

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


(defmethod parse-price (scraper node)
  "Extract the price. `node': plump node.
  Return: integer, the price as cents (real price multiplied by 100)."
  (uiop:not-implemented-error "parse-price: prices must be returned as integers (cents).
Take care of that function for each parser.

See existing scrapers for reference and how to handle different price representations."))

(defmethod parse-publisher (scraper node)
  (with-log-error (:publisher)
    (ignore-errors
      ;;TODO:
      (let ((css (slot-value scraper 'css-publisher)))
        (node-selector-to-text css node)))))

(defmethod parse-isbn (scraper node)
  (str:trim (first (last (str:lines
                          (node-selector-to-text (slot-value scraper 'css-isbn) node))))))

(defmethod parse-publication-date (scraper node)
  (with-log-error (:publication-date)
    (ignore-errors
      ;; ;TODO:
      (node-selector-to-text (slot-value scraper 'css-publication-date) node))))

(defmethod parse-cover-url (scraper node)
  (with-log-error (:cover)
    (let ((css (slot-value scraper 'css-cover-url)))
      ;; use (elt0) ?
      (lquery:$ node css (elt0) (attr "src")))))

(defmethod parse-details-url (scraper node)
  "Extract the url to the book online information.
  https://www.librairie-de-paris.fr/livre/9782742720682-antigone-henry-bauchau/"
  (with-log-error (:details-url)
    (let ((css (slot-value scraper 'css-details-url)))
      ;; Beware we don't have a double // in the URL.
      (str:concat (url-base scraper)
                  (lquery:$ node css (attr :href) (elt0))))))

(defun make-book (&key title isbn datasource cover-url authors details-url
                    price publisher date-publication)
  "Create a hash-table with these slots.
  Don't rely on the models in the data scrapers.

  tip: access the fields easily with the access library."
  (dict :title title
        :isbn isbn
        :datasource datasource
        :cover-url cover-url
        :authors authors
        :details-url details-url
        :price price
        :publisher publisher
        :date-publication date-publication))

(defmethod book-info (scraper node)
  "Takes a plump node and returns a list of book objects with: title, authors, price, publisher, date of publication, etc."
  (let ((title (parse-title scraper node))
        (authors (parse-authors scraper node))
        (price (parse-price scraper node))
        (publisher  (parse-publisher scraper node))
        (date-publication (parse-publication-date scraper node))
        (isbn (parse-isbn scraper node))
        (cover-url (parse-cover-url scraper node))
        (details-url (parse-details-url scraper node))
        bk)

    ;; Every field of this dict must be present in search.html
    ;; for the add-or-create and quick-add-stock forms, where we save
    ;; the book to our DB.
    (setf bk (make-book :title title
                        :isbn isbn
                        :datasource (slot-value scraper 'datasource-name)
                        :cover-url cover-url
                        :authors authors
                        :details-url details-url
                        :price price
                        :publisher publisher
                        :date-publication date-publication))
    bk))

(defmethod build-url (scraper query &key (encode t))
  "Build the search url with the query terms in it.
  URL-encode the search terms (if `:encode' is true, the default).

  - query: a str (possibly many words, they will be '+'-separated.).

  Return the URL (a string)."
  (let* ((words (str:words query))
         (joined (str:join "+" words))
         (encoded? (if encode
                       (quri:url-encode joined)
                       joined)))
    (str:replace-all "{QUERY}" encoded? (url-search scraper))))

(defparameter *devel-mode* nil "Set to T to be in devel mode. What it does:
 - cache the HTTP requests.

See: toggle-dev-mode")

(defun toggle-dev-mode (&optional (state nil state-p))
  (if state-p
      (setf *devel-mode* state)
      (setf *devel-mode* (not *devel-mode*))))

(defparameter *last-parsing-res* nil "for debug pursposes.")
(defparameter *last-results* nil "for debug pursposes.")

(defmethod books-container ((scraper base-scraper) root)
  (clss:select (slot-value scraper 'css-container) root))

(defmethod books-elements ((scraper base-scraper) root)
  "From this Plump root DOM object, return a vector of DOM elements that contain a book."
  (check-type root plump-dom:root)
  (clss:select (slot-value scraper 'css-elements) root))

(defmethod books (scraper query)
  "From a search query (str), return a list of book objects (with a title, a price, a date-publication, authors,...)."
  (let* ((url (build-url scraper query))
         (get-function (if *devel-mode* #'get-url-with-cache #'get-url))
         (req (funcall get-function url))
         (parsed (parse scraper req))
         ;; one node
         ;; XXX: clss can be replaced by lQuery.
         (node (clss:select (slot-value scraper 'css-container) parsed))
         ;; direct children:
         (res (clss:select (slot-value scraper 'css-elements) node)))
    (setf *last-parsing-res* (coerce res 'list))
    (setf *last-results* (map 'list
                              (lambda (it) (book-info scraper it))
                              res))))
