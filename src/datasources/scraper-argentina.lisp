(uiop:define-package bookshops.datasources.scraper-argentina
  (:use :cl
        :cl-ansi-text
        :parse-float
        :log4cl)
  (:export :books
           :get-scraper)
  (:documentation "Search for books by ISBN or keywords. Return a list of hash-tables (dict). The exported function is BOOKS.

We currently get data by scraping the site https://cuspide.com.

Usage:

Create an instance of this scraper:

(defparameter *argentina-scraper* (make-instance 'scraper-argentina))

Call the BOOKS function with this scraper object:

(books *argentina-scraper* \"antigona\")
"))

;;TODO: extract ISBN
;; https://www.cuspide.com/resultados.aspx?c=antigona&por=pal

(in-package :bookshops.datasources.scraper-argentina)

(defparameter *scraper-argentina* nil
  "Current scraper.")

(defun get-scraper ()
  "Get or create a scraper instance for Argentina."
  (if *scraper-argentina*
      *scraper-argentina*
      (setf *scraper-argentina* (make-instance 'bookshops.datasources.base-scraper::scraper-argentina))))

(defun make-argentina-scraper ()
  (make-instance 'bookshops.datasources.base-scraper::scraper-argentina))

;;;
;;; Change package to define our scraper.
;;;
(in-package :bookshops.datasources.base-scraper)

(defclass scraper-argentina (base-scraper)
  ((datasource-name :initform "Cuspide.com - Argentina")
   (url-base :initarg :url-base
             :initform "https://www.cuspide.com"
             :accessor url-base
             :type string
             :documentation "The base URL of the target website, sans trailing /.
  Example: http://www.librairie-de-paris.fr")
   (url-search :initarg :url-search
               :initform "https://www.cuspide.com/resultados.aspx?c={QUERY}&por=pal"
               :accessor url-search
               :type string
               :documentation "The URL, with query placeholders, that runs a search on the distant website. Placeholders include:
  - {QUERY} : most of the times, must be replaced with '+' separated search terms. Example: 'lisp+book'")

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
             :initform ".md-datos h1 a"
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


(defun extract-price-as-cents (s)
  "Extract the price as an integer from the given string,
  for prices represented like 1.500,00 (Argentinian prices).

  Return: an integer, or NIL on error."
  (check-type s string)
  ;; thanks @mmontone on Gitter.
  (labels ((round-amount (amount &optional (divisor 1))
             (multiple-value-bind (quotient remainder) (truncate (/ amount divisor))
               (if (>= (abs remainder) 1/2)
                   (+ quotient (truncate (signum remainder)))
                   quotient)))

           (parse-amount (string &optional (decimals 2))
             "Parse an amount"
             (setf string (remove (code-char 160) string))
             (setf string (remove #\Space string))
             (setf string (substitute #\. #\, string))
             (let ((decs (or (position #\. (reverse string)) 0)))
               (round-amount (* (expt 10 decimals)
                                (/ (parse-integer (remove #\. string))
                                   (expt 10 decs))))))

           (extract-price (s)
             "Cleanup the string: from AR$ 1.500,00 remove \"AR$\""
             (ppcre:scan-to-strings "\\d+.?\\d+,?\\d*" s)))
    (handler-case
        ;; The regexp we use in the french scraper is NOT enough for this one,
        ;; the prices represented as 1.500,00 can NOT be parsed with the parse-float library.
        (parse-amount (extract-price s))
      (error (c)
        (log:warn "Could not parse price in ~S: ~a" s c)))))
#+test(extract-price-as-cents "22")
#+test(extract-price-as-cents "22.50")
#+test(extract-price-as-cents "22.5066")
#+test(extract-price-as-cents "1.500,00")
#+test(extract-price-as-cents "2.500,90")
#+test(extract-price-as-cents "1.000,00")
#+test
(assert (extract-price-as-cents "AR$ 1.000,00"))

#|
May be useful:

(defun decimal (number &optional (decimals 2))
  (* number (expt 10 decimals)))
#+test(decimal 22)

(defun format-amount (amount &optional (decimals 2))
  "Format an amount"
  (multiple-value-bind (basic cents) (truncate amount (expt 10 decimals))
    (format nil "~a,~a" basic (abs cents))))

#+test(format-amount 3300)
#+test(format-amount 3345)
#+test(format-amount 99999)
|#

;; Title: OK, enough with CSS selector.
;; (defmethod parse-title (scraper node))

(defmethod parse-authors ((scraper scraper-argentina) node)
  (with-log-error (:authors)
    ;; slot-value is overriden by lquery, can't use it inside $
    (let ((css (slot-value scraper 'css-author)))
      (str:trim
       (lquery:$ node css (elt0) (text))))))

(defmethod parse-price ((scraper scraper-argentina) node)
  "Extract the price. `node': plump node.
  Return: integer, the price as cents (real price multiplied by 100)."
  (extract-price-as-cents (node-selector-to-text (slot-value scraper 'css-price) node)))

;; (defmethod parse-publication-date (scraper node)
;;   (with-log-error (:publication-date)
;;     (ignore-errors
;;       ;; ;TODO:
;;       (node-selector-to-text (slot-value scraper 'css-publication-date) node))))

(defmethod parse-cover-url ((scraper scraper-argentina) node)
  (with-log-error (:cover)
    (let ((css (slot-value scraper 'css-cover-url)))
      (lquery:$ node css (elt0) (attr "src")))))

;; OK
;; (defmethod parse-details-url (scraper node))

(defun probably-isbn (s)
  (when s
    (and (str:starts-with-p "9" s)
         (member (length s) '(10 13)))))
#+(or)
(progn
  (assert (probably-isbn "9782806284082"))
  (not (probably-isbn "Libro")))

(defmethod parse-isbn (scraper node)
  "Extract the ISBN from the search results.
  It is given in the HREF of the title."
  (with-log-error (:isbn)
    (let ((css (slot-value scraper 'css-isbn)))
      ;; We get "/Libro/9788415510529/Antigona"
      ;; Extract the ISBN, and be a bit lax on its position, just in case.
      (find-if #'probably-isbn
               (str:split "/"
                          (lquery:$ node css (elt0) (attr "href")))))))
