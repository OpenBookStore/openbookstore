(defpackage bookshops.datasources.dilicom
  (:use :cl)
  (:export :search-books
           :available-p)
  (:documentation "Search books by ISBN (and ISBN only, no free search) with the \"FEL à la demande\" by Dilicom, the professional provider."))

(in-package :bookshops.datasources.dilicom)

#|

SOAP request to the Dilicom web service (FEL à la demande).

We can query ISBNs and we can't do a free search by keyword.

The gist of it is:

- a POST request with the right header, user and password
- XML processing with lQuery to extract the books information.
- return a hash-table with keys being title, authors, price, isbn, etc.

TODO: password and user in config.lisp

|#

(defvar *user* nil
  "Dilicom user GSN (numbers)")

(defvar *password* nil
  "Dilicom user's password.")

(defun available-p ()
  "Is Dilicom configured and likely to be available?"
  (and *user* *password*))

;XXX: merged upstream in str.
(defun replace-pairs (pairs str)
  "Replace all associations in pairs (plist) and return a new string.

  Example:
  (replace-pairs (list \"{{phone}}\" \"987\") \"call {{phone}}\")
  =>
  \"call 987\""
  (assert (consp pairs))
  (dotimes (i (- (length pairs)
                 1))
    (setf str (str:replace-all (nth i pairs) (nth (incf i) pairs) str)))
  str)

(defun create-soap-content (isbns)
  (assert (and *user* *password*))
  (when isbns
    (let ((ean13s (with-output-to-string (s)
                    (loop for isbn in isbns
                       do (format s "<ean13s>~a</ean13s>" isbn)))))
      (replace-pairs (list "USER" *user* "PASSWORD" *password*
                           "EAN13S" ean13s)
                     ;; beware, there must be no space around the placeholders,
                     ;; or the SOAP service fails.
                     "<?xml version='1.0' encoding='utf-8'?>
<soap-env:Envelope xmlns:soap-env=\"http://schemas.xmlsoap.org/soap/envelope/\"><soap-env:Body><ns0:demandeFicheProduit xmlns:ns0=\"http://fel.ws.accelya.com/\"><demandeur>USER</demandeur><motDePasse>PASSWORD</motDePasse>EAN13S<multiple>false</multiple></ns0:demandeFicheProduit></soap-env:Body></soap-env:Envelope>"))))

(defun soap-request (isbns)
  "Create the SOAP request body from a list of ISBNs (strings) and send the POST request to Dilicom's WSDL server."
  (dexador:post "http://websfel.centprod.com/v2/DemandeFicheProduit"
                :headers '(("Content-Type" . "text/xml; charset=utf-8"))
                :content (create-soap-content isbns)))

(defun query (q)
  (lquery:$ (initialize (soap-request q))
            "elemReponse"))

(defun print-hash-entry (key value)
  (format t "~S: ~S~%" key value))

(defun print-book (ht)
  (log:info "~&Book:~&")
  (maphash #'print-hash-entry ht))

(lquery:define-lquery-list-function elt0 (vector)
  (elt vector 0))

(defun read-price (s)
  "From a string return a float."
  (handler-case (/ (parse-integer s)
                   1000.0)
    (error (c)
      (log:warn "Error while decoding dilicom price: ~a" c))))
#+nil
(assert (= 16.50 (read-price "0016500")))

(defun search-books (isbns &key debug)
  (loop for elt across (query isbns)
     for book = (make-hash-table)
     do (setf (gethash :datasource book)
              "dilicom")
     do (setf (gethash :details-url book)
              nil)
     do  (setf (gethash :title book)
               (str:capitalize
                (lquery:$ elt "libstd" (text) (elt0))))
     do (setf (gethash :authors book)
              (str:title-case
               (lquery:$ elt "auteur" (text) (elt0))))
     do (setf (gethash :isbn book)
              (lquery:$ elt "ean13" (text) (elt0)))
     do (setf (gethash :publisher book)
              (str:title-case
               (lquery:$ elt "edit" (text) (elt0))))
     do (setf (gethash :distributor book)
              (lquery:$ elt "gcddistrib" (text) (elt0)))
     do (setf (gethash :date-publication book)
              (lquery:$ elt "dtparu" (text) (elt0)))
     ;; theme: currently unused
     do (setf (gethash :theme book)
              (lquery:$ elt "theme" (text) (elt0)))
     ;; Price
     do (setf (gethash :price book)
              (read-price
               (lquery:$ elt "prix" (text) (elt0))))
     ;; availability: currently unused.
     do (setf (gethash :availability book)
              (lquery:$ elt "codedispo" (text) (elt0)))

     do (when debug (print-book book))
     collect book))

;; and voilà.
