
(uiop:define-package bookshops.datasources.main
  (:use :cl
        :cl-ansi-text
        :parse-float
        :log4cl)
  (:export :search-books)
  (:documentation "Search for books by ISBN or keywords on the given datasource (French scraper, Argitinian scraper…)."))

(in-package :bookshops.datasources.main)

(defun search-books (query &key (datasource :fr))
  "Dispatch this search query to the appropriate `DATASOURCE'.

  Available sources:
  - :fr or any string starting with \"fr\" -> french scraper
  - :arg or any string -> argentinian scraper
  - :dilicom -> dilicom connector."
  (cond
    ((or (equal :fr datasource)
         (str:starts-with-p "fr" datasource))
     (bookshops.datasources.scraper-fr:books query))
    ((or (equal :dilicom datasource)
         (equal "dilicom" datasource))
     (bookshops.datasources.dilicom:search-books query))

    ((or (equal :ar datasource)
         (str:starts-with-p "ar" datasource))
     ;; We don't use the same mechanism as the french datasource:
     ;; get a scraper instance and call the generic BOOKS function.
     (funcall
      #'bookshops.datasources.base-scraper:books
      (bookshops.datasources.scraper-argentina:get-scraper) query))
    (t
     (warn "Unknown books datasource: ~S. Trying with the default one." datasource)
     (bookshops.datasources.scraper-fr:books query))))
