
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
  - :fr -> french scraper
  - :arg -> argentinian scraper"
  (case datasource
    (:fr
     (bookshops.datasources.scraper-fr:books query))
    (:ar
     ;; We don't use the same mechanism as the french datasource:
     ;; get a scraper instance and call the generic BOOKS function.
     (funcall
      #'bookshops.datasources.base-scraper:books
      (bookshops.datasources.scraper-argentina:get-scraper) query))
    (t
     (warn "Unknown books datasource: ~S. Trying with the default one." datasource)
     (bookshops.datasources.scraper-fr:books query))))
