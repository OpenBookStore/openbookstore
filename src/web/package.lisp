(defpackage bookshops-web
  (:use :cl
        :bookshops.models
        :hunchentoot
        :log4cl)
  (:import-from :easy-routes
                :routes-acceptor
                :defroute)
  (:import-from :bookshops.datasources.dilicom
                :search-books)
  (:local-nicknames (#:dilicom #:bookshops.datasources.dilicom)
                    (#:fr #:bookshops.datasources.scraper-fr)))
