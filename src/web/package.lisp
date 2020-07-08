(defpackage bookshops-web
  (:use :cl)
  (:import-from :easy-routes
                :defroute)
  (:local-nicknames (#:dilicom #:bookshops.datasources.dilicom)
                    (#:fr #:bookshops.datasources.scraper-fr)
                    (#:models #:bookshops.models)
                    (#:utils #:bookshops.utils)))

(in-package #:bookshops-web)
(djula:add-template-directory
 (asdf:system-relative-pathname "bookshops" "src/web/templates/"))
