;; XXX: all packages are not here.

(defpackage bookshops.models
  (:use :cl
        :mito
        :sxql
        :cl-ansi-text
        :log4cl
        ;; internal
        :bookshops.parameters
        :bookshops.utils)

  (:import-from :access
                :access)

  (:export :connect
           :ensure-tables-exist
           ;; book accessors
           :name
           :book :make-book
           :publisher
           :find-book :find-by :find-existing :find-book-noisbn :last-books
           :title :details-url :authors :cover-url :isbn :price
           :print-book :print-book-details
           :count-book
           ;; book methods
           :save-book :create-book
           :quantity :set-quantity
           :delete-books :delete-obj :delete-objects
           ;; places
           :place :place-copies
           :make-place :create-place :current-place :save-place :print-place
           :find-places :find-place-by
           :default-place
           :add-to :remove-from :move
           :*current-place*
           ;; utils
           :print-quantity-red-green :negative-quantities
           :erase-metaclass-from))

(defpackage bookshops
  (:use :cl
        :cl-ansi-text
        :parse-float
        :log4cl
        ;;
        :bookshops.utils)

  (:import-from :bookshops.models
                :default-place
                ;; accessors
                :publisher
                :title
                :price
                :authors
                :name
                ;; functions
                :make-book
                :find-existing)
  (:import-from :bookshops.datasources.scraper-fr
                :books)
  ;; libs
  (:import-from :access
                :access)
  (:export :main
           :i18n-load
           :init
           ;; book accessors
           :publisher
           :title
           :price
           :authors
           :name
           ;; functions
           :books)
  (:local-nicknames (#:dilicom #:bookshops.datasources.dilicom)
                    (#:fr #:bookshops.datasources.scraper-fr))
  (:documentation "CLI interface"))
