;; XXX: all packages are not here.

(defpackage bookshops.models
  (:use :cl)

  (:import-from :access
                :access)

  (:export :connect
           :add-to
           :ensure-tables-exist
           :bootstrap-base-roles
           ;; book accessors
           :name
           :book :make-book
           :publisher
           :find-book :find-by :find-existing :find-book-noisbn :last-books
           :title :details-url :authors :cover-url :isbn :price
           :print-book :print-book-details
           :print-obj
           :count-book
           :date-publication
           ;; book methods
           :save-book :create-book
           :quantity :set-quantity
           :delete-books :delete-obj :delete-objects

           ;; places
           :place :place-copies :place-copies-book
           :make-place :create-place :current-place :save-place :print-place
           :find-places :find-place-by
           :default-place
           :add-to :remove-from :move
           :*current-place*
           :place-copy-quantity

           ;; lists, baskets
           :find-baskets
           :print-basket

           ;; authentication
           :user
           :user-name
           :role
           :user-role-user
           :user-role-role
           :role-copy
           :primary-role
           :inherited-role

           :create-user
           :create-role
           :create-contact
           :find-contacts :find-contact-by :print-contact
           :add-role
           :inherit-role
           :login
           :define-role-access

           ;; utils
           :print-quantity-red-green :negative-quantities
   :erase-metaclass-from
           :make-sale
           :list-of-books)
  (:local-nicknames
   (#:parameters #:bookshops.parameters)
   (#:utils #:bookshops.utils)
   (#:a #:alexandria)))

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
                    (#:fr #:bookshops.datasources.scraper-fr)
                    (#:a #:alexandria))
  (:documentation "CLI interface"))

(defpackage bookshops-web
  (:use :cl)
  (:import-from :easy-routes
                :defroute)
  (:import-from :access
                :access)
  (:local-nicknames (#:dilicom #:bookshops.datasources.dilicom)
                    (#:fr #:bookshops.datasources.scraper-fr)
                    (#:models #:bookshops.models)
                    (#:utils #:bookshops.utils)
                    (#:a #:alexandria)))
