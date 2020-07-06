;; XXX: all packages are not here.

(defpackage bookshops.models
  (:use :cl)

  (:import-from :access
                :access)

  (:export :connect
           :ensure-tables-exist
           :bootstrap-base-roles
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
           :place-copy-quantity
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
           :add-role
           :inherit-role
           :login
           :define-role-access

           ;; utils
           :print-quantity-red-green :negative-quantities
           :erase-metaclass-from)
  (:local-nicknames
   (#:parameters #:bookshops.parameters)
   (#:utils #:bookshops.utils)))

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
