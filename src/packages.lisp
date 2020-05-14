(defpackage bookshops.models
  (:use :cl
        :mito
        :sxql
        :cl-ansi-text
        :log4cl
        ;; internal
        :bookshops.parameters
        :bookshops.utils)

  (:export :connect
           :ensure-tables-exist
           ;; book accessors
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
