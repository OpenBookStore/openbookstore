(in-package :bookshops.models)

(defun data2books (data)
  "Transform search results to a list of book objects."
  (loop for elt in data
     collect (make-book
              :title (str:sentence-case
                      (access elt :title))
              :isbn (access elt :isbn)
              :authors (str:title-case
                        (access elt :authors))
              :price (access elt :price)
              :details-url (access elt :details-url)
              :cover-url (access elt :cover-url)
              :publisher (str:title-case
                          (access elt :publisher))
              :date-publication (access elt :date-publication)
              :datasource (access elt :datasource)
              )))

(defun check-in-stock (data)
  "DATA is not a list of book objects (but currently a hash-table).
  Add an IN-STOCK field by looking up there ISBN."
  ;; 1 query to get the ones in stock.
  (setf data (alexandria:ensure-list data))
  (let* ((in-stock (mito:select-dao 'book
                     (sxql:where
                      (:in :isbn (print (remove-if #'null
                                                   (mapcar (lambda (it)
                                                             (access it :isbn))
                                                           data))))))))
    ;; Add :in-stock to matches only.
    (loop for book in (print in-stock)
       for elt = (find (isbn book) data
                       :key (lambda (it) (access it :isbn))
                       :test #'string-equal)
       ;; do (format t "book isbn: ~a, elt found: ~a~&, quantity in stock: ~a" (isbn book) elt (quantity book))
       do (setf (access elt :in-stock)
                (quantity book))
       do (setf (access elt :id)
                (mito:object-id book)))
    data))
