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
  "DATA is not a list book objects (but currently a hash-table).
  Add an IN-STOCK field by looking up there ISBN."
  ;; 1 query to get the ones in stock.
  (let* ((in-stock (select-dao 'book
                     (where (:in :isbn (print (remove-if #'null
                                                         (mapcar (lambda (it)
                                                                   (access it :isbn))
                                                                 data))))))))
    ;; Add :in-stock to matches only.
    (loop for book in (print in-stock)
       for elt = (find (isbn book) data
                       :key (lambda (it) (access it :isbn))
                       :test #'string-equal)
       ;; do (format t "book isbn: ~a, elt found: ~a~&" (isbn book) elt)
       do (setf (access elt :in-stock)
                (quantity book))
       ;; TODO does it make sense to put a reference to the books in here?
       ;; This is done so the search page can refer to books by id rather than ISBN
       ;; but should the page even be doing that?
       do (setf (access elt :book)
                book))
    data))
