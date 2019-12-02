;;;;
;;;; Show the stock on a web page.
;;;; (in construction, currently not used)
;;;;
;;;; Go to localhost:4242/stock/
;;;;
;;;;

(defpackage bookshops-web
  (:use :cl
        :bookshops.models
        :hunchentoot
        :spinneret
        :parenscript
        :log4cl)
  (:import-from :easy-routes
                :routes-acceptor
                :defroute))

(in-package :bookshops-web)

(defmacro with-page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

(defun books-list (books)
  (with-page (:title "Books stock")
    (:header
     (:h1 "Books stock"))
    (:section
     (:table
      (:thead
       (:th "id")
       (:th "Title")
       (:th "in stock"))
      (:tbody
       (dolist (bk books)
         (:tr
          (:td (mito:object-id bk))
          (:td
           (:a :href (format nil "/book/~a" (mito:object-id bk))
               (title bk)))
          (:td (quantity bk)))))))
    (:footer ("footer"))))

(defun book-show (book)
  (with-page (:title (title book))
    (:header (:h1 (title book)))
    (:section
     (:div (:button :onclick (ps (alert "Not implemented")) "Command"))
     (:span
      (:td (mito:object-id book))
      (:td (title book)))
     (:div (authors book)))))


(defroute stock ("/stock/") ()
  (let ((books (find-book)))
    (books-list books)))

(defroute book-details ("/book/:id") ()
  (book-show (find-by :id id)))

(defvar *server* nil
  "Current instance of easy-acceptor.")

(defun start-app (&key (port 4242))
  (bookshops.models:connect)
  (setf *server* (make-instance 'routes-acceptor :port port))
  (start *server*))

(defun stop-app ()
  ;; disconnect db ?
  (stop *server*))
