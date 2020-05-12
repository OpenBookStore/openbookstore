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
        :log4cl)
  (:import-from :easy-routes
                :routes-acceptor
                :defroute))

(in-package :bookshops-web)


(defroute stock ("/stock/") ()
  (let ((books (find-book)))
    (books-list books)))

(defroute book-details ("/book/:id") ()
  (book-show (find-by :id id)))

(defroute command-book-route ("/book/:id/command" :method :post)
    (&post qty)
  (format t "~&--- command for id ~a, qty: ~a~&" id qty)
  (format t "~&hunchentoot post params: ~a~&" (hunchentoot:post-parameters hunchentoot:*request*))
  (with-html-string
    (:div (format nil "qty: ~a" qty))))

(defroute command-book-route ("/book/:id/command" :method :get) ()
  (format t "~&--- command ~a with GET: do nothing~&" id)
  (format nil (books-table-body (find-book))))

(defroute api-stock ("/api/stock") ()
  "Return a list of books as html to replace the table's body."
  (format nil (books-table-body (last-books))))

(defroute testroute ("/test") (x y)
  (format t "~&---- TEST x: ~a, y: ~a~&" x y)
  (with-html-string
    (:div (format nil "x: ~a, y: ~a" x y))))

(defroute testroute ("/test" :method :post) (x y)
  (format t "~&---- POST test x: ~a, y: ~a~&" x y)
  (with-html-string
    (:div (format nil "x: ~a, y: ~a" x y))))

(defvar *server* nil
  "Current instance of easy-acceptor.")

(defun start-app (&key (port 4242))
  (bookshops.models:connect)
  (setf *server* (make-instance 'routes-acceptor :port port))
  (start *server*))

(defun stop-app ()
  ;; disconnect db ?
  (stop *server*))
