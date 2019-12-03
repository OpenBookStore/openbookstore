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
     (:div "Command copies:")
     (:form :action (format nil "/book/~a/command" (mito:object-id book))
            :method "POST"
            ;; reminder: spinneret accepts #ids and .classes notations.
            :id "commandForm"
            (:p (:button
                 :type "button"
                 :onclick (ps
                            (let ((elt (chain document (get-element-by-id "qty"))))
                              (incf (chain elt value))
                              (setf (chain document (get-element-by-id "show-qty") |innerText|)
                                    (chain elt value))
                              (chain console (log "new qty: "
                                                  (chain elt value)))
                              nil))
                 "Add 1"))
            (:p (:button
            (:span (:button
                 :type "button"
                 :onclick (ps
                            (let* ((elt (chain document (get-element-by-id "qty")))
                                   (qty (parse-int (@ elt value)))
                                   (nb (chain document (get-element-by-id "show-qty"))))
                              (decf (chain elt value))
                              (setf (@ nb inner-text)
                                    (chain elt value))
                              (chain console (log "type of qty" (typeof qty)))
                              (when (>= qty 79)
                                (chain console (log "when"))
                                (setf (@ nb style background-color)
                                      "red"))
                              (when (< qty 79)
                                (setf (@ nb style background-color)
                                      "white"))
                              (chain console (log "new qty: "
                                                  (chain elt value)))
                              nil))
                 "Remove 1"))

            (:span (:span "to command: ")
                   (:span :id "show-qty"))
            (:input :type "hidden" :name "qty" :id "qty" :value 1)
            (:p (:input :class "button" :type "submit" :value "Validate")))
     (:span
      (:td (mito:object-id book))
      (:td (title book)))
     (:div (authors book)))))


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
  (format t "~&--- command ~a with GET: do nothing~&" id))

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
