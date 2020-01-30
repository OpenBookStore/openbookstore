(defpackage bookshops-weblocks
  (:use #:cl
        #:weblocks-ui/form
        #:weblocks/html)
  (:import-from #:weblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:weblocks/actions
                #:make-js-action)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks-navigation-widget
                #:defroutes))

(in-package :bookshops-weblocks)

(defapp stock)

(weblocks/debug:on)

(defparameter *port* 8888)

(defwidget book-widget (weblocks-ui:widget)
    ((book
      :initarg :book
      :initform nil
      :accessor book)))

(defun make-book-widget (book)
  (make-instance 'book-widget :book book))

(defmethod render ((widget book-widget))
  (let ((book (book widget)))
    (with-html
      (:h4 (bookshops.models:title book))
      (:div (bookshops.models:authors book))
      (:div (bookshops.models:price book)))))

(defwidget book-list-widget ()
  ((books
    :initarg :books
    :initform nil
    :accessor books)))

(defmethod render ((widget book-list-widget))
  (with-html
    (loop for elt in (books widget)
       do (render elt))))

(defun make-book-list-widget (books)
  (let ((widgets (mapcar #'make-book-widget books)))
    (make-instance 'book-list-widget :books widgets)))

(defmethod weblocks/session:init ((app stock))
  (declare (ignorable app))
  (make-book-list-widget (bookshops.models:find-book)))

(defun start ()
  ;; (weblocks/server:start :port *port*))
  (weblocks/server:start))

#+nil
(weblocks/debug:reset-latest-session)
