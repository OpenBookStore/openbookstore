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

(defapp stock :prefix "/")

(weblocks/debug:on)

(defparameter *port* 8888)

(defroutes main-routes
  ("/stock/" (make-book-list-widget (openbookstore.models:find-book)))
  ("/search/" (make-search-widget))
  ("/" (weblocks/response:redirect "/stock/")))

(defwidget book-widget (weblocks-ui:ui-widget)
    ((book
      :initarg :book
      :initform nil
      :accessor book)))

(defun make-book-widget (book)
  (make-instance 'book-widget :book book))

(defun add-book (book-widget)
  "Add one copy to the default place."
  (let ((book (book book-widget)))
    (openbookstore.models:add-to (openbookstore.models:default-place)
                             book)
    (update book-widget)))

;; (defmethod render ((widget book-widget))
;;   (log:debug "-- render book-widget")
;;   (let ((book (book widget)))
;;     (with-html
;;       (:td (openbookstore.models:title book))
;;       (:td (openbookstore.models:authors book))
;;       (:td (openbookstore.models:price book) "€")
;;       (:td (format nil "x ~a" (openbookstore.models:quantity book)))
      ;; (:td (with-html-form (:POST (lambda (&key &allow-other-keys)
      ;;                               (add-book widget)))
      ;;        (:input :type "submit"
      ;;                :title "Add 1 copy to your stock"
      ;;                :value "+ 1"))))))

(defmethod render ((widget book-widget))
  (let ((book (book widget)))
    (with-html
      (:div :class "grid-x"
            (:div :class "cell medium-6"
                  (:div :class "cell medium-6" (openbookstore.models:title book))
                  (:div :class "cell medium-4" (openbookstore.models:authors book)))
            (:div :class "cell medium-5"
                  (:div :class "cell medium-1" (openbookstore.models:price book) "€")
                  (:div :class "cell medium-2" "x" (openbookstore.models:quantity book)))
            (with-html-form (:POST (lambda (&key &allow-other-keys)
                                     (add-book widget)))
              (:input :type "submit"
                      :class "button"
                      :title "Add 1 copy to your stock"
                      :value "+ 1"))))))

(defwidget book-list-widget ()
  ((books
    :initarg :books
    :initform nil
    :accessor books)))

(defun stock-search (list-widget query)
  (let* ((results (openbookstore.models:find-book :query query))
         (book-widgets (mapcar #'make-book-widget results)))
    (setf (books list-widget) book-widgets)
    (update list-widget)))

(defmethod render ((widget book-list-widget))
  (with-html
    (with-html-form (:POST (lambda (&key query &allow-other-keys)
                             (stock-search widget query)))
      (:div :class "cell medium-9"
            (:input :type "text"
                    :name "query"
                    :placeholder "search title")
            (:input :type "submit"
                    :class "button"
                    :value "Search"))

      (:div :class "grid-container"
            (loop for elt in (books widget)
               do (with-html
                    (render elt)))))))

(defun make-book-list-widget (books)
  (let ((widgets (mapcar #'make-book-widget books)))
    (make-instance 'book-list-widget :books widgets)))

(defmethod weblocks/session:init ((app stock))
  (declare (ignorable app))
  (make-main-routes))

(defun start ()
  ;; (weblocks/server:start :port *port*))
  ;(setf weblocks/default-init::*welcome-screen-enabled* nil)
  (unless mito::*connection*
    (openbookstore.models:connect))
  (restart-case
      (weblocks/server:start :port *port*)
    (connect-to-db ()
      :report "Connect to the DB"
      (openbookstore.models:connect))))


#+nil
(weblocks/debug:reset-latest-session)
