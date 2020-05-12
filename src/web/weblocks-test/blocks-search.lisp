;; (defpackage bookshops-weblocks
;;   (:use #:cl
;;         #:weblocks-ui/form
;;         #:weblocks/html)
;;   (:import-from #:weblocks/widget
;;                 #:render
;;                 #:update
;;                 #:defwidget)
;;   (:import-from #:weblocks/actions
;;                 #:make-js-action)
;;   (:import-from #:weblocks/app
;;                 #:defapp)
;;   (:import-from #:weblocks-navigation-widget
;;                 #:defroutes))

(in-package :bookshops-weblocks)

(defwidget search-widget (weblocks-ui:ui-widget)
    ((results
      :initarg :results
      :initform nil
      :accessor results)))

(defun make-search-widget ()
  (make-instance 'search-widget))

(defwidget search-result-widget (weblocks-ui:ui-widget)
  ((result
    :initarg :result
    :initform nil
    :accessor result)))

(defun data-search (search-widget query)
  (let* ((results (bookshops:books query))
         (results-widgets (loop for res in results
                             collect (make-instance 'search-result-widget :result res))))
    (setf (results search-widget) results-widgets)
    (update search-widget)))

(defmethod render ((widget search-widget))
  (with-html
    (:div :class "grid-container"
          (with-html-form (:POST (lambda (&key query &allow-other-keys)
                                   (data-search widget query)))
            (:div :class "cell medium-9"
                  (:input :type "text"
                          :name "query"
                          :placeholder "Search")
                  (:input :type "submit"
                          :class "button"
                          :value "Search"))

            (:div (loop for elt in (results widget)
                     do (with-html
                          (render elt))))))))

;TODO: doesn't save the result.
;function not called.
(defun save-book (result-widget)
  "Save this book in the stock, aka add one copy to the default place."
  (log:info "--- calling save-book with " result-widget)
  (let ((book (result result-widget)))
    (log:info "-- bookresult is" book)  ;; not seen.
    (bookshops.models:add-to (bookshops.models:default-place)
                             book)
    (update result-widget)))

(defmethod render ((widget search-result-widget))
  (let ((book (result widget)))
    (with-html
      (:div :class "grid-x"
            (:div :class "cell medium-6"
                  (:div :class "cell medium-6" (bookshops.models:title book))
                  (:div :class "cell medium-4" (bookshops.models:authors book)))
            (:div :class "cell medium-5"
                  (:div :class "cell medium-1" (bookshops.models:price book) "€")
                  (:div :class "cell medium-2" "x" (bookshops.models:quantity book)))
            (with-html-form (:POST (lambda (&key query &allow-other-keys)
                                     (declare (ignorable query))
                                     ;TODO:
                                     ;; function pas appellée
                                     ;; Quelle est l'URL du POST? retourne sur search/
                                     (save-book widget)))
              (:input :type "submit"
                      :class "button"
                      :title "Add 1 copy to your stock"
                      :value "Save"))))))

(defmacro defrun-test (name &body body)
  `(progn
    (defun ,name ()
       ,@body)
    (funcall #',name)))

(defrun-test hello-test
  (print "now testing…")
  (assert (= 1 1)))
