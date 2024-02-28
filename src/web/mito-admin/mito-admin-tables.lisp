
(in-package :openbookstore.models)

;; We have Mito *tables*
;; (BOOK PLACE PLACE-COPIES CONTACT CONTACT-COPIES BASKET BASKET-COPIES USER ROLE USER-ROLE ROLE-COPY SELL SOLD-CARDS SHELF PAYMENT-METHOD)

(djula:add-template-directory
 (asdf:system-relative-pathname "openbookstore" "src/web/"))

(defparameter *admin-index* (djula:compile-template* "mito-admin/templates/index.html"))
(defparameter *admin-table* (djula:compile-template* "mito-admin/templates/table.html"))

;;; We might want a admin-page class and instances, to set parameters:
;;; - show the search input on the table view?
;;; - action buttons (to do)
;;; - more settings.

(defgeneric tables ()
  (:method ()
    *tables*))

(defgeneric render-index ()
  (:method ()
    (djula:render-template* *admin-index* nil
                            :tables (tables))))

(defvar *pagination-template*)

(defun pagination-template ()
  (unless (boundp '*pagination-template*)
    (setf *pagination-template*
          (asdf:system-relative-pathname :cosmo-pagination
                                         "src/templates/pagination.html")))
  *pagination-template*)

(djula:add-template-directory (asdf:system-relative-pathname :cosmo-pagination
                                                             "src/templates/"))

(defparameter *page-size* 50
  "Default page size when listing records.")

(defun table-records (table q &key (page 1) (page-size *page-size*))
  (let ((offset (* (1- page) page-size)))
    (mito:select-dao table
      ;; (if (str:non-blank-string-p q))
      (sxql:order-by (:desc :created-at))
      (sxql:limit page-size)
      (sxql:offset offset))))
#+test-openbookstore
(progn
  ;; latest books
  (table-records 'book)
  ;; older books:
  (table-records 'book :page 2)
  )


(defgeneric render-table (table &key records search-value page page-size) ;; &key (order-by :desc))
  (:method (table &key (records nil records-provided-p)
                    search-value
                    (page 1)
                    (page-size *page-size*))
    ;; Our Hunchentoot route ensures page and page-size are transformed to an integer.
    ;; The route always adds :page key params, so they can be null.
    ;; Set the default values there.
    (let* ((records (if records-provided-p
                        records
                        (table-records table :page (or page 1)
                                             :page-size (or page-size *page-size*))))
           (count (mito:count-dao table))
           (pagination (cosmo/pagination:make-pagination
                        :href (format nil "/admin/~a?q=~a"
                                      (str:downcase table)
                                      (or search-value ""))
                        :page (or page 1)
                        :page-size (or page-size *page-size*)
                        :nb-elements count))
           (messages (bookshops.messages:get-message/status)))

      (log:info messages pagination)

      (djula:render-template* *admin-table* nil
                              :messages messages
                              :table table
                              :search-value search-value
                              :tables (tables)
                              :records records
                              ;; :pagination-template (pagination-template)
                              :pagination-template
                              (djula:render-template* (pagination-template) nil
                                                      :pagination pagination)
                              ))))
