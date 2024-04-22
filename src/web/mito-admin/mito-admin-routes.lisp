
;; bug? lol
#+openbookstore
(defun openbookstore::render-index ()
  (openbookstore.models::render-index))

(in-package :mito-admin)

(djula:def-filter :render-object-field (obj val)
  (declare (ignorable obj val))
  (format nil "hello"))

;; XXX: this filter shouldn't be needed.
;; In Djula templates we should be able to call methods like:
;; {{ record.print-record }}
;; but… doesn't work? => use *djula-execute-package*
;; This filter always works.
(djula:def-filter :print-record (obj)
  (print-record obj))

(defmacro with-app (() &body body)
  "Run BODY under our app registered with REGISTER-APP.

  In other words, set the current *package* to our app's, so than we can reference its table symbols.
  Indeed, our default routes are defined under mito-admin, our DB classes under our project package, and 'MITO-ADMIN:BOOK is not the same as 'MY-APP::BOOK."
  ;; The issue is manipulating symbols, in the mito-admin package,
  ;; that exist in another package/app.
  ;; Instead of alexandria:symbolicate we'd need (find-symbol "BOOK" *my-app-package*).
  ;; The crux of the issue is slot-value obj slot:
  ;; wether slot is 'book or cosmo-admin-demo::book is not the same.
  `(let ((*package* (find-package *app-package*)))
    (progn ,@body)))

(easy-routes:defroute route-admin-index ("/admin/" :method :get) ()
  "Admin index: show our database tables and a simple dashboard."
  (let ((*package* (find-package *app-package*)))
    (with-app ()
      (render-index))))

(easy-routes:defroute route-admin-table ("/admin/:table" :method :get)
    (q (page :parameter-type 'integer) (page-size :parameter-type 'integer))
  "List this table's records with pagination."
  (with-app ()
    (render-table (alexandria:symbolicate (str:upcase table))
                  :search-value q
                  :page page
                  :page-size page-size)))

(easy-routes:defroute route-admin-record ("/admin/:table/:id" :method :get) ()
  "Show record."
  (with-app ()
    (if (str:blankp id)
        ;; Happens with /admin/book/
        ;; we should define a route with regexp and optional trailing /
        (hunchentoot:redirect (easy-routes:genurl* 'route-admin-table :table table))
        (render-record (print (alexandria:symbolicate (str:upcase table))) id))))

(easy-routes:defroute route-admin-record-create ("/admin/:table/create" :method :get) ()
  "Create a record: show a form."
  (with-app ()
    (log:info *package*)
    (create-record (alexandria:symbolicate (str:upcase table)))))

(easy-routes:defroute route-admin-record-create/post ("/admin/:table/create" :method :post) ()
  "Create record (POST).

  Return to the table view or display the same form with errors."
  (with-app ()
    (let ((action (save-record (alexandria:symbolicate (str:upcase table))
                               :params (hunchentoot:post-parameters*))))
      (cond
        ((equal :error (access action :status))
         (log:info "on error: return template…")
         (apply #'djula:render-template* (access action :render)))
        (t
         (log:info "redirect…" action)
         ;; Does my messages helper work? nope
         ;; Does it in the session activated?
         ;; XXX: add our messages utility.
         ;; (mapcar #'bookshops.messages:add-message (access action :successes))
         (hunchentoot:redirect (access action :redirect)))))))

(easy-routes:defroute route-admin-record-delete/post ("/admin/:table/:id/delete" :method :post)
    ()
  "Delete record."
  (with-app ()
    (let ((action (delete-record (alexandria:symbolicate (str:upcase table))
                                 id
                                 :params (hunchentoot:post-parameters*))))
      (cond
        ((equal :error (access action :status))
         (hunchentoot:redirect (access action :redirect)))
        (t
         (log:info action)
         ;; Does my messages helper work? nope
         ;; Does it in the session activated?
         ;; XXX: add our messages utility.
         ;; (mapcar #'bookshops.messages:add-message (access action :messages))
         (hunchentoot:redirect (access action :redirect)))))))

(easy-routes:defroute route-admin-record-edit ("/admin/:table/:id/edit" :method :get) ()
  "Edit record: show a pre-filled form."
  (with-app ()
    (edit-record (alexandria:symbolicate (str:upcase table))
                 id)))

(easy-routes:defroute route-admin-record-edit/post ("/admin/:table/:id/edit" :method :post) ()
  "Save edited record (POST)."
  (log:info table id (hunchentoot:post-parameters*))
  (with-app ()
    (let* ((table (alexandria:symbolicate (str:upcase table)))
           (record (mito:find-dao table :id id))
           (action (save-record table
                                :params (hunchentoot:post-parameters*)
                                :record record)))
      (cond
        ((equal :error (access action :status))
         (log:info "edit: ")
         ;; why did I have redirect here?
         ;; (hunchentoot:redirect (access action :redirect)))
         (apply #'djula:render-template* (access action :render)))

        (t
         (log:info "edit: redirect…" action)
         ;; Does my messages helper work? nope
         ;; Does it in the session activated?
         ;; XXX: add our messages utility.
         ;; (mapcar #'bookshops.messages:add-message (access action :messages))
         (hunchentoot:redirect (or (access action :redirect)
                                   (error "The URL to redirect to is null."))))))
    ))


;;;
;;; section: search
;;;

(easy-routes:defroute route-admin-table-search ("/admin/:table/search" :method :get)
  (q (page :parameter-type 'integer) (page-size :parameter-type 'integer))
  "Search records.

  Search on the table or form `search-fields'."
  (log:info "search! " q)
  (with-app ()
    (let* ((table (alexandria:symbolicate (str:upcase table)))
           (records (search-records table q :page (or page 1) :page-size (or page-size *page-size*))))
      (render-table table
                    :records records
                    :search-value q))))

(defun toggle-devel-profile (&optional (val nil val-p))
  "Show Lisp errors in the browser."
  ;; XXX: do better.
  (setf hunchentoot:*catch-errors-p* (if val-p val t))
  (setf hunchentoot:*show-lisp-errors-p* (if val-p val t)))

#+(or)
(toggle-devel-profile nil)
