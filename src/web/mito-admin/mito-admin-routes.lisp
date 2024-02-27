
;; bug? lol
(defun openbookstore::render-index ()
  (openbookstore.models::render-index))

(in-package :openbookstore.models)

(djula:def-filter :render-object-field (obj val)
  (declare (ignorable obj val))
  (format nil "hello"))

(easy-routes:defroute route-admin-index ("/admin/" :method :get) ()
  "Admin index: show our database tables and a simple dashboard."
  (openbookstore.models::render-index))

(easy-routes:defroute route-admin-table ("/admin/:table" :method :get) ()
  "List this table's records."
  (render-table (alexandria:symbolicate (str:upcase table))))

(easy-routes:defroute route-admin-record ("/admin/:table/:id" :method :get) ()
  "Show record."
  (render-record (alexandria:symbolicate (str:upcase table)) id))

(easy-routes:defroute route-admin-record-create ("/admin/:table/create" :method :get) ()
  "Create record: show a form."
  (create-record (alexandria:symbolicate (str:upcase table))))

(easy-routes:defroute route-admin-record-create/post ("/admin/:table/create" :method :post) ()
  "Create record (POST).

  Return to the table view or display the same form with errors."
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
       (mapcar #'bookshops.messages:add-message (access action :successes))
       (hunchentoot:redirect (access action :redirect))))))

(easy-routes:defroute route-admin-record-delete/post ("/admin/:table/:id/delete" :method :post) ()
  "Delete record."
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
       (mapcar #'bookshops.messages:add-message (access action :messages))
       (hunchentoot:redirect (access action :redirect))))))

(easy-routes:defroute route-admin-record-edit ("/admin/:table/:id/edit" :method :get) ()
  "Edit record: show a pre-filled form."
  (edit-record (alexandria:symbolicate (str:upcase table))
               id))

(easy-routes:defroute route-admin-record-edit/post ("/admin/:table/:id/edit" :method :post) ()
  "Save edited record (POST)."
  (log:info table id (hunchentoot:post-parameters*))
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
       (mapcar #'bookshops.messages:add-message (access action :messages))
       (hunchentoot:redirect (or (access action :redirect)
                                 (error "The URL to redirect to is null."))))))
  )


;;;
;;; section: search
;;;

(easy-routes:defroute route-admin-table-search ("/admin/:table/search" :method :get) (q)
  "Search records.

  Search on the table or form `search-fields'."
  (log:info "search! " q)
  (let* ((table (alexandria:symbolicate (str:upcase table)))
         (records (search-records table q)))
    (render-table table
                  :records records
                  :search-value q)))

(defun toggle-devel-profile (&optional (val nil val-p))
  "Show Lisp errors in the browser."
  (setf hunchentoot:*catch-errors-p* (if val-p val t))
  (setf hunchentoot:*show-lisp-errors-p* (if val-p val t)))

#+(or)
(toggle-devel-profile nil)
