
;; bug? lol
(defun openbookstore::render-index ()
  (openbookstore.models::render-index))

(in-package :openbookstore.models)

(djula:def-filter :render-object-field (obj val)
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

(easy-routes:defroute route-admin-record-edit ("/admin/:table/edit" :method :get) ()
  "Edit record: show a pre-filled form."
  (edit-record (alexandria:symbolicate (str:upcase table))))

(easy-routes:defroute route-admin-record-create/post ("/admin/:table/create" :method :post) ()
  "Create record (POST).

  Return to the table view or display the same form with errors."
  (let ((action (save-record (alexandria:symbolicate (str:upcase table))
                             :params (hunchentoot:post-parameters*))))
    (cond
      ((equal :error (access action :status))
       (apply #'djula:render-template* (access action :render)))
      (t
       (log:info action)
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
