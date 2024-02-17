
;; bug? lol
(defun openbookstore::render-index ()
  (openbookstore.models::render-index))

(in-package :openbookstore.models)

(djula:def-filter :render-object-field (obj val)
  (format nil "hello"))

(easy-routes:defroute route-admin-index ("/admin/" :method :get) ()
  (openbookstore.models::render-index))

(easy-routes:defroute route-admin-table ("/admin/:table" :method :get) ()
  (render-table (alexandria:symbolicate (str:upcase table))))

(easy-routes:defroute route-admin-record ("/admin/:table/:id" :method :get) ()
  (render-record (alexandria:symbolicate (str:upcase table)) id))

(easy-routes:defroute route-admin-record-create ("/admin/:table/create" :method :get) ()
  (create-record (alexandria:symbolicate (str:upcase table))))

(easy-routes:defroute route-admin-record-create/post ("/admin/:table/create" :method :post) ()
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
