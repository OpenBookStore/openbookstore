
;; bug? lol
(defun openbookstore::render-index ()
  (openbookstore.models::render-index))

(in-package :openbookstore.models)

(djula:def-filter :render-object-field (obj val)
  (format nil "hello"))

(easy-routes:defroute route-admin-index ("/admin/" :method :get) ()
  (openbookstore.models::render-index))

(easy-routes:defroute route-admin-table ("/admin/:table" :method :get) ()
  (render-table (alexandria:symbolicate table)))

(easy-routes:defroute route-admin-record ("/admin/:table/:id" :method :get) ()
  (render-record (alexandria:symbolicate table) id))
