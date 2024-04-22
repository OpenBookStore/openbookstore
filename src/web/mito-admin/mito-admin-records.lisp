(in-package :mito-admin)

;;; Functions to deal with single records.
;;; (more are in mito-forms)

(defparameter *admin-record* (djula:compile-template* "record.html"))

(defun url-parameter (name)
  "Get Hunchentoot's parameter NAME if we are inside a web request, otherwise return NIL."
  ;; Helps accessing URL parameters from inside say render-record
  ;; without adding too many key arguments to the generic method.
  ;; So this should be used for optional parameters, like ?debug=t.
  (if (boundp 'hunchentoot:*request*)
      (hunchentoot:parameter name)
      (log:debug "not inside a web request, cannot get parameter ~a" name)))

(defgeneric render-record (table id)
  (:documentation "Render a record of id ID from TABLE to HTML. This method renders a Djula template.")
  (:method (table id)
    ;; XXX: a little macro to abstract this.
    (let* ((form (make-form table))
           (record (mito:find-dao table :id id))
           (raw (print-instance-slots record :stream nil))
           ;; (fields (collect-slots-values record))
           ;;TODO: we want to see created-at
           (fields/values (collect-fields-values record (form-fields form)))
           ;; (rendered-fields (collect-rendered-slots record)))
           (rendered-fields/values
             (collect-rendered-fields-values record (form-fields form))))

      ;; Arf… a discovered Djula thingy. To call a function/method on an object,
      ;; like {{ record.print-record }}
      ;; we need to set Djula's execute package. It uses cl-user by default.
      ;; see https://github.com/mmontone/djula/issues/34
      ;; Here we need the app package to call the print-record method on the object.
      ;; For a book, this displays its title as the page header and page title.
      (let ((djula:*djula-execute-package* *app-package*))
        (djula:render-template* *admin-record* nil
                                :raw raw
                                ;; :fields fields
                                :fields fields/values
                                ;; :rendered-fields rendered-fields
                                :rendered-fields rendered-fields/values
                                :table table
                                :tables (tables)
                                :record record
                                :debug (url-parameter "debug")
                                ))
      )))

(defgeneric delete-record (table id &key params &allow-other-keys)
  (:documentation "Delete record.

    Return a hash-table to tell the route what to do: render a template or redirect.")
  (:method (table id &key params &allow-other-keys)
    (declare (ignorable params))
    (log:info table id)

    (handler-case
        ;; (let ((record (mito:find-dao table id)))
        ;;   (unless record
        ;;     (return-from delete-record
        ;;       (dict :status :error
        ;;             :message (format nil "The record of ID ~a doesn't exist." id)
        ;;             :render (list *admin-table* nil
        ;;                           ;; errors:
        ;;                           :table table
        ;;                           ;; global display
        ;;                           :tables (tables))))))

        (progn
          ;; This doesn't throw any error or warning on non-existing record?
          (mito:delete-by-values table :id id)

          (dict :status :success
                :redirect (str:concat "/admin/" (str:downcase table))))

      (error (c)
        (log:error c)
        (dict :status :error
              :redirect (str:concat "/admin/" (str:downcase table)))))))
