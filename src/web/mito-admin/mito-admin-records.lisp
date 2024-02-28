(in-package :openbookstore.models)

;;; Functions to deal with single records.
;;; (more are in mito-forms)

(defparameter *admin-record* (djula:compile-template* "mito-admin/templates/record.html"))

(defgeneric render-record (table id)
  (:method (table id)
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
      ;; (let ((djula:*djula-execute-package* :openbookstore.models))
        (djula:render-template* *admin-record* nil
                                :raw raw
                                ;; :fields fields
                                :fields fields/values
                                ;; :rendered-fields rendered-fields
                                :rendered-fields rendered-fields/values
                                :table table
                                :tables (tables)
                                :record record
                                )
      ;; )
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
