(in-package :openbookstore.models)

;;; More functions that don't really fit into mito-forms.

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
