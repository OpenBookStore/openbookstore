
(in-package :openbookstore.models)

;;;
;;; This is were cl-forms seems good at.
;;;


#+(or)
;; do we want this?
;; Actually, we want to infer field types from mito types.
(defform book ()
  ((name
    :constraints (list (clavier:not-blank))
    :formatter #'identity)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "clavier"))

#+(or)
(clavier:not-blank)
;; aka (make-instance 'clavier::not-blank-validator)
;; then (clavier:validate * "")
;; => 2 values: NIL and error message: "Should not be blank"

(defgeneric validators (obj)
  (:documentation "Return a hash-table of validators for this table's fields.")
  (:method (obj)
    (dict)))

(defmethod validators ((obj (eql 'place)))
  (dict 'name (list (clavier:not-blank)
                    (clavier:len :max 200)
                    (clavier:len :min 2))))

(defmethod validators ((obj (eql 'book)))
  (dict 'isbn (clavier:len :min 10 :max 13)
        'title (make-instance 'clavier:not-equal-to-validator :object "test")))

(defun field-validators (table field)
  ;; f**, access returns NIL,T ??
  (uiop:ensure-list
   (gethash field (validators table))))

#+test-openbookstore
(validate-field 'book 'isbn "123")

(defgeneric validate-field (table field val)
  (:documentation "Return two values: T if the field passes validation, a list of messages.")
  (:method (table field val)
    (let ((validators (field-validators table field))
          (messages '())
          (ok t))
      (loop for validator in validators
            do (multiple-value-bind (success msg)
                   (clavier:validate validator val)
                 (unless success
                   (setf ok nil)
                   (push (format nil "~a: ~a"
                                 (str:downcase field)
                                 (str:downcase msg))
                         messages))))
      (values ok messages))))

#+test-openbookstore
(validate-field 'place 'name "rst")

(defgeneric validate-form (form field/values)
  (:documentation "values: alist with field name (symbol) and its value.

  We prefer to use `validate-collect-slot-inputs'.")
  (:method (form field/values)
    (let ((messages nil)
          (ok t)
          (table (slot-value form 'model)))
      (loop for (field . value) in field/values
            do (multiple-value-bind (status msgs)
                   (validate-field table field value)
                 (unless status
                   (setf ok nil)
                   ;; (push (list :name field
                   ;;             :status "error"
                   ;;             :message (format nil "~a: ~a"
                   ;;                              (str:downcase field)
                   ;;                              msg))
                   ;;       messages)
                   (setf messages
                         (append msgs messages)))))
      (values ok messages))))

#+test-openbookstore
(progn
  ;; ok:
  (validate-form (make-form 'place) '((name . "test")))
  ;; name too short:
  (validate-form (make-form 'place) '((name . "t")))
  )

(defgeneric validate-collect-slot-inputs (form fields/values)
  (:documentation "Return two values: t if all fields pass validation and a plist of field inputs. When the field isn't validated against the given input value, add an error message under the field input.")
  (:method (form fields/values)
    (let ((inputs nil)
          (ok t)
          (table (slot-value form 'model)))
      (loop for (field . value) in fields/values
            do (multiple-value-bind (status msgs)
                   (validate-field table field value)
                 (unless status
                   (setf ok nil))
                 (push (list :name field
                             :html (field-input form field :errors msgs :value value))
                       inputs)))
      (values ok inputs))))

#+(or)
;; Must return NIL and the HTML contain a validation error:
(validate-collect-slot-inputs (make-form 'place) '((name . "t")))

;; TODO: allow blank field and validate only if non blank.
