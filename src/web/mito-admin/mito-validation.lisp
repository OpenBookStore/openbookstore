
(in-package :mito-admin)

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


#+(or)
(clavier:not-blank)
;; aka (make-instance 'clavier::not-blank-validator)
;; then (clavier:validate * "")
;; => 2 values: NIL and error message: "Should not be blank"

(defgeneric validators (obj)
  (:documentation "Return a hash-table of validators for this table's fields.")
  (:method (obj)
    (dict)))

#+openbookstore
(defmethod validators ((obj (eql 'place)))
  (dict 'name (list (clavier:not-blank)
                    (clavier:len :max 200)
                    (clavier:len :min 2))))

#+(or)
(defparameter *validator* (clavier:||
                                   (clavier:blank)
                                   (clavier:&& (clavier:is-a-string)
                                               (clavier:len :min 10)))
  "Allow blank. When non blank validate.")
;; I want to simplify this to:
;;
;; (list :allow-blank (clavier:len :min 10))
;;
;; see https://github.com/mmontone/clavier/pull/10

;; moved to demo app:
;; (defmethod mito-admin::validators ((obj (eql 'book)))
;;   (dict 'isbn (list :allow-blank
;;                     (clavier:len :min 10 :max 13
;;                                  ;; :message works with clavier's commit of <2024-02-27>
;;                                  ;; :message "an ISBN must be between 10 and 13 characters long"
;;                                  ))
;;         'title (clavier:~= "test"
;;                            "this title is too common, please change it!")))

(defun field-validators (table field)
  ;; f**, access returns NIL,T ??
  (uiop:ensure-list
   (gethash field (validators table))))

#+test-openbookstore
(progn
  ;; Allow blanks:
  (validate-field 'book 'isbn "")
  ;; if non blank, validate:
  (validate-field 'book 'isbn "123")
  )
(defun validate-all (validators object)
  "Run all validators in turn. Return two values: the status (boolean), and a list of messages.

  Allow a keyword validator: :allow-blank. Accepts a blank value. If not blank, validate."
  ;; I wanted this to be part of clavier, but well.
  ;; https://github.com/mmontone/clavier/pull/10
  (let ((messages nil)
        (valid t))
    (loop for validator in validators
          if (and (eql :allow-blank validator)
                  (str:blankp object))
            return t
          else
            do (unless (symbolp validator)
                 (multiple-value-bind (status message)
                     (clavier:validate validator object :error-p nil)
                   (unless status
                     (setf valid nil))
                   (when message
                     (push message messages)))))
    (values valid
            (reverse (uiop:ensure-list messages)))))

(defgeneric validate-field (table field val)
  (:documentation "Return two values: T if the field passes validation, a list of messages.")
  (:method (table field val)
    (validate-all (field-validators table field) val)))

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
      (values ok (reverse inputs)))))

#+(or)
;; Must return NIL and the HTML contain a validation error:
(validate-collect-slot-inputs (make-form 'place) '((name . "t")))

;; TODO: allow blank field and validate only if non blank.
