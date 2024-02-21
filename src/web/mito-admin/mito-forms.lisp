
(in-package :openbookstore.models)


#|
Trying…

Find a Mito slot

(mito.class:find-slot-by-name 'book 'shelf)
#<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::SHELF>

(inspect *)

The object is a STANDARD-OBJECT of type MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS.
0. SOURCE: #S(SB-C:DEFINITION-SOURCE-LOCATION
              :NAMESTRING "/home/vince/projets/openbookstore/openbookstore/src/models/models.lisp"
              :INDICES 262185)
1. NAME: SHELF
2. INITFORM: NIL
3. INITFUNCTION: #<FUNCTION (LAMBDA (&REST REST)
                              :IN
                              "SYS:SRC;CODE;FUNUTILS.LISP") {5374305B}>
4. INITARGS: (:SHELF)
5. %TYPE: T
6. %DOCUMENTATION: "Shelf"
7. %CLASS: #<MITO.DAO.TABLE:DAO-TABLE-CLASS OPENBOOKSTORE.MODELS:BOOK>
8. READERS: (SHELF)
9. WRITERS: ((SETF SHELF))
10. ALLOCATION: :INSTANCE
11. ALLOCATION-CLASS: NIL
12. COL-TYPE: (OR :NULL SHELF)
13. REFERENCES: NIL
14. PRIMARY-KEY: NIL
15. GHOST: T
16. INFLATE: "unbound"
17. DEFLATE: "unbound"
>

List of slot names with normal MOP:

(mapcar #'mopp:slot-definition-name slots)
(MITO.DAO.MIXIN::CREATED-AT MITO.DAO.MIXIN::UPDATED-AT MITO.DAO.MIXIN::SYNCED
 MITO.DAO.MIXIN::ID DATASOURCE DETAILS-URL TITLE TITLE-ASCII ISBN PRICE
 DATE-PUBLICATION PUBLISHER PUBLISHER-ASCII AUTHORS AUTHORS-ASCII SHELF
 SHELF-ID COVER-URL REVIEW)

List of Mito slots from those names:

(mapcar (^ (name) (mito.class:find-slot-by-name 'book name))
        '(MITO.DAO.MIXIN::ID title shelf))
(#<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::ID>
 #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:TITLE>
 #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::SHELF>)

Mito column types:

(mapcar #'mito.class:table-column-type *)
;; (:BIGSERIAL (:VARCHAR 128) SHELF)

but not (or null shelf) ?

|#

(defclass form ()
  ((model
    :initarg :model
    :initform nil
    :documentation "Class table symbol. Eg: 'book")
   (fields
    :initarg :fields
    :initform nil
    :accessor fields
    :documentation "List of field names to render in the form.")
   (exclude-fields
    :initarg :exclude-fields
    :initform nil
    :documentation "List of field names to exclude. Better use the `exclude-fields' method.")
   (target
    :initarg :target
    :initform "/admin/:table/create"
    :documentation "The form URL POST target when creating or updating a record.
      Allows a :table placeholder to be replaced by the model name. Use with `form-target'.")))

(defclass book-form (form)
  ())

(defclass place-form (form)
  ())

;; => automatically create a <table>-form class for all tables.

(defparameter book-form (make-instance 'book-form :model 'book))


(defparameter *select-input* (djula:compile-template* "mito-admin/templates/includes/select.html"))

(defparameter *admin-create-record* (djula:compile-template* "mito-admin/templates/create.html"))

(defmethod initialize-instance :after ((obj form) &key)
  "Populate fields from the class name."
  (when (slot-boundp obj 'model)
    (with-slots (fields model) obj
      (setf fields
            ;; Use direct slots.
            ;; If we get all slot names, we get MITO.DAO.MIXIN::SYNCED and the like.
            ;;TODO: exclude -ID fields.
            ;;(mito.class:table-column-type (mito.class:find-slot-by-name 'book 'shelf-id))
            ;; SHELF
            ;; => it's a table, so exclude shelf-id.
            (class-direct-slot-names model)))))

(defmethod exclude-fields (book-form)
  "Return a list of field names (symbols) to exclude from the creation form."
  '(title-ascii
    publisher-ascii
    authors-ascii
    datasource
    ;; ongoing: handle relations
    ;; we need to exclude shelf, or we'll get an error on mito:insert-dao if the field is NIL.
    ;; shelf
    shelf-id
    shelf2-id
    ))

(defgeneric form-fields (form)
  (:documentation "Return a list of this form's fields, excluding the fields given in the constructor and in the `exclude-fields' method.")
  (:method (form)
    (with-slots (fields exclude-fields) form
      ;; set operations don't preserve field order? I think there were in order until now.
      (set-difference fields (set-exclusive-or
                              exclude-fields
                              (exclude-fields form))))))

#+(or)
(assert (equal 0 (mismatch
                  (form-fields (make-instance 'book-form :model 'book))
                  '(COVER-URL SHELF-ID SHELF AUTHORS PUBLISHER DATE-PUBLICATION PRICE ISBN TITLE DETAILS-URL))))


;;;
;;; How best define the input types/widgets ?
;;;
(defgeneric input-fields (form)
  (:documentation "Define the fields' widgets. Return: a hash-table with key (field symbol name) and property list with keys: :widget, :validator…

  Widgets are: :textarea, :datetime-local… TBC

  Shall we infer the widgets more thoroughly?")
  (:method (form)
    (dict)))

(defun validate-ok (&optional it)
  (declare (ignorable it))
  (print "ok!"))

(defmethod input-fields ((form book-form))
  ;; Here we don't use another class to not bother with special :widget slots and a metaclass…
  (dict 'review (list :widget :textarea :validator #'validate-ok)
        'date-publication (list :widget :datetime-local)))

;; so, an "accessor":
(defun input-field-widget (form field)
  (access:accesses (input-fields form) field :widget))
#+(or)
(input-field-widget BOOK-FORM 'review)
;; :TEXTAREA

;; That's a second way to define form inputs isn't it?
;; We added field-input first, but it's to render HTML.
(defgeneric render-widget (form field widget &key name)
  (:documentation "Return HTML to render a widget given a form, a field name (symbol), a widget type (keyword).

  The user simply chooses the widget type and doesn't write HTML.

  NAME: the column type field (symbol). If \"shelf2\" refers to the \"shelf\" table, FIELD is \"shelf2\" and NAME must be \"shelf\".

  See also `field-input'.")
  (:method (form field widget &key name)
    (case widget
      (:select
          (let ((objects (mito:select-dao field)))
            (log:info "caution: we expect ~a objects to have an ID and a NAME field" field)
            (djula:render-template* *select-input* nil
                                    :name (or name field)
                                    :options objects
                                    :empty-choice t
                                    :empty-choice-label (format nil "-- choose a ~a… --"
                                                                (str:downcase field))
                                    :label field
                                    :select-id (format nil "~a-select" (str:downcase field)))))
      (:textarea
       (format nil "<div class=\"field\">
<label class=\"label\"> ~a </label>
 <div class=\"control\">
  <textarea name=\"~a\" class=\"input\" rows=\"5\"> </textarea>
</div>
</div>" field field))
      (:datetime-local
       (format nil "<div class=\"field\">
<label class=\"label\"> ~a </label>
 <div class=\"control\">
  <input type=\"datetime-local\" name=\"~a\" class=\"input\"> </input>
</div>
</div>" field field))
      (t
       (format nil "<div> todo for ~a </div>" field)))))

;;;
;;; end of input types/widgets experiment.
;;;

;;;
;;; Let's continue and generate a form.
;;;

(defgeneric field-input (form field)
  (:documentation "Return HTML for this field. It's important to have a name=\"FIELD\" for each input, so that they appear in POST parameters.

  This is the method called by the form creation. When we find a widget specified for a field, we call render-widget.

  A user can subclass this method and return HTML or just set the widget type and let the default rendering.")
  (:method (form field)
    (declare (ignorable form))
    (cond
      ((field-is-related-column field)
       (log:info "render column for ~a" field)
       (render-widget form (field-is-related-column field) :select :name field))
      ((not (null (input-field-widget form field)))
       (render-widget form field (input-field-widget form field)))
      (t
       (format nil "<div class=\"field\">
<label class=\"label\"> ~a </label>
 <div class=\"control\">
  <input name=\"~a\" class=\"input\" type=\"text\"> </input>
</div>
</div>" field field)))))

;; We can override an input fields for a form & field name
;; by returning HTML.
(defmethod field-input ((form book-form) (field (eql 'shelf)))
  (let ((shelves (mito:select-dao field)))
    (djula:render-template* *select-input* nil
                            :name field
                            :options shelves
                            :empty-choice t
                            :empty-choice-label "-- choose a shelf… --"
                            :label "select a shelf"
                            :select-id "shelf-select")))

(defun collect-slot-inputs (form fields)
  (loop for field in fields
        collect (list :name field
                      :html (field-input form field))))

(defun make-form (table)
  "From this table name, return a new form."
  (make-instance (alexandria:symbolicate (str:upcase table) "-" 'form)
                 :model (alexandria:symbolicate (str:upcase table))))

(defmethod form-target ((obj form))
  "Format the POST URL.

  Replace the :table placeholder by the model name."
  (cond
    ((slot-boundp obj 'model)
     (with-slots (target model) obj
       (if (str:containsp ":table" target)
           (str:replace-all ":table" (str:downcase model) target)
           target)))
    (t
     "")))

;; Serve the form.
(defgeneric create-record (table)
  (:method (table)
    (let* ((form (make-form table))
           (fields (form-fields form))
           (inputs (collect-slot-inputs form fields)))
      (log:info form (form-target form))
      (djula:render-template* *admin-create-record* nil
                              :form form
                              :target (form-target form)
                              :fields fields
                              :inputs inputs
                              :table table
                              ;; global display
                              :tables (tables))
      )))

#|
Tryng out…

(defparameter params '(("NAME" . "new place")))

(cdr (assoc "NAME" params :test #'equal))

;; transform our "PARAM" names (strings) to symbols (to hopefully map our field names)
(defun params-symbols-alist (params)
 (loop for (key . val) in params
  collect (cons (alexandria:symbolicate key) val))
  )

(params-symbols-alist params)
;; ((NAME . "new place"))

(defun params-keywords-alist (params)
 (loop for (field . val) in params
  collect (cons (alexandria:make-keyword field) val))
  )
(params-keywords-alist (params-symbols-alist params))
((:NAME . "new place"))

(defun params-keywords (params)
 (params-keywords-alist (params-symbols-alist params)))


`(make-instance 'place ,@(alexandria:flatten '((:NAME . "new place"))))
(MAKE-INSTANCE 'PLACE :NAME "new place")

ok!
|#

(defparameter *catch-errors* nil
  "Set to T to not have the debugger on an error. We could use hunchentoot's *catch-errors*.")

(defun params-symbols-alist (params)
 (loop for (field . val) in params
       collect (cons (alexandria:symbolicate (str:upcase field)) val)))

(defun params-keywords-alist (params)
 (loop for (field . val) in params
       collect (cons (alexandria:make-keyword field) val)))

(defun params-keywords (params)
  (params-keywords-alist (params-symbols-alist params)))


(defun field-is-related-column (field)
  "The Mito table-column-type of this field (symbol) is a DB table (listed in our *tables*).

  Return: field symbol or nil."
  (let* ((slot (mito.class:find-slot-by-name 'book (alexandria:symbolicate (str:upcase field))))
         (column-type (and slot (mito.class:table-column-type slot))))
    (find column-type (tables))))
#++
(field-is-related-column 'shelf)
;; SHELF
#++
(field-is-related-column 'title)
;; NIL

(defun replace-related-objects (params)
  "If a parameter is a related column, find the object by its ID and put it in the PARAMS alist.
  Change the alist in place.

  This is done before the form validation.

  PARAMS: alist of keywords (string) and values.

  Return: params, modified."
  (loop for (field . val) in params
        for slot = (mito.class:find-slot-by-name 'book (alexandria:symbolicate (str:upcase field)))
        for col = (and slot (mito.class:table-column-type slot))
        for col-obj = nil
        if (find col *tables*)
          do (setf col-obj (mito:find-dao col :id val))
             (log:debug "~a is a related column: ~s" field col-obj)
             (setf (cdr (assoc field params :test #'equal))
                   col-obj))
  params)

#+test-openbookstore
(let ((params (replace-related-objects '(("TITLE" . "test") ("SHELF" . "1")))))
  ;; => (("TITLE" . "test") ("SHELF" . #<SHELF 1 - Histoire>))
  (assert (not (equal (cdr (assoc "SHELF" params :test #'equal))
                      "1"))))


(defgeneric save-record (table &key params &allow-other-keys)
  (:documentation "Process POST paramaters, create a new record or return a form with errors.

    Return a hash-table to tell the route what to do: render a template or redirect.")
  (:method (table &key params &allow-other-keys)
    (log:info params)
    (setf params (replace-related-objects params))
    (log:info "params with relations: ~a" params)

    (let* ((form (make-form table))
           (fields (form-fields form))
           (inputs (collect-slot-inputs form fields))
           (keywords (params-keywords params))
           (params-symbols-alist (params-symbols-alist params))
           (record nil)
           (model (slot-value form 'model))
           (errors nil))

      ;;TODO: form validation…

      ;; Create object.
      (handler-case
          ;; produce:
          ;; (MAKE-INSTANCE 'BOOK :TITLE "new title")
          (setf record
                (apply #'make-instance model (alexandria:flatten keywords)))
        (error (c)
          (push (format nil "~a" c) errors)))

      (when errors
        (return-from save-record
          (dict
           :status :error
           ;; list of keys to call djula:render-template*
           :render (list *admin-create-record* nil
                                  ;; errors:
                                  :form-errors errors
                                  :form form
                                  :target (form-target form)
                                  :fields fields
                                  :inputs inputs
                                  :table table
                                  ;; global display
                                  :tables (tables)))))

      ;; Save record.
      (handler-case
          (progn
            (log:info "saving record in DB…" record)
            (mito:insert-dao record))
        (error (c)
          ;; dev
          (unless *catch-errors*
            (error c))
          (push (format nil "~a" c) errors)))
      (when errors
        (return-from save-record
          (dict
           :status :error
           :render (list *admin-create-record* nil
                                  ;; errors:
                                  :errors errors
                                  :form form
                                  :target (form-target form)
                                  :fields fields
                                  :inputs inputs
                                  :table table
                                  ;; global display
                                  :tables (tables)))))

      ;; Success: redirect to the table view.
      (values
       (dict
        :status :success
        :redirect (str:concat "/admin/" (str:downcase model))
        ;; Use my messages.lisp helper?
        :successes (list "record created"))
       record)
       )))

#+(or)
(save-record 'place :params '(("NAME" . "new place test")))

#+(or)
(save-record 'book :params '(("TITLE" . "new book")))


#|
This fails:

(apply #'make-instance 'book (alexandria:flatten
                              (params-keywords '(("REVIEW" . "") ("COVER-URL" . "")
                                                 ("SHELF-ID" . "") ("SHELF" . "")
                                                 ("AUTHORS" . "") ("PUBLISHER" . "") ("DATE-PUBLICATION" . "")
                                                 ("PRICE" . "") ("ISBN" . "") ("TITLE" . "crud")
                                                 ("DETAILS-URL" . "")))))

(mito:insert-dao *)
=> error: mito.dao.mixin::ID missing from object

=> exclude relational columns for now.


Works:
(apply #'make-instance 'book (alexandria:flatten
                              (params-keywords '(("REVIEW" . "") ("COVER-URL" . "")
                                                 ;; ("SHELF-ID" . "")
                                                 ;; ("SHELF" . "")
                                                 ("AUTHORS" . "") ("PUBLISHER" . "") ("DATE-PUBLICATION" . "")
                                                 ("PRICE" . "") ("ISBN" . "") ("TITLE" . "crud")
                                                 ("DETAILS-URL" . "")))))

(mito:insert-dao *)

|#
