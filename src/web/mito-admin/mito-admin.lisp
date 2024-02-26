(IN-PACKAGE :openbookstore.models)

;; A class.
;; book
;;
;; (defclass person ()
;;   ((name
;;     :initarg :name
;;     :accessor name)
;;    (lisper
;;     :initform nil
;;     :accessor lisper)))
;;
;; Column types:
;;
;;     :col-type (:varchar 128))
;;     :col-type (or :null shelf)

;; An object.
(defparameter p (make-instance 'book :title "clos introspection" :price "10k"))

;; The object's class.
(defparameter p-class (class-of p))
;; or
(defparameter p-class (find-class 'book))
#<MITO.DAO.TABLE:DAO-TABLE-CLASS OPENBOOKSTORE.MODELS:BOOK>

;; The class direct slots.
(defparameter slots
  (closer-mop:class-slots p-class))  ;; all slots, not only direct ones.
;; (#<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION MITO.DAO.MIXIN::CREATED-AT>
;; #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION MITO.DAO.MIXIN::UPDATED-AT>
;; #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION MITO.DAO.MIXIN::SYNCED>
;; #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION MITO.DAO.MIXIN::ID>
;; #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::DATASOURCE>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:DETAILS-URL>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:TITLE>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::TITLE-ASCII>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:ISBN>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:PRICE>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:DATE-PUBLICATION>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:PUBLISHER>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::PUBLISHER-ASCII>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:AUTHORS>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::AUTHORS-ASCII>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::SHELF>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::SHELF-ID>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS:COVER-URL>
;;  #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS OPENBOOKSTORE.MODELS::REVIEW>)

(defparameter slots *)

;; Get slot values.
(defun slot-name (slot-definition)
  (sb-mop:slot-definition-name slot-definition))

(mapcar #'slot-name slots)
;; (DATASOURCE DETAILS-URL TITLE TITLE-ASCII ISBN PRICE DATE-PUBLICATION PUBLISHER
;; PUBLISHER-ASCII AUTHORS AUTHORS-ASCII SHELF SHELF-ID COVER-URL
;; REVIEW)
;; so:
(defun class-direct-slot-names (class)
  "class: symbol or class object.

  (class-direct-slot-names (class-of *book*))
  =
  (class-direct-slot-names 'book)"
  (let ((class (if (symbolp class)
                   (find-class class)
                   class)))
    (mapcar #'slot-name (mopp:class-direct-slots class))))
#+(or)
(class-slot-names 'book)

(defgeneric slot-repr (o slot)
  (:documentation "String representation of this object slot to be shown on the record view page.

  It is usually the slot name, but in case of related columns, we need to return a string representing this column, instead of the unredeable object representation.

  To render HTML, use `render-slot'.

  Example:

  (slot-value *some-book* 'shelf) ;; => #<SHELF 3 - Littérature>

  but the object representation isn't good for a user-level view page, not considering the fact that the #<…> won't show up in some HTML. What we want is:

  (slot-repr *some-book* 'shelf) ;; => \"Littérature\"

  Specialize methods for your database models like so:

  (defmethod ((obj book) (slot (eql 'field-name)))
    (when obj
      (access:accesses obj slot 'related-field-name)))

  Beware: this method is called by `slot-value?', so don't call it here.

  note: we strip the #< and > parts of the object representation for now, let's see in usage.
 ")
  (:method (o slot)
    (when (and o (slot-boundp o slot))
      (str:replace-using (list "#<" ""
                               ">" "")
                         (princ-to-string
                          (slot-value o slot))))))

(defmethod slot-repr (o (slot (eql 'shelf)))
  (when o
    (access:accesses o slot 'name)))

;; or simply use pprint for a class object?!
(defgeneric object-repr (s)
  (:documentation "Return a string representation of this object. Specialize to print a user-viewable representation of a table.

  Example:

  (defmethod ((obj shelf))
    (name obj))")
  (:method (s)
    s))

(defmethod object-repr ((obj shelf))
  (name obj))

#+(or)
(slot-repr (mito:find-dao 'book :id 109) 'shelf)

(defun slot-value? (o slot)
  "slot: slot-definition object or symbol."
  (cond
    ((and (symbolp slot)
          (field-is-related-column slot))
     (log:info "render column for ~a" slot)
     (slot-repr o slot))
     ;; that's just less natural to call:
     ;; (object-repr (slot-value o slot)))
    ((symbolp slot)
     (when (slot-boundp o slot)
       (slot-value o slot)))
    (t
     (let ((name (slot-name slot)))
       (when (slot-boundp o name)
         (slot-value o name))))))

(mapcar (^ (slot) (slot-value? p slot)) slots)
;; (NIL NIL "clos introspection" NIL NIL "10k" NIL NIL NIL NIL NIL NIL NIL NIL NIL)

(defun print-instance-slots (o &key (stream t))
  (let* ((class (class-of o))
         (slots (mopp:class-slots class)))
    (loop for slot in slots
          for name = (slot-name slot)
          for val = (slot-value? o slot)
          collect
             (format stream "~s (~a) = ~s~&" name (type-of name) val))))

#+(or)
(print-instance-slots p)
;; MITO.DAO.MIXIN::CREATED-AT (SYMBOL) = NIL  ;; <= don't print package.
;; MITO.DAO.MIXIN::UPDATED-AT (SYMBOL) = NIL
;; MITO.DAO.MIXIN::SYNCED (SYMBOL) = NIL
;; MITO.DAO.MIXIN::ID (SYMBOL) = NIL
;; DATASOURCE (SYMBOL) = NIL
;; DETAILS-URL (SYMBOL) = NIL
;; TITLE (SYMBOL) = "clos introspection"
;; TITLE-ASCII (SYMBOL) = NIL
;; ISBN (SYMBOL) = NIL
;; PRICE (SYMBOL) = "10k"
;; DATE-PUBLICATION (SYMBOL) = NIL
;; PUBLISHER (SYMBOL) = NIL
;; PUBLISHER-ASCII (SYMBOL) = NIL
;; AUTHORS (SYMBOL) = NIL
;; AUTHORS-ASCII (SYMBOL) = NIL
;; SHELF (SYMBOL) = NIL
;; SHELF-ID (SYMBOL) = NIL
;; COVER-URL (SYMBOL) = NIL
;; REVIEW (SYMBOL) = NIL
;; NIL

(defgeneric render-slot (object slot)
  (:documentation "try it")
  (:method (object slot)
    (format nil " <div>~a</div>" (slot-value? object slot))))

(defmethod render-slot ((obj book) (slot (eql 'title)))
  (format nil "<div> ~a </div>"
          (if (slot-boundp obj slot)
              (slot-value obj slot)
              "")))

(defmethod render-slot ((obj book) (slot (eql 'cover-url)))
  (let ((val (or (slot-value? obj slot)
                 "")))
    (format nil "<a href=\"~a\"> ~a </a>" val val)))

(defun short-timestamp (date)
  (local-time:format-timestring
     nil date :format '(:year "/" (:month 2) "/" (:day 2) " " (:hour 2) ":" (:min 2))))

(defmethod render-slot ((obj book) (slot (eql 'mito.dao.mixin::created-at)))
  (let ((val (or (slot-value? obj slot)
                 "")))
    (short-timestamp val)))

(defmethod render-slot ((obj book) (slot (eql 'mito.dao.mixin::updated-at)))
  (let ((val (or (slot-value? obj slot)
                 "")))
    (short-timestamp val)))

(defmethod render-slot ((obj book) (slot (eql 'shelf)))
  (let ((val (or (slot-value? obj slot)
                 "")))
    (format nil "~a" val val)))

#++
(render-slot p 'title)

;; DEPRECATED
(defun collect-rendered-slots (o)
  (let* ((class (class-of o))
         (slots (mopp:class-slots class)))
    (loop for slot in slots
          for name = (slot-name slot)
          collect
          (list :name name
                :html (render-slot o name)))))

(defun collect-rendered-fields-values (o fields)
  "Similar as `collect-fields-values', but renders the HTML of fields with `render-slot'."
  (loop for field in fields
        collect (list :name field
                      :html (render-slot o field))))


;; DEPRECATED
(defun collect-slots-values (o)
  "Collect a plist of this record's slot names and values.
  But this collects *all* slots. Use `collect-fields-values' instead that takes the list of fields, given by `form-fields'.

  Thus, this function isn't useful to me anymore and is DEPRECATED."
  (let* ((class (class-of o))
         (slots (mopp:class-slots class)))
    (loop for slot in slots
          for name = (slot-name slot)
          for val = (slot-value? o slot)
          collect
            (list :name name :value val))))

(defun collect-fields-values (o fields)
  (loop for field in fields
        collect (list :name field :value (slot-value? o field))))


(defgeneric render-input (obj slot)
  (:method (obj slot)
    (let ((name (str:downcase slot))
          (val (if (slot-boundp obj slot)
                   (slot-value obj slot)
                   "")))
      (format t "<label for=\"~a\"> ~a </label>
<input type=\"text\" placeholder=\"~a\"></input>" name name val))))

#++
(render-input p 'title)

#++
(mapcar (^ (slot) (render-input p (slot-name slot)))
        slots)

;; Don't re-do cl-forms!
;; But it doesn't totally do what we need: automatic form, exclude some fields.
