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

(defun slot-value? (o slot)
  "slot: slot-definition or symbol."
  (cond
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

#++
(render-slot p 'title)

(defun collect-rendered-slots (o)
  (let* ((class (class-of o))
         (slots (mopp:class-slots class)))
    (loop for slot in slots
          for name = (slot-name slot)
          collect
          (list :name name
                :html (render-slot o name)))))


(defun collect-slots-values (o)
  (let* ((class (class-of o))
         (slots (mopp:class-slots class)))
    (loop for slot in slots
          for name = (slot-name slot)
          for val = (slot-value? o slot)
          collect
            (list :name name :value val))))


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

;; don't re-do cl-forms!
