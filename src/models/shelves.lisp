
(in-package :bookshops.models)

(defclass shelf ()
  ((name
    :initarg :shelf
    :accessor name
    :initform ""
    :col-type (or :null (:varchar 128)))
   (name-ascii
    :accessor name-ascii
    ;; no initarg, done an instance initialization.
    :initform nil
    :col-type (or :null (:varchar 128))))
  (:metaclass mito:dao-table-class)
  (:unique-keys name)
  (:documentation "Shelves are categories for cards, but they have a physical location
    in the bookstore. A Card has only one shelf."))

(defmethod initialize-instance :after ((obj shelf) &key)
  "Create the shelf's name-ascii, just after the shelf instance is created, but before it is initialized with the initargs and initforms."
  (with-slots (name) obj
    (let ((name-ascii (utils:asciify name)))
      (setf (name-ascii obj) name-ascii))))

(defmethod print-object ((obj shelf) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name))
        obj
      (format stream "~a" name))))

(defun make-shelf (name)
  "Save a new shelf in DB.

  Check if a shelf with that name already exists.
  The check is done with a lowercase and ascii version of the name.

  Return two values: T if the shelf exists or was created, T if the shelf was created."
  ;; Doing that because mito:save-dao with a name constraint error would throw
  ;; a sql error, not a very precise condition.
  (when (str:blankp name)
    (error "Could not create a shelf with a blank name."))
  (let* ((name-ascii (utils:asciify name))
         (existing (mito:find-dao 'shelf :name-ascii name-ascii)))
    (if existing
        (progn
          (log:info "~&A shelf with that name (~a) (but lowercase and in ascii (~a) already exists. Do nothing.~&" name name-ascii)
          (values t nil))
        (values
         (mito:save-dao (make-instance 'shelf :name name))
         t))))

(defun find-shelf (&key query (order :asc) (order-by :name-ascii) (limit 50))
  "Return a list of book objects. If a query string is given, filter by the ascii name.
  By default, sorted alphabetically by name-ascii."
  ;; Took inspiration from find-book. Beginning to have a pattern…
  ;; Added order-by
  (assert (member order (list :asc :desc)) nil "~&Wrong :order choice. Please use either :asc or :desc (default). You might want to change the :order-by field.~&Example: (find-shelf :order: asc :order-by :name-ascii)~&")
  (mito:select-dao 'shelf
    (when (str:non-blank-string-p query)
      (sxql:where
       `(:and
         (:like :name-ascii ,(str:concat "%" (str:downcase query) "%")))))
    (sxql:limit limit)
    (sxql:order-by `(,order ,order-by))))

(defun find-shelf-by (key val)
  (when val
    (mito:find-dao 'shelf key val)))

;;; Devel
(defun ensure-shelf-name-ascii ()
  "Parse all rows and ensure they have an ascii name.

  A mismatch can happen during development. Mostly because I added the field when rows already existed. Should not happen from now! should not…"
  (loop for shelf in (mito:select-dao 'shelf)
     if (and (name shelf)
             (str:blankp (name-ascii shelf)))
     do (format t "setting name-ascii for ~a…~&" shelf)
       (setf (name-ascii shelf) (utils:asciify (name shelf)))
       (mito:save-dao shelf)
     finally (return t)))
