
(in-package :bookshops.models)

;; see exports below.

;; A contact is like a place, except we know it is a person (useful for completion).
;; Books at someone's place aren't in at home now.
(defclass contact (place)
  ()
  (:metaclass dao-table-class))

;; (defclass contact-copies (place-copies)
;;   ((max-time
;;     :col-type :integer
;;     :initform 60                        ;; days
;;     :accessor max-time))
;;   (:metaclass dao-table-class))

(defclass contact-copies ()
  ;xxx: use dao-table-mixin, do not inherit from place-copies, or maybe factorize book and quantity, not place and contact (we'll get place of id 1 instead of the contact).
  ;; Not inheriting = having to define the methods (title, name, quantity).
  ((book
    :accessor contact-copies-book
    :initarg :book
    :col-type book)
   (contact
    :accessor contact-copies-contact
    :initarg :contact
    :col-type contact)
   (quantity
    :accessor contact-copies-quantity
    :initform 0
    :col-type (or (:integer) :null))
   (max-time
    :col-type :integer
    :initform 60                        ;; days
    :accessor max-time))
  (:metaclass dao-table-class))

(defmethod quantity ((it contact-copies))
  (contact-copies-quantity it))

(defmethod (setf quantity) (val (it contact-copies))
  (setf (contact-copies-quantity it) val))

(defmethod title ((it contact-copies))
  (title (contact-copies-book it)))

(defmethod name ((it contact-copies))
  (name (contact-copies-contact it)))

(defun contact-books (contact)
  (contact-copies-book contact))

(defmethod print-object ((contact contact) stream)
  (print-unreadable-object (contact stream :type t)
      (format stream "~a" (name contact))))

(defun print-borrowed-book (book)
  (format t "~2a- ~40a borrowed on ~a~&"
          (object-id book)
          (title book)
          (object-created-at book)))

(defmethod print-object ((it contact-copies) stream)
  (print-unreadable-object (it stream :type t)
    (format stream "~a lended to ~a on ~a"
            (title it)
            (name it)
            (object-created-at it))))

(defun make-contact (name)
  "Make a new contact (no DB registration)."
  (make-instance 'contact :name name))

(defun create-contact (name)
  "Create a new contact (with DB reistration)."
  (create-dao 'contact :name name))

(defun find-contacts (&optional query)
  "If query (list of strings), return contacts matching this name. Otherwise, return all contacts."
  ;; xxx: factorize with find-places
  (if query
      (progn
        ;; xxx should be same interface as find-book
        (unless (consp query)
          (setf query (cons query nil)))
        (select-dao 'contact
          (where (:like :name (str:concat "%" (str:join "%" query) "%")))))
      (select-dao 'contact)))

(defun find-contact-by (key val)
  (when val
    (select-dao 'contact
      (where (:= key val)))))

(defun find-contacts-copies ()
  "Return the list of borrowed books, most recent last."
  ;; (warn "Exclude loans with a quantity at 0 ?")
  (select-dao 'contact-copies
    (order-by :object-created)))

(defgeneric loan-too-long-p (obj)
  (:documentation "Return t if this loan bypasses the number of days allowed."))

(defmethod loan-too-long-p ((obj contact-copies))
  (let* ((now (local-time:now))
         (time-difference (ltd:timestamp-difference (object-created-at obj) now)))
    (> (abs (ltd:duration-as time-difference :day))
       (max-time obj))))

(defgeneric loan-danger-p (obj)
  (:documentation "Return t if the loan is approching the maximum time allowed."))

(defmethod loan-danger-p ((obj contact-copies))
  (let* ((now (local-time:now))
         (danger-days 10)
         (diff (ltd:timestamp-difference (object-created-at obj) now)))
    (> (abs (ltd:duration-as diff :day))
       (- (max-time obj)
          danger-days))))

(defun princ-color-flags (what loan)
  "Return `what` (a string) with red ansi colors if the loan is too long, yellow if danger. To print with `format t`."
  (if (loan-too-long-p loan)
      (red (princ-to-string what))
      (progn
        (if (loan-danger-p loan)
            (yellow (princ-to-string what))
            what))))


(defun print-borrowed-books (contact)
  (mapcar (lambda (it)
            (format t "~t~2a- ~40a since ~a~&"
                    (object-id (contact-copies-book it))
                    (title it)
                    (princ-color-flags (object-created-at it) it)))
          (select-dao 'contact-copies
            (where (:= :contact contact)))))

(defun print-contact (contact &key (stream t) (details *print-details*))
  "Print the given contact, the books she borrowed and if it's been too long."
  ;; format-object method ?
  (format stream "~2a - ~40a~t ~&"
          (object-id contact)
          (cyan (name contact))
          ;; (length (contact-books contact))
          )
  (when details
    (print-borrowed-books contact)))
;;
;; Commands
;;
(defun lend (book contact &key (quantity 1))
  "Lend this book to this contact."
  ;; xxx: modeled after add-to.
  (assert book)
  (assert contact)
  (unless (object-id book)
    (error "The book ~a is not saved in DB." book))
  (let ((existing (find-dao 'contact-copies :contact contact :book book))
        contact-copy)
    (if existing
        (progn
          (log:info "The book ~a was already lended to ~a." book contact)
          (incf (contact-copies-quantity existing) quantity)
          (save-dao existing)
          (quantity existing))
        (progn
          (log:info "~a wasn't lended' to ~a yet, let's do it.~&" book contact)
          (setf contact-copy (make-instance 'contact-copies
                                            :contact contact
                                            :book book
                                            :quantity quantity))
          (insert-dao contact-copy)
          (quantity contact-copy)))))


(defun loans ()
  "Print who borrowed what book and since when (most recent last)."
  (let ((copies (find-contacts-copies)))
    (mapcar (lambda (copy)
              (format t "~2a- ~30a since ~a by ~a~&"
                      (object-id (contact-copies-book copy))
                      (blue (str:prune 30 (title copy)))
                      (princ-color-flags
                       (local-time:format-timestring nil (object-created-at copy) :format +date-y-m-d+)
                       copy)
                      (name copy)))
            copies)
    ;; We return a list of copies, not contact-copies, for the command level,
    ;; to get pagination completion right.
    (mapcar #'contact-copies-book copies)))

(defun receive (book &optional contact)
  "Return this book.
   In case of ambiguity, give the contact as optional argument."
  (declare (ignorable contact))
  (let ((copies (select-dao 'contact-copies
                  ;; How to chain queries, to filter on contact only if given ?
                  (where (:= :book book)))))
    (case (length copies)
      (0
       (format t "It seems that this book was not lended to anyone.~&"))
      (1
       (let* ((copy (first copies))
              (quantity (quantity copy)))
         (decf (quantity copy))
         (save-dao copy)
         ;; Delete records back to 0 ? We're not handling history yet, but rather no.
         (if (> quantity 1)
             (format t "~a still has ~a cop~@:p of this book.~&"
                     (name copy)
                     (quantity copy))
             (format t "We got '~a' back from ~a, ok.~&."
                     (str:prune 30 (title copy))
                     (name copy)))
         (quantity copy)))
      (t
       (error "This book was borrowed by more than one contact at the same time. Not implemented yet. But it should be.")))))

;;
;; Interactive, development stuff
;;

;; We can compile the following ourselves, it is ignored at compile time.
#+nil
(defvar *contact* (find-dao 'contact))

#+nil
(defvar *thule* (first (find-book "thule")))


;;
;; Export
;;
(export '(make-contact
          create-contact
          find-contacts
          find-contact-by
          print-contact
          lend
          loans
          receive
          ))
