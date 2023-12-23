
(in-package :bookshops.models)

;; see exports below.

;; A contact is like a place, except we know it is a person (useful for completion).
;; Books at someone's place aren't in at home now.
(defclass contact (place)
  ()
  (:metaclass mito:dao-table-class))

;; (defclass contact-copies (place-copies)
;;   ((max-time
;;     :col-type :integer
;;     :initform 60                        ;; days
;;     :accessor max-time))
;;   (:metaclass dao-table-class))

(defclass contact-copies ()
  ;; XXX: use dao-table-mixin, do not inherit from place-copies, or maybe factorize book and quantity, not place and contact (we'll get place of id 1 instead of the contact).
  ;; Not inheriting = having to define the methods (title, name, quantity).
  ;; Update <2023-12-22 Fri> uh what does that old comment mean?
  ;; I simplified the accessor names to simply book, contact, quantity, and defined title and name. All is well…
  ((book
    :accessor book
    :initarg :book
    :initform nil
    :col-type book)
   (contact
    :accessor contact
    :initarg :contact
    :initform nil
    :col-type contact)
   (quantity
    :accessor quantity
    :initform 0
    :col-type (or (:integer) :null))
   (max-time
    :col-type :integer
    :initform 60 ;; days
    :accessor max-time)
   (due-date
    :col-type (or :null :timestamp)     ;; null needed for a successfull Mito migration.
    :initform (local-time:timestamp+ (local-time:now) 60 :day)
    :accessor due-date
    :documentation "Date when the loan should end and the book come back."))
  (:metaclass mito:dao-table-class))

;; Other generic functions to shorten chaining to common elements:
;; use (title <contact-copies>) instead of (title (book <contact-copies>)).
(defmethod title ((it contact-copies))
  (title (book it)))

(defmethod name ((it contact-copies))
  (name (contact it)))

(defmethod created-at ((it contact-copies))
  "Wrapper around mito:object-created-at, to have it in templates,
  and to control how the date is printed."
  ;; Doesn't show??
  (local-time:format-timestring nil (mito:object-created-at it)
                                :format local-time:+asctime-format+))

(defmethod print-object ((contact contact) stream)
  (print-unreadable-object (contact stream :type t)
      (format stream "~a" (name contact))))

(defmethod print-object ((it contact-copies) stream)
  (print-unreadable-object (it stream :type t)
    (format stream "~a lended to ~a on ~a"
            (title it)
            (name it)
            (mito:object-created-at it))))

(defun make-contact (name)
  "Make a new contact (no DB registration)."
  (make-instance 'contact :name name))

(defun create-contact (name)
  "Create a new contact (with DB registration)."
  (mito:create-dao 'contact :name name))

(defun find-contacts (&optional query)
  "If query (list of strings), return contacts matching this name. Otherwise, return all contacts."
  ;; xxx: factorize with find-places
  (if query
      (progn
        ;; xxx should be same interface as find-book
        (unless (consp query)
          (setf query (cons query nil)))
        (mito:select-dao 'contact
          (sxql:where (:like :name (str:concat "%" (str:join "%" query) "%")))))
      (mito:select-dao 'contact)))

(defun find-contact-by (key val)
  (when val
    (mito:select-dao 'contact
      (sxql:where (:= key val)))))

(defun find-contacts-copies (&key contact)
  "Return the list of borrowed books, most recent last.
If `contact' is given, filter by this contact."
  ;; (warn "Exclude loans with a quantity at 0 ?")
  (remove-if-not #'book
                 (mito:select-dao 'contact-copies
                   (when contact
                     (sxql:where (:= :contact contact)))
                   (sxql:order-by :object-created))))

(defun find-contact-loans (contact)
  "Find this contact's borrowed books."
  (declare (type contact contact))
  ;; I initially wrote the following function. Some refactoring might be needed.
  (find-contacts-copies :contact contact))

(defun find-book-loans (book)
  "Find who borrowed this book."
  (mito:select-dao 'contact-copies
    (when book
      (sxql:where (:= :book book)))
    (sxql:order-by :object-created)))
#+(or)
(find-book-loans (third (bookshops.models::find-book :query "comprendre")))

(defgeneric loan-too-long-p (obj)
  (:documentation "Return t if this loan bypasses the number of days allowed."))

(defmethod loan-too-long-p (obj)
  (error "no method for this type: ~a" (type-of obj)) )

(defmethod loan-too-long-p ((obj contact-copies))
  (let* ((now (local-time:now))
         (time-difference (ltd:timestamp-difference
                           (mito:object-created-at obj) now)))
    (> (abs (ltd:duration-as time-difference :day))
       (max-time obj))))

(defgeneric loan-danger-p (obj)
  (:documentation "Return t if the loan is approching the maximum time allowed."))

(defmethod loan-danger-p ((obj contact-copies))
  (let* ((now (local-time:now))
         (danger-days 10)
         (diff (ltd:timestamp-difference (mito:object-created-at obj) now)))
    (> (abs (ltd:duration-as diff :day))
       (- (max-time obj)
          danger-days))))

(defun princ-color-flags (what loan)
  "Return `what` (a string) with red ansi colors if the loan is too long, yellow if danger. To print with `format t`."
  (if (loan-too-long-p loan)
      (cl-ansi-text:red (princ-to-string what))
      (progn
        (if (loan-danger-p loan)
            (cl-ansi-text:yellow (princ-to-string what))
            what))))

(defun print-borrowed-book (book)
  ;; unused.
  (format t "~2a- ~40a borrowed on ~a~&"
          (mito:object-id book)
          (title book)
          (mito:object-created-at book)))

(defun print-borrowed-books (contact)
  (let ((title-length 40))
    (mapcar (lambda (it)
              (let ((book (book it)))
                (if book
                    (format t "~t~2a- ~va since ~a~&"
                            (mito:object-id book)
                            title-length
                            (str:prune title-length (title it))
                            (princ-color-flags
                             (utils:format-date (mito:object-created-at it))
                             it))
                    (format t "no book for ~a?" it))))
            (mito:select-dao 'contact-copies
              (sxql:where (:= :contact contact))))))

(defun print-contact (contact &key (stream t) (details *print-details*))
  "Print the given contact, the books she borrowed and if it's been too long."
  ;; format-object method ?
  (let* ((title-length 40)
         (padding (+ title-length 9)))
    (format stream "~2a - ~va~t ~&"
            (mito:object-id contact)
            padding
            (cl-ansi-text:cyan (str:prune title-length (name contact)))
            ;; (length (contact-books contact))
            )
    (when details
      (print-borrowed-books contact))))
;;
;; Commands
;;
(defun lend (book contact &key (quantity 1))
  "Lend this book to this contact."
  ;; xxx: modeled after add-to.
  (assert book)
  (assert contact)
  (unless (mito:object-id book)
    (error "The book ~a is not saved in DB." book))
  (let ((existing (mito:find-dao 'contact-copies :contact contact :book book))
        contact-copy)
    (if existing
        (progn
          (log:info "The book ~a was already lended to ~a." book contact)
          (incf (quantity existing) quantity)
          (mito:save-dao existing)
          (quantity existing))
        (progn
          (log:info "~a wasn't lended' to ~a yet, let's do it.~&" book contact)
          (setf contact-copy (make-instance 'contact-copies
                                            :contact contact
                                            :book book
                                            :quantity quantity))
          (mito:insert-dao contact-copy)
          (quantity contact-copy)))))


(defun print-loans (&key name)
  "Print who borrowed what book and since when (most recent last). If `name', filter by this contact."
  (let* ((contact (first (when name (find-contacts name))))
         (copies (if contact
                     (find-contacts-copies :contact contact)
                     (find-contacts-copies)))
         (title-length 40)
         (padding (+ title-length 9))) ;; color escape strings.
    (mapcar (lambda (copy)
              (let ((book (book copy)))
                (if book
                    (format t "~2a- ~va since ~a by ~a~&"
                            (mito:object-id book)
                            padding
                            (cl-ansi-text:blue (str:prune title-length (title copy)))
                            (princ-color-flags
                             (utils:format-date (mito:object-created-at copy))
                             copy)
                            (name copy))
                    (format t "no book for copy ~a?" copy))))
            copies)
    ;; We return a list of copies, not contact-copies, for the command level,
    ;; to get pagination completion right.
    (mapcar #'book copies)))

(defun find-loans (&key (limit 20) (order :asc))
  (remove-if-not #'book
                 (mito:select-dao 'contact-copies
                   (sxql:limit limit)
                   (sxql:order-by `(,order :due-date)))))

(defun outdated-loans (&key (limit 20) (order :asc))
  "Return a list of loans whose due date is exhausted (lower than today).

  Filter out loans whose card is null."
  (remove-if-not #'book
                 (mito:select-dao 'contact-copies
                   (sxql:where (:< :due-date (local-time:now)))
                   (sxql:limit limit)
                   (sxql:order-by `(,order :due-date)))))

(defun loans-without-cards ()
  "Find loans without a card. Should not happen, but can happen during development when a card is deleted."
  (remove-if #'book
             (mito:select-dao 'contact-copies)))

(defun count-outdated-loans ()
  "Return the number (integer) of outdated loans."
  ;; Uses LENGTH instead of a proper SQL count.
  (length (outdated-loans :limit 1000000)))

(defun receive (book &optional contact)
  "Return this book.
   In case of ambiguity, give the contact as optional argument."
  (declare (ignorable contact))
  (let ((copies (mito:select-dao 'contact-copies
                  ;; How to chain queries, to filter on contact only if given ?
                  (sxql:where (:= :book book)))))
    (case (length copies)
      (0
       (format t "It seems that this book was not lended to anyone.~&"))
      (1
       (let* ((copy (first copies))
              (quantity (quantity copy)))
         (decf (quantity copy))
         (mito:save-dao copy)
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
(defvar *thule* (first (find-book :query "thule")))

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
