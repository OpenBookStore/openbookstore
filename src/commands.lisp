(defpackage bookshops.commands
  (:use :cl
        :mito
        :cl-ansi-text)
  (:local-nicknames (:models :bookshops.models))
  (:shadow :search
           :delete)
  (:import-from :bookshops
                :search-books
                :_)

  (:import-from :bookshops.models
                ;; utils
                print-quantity-red-green)

  (:export :main
           :search
           :add
           :baskets
           :details
           :stock
           :next
           :previous
           :stats
           :create
           :delete
           :places
           :move
           :lend
           :contacts
           :loans
           :receive
           :inside
           :fortune
           :*page-size*))
(in-package :bookshops.commands)

(defvar *page-size* 15
  "Maximum number of lines to show when printing results.")
(setf *page-size* 15)

(defvar *current-page* 1
  "Current page of the stock pager.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun percentage (low max)
  (* 100 (/ (float low)
            max)))

(defun sublist (seq start end)
  (if (> (length seq)
         end)
      (subseq seq start end)
      (subseq seq start (length seq))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *last-results* nil
  "List of last search results (for now, a list of book objects).")

(defvar *last-page* nil
  "Sublist of search results. Used to autocomplete the ids of books lastly printed.")

(defvar *last-search* nil
  "List of keywords used on the last search. For example, \"stock ant\" to filter on titles.")

(defun search (query &rest rest)
  "Search for books with `query` on `datasource`, nicely print the result."
  ;; the &rest is for the readline repl, that gives many string arguments to this function.
  (let* ((query (str:unwords (cons query rest)))
         (results (setf *last-results* (search-books query))))
    (models::pprint-books results)))

(defun last-page-book-ids ()
  "We want to complete the ids and the titles."
  (mapcan (lambda (it)
            (list (prin1-to-string (object-id it))
                  ;TODO: TAB-completing a string fails.
                  (format nil "\"~a\"" (models:title it))))
          *last-page*))

(defun basket-names ()
  (mapcar #'models::name (models:find-baskets)))

(defun baskets (&optional name)
  (let ((models::*print-details* (not (str:emptyp name))))
    (mapcar #'models:print-basket (models:find-baskets))))

(replic.completion:add-completion "baskets" #'basket-names)

(defun add (index &optional basket-name)
  "Add this book (by index of the last search) into the DB.

By default, add to the stock. If an optional list name is given, add it to the list without modifying the actual stock."
  (declare (ignorable basket-name))
  (when (stringp index)
    ;; generics ?
    (setf index (parse-integer index)))
  (decf index)
  (unless *last-results*
    (format t "Please do a search before.~&"))
  (when *last-results*
    (let* ((bk (nth index *last-results*)))
      (format t "Registering ~a..." (models:title bk))
      (let ((new (models:save-book bk)))
        (models:add-to (models:default-place) new)
        (format t "done. id: ~a~&" (object-id new))))))

(defun total-pages (total)
  "Compute the number of pages given this total quantity."
  (multiple-value-bind (fl rest)
      (floor (/ total *page-size*))
    (if (= 0 rest)
        fl
        (incf fl))))

(defun next ()
  "Print next page of results (now Stock, should be the last function
  that printed results)."
  (when (< *current-page*
           (total-pages (length *last-results*)))
    (incf *current-page*))
  (print-page *last-results*))

(defun previous ()
  "Print the previous page of results (the stock for now)."
  (when (> *current-page* 1)
    (decf *current-page*))
  (print-page *last-results*))

(defun print-page (seq &optional (page *current-page*))
  "Usage: print-page <full list of results>,
   and it prints respecting the list page by page."
  (setf *last-results* seq)
  (setf *last-page* (sublist seq
                             (* (- page 1) *page-size*)
                             (*  page *page-size*)))
  (format t (_ "Results: ~a. Page: ~a/~a~&")
          (length seq)
          page
          (total-pages (length seq)))
  (mapcar (lambda (it)
            (models:print-obj it))
          *last-page*))

(defun stock (&optional title-kw &rest rest)
  "Show our stock (books in DB)."
  (let* ((query (if title-kw (str:join "%" (cons title-kw rest))))
         (results (models:find-book :query query)))
    (setf *last-search* query)
    (print-page results *current-page*)))

(defun details (pk/title)
  "Print all information about the book of the given id/title.

   You can complete the argument with the TAB key."
  (let ((pk))
    ;; We have either an id (str) or a title. Consider the id first, fallback to the title.
    (when (stringp pk/title)
      (setf pk (ignore-errors
                 (parse-integer pk/title))))
    (models:print-book-details (or pk pk/title))))

;; Get a list of ids of the last search.
;; Specially handy when we have filtered the search.
(replic.completion:add-completion "details" #'last-page-book-ids)

(defun stats (&optional arg)
  "Print some numbers about the stock.

   Prints the total number of books and ones without isbn.

   If given an argument (use the TAB key to choose it), print the list of results."
  (format t "Books in stock: ~a~&" (models:count-book))
  (let ((res (models:find-book-noisbn)))
    (format t "Books without isbn: ~a (~,2f%)~&" (length res) (percentage (length res) (models:count-book)))
    (str:string-case arg
      ("noisbn"
       (setf *current-page* 1)
       (format t "-----------------~&")
       (print-page res))
      ("negative"
       (let ((negative (models:negative-quantities)))
         (format t "~a book(s) have a negative stock:~&" (length negative))
         (mapcar (lambda (it)
                   (format t "~2a- ~35a ~2a- ~20a: x~a~&"
                           (object-id (models:place-copies-book it))
                           (models:title it)
                           (object-id (models:place it))
                           (models:name it)
                           (print-quantity-red-green (models:quantity it))))
                 negative))))))

(replic.completion:add-completion "stats" '("noisbn" "negative"))

(defun create (&optional what)
  "Create a new book or a new place."
  (unless what
    (setf what "book"))
  (when (symbolp what)
    (string-downcase (symbol-name what)))
  (cond
    ((string-equal what "book")
     (create-book))
    ((string-equal what "place")
     (create-place))
    (t
     (format t "Unrecognized command. Nothing to do."))))

(defun create-book-form ()
  "Ask for data, return a book object, but don't save it on DB yet.
   Function used for book creation and edition."
  ;; Next, we want to create this form with class introspection and additional model fields (required, etc).
  (let (title authors price quantity)
    (setf title (rl:readline :prompt (format nil (str:concat "Title"
                                                  (cl-ansi-text:red "*")
                                                  " ? "))))
    (when (str:blank? title)
      (error "The title field is mandatory, please try again."))
    (setf authors (rl:readline :prompt "Authors ? (comma separated) "))
    (setf price (rl:readline :prompt "Price ? [0]"))
    (if (str:blank? price)
        (setf price 0)
        (setf price (parse-integer price)))
    (setf quantity (rl:readline :prompt "Quantity ? [0]"))
    (if (str:blank? quantity)
        (setf quantity 0)
        (setf quantity (parse-integer quantity)))
    (models:make-book :title title :authors authors :price price)))

(defun create-book ()
  "Create a new book."
  ;; next step: class and column introspection, data validation,
  ;; completion of fields etc.
  (let (bk)
    (setf bk (create-book-form))
    (models:save-book bk)
    (models:add-to (models:default-place) bk)
    ;; set this for completion of ids of other commands.
    (setf *last-page* (list bk))
    (models:print-obj bk)))

(defun create-place ()
  "Interactively create a new place."
  (let (name)
    (setf name (rl:readline :prompt (str:concat "Name" (cl-ansi-text:red "*") " ? ")))
    (when (str:blank? name)
      (error "The name field is mandatory, please try again."))
    (models::create-place name)))

(replic.completion:add-completion "create" '("book"
                                             "place"))

(defun delete (what &rest kw)
  "Delete, after confirmation, the books or places whose title (or name) match the given keywords. Also accepts ids as arguments.

   Usage: delete books/places <keywords or ids>

   For example, `delete books onc tim` will find a book with title 'once upon a time'."
  ;; (unless (eql (type-of kw) 'cons)
  ;;   (error "foo"))
  (let* ((what (str:string-case what
                 ("books"
                  #'models:find-book)
                 ("places"
                  #'models:find-places)
                 (t
                  (error "We don't know how to delete '~a'. Please give one of 'books', 'places' as the first argument (use TAB-completion)." what))))
         (objlist (when kw
                    (funcall what (str:join "%" kw)))))
    (if objlist
        (progn
          (print-page objlist)
          (finish-output)
          ;TODO: confirm: use eval in the repl, readline in terminal.
          (when (replic:confirm :prompt (_ "Do you want to delete all of these ?"))
            (models:delete-objects objlist)))
        (format t (_ "~&No results, nothing to do.~&")))))

(replic.completion:add-completion "delete" '("books"
                                             "places"))

;;
;; Places
;;

(defun places (&optional name &rest rest)
  "Show a summary of all places or the given one.

   If print-details is t, print a paginated list of books inside this place."
  ;; a name can be of many words. Join them.
  (when rest
    (setf name (cons name rest)))
  (let ((models::*print-details* name))
    (mapcar #'models:print-place (models:find-places name))))

(defun place-names ()
  (mapcar #'models:name (models:find-places)))

(defun contact-names ()
  (mapcar #'models:name (models:find-contacts)))

(defun parse-quantity (rest)
  "Given a list of strings, extract the integer if the last element starts with an x.
   For example, '(\"rst\" \"x3\") will return a quantity of 3."
  (when (str:starts-with? "x" (car (last rest)))
    (parse-integer (str:substring 1 t (car (last rest))))))

(defun move (bk name &rest rest)
  "Move a book to the given place.

  Give the book id and the place name (use TAB-completion).
  The place of origin is the current one we are in. Change it with the command 'inside ...' (use TAB completion again)."
  (when (stringp bk)
    (setf bk (parse-integer bk)))
  (let* ((book (models:find-by :id bk))
         (quantity (or (parse-quantity rest)
                       1))
         (name (if (str:starts-with? "x" (car (last rest)))
                   (cons name (butlast rest))
                   (cons name rest)))
         (place (models:find-place-by :name (str:unwords name))))
    (models:move book place :quantity quantity)))

(replic.completion:add-completion "move" #'place-names)

(defun contacts (&optional name)
  "Show our contacts and the books they borrowed."
  (let ((models::*print-details* t))
    (mapcar #'models:print-contact (models:find-contacts name))))

(defun loans (&optional name)
  ;TODO: filter by name
  "Print who borrowed what book and since when, ordered by date (oldest first)."
  (setf *last-page* (models::loans :name name)))

(replic.completion:add-completion "contacts" #'contact-names)
(replic.completion:add-completion "loans" #'contact-names)

(defun receive (bk &optional contact)
  "Mark this book as returned. Give an optional contact as second parameter."
  (when (stringp bk)
    (setf bk (parse-integer bk)))
  (when (and contact
             (stringp contact))
    (warn "this is actually untested :D")
    (setf contact (first (models:find-contact-by :name contact))))
  (let ((book (models:find-by :id bk)))
    (models::receive book contact)))

(replic.completion:add-completion "receive" (lambda ()
                                              (append (last-page-book-ids)
                                                      (contact-names))))

(defvar *yes-p* nil
  "For development: set to t and bypass some confirmation questions.")

(defun lend (bk name &rest rest)
  "Lend a book to a contact.

   We expect the book to come back. When it exceeds some time (2 months by default), show an alert."
  (when (stringp bk)
    (setf bk (parse-integer bk)))
  (when rest
    (setf name (str:unwords (cons name rest))))
  (let* ((book (models:find-by :id bk))
         (res (models:find-contact-by :name name))
         contact)
    (if res
        (setf contact (first res))
        ;; Create a new contact on the fly.
        (when (or *yes-p*
                  (replic:confirm :prompt (format nil "Create the new contact ~a ?~&" name)))
          (log:info "Creating new contact: ~a~&" name)
          (push (models:create-contact name) res)))
    (if (> (length res) 1)
        (format t "We found more than one contact matching this query. Please adjust it.")
        (progn
          (setf contact (first res))
          (models::lend book contact)
          (format t "Lended ~a to ~a~&" (models:title book) (models:name contact))))))

(replic.completion:add-completion "lend" (lambda ()
                                           (append (last-page-book-ids)
                                                   (contact-names))))



;;
;; Others
;;

(defun inside (&rest rest)
  "Print the current place, or change it."
  (if rest
      ;; The name of the place can be of several words.
      (let* ((name (str:unwords rest))
             (place (models:find-place-by :name name)))
        (setf models:*current-place* place)
        (setf replic:*prompt-prefix* (format nil "(~a) " (models:name models:*current-place*)))
        (format t "Now inside ~a.~&" name))
      (progn
        (format t "Current place: ~a.~&" (models:name (models:current-place))))))

(replic.completion:add-completion "places" #'place-names)
(replic.completion:add-completion "inside" #'place-names)

(defun fortune ()
  (if (probe-file "/usr/games/fortune")
      (uiop:run-program "/usr/games/fortune" :output *standard-output*)
      (format t "nothing in /usr/games/fortune, man.~&")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reset ()
  "For use in the repl."
  (setf *last-results* nil)
  (setf *last-search* nil)
  (setf *page-size* 15)
  (setf *current-page* 1)
  (setf models:*current-place* (models:default-place)))
