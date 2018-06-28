(defpackage bookshops.commands
  (:use :cl
        :bookshops.utils
        :mito
        :cl-ansi-text)
  (:shadow :search
           :delete)
  (:import-from :bookshops
                :books)
  (:import-from :bookshops.models
                :book
                :make-book
                :save-book
                :find-book
                :find-book-noisbn
                :find-by
                :print-obj
                :print-book-details
                :count-book
                :title
                :editor
                :authors
                :quantity
                :add-to
                :set-quantity
                :delete-books
                :delete-objects
                :price
                ;; places
                :place-copies-book
                :place-copies-place
                :print-place
                :name
                :find-places
                :find-place-by
                :default-place
                :*current-place*
                ;; utils
                :print-quantity-red-green
                )
  (:export :main
           :search
           :add
           :details
           :stock
           :next
           :previous
           :stats
           :create
           :delete
           :places
           :move
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
         (results (books query))
         (i (length results)))
    (mapcar (lambda (it)
              (format t "~2@a- ~a, ~a~t~a stock ? x~a~&" i
                      (blue (title it))
                      (authors it)
                      (price it)
                      (print-quantity-red-green (quantity it)))
              (decf i))
            (reverse results))))

(defun add (index)
  "Add this book (by index of the last search) into the DB."
  (when (stringp index)
    ;; generics ?
    (setf index (parse-integer index)))
  (decf index)
  (unless bookshops::*last-results*
    (format t "Please do a search before."))
  (when bookshops::*last-results*
    (let* ((bk (nth index bookshops::*last-results*)))
      (format t "Gonna register ~a~&" (title bk))
      (save-book bk)
      (print "done."))))

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
            (print-obj it))
          *last-page*))

(defun stock (&optional title-kw &rest rest)
  "Show our stock (books in DB)."
  (let* ((query (if title-kw (str:join "%" (cons title-kw rest))))
         (results (find-book query)))
    (setf *last-search* query)
    (print-page results *current-page*)))

(defun details (pk)
  "Print all information about the book of the given id.

   You can complete the argument with the TAB key."
  (when (stringp pk)
    (setf pk (parse-integer pk)))
  (print-book-details pk))

;; Get a list of ids of the last search.
;; Specially handy when we have filtered the search.
(replic.completion:add-completion "details" (lambda ()
                                              (mapcar (lambda (it)
                                                        (prin1-to-string (object-id it)))
                                                      *last-page*)))

(defun stats (&optional arg)
  "Print some numbers about the stock.

   Prints the total number of books and ones without isbn.

   If given an argument (use the TAB key to choose it), print the list of results."
  (format t "Books in stock: ~a~&" (count-book))
  (let ((res (find-book-noisbn)))
    (format t "Books without isbn: ~a (~,2f%)~&" (length res) (percentage (length res) (count-book)))
    (str:string-case arg
      ("noisbn"
       (setf *current-page* 1)
       (format t "-----------------~&")
       (print-page res))
      ("negative"
       (let ((negative (bookshops.models:negative-quantities)))
         (format t "~a book(s) have a negative stock:~&" (length negative))
         (mapcar (lambda (it)
                   (format t "~2a- ~35a ~2a- ~20a: x~a~&"
                           (object-id (place-copies-book it))
                           (title it)
                           (object-id (place-copies-place it))
                           (name it)
                           (print-quantity-red-green (quantity it))))
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

(defun create-book ()
  "Create a new book."
  ;; next step: class and column introspection, data validation,
  ;; completion of fields etc.
  (let (bk title authors price quantity)
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

    (setf bk (make-book :title title :authors authors :price price))
    ;; xxx save-book increments quantity
    (save-book bk)
    (add-to (default-place) bk)
    ;; set this for completion of ids of other commands.
    (setf *last-page* (list bk))
    (print-obj bk)))

(defun create-place ()
  "Interactively create a new place."
  (let (name)
    (setf name (rl:readline :prompt (str:concat "Name" (cl-ansi-text:red "*") " ? ")))
    (when (str:blank? name)
      (error "The name field is mandatory, please try again."))
    (bookshops.models::create-place name)))

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
                  #'find-book)
                 ("places"
                  #'find-places)
                 (t
                  (error "We don't know how to delete '~a'. Please give one of 'books', 'places' as the first argument (use TAB-completion)." what))))
         (objlist (when kw
                    (funcall what (str:join "%" kw)))))
    (if objlist
        (progn
          (print-page objlist)
          (finish-output)
          ;TODO: confirm: use eval in the repl, readline in terminal.
          (when (replic:confirm (_ "Do you want to delete all of these ?"))
            (delete-objects objlist)))
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
  (let ((bookshops.models::*print-details* name))
    (mapcar #'print-place (find-places name))))

(defun place-names ()
  (mapcar #'name (bookshops.models:find-places)))

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
  (let* ((book (find-by :id bk))
         (quantity (or (parse-quantity rest)
                       1))
         (name (if (str:starts-with? "x" (car (last rest)))
                   (cons name (butlast rest))
                   (cons name rest)))
         (place (find-place-by :name (str:unwords name))))
    (bookshops.models:move book place :quantity quantity)))

(replic.completion:add-completion "move" #'place-names)

(defun inside (&rest rest)
  "Print the current place, or change it."
  (if rest
      ;; The name of the place can be of several words.
      (let* ((name (str:unwords rest))
             (place (find-place-by :name name)))
        (setf *current-place* place)
        (setf replic:*prompt-prefix* (format nil "(~a) " (name *current-place*)))
        (format t "Now inside ~a.~&" name))
      (progn
        (format t "Current place: ~a.~&" (name (current-place))))))

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
  (setf *current-place* (default-place)))
