(defpackage bookshops.commands
  (:use :cl
        :mito
        :cl-ansi-text)
  (:shadow :search)
  (:import-from :bookshops
                :books)
  (:import-from :bookshops.models
                :book
                :make-book
                :save-book
                :find-book
                :print-book
                :print-book-details
                :count-book
                :title
                :editor
                :authors
                :quantity
                :price)
  (:export :main
           :search
           :add
           :details
           :stock
           :next
           :previous
           :stats
           :*page-size*))
(in-package :bookshops.commands)

(defvar *last-results* nil
  "List of last search results (for now, a list of book objects).")

(defvar *last-search* nil
  "List of keywords used on the last search. For example, \"stock ant\" to filter on titles.")

(defvar *page-size* 15
  "Maximum number of lines to show when printing results.")
(setf *page-size* 15)

(defvar *current-page* 1
  "Current page of the stock pager.")

(defun search (query &rest rest)
  "Search for books with `query` on `datasource`, nicely print the result."
  ;; the &rest is for the readline repl, that gives many string arguments to this function.
  (let* ((query (str:unwords (cons query rest)))
         (results (books query))
         (i (length results)))
    (mapcar (lambda (it)
              (format t "~2@a- ~a, ~a~t~a~&" i (blue (title it)) (authors it) (price it))
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

(defun sublist (seq start end)
  (if (> (length seq)
         end)
      (subseq seq start end)
      (subseq seq start (length seq))))

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
  (stock *last-search*))

(defun previous ()
  "Print the previous page of results (the stock for now)."
  (when (> *current-page* 1)
    (decf *current-page*))
  (stock *last-search*))

(defun stock (&optional title-kw)
  "Show our stock (books in DB)."
  (setf *last-search* title-kw)
  (setf *last-results* (find-book title-kw))
  (format t "Results: ~a. Page: ~a/~a~&"
          (length *last-results*)
          *current-page*
          (total-pages (length *last-results*)))
    (mapcar (lambda (it)
              (print-book it))
            (sublist *last-results*
                     (* (- *current-page* 1) *page-size*)
                     (*  *current-page* *page-size*))))

(defun details (pk)
  "Print all information about the book of the given id.

   You can complete the argument with the TAB key."
  (when (stringp pk)
    (parse-integer pk))
  (print-book-details pk))

;; Get a list of ids of the last search.
;; Specially handy when we have filtered the search.
(replic.completion:add-completion "details" (lambda ()
                                              (mapcar (lambda (it)
                                                        (prin1-to-string (object-id it)))
                                                      *last-results*)))

(defun stats ()
  "Print some numbers about the stock."
  (format t "Books in stock: ~a~&" (count-book)))
