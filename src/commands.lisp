(defpackage bookshops.commands
  (:use :cl
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
                :price)
  (:export :main
           :search
           :add
           :details
           :stock
           :stats))
(in-package :bookshops.commands)

(defvar *max-lines* 15
  "Truncate prints that exceed this number of lines.")
(setf *max-lines* 15)

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

(defvar *current-page* 1
  "Current page of the stock pager.")

(defun total-pages (total)
  "Compute the number of pages given this total quantity."
  (multiple-value-bind (fl rest)
      (floor (/ total *max-lines*))
    (if (= 0 rest)
        fl
        (incf fl))))

(defun next ()
  "Print next page of results (now Stock, should be the last function
  that printed results)."
  (when (< *current-page*
           (total-pages (count-book)))
    (incf *current-page*))
  (stock))

(defun previous ()
  "Print the previous page of results (the stock for now)."
  (when (> *current-page* 1)
    (decf *current-page*))
  (stock))

(defun stock ()
  "Show our stock (books in DB)."
  (let ((all (find-book)))
    (format t "Results: ~a. Page: ~a/~a~&"
            (length all)
            *current-page*
            (total-pages (count-book)))
    (mapcar (lambda (it)
              (print-book it))
            (sublist all
                     (* (- *current-page* 1) *max-lines*)
                     (*  *current-page* *max-lines*)))))

(defun details (pk)
  "Print all the book information."
  (when (stringp pk)
    (parse-integer pk))
  (print-book-details pk))

(defun stats ()
  "Print some numbers about the stock."
  (format t "Books in stock: ~a~&" (count-book)))
