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
                :title
                :editor
                :authors
                :price)
  (:export :main
           :search
           :add
           :stock))
(in-package :bookshops.commands)

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

(defvar *max-lines* 15
  "Truncate prints that exceed this number of lines.")

(defun sublist (seq start end)
  (if (> (length seq)
           end)
      (subseq seq start end))
  seq)

(defun stock ()
  "List our stock (books in DB)."
  (let ((all (find-book)))
    (format t "Results: ~a. Page: ~a~&" (length all) *max-lines*)
    (mapcar (lambda (it)
              (print-book it))
            (sublist all 0 *max-lines*))))
