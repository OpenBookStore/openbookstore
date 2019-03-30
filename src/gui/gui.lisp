(defpackage :bookshops.gui
  (:use :cl
        :nodgui)
  (:import-from :bookshops
                :books
                :authors
                :editor
                :price)
  (:import-from :bookshops.models
                :find-book
                :last-books)
  (:export :main))

(in-package :bookshops.gui)

(defparameter +results-columns-width+ nil
  "Width of the treeview columns. List of integers.")

(setf +results-columns-width+ '(300 200 200 80))

(defparameter *search-function* #'books
  "Function that accepts one argument, the text of the search input. Results are displayed in the tree. For now clicking on the side menu doesn't change widgets but only this function.")

(defun side-menu ()
  "Side menu: buttons to switch to Stock, Search,…"
  (let* ((frame (make-instance 'frame))
         (btn-stock (make-instance 'button
                                   :master frame
                                   :text "Stock"
                                   :command (lambda ()
                                              ;; Searching our stock should have more inputs
                                              ;; (author, sorting,...)
                                              (setf *search-function* #'find-book))))
         (btn-search (make-instance 'button
                                    :master frame
                                    :text "Search"
                                    :command (lambda ()
                                               (setf *search-function* #'books))))
         (row 0))
    (grid btn-stock row 0
          :sticky "w")
    (grid btn-search (incf row) 0
          :sticky "w")
    frame))

(defun search-tree ()
  "Search input and display search results in a scrollable tree."
  (let* ((frame (make-instance 'frame))
         (searchbox (grid (make-instance 'entry :width 50
                                         :master frame)
                          0 0 :sticky "we" :padx 5 :pady 5))
         (tree (make-instance 'scrolled-treeview
                              :master frame
                              ;; These are the second and third columns.
                              :columns (list "authors"
                                             "editor"
                                             "price")
                              :columns-width +results-columns-width+))
         (button (make-instance 'button
                                :master frame
                                :text "OK"
                                :command (lambda ()
                                           (format t "the treeview selection is: ~a~&"
                                                   (treeview-get-selection tree))
                                           (format t "text is: ~a~&" (text searchbox))
                                           ;; There is an error with "latin capital letters"
                                           ;; when searching "tears of steel".
                                           ;; => escape tildes in title. Fixed in nodgui.
                                           (insert-results tree
                                                           (funcall *search-function*
                                                                    (text searchbox)))))))

    (insert-results tree (last-books))

    ;; Name the first column:
    (treeview-heading tree +treeview-first-column-id+ :text "title")
    (print :RST)
    (grid searchbox 0 0
          :sticky "we")
    (grid button 0 1
          ;; stick to the right (east).
          :sticky "e")
    (grid tree 1 0
          ;; so the button doesn't have a column by itself.
          :columnspan 2
          ;; sticky by all sides, for resizing to do something.
          :sticky "nsew")
    (format t "returning frame ~a~&" frame)
    frame))

(defun insert-results (tree results)
  "Insert torrents last results into that treeview."
  ;; Clear content.
  ;; this needs nodgui newer than feb, 24th 2019
  ;; with commit c9ae0ec389.
  (treeview-delete-all tree)
  (loop for result in results
     do (treeview-insert-item tree
                              ;; title is only a nodgui generic method.
                              :text (bookshops:title result)
                              ;; xxx: numbers stick to the left instead of the right.
                              ;; note: repetition in the ordering and naming of columns.
                              :column-values (list (authors result)
                                                   (editor result)
                                                   (price result)))))

(defun main ()
  "Connect to the database and start the main GUI."
  (bookshops.models:connect)
  (with-nodgui ()
    (wm-title *tk* "OpenBookStore")
    (minsize *tk* 500 170)
    ;; For resizing to do something: weight > 0
    (grid-columnconfigure *tk* 0 :weight 1)

    (let ((row 0)
          (col 0))
      (declare (ignorable row))
      (grid (side-menu) 0 col
            :sticky "nsew")
      (grid (search-tree) 0 (incf col)
            :sticky "nsew"))))
