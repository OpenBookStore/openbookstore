(defpackage :bookshops.gui
  (:use :cl
        :nodgui)
  (:import-from :bookshops
                :books
                :authors
                :editor
                :price)
  (:export :main))

(in-package :bookshops.gui)


(defun search-tree ()
  (with-nodgui ()
    (wm-title *tk* "OpenBookStore")
    (let* ((tree (make-instance 'scrolled-treeview
                                ;; These are the second and third columns.
                                :columns (list "authors"
                                               "editor"
                                               "price")))
           (searchbox (grid (make-instance 'entry :width 7)
                            0 0 :sticky "we" :padx 5 :pady 5))
           (button (make-instance 'button
                                  :text "OK"
                                  :command (lambda ()
                                             (format t "the treeview selection is: ~a~&"
                                                     (treeview-get-selection tree))
                                             (format t "text is: ~a~&" (text searchbox))
                                             ;; There is an error with "latin capital letters"
                                             ;; when searching "tears of steel".
                                             (insert-results tree
                                                             (books (text searchbox)))))))

      (minsize *tk* 500 170)
      ;; Name the first column:
      (treeview-heading tree +treeview-first-column-id+ :text "title")
      ;; For resizing to do something: weight > 0
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid searchbox 0 0
            ;; it goes below the button :S
            :columnspan 2)
      (grid button 0 1
            ;; stick to the right (east).
            :sticky "e")
      (grid tree 1 0
            ;; so the button doesn't have a column by itself.
            :columnspan 2
            ;; sticky by all sides, for resizing to do something.
            :sticky "nsew"))))

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
  (search-tree))
