
(eval-when (:compile-toplevel)
  (ql:quickload :iup))

(defpackage :bookshops.iup
  (:use :cl)
  (:import-from :bookshops
                :books
                :authors
                :editor
                :price)
  (:import-from :bookshops.models
                :find-book
                :last-books)
  (:export :main))

(in-package :bookshops.iup)

(defun hello ()
  (iup:with-iup ()
    (let* ((label (iup:label :title (format nil "Hello, World!~%IUP ~A~%~A ~A"
                                            (iup:version)
                                            (lisp-implementation-type)
                                            (lisp-implementation-version))))
           (dialog (iup:dialog label :title "Hello, World!")))
      (iup:show dialog)
      (iup:main-loop))))

(defun bookslist ()
  (iup:with-iup ()
    (let*  ((books (books "antigone"))
            (frame (iup:frame
                    (iup:vbox (loop for list in (list (iup:list :value 1 :tip "List 1" :multiple :yes)
                                                      (iup:list :value 2 :tip "list 2" :dropdown :yes)
                                                      (iup:list :value 3 :tip "List 3" :editbox :yes))
                                 do (loop for i from 1 upto 3
                                       do (setf (iup:attribute list i)
                                                (format nil "Item ~A" i)))
                                 collect list))
                    :title "IUP List"))
            (dialog (iup:dialog frame :menu "menu" :title "a title")))
      (iup:map dialog)
      (iup:show dialog)
      (iup:main-loop))))

(defun main ()
  #-sbcl (hello)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (bookslist)))
