
(eval-when (:compile-toplevel)
  (ql:quickload :iup))

(defpackage :bookshops.iup
  (:use :cl
        :bookshops.models)
  (:import-from :bookshops
                :books
                :authors
                :editor
                :price)
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

(defun gridexample ()
  (iup:with-iup ()
    (let* ((books (find-book))
           (flat-results (mapcan (lambda (bk) (list (title bk) (authors bk)))
                                 books))
           (button (iup:flat-button :expand :yes :canfocus :no))
           (label (iup:label :expand :horizontal :title "#x00000" :alignment "ACENTER:ACENTER"))
           (r (iup:val :expand :horizontal :min 0 :max 255))
           (g (iup:val :expand :horizontal :min 0 :max 255))
           (b (iup:val :expand :horizontal :min 0 :max 255))
           (vbox (iup:vbox
                  (list (iup:grid-box
                         (loop for elt in flat-results
                            collect (iup:label :title elt))
                         ;; (list (iup:label :title "&Red")
                         ;;       (iup:label :title "foo")
                         ;;       (iup:label :title "&Green") g
                         ;;       (iup:label :title "&Blue")  b)
                         :numdiv 2
                         :expandchildren "HORIZONTAL"
                         :cgapcol 10
                         :cgaplin 5)
                        button
                        label)
                  :cmargin 5
                  :cgap 5
                  :margin "x5"))
           (dialog (iup:dialog vbox :title "Color Mixer Example" :size "QUARTERxQUARTER")))
      (loop :with action := (format t "~a ~a ~a ~a ~a" r g b button label)
            :for handle :in (list r g b)
            :do (setf (iup:callback handle :valuechanged_cb)  action))
      (iup:show dialog)
      (iup:main-loop))))

(defun grid ()
  #-sbcl (hello)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (gridexample)))


(defun bookslist ()
  (iup:with-iup ()
    (let*  ((books (find-book))
            (frame (iup:frame
                    (iup:vbox (loop for list in (list (iup:list :value 1 :tip "List 1" :multiple :yes))
                                 do (loop for i from 1 upto 10
                                       do (setf (iup:attribute list i)
                                                (format nil "~A"
                                                        (title (elt books (+ i 10))))))
                                 collect list))
                    :title "IUP List"))
            (dialog (iup:dialog frame :menu "menu" :title "OpenBookstore")))
      (iup:map dialog)
      (iup:show dialog)
      (iup:main-loop))))

(defun main ()
  #-sbcl (hello)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (bookslist)))
