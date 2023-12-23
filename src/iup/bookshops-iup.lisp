(eval-when (:compile-toplevel)
  (ql:quickload :iup))

(defpackage :bookshops.iup
  (:use :cl
        :openbookstore.models)
  (:import-from :openbookstore
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
           (button (iup:button :title "Click me"
                               :expand :yes
                               :tip "yes, click me"
                               :action (lambda (handle)
                                         (declare (ignorable handle))
                                         (iup:message "title" "button clicked")
                                         iup:+default+)))
           (vbox
            (iup:vbox (list label button)
                      :gap "10"
                      :margin "10x10"
                      :alignment :acenter))
           (dialog (iup:dialog vbox :title "Hello, World!")))
      (iup:show dialog)
      (iup:main-loop))))

(defun counter ()
  (iup:with-iup ()
    (let* ((counter (iup:label :title 0))
           (label (iup:label :title (format nil "The button was clicked ~a time(s)."
                                            (iup:attribute counter :title))))
           (button (iup:button :title "Click me"
                               :expand :yes
                               :tip "yes, click me"
                               :action (lambda (handle)
                                         (declare (ignorable handle))
                                         (setf (iup:attribute counter :title)
                                               (1+ (iup:attribute counter :title 'number)))
                                         (setf (iup:attribute label :title)
                                               (format nil "The button was clicked ~a times."
                                                       (iup:attribute counter :title)))
                                         iup:+default+)))
           (vbox
            (iup:vbox (list label button)
                      :gap "10"
                      :margin "10x10"
                      :alignment :acenter))
           (dialog (iup:dialog vbox :title "Counter")))
      (iup:show dialog)
      (iup:main-loop))))

(defun run-counter ()
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (counter)))

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

(defun list-test ()
  (iup:with-iup ()
    (let*  ((list-1 (iup:list :tip "List 1"  ;; tooltip
                              :expand :yes
                              ;; multiple selection
                              :multiple :yes))
            (list-2 (iup:list :value 2   ;; default index of the selected row
                              :expand :yes
                              :tip "List 2"))
            (list-3 (iup:list :value 9 :tip "List 3" :expand :yes))
            (frame (iup:frame
                    (iup:hbox
                     (progn
                       ;; display a list of integers.
                       (loop for i from 1 upto 10
                          do (setf (iup:attribute list-1 i)
                                   (format nil "~A" i))
                          do (setf (iup:attribute list-2 i)
                                   (format nil "~A" (+ i 10)))
                          do (setf (iup:attribute list-3 i)
                                   (format nil "~A" (+ i 50))))
                       ;; vbox wants a list of widgets.
                       (list list-1 list-2 list-3)))

                    :title "IUP List"))
            (dialog (iup:dialog frame :menu "menu" :title "List example")))

      (iup:map dialog)
      (iup:show dialog)
      (iup:main-loop))))

(defun run-list-test ()
  #-sbcl (hello)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (list-test)))

(defun main ()
  #-sbcl (hello)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (restart-case
        (bookslist)
      (connect-db ()
        :report "Connect to the DB"
        (openbookstore.models:connect)
        (main)))))
