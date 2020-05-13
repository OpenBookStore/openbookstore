;;;;
;;;; Show the stock on a web page.
;;;; (in construction, currently not used)
;;;;
;;;; Go to localhost:4242/stock/
;;;;
;;;;

(defpackage bookshops-web
  (:use :cl
        :bookshops.models
        :hunchentoot
        :log4cl)
  (:import-from :easy-routes
                :routes-acceptor
                :defroute))

(in-package :bookshops-web)


(djula:add-template-directory
 (asdf:system-relative-pathname "bookshops" "src/web/templates/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +dashboard.html+ (djula:compile-template* "dashboard.html"))

(defroute home-route ("/") ()
  (djula:render-template* +dashboard.html+ nil
                          :data (list :nb-titles (bookshops.models:count-book))))

(defvar *server* nil
  "Current instance of easy-acceptor.")

(defun start-app (&key (port 4242))
  (bookshops.models:connect)
  (setf *server* (make-instance 'routes-acceptor :port port))
  (start *server*))

(defun stop-app ()
  ;; disconnect db ?
  (stop *server*))
