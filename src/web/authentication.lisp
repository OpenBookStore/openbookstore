(in-package #:openbookstore/web)

(defun current-user ()
  (hunchentoot:session-value :user))

(defun web-login (email password)
  (let ((user (openbookstore.models:login email password)))
    (when user
      (setf (hunchentoot:session-value :user) user))))

(defun logout ()
  (setf (hunchentoot:session-value :user) nil))

;; We define these templates in web.lisp, where we add code to compile templates in memory (for binary releases).
;; (defparameter +no-nav-base.html+ (djula:compile-template* "no-nav-base.html"))
;; (defparameter +permission-denied.html+ (djula:compile-template* "permission-denied.html"))
;; (defparameter +login.html+ (djula:compile-template* "login.html"))

(defmacro render-template* (template &optional stream &rest template-arguments)
  (let ((template-arguments
         (list* :current-user '(current-user)
                :current-user-roles '(mapcar #'string-downcase (can:user-roles (current-user)))
                :request-uri '(hunchentoot:request-uri*)
                template-arguments)))
    `(djula:render-template* ,template ,stream ,@ template-arguments)))

(djula:def-filter :user-name (user)
  (openbookstore.models::user-name user))

(djula:def-filter :user-roles (user)
  ;; iterate the roles, convert them to lower case and ensure there's a trailing comma.
  (format nil "~{~(~a~)~^, ~}" (can:user-roles user)))

(defroute login-route ("/login" :method :get)
    ((referer-route :parameter-type 'string :init-form "/"))
  (render-template* +login.html+ nil
                    :referer-route referer-route))

(defroute post-login-route ("/login" :method :post)
    ((email :parameter-type 'string :init-form "")
     (password :parameter-type 'string :init-form "")
     (referer-route :parameter-type 'string :init-form "/"))
  (web-login email password)
  (hunchentoot:redirect referer-route))

(defroute post-logout-route ("/logout" :method :post) ()
  (logout)
  (hunchentoot:redirect "/login"))

(defroute get-logout-route ("/logout" :method :get)
    ()
  (hunchentoot:redirect "/login"))

;;; TODO maybe it will be better to wrap hunchentoot:redirect later on
;;; (for this use case anyways) so that can ensure parmeters exist without
;;; potentially clobbering existing params.
(defun login-redirect ()
  (hunchentoot:redirect (format nil "/login?referer-route=~a" (hunchentoot:request-uri*))))

(defun @check-roles (route-name next)
  (let ((current-user (current-user)))
    (cond
      ((not current-user)
       (login-redirect))

      ((can:can current-user :view route-name)
       (funcall next))

      (t (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
         (render-template* +permission-denied.html+ nil)))))

;;; TODO introduce a render-template macro that will provide the current-user
;;; argument already for convenience
