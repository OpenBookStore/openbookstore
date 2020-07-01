(in-package #:bookshops-web)

(defun current-user ()
  (hunchentoot:session-value :user))

(defun web-login (email password)
  (let ((user (bookshops.models:login email password)))
    (when user
      (setf (hunchentoot:session-value :user) user))))

(defun logout ()
  (setf (hunchentoot:session-value :user) nil))

(defparameter +permission-denied.html+ (djula:compile-template* "permission-denied.html"))
(defparameter +login.html+ (djula:compile-template* "login.html"))

(defmacro render-template* (template &optional stream &rest template-arguments)
  (let ((template-arguments
         (list* :current-user '(current-user) template-arguments)))
    `(djula:render-template* ,template ,stream ,@ template-arguments)))

(djula:def-filter :user-name (user)
  (bookshops.models::user-name user))

(djula:def-filter :user-roles (user)
  ;; iterate the roles, convert them to lower case and ensure there's a trailing comma.
  (format nil "~{~(~a~)~^, ~}" (can:user-roles user)))

(defroute login-route ("/login" :method :get) ()
  (render-template* +login.html+ nil))

(defroute post-login-route ("/login" :method :post)
    ((email :parameter-type 'string :init-form "")
     (password :parameter-type 'string :init-form ""))
  (web-login email password)
  (hunchentoot:redirect "/"))

(defroute post-logout-route ("/logout" :method :post) ()
  (logout)
  (hunchentoot:redirect "/login"))

(defroute get-logout-route ("/logout" :method :get) ()
  (hunchentoot:redirect "/login"))

(defun @check-roles (route-name next)
  (let ((current-user (current-user)))
    (cond
      ((and current-user
            (can:can current-user :view route-name))
       (funcall next))

      (t (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
         (render-template* +permission-denied.html+ nil)))))

;;; TODO introduce a render-template macro that will provide the current-user
;;; argument already for convenience
