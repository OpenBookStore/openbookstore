(in-package #:bookshops-web)

(defun current-user ()
  (hunchentoot:session-value :user))

(defun login (email password)
  (let ((user (bookshops.models::login email password)))
    (when user
      (setf (hunchentoot:session-value :user) user))))

(defun logout ()
  (setf (hunchentoot:session-value :user) nil))

(djula:add-template-directory
 (asdf:system-relative-pathname "bookshops" "src/web/templates/"))
(defparameter +permission-denied.html+ (djula:compile-template* "permission-denied.html"))
(defparameter +login.html+ (djula:compile-template* "login.html"))

(djula:def-filter :user-name (user)
  (bookshops.models::user-name user))


(defroute login-route ("/login" :method :get) ()
  (djula:render-template* +login.html+ nil
                          :current-user (current-user)))

(defroute post-login-route ("/login" :method :post)
    ((email :parameter-type 'string :init-form "")
     (password :parameter-type 'string :init-form ""))
  (login email password)
  (djula:render-template* +login.html+ nil
                          :current-user (current-user)))

(defroute post-logout-route ("/logout" :method :post) ()
  (logout)
  (hunchentoot:redirect "/login"))

(defroute get-logout-route ("/logout" :method :get) ()
  (hunchentoot:redirect "/login"))

(defun @check-roles (roles next)
  (let ((current-user (current-user)))
    (cond
      ((and current-user
            (apply #'bookshops.models::authorize current-user roles))
       (funcall next))

      (t (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
         (djula:render-template* +permission-denied.html+ nil
                                 :roles roles
                                 :current-user current-user)))))
