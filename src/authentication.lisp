(in-package #:bookshops.models)

(defclass user (mito-auth:has-secure-password)
  ((name :col-type (:varchar 60)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 255)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class))

(defclass user-role ()
  ((user :col-type user
         :initarg :user
         :accessor user-role-user)

   (role :col-type (:varchar 50)
         :initarg :role
         :inflate (alexandria:compose #'alexandria:make-keyword #'string-upcase)
         :deflate #'string-downcase
         :accessor user-role-role))
  (:metaclass mito:dao-table-class)
  (:primary-key user role)
  (:auto-pk nil)
  (:record-timestamps nil))

(defmethod can:user-roles ((user user))
  (mapcar #'user-role-role
          (mito:retrieve-dao 'user-role :user user)))

#+ (or)
(defmethod can:user-roles-for-resource ((user user) resource)
  (user-roles user))

(defun login (email password)
  "Return the user if the credentials are correct, otherwise nil."
  (let ((user (mito:find-dao 'user :email email)))
    (when (and user
               (mito-auth:auth user password))
      user)))

(defmacro define-role-access (resource action role)
  `(defmethod can:resource-allowed-p ((resource (eql ',resource))
                                      (action (eql ',action))
                                      (role (eql ',role)))
     (declare (ignore resource action role))
     t))
