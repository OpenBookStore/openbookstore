(in-package #:bookshops.models)

#|

Create a superuser:

(create-superuser name email password)

Create a normal user:

(bookshops.models::create-user "Joe Blogg" "JoeBlogg@example.com" "i<3books")

Give him rights:

(add-role user :admin)

Bootstrap roles: see database.lisp bootstrap-base-roles.

|#

(defclass user (mito-auth:has-secure-password)
  ((name :col-type (:varchar 60)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 255)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class))

(defclass role ()
  ((name :col-type (:varchar 50)
         :initarg :name
         :inflate (alexandria:compose #'alexandria:make-keyword #'string-upcase)
         :deflate #'string-downcase
         :accessor role-name))
  (:metaclass mito:dao-table-class)
  (:primary-key name)
  (:auto-pk nil)
  (:record-timestamps nil))

(defclass user-role ()
  ((user :col-type user
         :initarg :user
         :accessor user-role-user)

   (role :col-type role
         :initarg :role
         :accessor user-role-role))
  (:metaclass mito:dao-table-class)
  (:primary-key user role)
  (:auto-pk nil)
  (:record-timestamps nil))

(defclass role-copy ()
  ((primary-role :col-type role
                 :initarg :primary-role
                 :accessor primary-role)
   (inherited-role :col-type role
                   :initarg :inherited-role
                   :accessor inherited-role))
  (:metaclass mito:dao-table-class)
  (:primary-key primary-role inherited-role)
  (:auto-pk nil)
  (:record-timestamps nil))

(defun create-user (name email password)
  (mito:create-dao 'user :name name :email email :password password))

(defun create-superuser (name email password)
  "Create a user with the admin role."
  (let ((user (create-user name email password)))
    (add-role user :admin)))

(defun create-role (name)
  (or (mito:find-dao 'role :name name)
      (mito:create-dao 'role :name name)))

(defun %find-role-from-keyword (role-name)
  "Return a role object from a keyword and throw an error if an associated role cannot be found"
  (let ((role (mito:find-dao 'role :name role-name)))
    (assert role (role) "There is no role named ~a" role-name)
    role))

(defgeneric add-role (user role)
  (:documentation "Add the given role to this user. ROLE is either a role object or a symbol. An example role is ':admin`.")
  (:method ((user user) (role role))
    (or (mito:find-dao 'user-role :user user :role role)
        (mito:create-dao 'user-role :user user :role role)))
  (:method ((user user) (role-name symbol))
    (let ((role (%find-role-from-keyword role-name)))
      (add-role user role))))

(defgeneric inherit-role (primary-role inherited-role)
  (:method ((primary-role role) (inherited-role role))
    (or (mito:find-dao 'role-copy :primary-role primary-role :inherited-role inherited-role)
        (mito:create-dao 'role-copy :primary-role primary-role :inherited-role inherited-role)))
  (:method ((primary-role-name symbol) (inherited-role-name symbol))
    (let ((primary-role (%find-role-from-keyword primary-role-name))
          (inherited-role (%find-role-from-keyword inherited-role-name)))
      (inherit-role primary-role inherited-role))))

;;; We deal with all roles as keywords only once they are within the context of can.
(defmethod can:user-roles ((user user))
  "This is somewhat inefficent and would be better if user-role was a view with a union
between role-copy and role, however I have struggled to make mito work
with such a view.

It is permitted for a role to appear more than once in the result."
  (labels ((all-roles (role)
             (list* role
                    (loop :for role :in (mito:retrieve-dao 'role-copy :primary-role role)
                       :append (all-roles (inherited-role role))))))
    ;; here we move down the inheritance hierarchy by navigating the role-copy table
    ;; recursively until we reach the end of each branch and have collected all the roles.
    (let ((direct-roles
           (mapcar #'user-role-role (mito:retrieve-dao 'user-role :user user))))
      (mapcar #'role-name
              (loop :for role :in direct-roles
                 :append (all-roles role))))))

(defun login (email password)
  "Return the user if the credentials are correct, otherwise nil."
  (let ((user (mito:find-dao 'user :email email)))
    (when (and user
               (mito-auth:auth user password))
      user)))

(defmacro define-role (name (&rest inherits))
  `(progn
     (create-role ',name)
     ,@(loop :for role :in inherits
            :collect `(inherit-role ',name ',role))))

(defmacro define-role-access (resource action role)
  `(defmethod can:resource-allowed-p ((resource (eql ',resource))
                                      (action (eql ',action))
                                      (role (eql ',role)))
     (declare (ignore resource action role))
     t))
