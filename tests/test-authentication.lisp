(in-package #:bookshops-test)

(with-empty-db
  (openbookstore.models:bootstrap-base-roles)
  (let ((user
         (openbookstore.models:create-user "Joe Blogg" "JoeBlogg@example.com" "secret")))
    (openbookstore.models:add-role user :admin)
    (assert (member :visitor (can:user-roles user))
            nil "Visitor should be an inherited role for the admin role.")
    (let ((login-attempt (openbookstore.models:login "JoeBlogg@exmaple.com" "wrong")))
      (assert (null login-attempt) nil "Should not have authenticated with wrong password"))
    (let ((login-attempt (openbookstore.models:login "JoeBlogg@example.com" "secret")))
      (assert (typep login-attempt 'openbookstore.models:user)
              nil "login does not return the user when they login successfully"))))
