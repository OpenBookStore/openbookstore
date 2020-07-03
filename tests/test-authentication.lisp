(in-package #:bookshops-test)

(with-empty-db
  (bookshops.models:bootstrap-base-roles)
  (let ((user
         (bookshops.models:create-user "Joe Blogg" "JoeBlogg@example.com" "secret")))
    (bookshops.models:add-role user :admin)
    (assert (member :visitor (can:user-roles user))
            nil "Visitor should be an inherited role for the admin role.")
    (let ((login-attempt (bookshops.models:login "JoeBlogg@exmaple.com" "wrong")))
      (assert (null login-attempt) nil "Should not have authenticated with wrong password"))
    (let ((login-attempt (bookshops.models:login "JoeBlogg@example.com" "secret")))
      (assert (typep login-attempt 'bookshops.models:user)
              nil "login does not return the user when they login successfully"))))
