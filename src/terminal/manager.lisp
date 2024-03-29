#|

A way to create custom commands

Just add a function like ADD-SUPERUSER (after MANAGE function definition) and add its string name
to the replic call

For example

'''
...

(defun foo ()         ;; your function/command
  (print 'FOO))
....

(defun manage (...)) ;; MANAGE definition

(replic.completion:add-completion "manage" '("add-superuser" "foo"))

'''

Then, it can be invoked from the binary application:

$ ./bookshops -i
(home) bookshops > manage foo


|#

(defpackage bookshops.manager
  (:use :cl)
  (:import-from :openbookstore.models
                :create-superuser
                :create-role
                :search-user
                :list-admin-users
                :is-superuser
                :remove-user
                :user)
  (:export :manage))

(in-package :bookshops.manager)

(defun %add-superuser% (name email password)
  (create-role :admin)
  (create-superuser name email password))

(defun add-superuser (&optional name-input email-input password-input)
  "Command for create a custom superuser"
  (let ((name name-input)
        (email email-input)
        (password password-input))

    (if (not name)
        ;; this %readline works on readline and Slime too.
        (setf name (bookshops.commands::%readline :prompt
                                (format nil (str:concat "Enter username"
                                                        (cl-ansi-text:red "*")
                                                        " ? ")))))
    (cond ((str:blank? name)
           (error "The username field is mandatory, please try again."))
          ((search-user :name name)
           (error "Username ~a already exists" name))
          (t nil))

    (if (not email)
        (setf email (bookshops.commands::%readline :prompt
                             (format nil (str:concat "Enter email"
                                                     (cl-ansi-text:red "*")
                                                     " ? ")))))
    (cond ((str:blank? email)
           (error "The email field is mandatory, please try again."))
          ((search-user :email email)
           (error "User with email ~a already exists" email))
          (t nil))

    (if (not password)
        (setf password (bookshops.commands::%readline :prompt
                                    (format nil (str:concat "Enter password"
                                                            (cl-ansi-text:red "*")
                                                            " ? ")))))
    (cond ((str:blank? password)
           (error "The password field is mandatory, please try again."))
          (t nil))

    (%add-superuser% name email password)

    (format t "User ~a was created successfully~%" name)))


(defun list-superusers ()
  "Command for list all superusers in the system"
  (dolist (admin (list-admin-users :pprint-result t))
    (format t "~a ~%" admin)))


(defun remove-superuser (&optional name-or-email-input)
  "Command for remove a superuser for its name or email"
  (let ((name-or-email name-or-email-input)
        user)
    (if (not name-or-email)
        (setf name-or-email
              (rl:readline :prompt
                           (format nil (str:concat "Enter username or email"
                                                   (cl-ansi-text:red "*")
                                                   " ? ")))))
    (setf user (or (search-user :name name-or-email)
                   (search-user :email name-or-email)))

    (cond ((str:blank? name-or-email)
           (error "The field is mandatory, please try again."))
          ((not user)
           (error "The user not exists"))
          ((not (is-superuser user))
           (error "The user is not a superuser"))
          (t nil))

    (remove-user user)

    (format t "User was deleted successfully ~%")))

(defun manage (&optional what &rest params)
  (apply (symbol-function (read-from-string (format nil "bookshops.manager::~a" what))) params))

(replic.completion:add-completion "manage" '("add-superuser" "list-superusers" "remove-superuser"))
