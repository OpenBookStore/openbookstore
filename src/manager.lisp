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
  (:use :cl
        :mito
        :cl-ansi-text)
  (:import-from :bookshops.models
                :create-superuser
                :create-role
                :user)
  (:export :manage))

(in-package :bookshops.manager)

(defun %add-superuser% (name email password)
  (create-role :admin)
  (create-superuser name email password))

(defun add-superuser ()
  (let (name email password)
    (setf name (rl:readline :prompt
                            (format nil (str:concat "Enter username"
                                                    (cl-ansi-text:red "*")
                                                    " ? "))))
    (cond ((str:blank? name)
           (error "The username field is mandatory, please try again."))
          ((mito:find-dao 'user :name name)
           (error "Username ~a already exists" name))
          (t nil))

    (setf email (rl:readline :prompt
                             (format nil (str:concat "Enter email"
                                                     (cl-ansi-text:red "*")
                                                     " ? "))))
    (cond ((str:blank? email)
           (error "The email field is mandatory, please try again."))
          ((mito:find-dao 'user :email email)
           (error "User with email ~a already exists" email))
          (t nil))

    (setf password (rl:readline :prompt
                                (format nil (str:concat "Enter password"
                                                     (cl-ansi-text:red "*")
                                                     " ? "))))
    (cond ((str:blank? password)
           (error "The password field is mandatory, please try again."))
          (t nil))

    (%add-superuser% name email password)))

(defun manage (&optional what)
  (funcall (symbol-function (read-from-string (format nil "bookshops.manager::~a" what)))))

(replic.completion:add-completion "manage" '("add-superuser"))
