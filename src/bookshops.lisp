(in-package :bookshops)

(defun init ()
  "Init i18n, connect to the DB,..."
  (bookshops.models:connect)
  (i18n-load)
  (log:config :error))

(defun handle-parser-error (c)
  (format t "Argument error: ~a~&" (opts:option c))
  ;; XXX: probably don't quit.
  (uiop:quit 1))

(defparameter +version+
  (let ((version (asdf/component:component-version (asdf:find-system :bookshops)))
        (directory (asdf:system-source-directory :bookshops)))
    (or (ignore-errors
          (uiop:with-current-directory (directory)
            (multiple-value-bind (current-commit)
                (uiop:run-program (list "git" "describe" "--always")
                                  :output '(:string :stripped t))
              (concatenate 'string
                           version
                           (format nil "-~a" current-commit)))))
        version))
  "The version number as in the asd appended with the current commit id.")

(defun search-books (query)
  "Search on datasources, get a list of hash-tables, transform them to book objects,
  and check if some already exist in our DB. In that case, update them."
  (let ((res (books query)))
    (loop for bk in res
       collect (find-existing (make-book
                               :title (access bk :title)
                               :isbn (access bk :isbn)
                               :authors (access bk :authors)
                               :details-url (access bk :details-url)
                               :cover-url (access bk :cover-url)
                               :publisher (access bk :publisher)
                               :date-publication (access bk :date-publication)
                               :price (access bk :price)
                               :datasource (access bk :datasource))
                              :update t))))

(defun print-system-info (&optional (stream t))
  ;; see also https://github.com/40ants/cl-info
  (format stream "~&OS: ~a ~a~&" (software-type) (software-version))
  (format stream "~&Lisp: ~a ~a~&" (lisp-implementation-type) (lisp-implementation-version))
  #+asdf
  (format stream "~&ASDF: ~a~&" (asdf:asdf-version))
  #-asdf
  (format stream "NO ASDF!")
  ;; #+quicklisp
  ;; (format stream "~&Quicklisp: ~a~&" (ql-dist:all-dists))  ;; not for release?
  #-quicklisp
  (format stream "!! Quicklisp is not installed !!"))

(defun main ()

  (unless (uiop:file-exists-p (bookshops.models::db-name))
    (uiop:format! t "Creating the database into ~a...~&" (bookshops.models::db-name))
    (bookshops.models::initialize-database))

  (opts:define-opts
    (:name :help
           :description "print this help and exit."
           :short #\h
           :long "help")

    (:name :version
           :description "print the version number and exit."
           :short #\v
           :long "version")
    (:name :verbose
           :description "print debug info."
           :short #\V
           :long "verbose")

    (:name :interactive
           :description "enter the interactive prompt."
           :short #\i
           :long "interactive")

    (:name :web
           :description "run the web application."
           :short #\w
           :long "web")

    (:name :port
           :arg-parser #'parse-integer
           :description "set the port for the web server. You can also use the OBS_PORT environment variable."
           :short #\p
           :long "port")

    (:name :manage
           :arg-parser #'identity
           :description "Run a management command, such as createsuperuser"
           :long "manage"))

  (multiple-value-bind (options free-args)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (format t "OpenBookStore version ~a~&" +version+)

    (when (getf options :version)
      (print-system-info)
      (uiop:quit))

    (when (getf options :help)
      (opts:describe)
      (uiop:quit))

    (when (getf options :verbose)
      (print-system-info))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Management commands.
    ;; Create a superuser with admin rights.
    (when (getf options :manage)
      (let ((command (getf options :manage)))
        (when (equal "createsuperuser" (str:downcase (str:trim command)))
          (format t "Initializing...~&")
          ;; Connect to the DB.
          (init)
          (uiop:format! t "Running management command ~a…~&" command)
          ;; XXX: avoid circular dependencies:
          ;; we now want to call bookshops.manager, but this package relies on models,
          ;; we can't load it before. Fix.
          (eval (read-from-string "(bookshops.manager::add-superuser)")))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Run the interactive terminal application.
    (when (getf options :interactive)
      (format t "Initializing...~&")
      (init)

      (setf replic:*prompt* (cl-ansi-text:green "bookshops > "))
      (setf replic:*prompt-prefix* (format nil "(~a) " (name (default-place))))

      ;; create commands from the exported functions and variables.
      (replic.completion:functions-to-commands :replic.base)

      (setf replic:*help-preamble* "With cl-bookshops you can search for books by keywords or isbn, add some to your stock and explore it.")
      (replic.completion:functions-to-commands :bookshops.commands)
      (replic.completion:functions-to-commands :bookshops.manager)

      ;; define completions.
      ;; (push '("add" . *results*) replic:*args-completions*)

      (replic:repl)

      (handler-case
          (when free-args
            (search-books (str:join " " free-args)))
        (error (c)
          (progn
            (format *error-output* "~a~&" c)
            (uiop:quit 1)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Run the web app.
    (when (getf options :web)
      (handler-case
          (progn
            (bookshops/web::start-app :port (or (getf options :port)
                                                (ignore-errors (parse-integer (uiop:getenv "OBS_PORT")))
                                                bookshops/web::*port*))
            ;; Without this, the binary exits immediately after having
            ;; run the web server in its thread.
            (bt:join-thread
             (find-if (lambda (th)
                        (search "hunchentoot" (bt:thread-name th)))
                      (bt:all-threads))))
        (usocket:address-in-use-error ()
          (format *error-output* "This port is already taken. You can use the --port option or the OBS_PORT environment variable to specify a new port.~&"))
        #+sbcl
        (sb-sys:interactive-interrupt ()
          (format *error-output* "~&Bye!~&")
          (uiop:quit))
        (error (c)
          (format *error-output* "~&An error occured: ~a~&" c)
          ;; XXX: quit also kills the current lisp process, which is
          ;; annoying when developing with a REPL.
          ;; (uiop:quit 1)
          )))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Search on data sources, print results and exit.
    (when free-args
      (handler-case
          (progn
            (init)
            (bookshops.models::pprint-books (search-books (str:join " " free-args))))
        (error (c)
          (progn
            (format *error-output* "~a~&" c)
            (uiop:quit 1)))))

    ))
