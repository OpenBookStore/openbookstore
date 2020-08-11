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

(defun main ()

  (unless (uiop:file-exists-p bookshops.models::*db-name*)
    (uiop:format! t "Creating the database into ~a...~&" bookshops.models::*db-name*)
    (bookshops.models::ensure-tables-exist))

  (opts:define-opts
      (:name :help
             :description "print this help and exit."
             :short #\h
             :long "help")

      (:name :version
             :description "print the version number and exit."
             :short #\v
             :long "version")

    (:name :interactive
           :description "enter the interactive prompt."
           :short #\i
           :long "interactive"))

  (multiple-value-bind (options free-args)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (if (getf options :version)
        (progn
          (format t "~a~&" +version+)
          (uiop:quit)))

    (if (getf options :help)
        (progn
          (opts:describe)
          (uiop:quit)))

    (if (getf options :interactive)
        (progn
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

          (replic:repl))

        (handler-case
            (if free-args
                (search-books (str:join " " free-args)))
          (error (c)
            (progn
              (format *error-output* "~a~&" c)
              (uiop:quit 1)))))))
