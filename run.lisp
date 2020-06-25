"
Usage:

sbcl --load run.lisp

This loads the project's asd, loads the quicklisp dependencies, and
starts the web server.

Then, we are given the lisp prompt: we can interact with the running application.

Another solution to run the app is to run the executable (see README).
"

(load "bookshops.asd")

(ql:quickload "bookshops")

(in-package :bookshops-web)
(handler-case
    (start-app :port (or (ignore-errors (parse-integer (uiop:getenv "OBS_PORT")))
                         *port*))
  (error (c)
    (format *error-output* "~&An error occured: ~a~&" c)
    ;; XXX: quit also kills the current lisp process, which is
    ;; annoying when developing with a REPL.
    ;; (uiop:quit 1)
    ))
