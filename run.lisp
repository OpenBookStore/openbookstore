"
Usage:

sbcl --load run.lisp

This loads the project's asd, loads the quicklisp dependencies, and
starts the web server.

Then, we are given the lisp prompt: we can interact with the running application.

Another solution to run the app is to run the executable (see README).
"

(require "asdf")  ;; for Docker
(load "openbookstore.asd")

(ql:quickload "openbookstore")

(in-package :openbookstore/web)
(handler-bind ((error (lambda (c)
                        (format *error-output* "~&An error occured: ~a~&" c)
                        (format *error-output* "~&Backtrace:~&")
                        (trivial-backtrace:print-backtrace c))
                 ;; XXX: quit also kills the current lisp process, which is
                 ;; annoying when developing with a REPL.
                 ;; (uiop:quit 1)
                 ))
  (start-app :port (or (ignore-errors (parse-integer (uiop:getenv "OBS_PORT")))
                         *port*)))
