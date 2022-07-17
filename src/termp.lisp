
(in-package :bookshops.utils)

;; Are we running inside a real terminal window? Unbound by default.
(defvar *termp*)

;; copy-pasted from
;; https://github.com/vindarel/termp

(defun termp (&key force)
  "Return T if we run on a terminal.
  This must fail on Slime (on Emacs' default shell prompt) and succeed on a Lisp in a terminal window."
  (if (and (not force)
           (boundp '*termp*))
      *termp*
      (setf *termp* (not (equalp "dumb" (uiop:getenv "TERM"))))))

(defun quit (&optional (status 0) message)
  "If we are in a real terminal, really quit. Otherwise, error out."
  (cond
    ((termp)
     (when message
       (format t "~a~&" message))
     (uiop:quit status))
    (t
     (error (or message "termp: not on a terminal, not quitting.~&")))))
