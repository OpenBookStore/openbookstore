(in-package :bookshops/web)

;;; Parameters and functions required before loading web.lisp
;;;
;;; We read the content of our static files and put into variables, so that they can be saved in the Lisp image.
;;; We define %serve-static-file to simply return their content (as string),
;;; and because we use with the #. reader macro, we need to put these in another file than web.lisp.

(defparameter *default-static-directory* "src/static/"
  "The directory where to serve static assets from (STRING). If it starts with a slash, it is an absolute directory. Otherwise, it will be a subdirectory of where the system :abstock is installed.
  Static assets are reachable under the /static/ prefix.")

(defparameter *static-files-content* (dict)
  "Content of our JS and CSS files.
  Hash-table with file name => content (string).")

(defun %read-static-files-in-memory ()
  "Save the JS and CSS files in a variable in memory, so they can be saved at compile time."
  (loop for file in (list "openbookstore.js"
                          "card-page.js")
     with static-directory = (merge-pathnames *default-static-directory*
                                              (asdf:system-source-directory :openbookstore))
     for content = (uiop:read-file-string (merge-pathnames file static-directory))
     do (setf (gethash file *static-files-content*) content)
     finally (return *static-files-content*)))

;; At compile time, read the content of our static files.
;; XXX: feature flag to know when we are building a binary?
(%read-static-files-in-memory)

(defun %serve-static-file (path)
  "Return the content as a string of this static file.
  For standalone binaries delivery."
  ;; "alert('yes');"  ;; witness to check this dispatcher works.
  (gethash path *static-files-content*))  ;; this would not work without the #. reader macro.
