(defpackage :bookshops.i18n
  (:use :cl)
  (:import-from :gettext #:*current-locale*)
  (:export
   #:_
   #:n_
   #:*current-locale*
   #:list-loaded-locales
   #:set-locale
   #:with-locale
   #:update-djula.pot)
  (:documentation "Internationalisation utilities"))

(in-package :bookshops.i18n)

(setf (gettext:textdomain) "openbookstore")

(gettext:setup-gettext #.*package* "openbookstore")

;; Only preload the translations into the image if we're not deployed yet.
(unless (deploy:deployed-p)
  (format *debug-io* "~%gettext: reading all *.mo files...")
  (gettext:preload-catalogs
   ;; Tell gettext where to find the .mo files
   #.(asdf:system-relative-pathname :openbookstore "locale/"))
  (format *debug-io* "done.~&"))

;; Run this when developping to reload the translations
#+ (or)
(progn
  ;; Clear gettext's cache
  (clrhash gettext::*catalog-cache*)
  (gettext:preload-catalogs
   ;; Tell gettext where to find the .mo files
   #.(asdf:system-relative-pathname :openbookstore "locale/")))

;; Run this to see the list of loaded message for a specific locale
#+ (or)
(gettext::catalog-messages
 (gethash '("fr_fr" :LC_MESSAGES "openbookstore")
	  gettext::*catalog-cache*))

;; Test the translation of a string
#+ (or)
(with-locale ("fr_fr")
  (_ "Please login to continue"))


#+ (or)
(set-locale "fr_fr")

#+ (or)
*current-locale*


(defun list-loaded-locales ()
  "Get the list of locales loaded in gettext's cache."
  (remove-duplicates
   (mapcar #'first
           (alexandria:hash-table-keys
            gettext::*catalog-cache*))
   :test #'string=))

(defun set-locale (locale)
  "Setf gettext:*current-locale* and djula:*current-language* if LOCALE seems valid."
  ;; It is valid to set the locale to nil.
  (when (and locale
             (not (member locale (list-loaded-locales)
                          :test 'string=)))
    (error "Locale not valid or not available: ~s" locale))
  (setf *current-locale* locale
        djula:*current-language* locale))

(defmacro with-locale ((locale) &body body)
  "Calls BODY with gettext:*current-locale* and djula:*current-language* set to LOCALE."
  `(let (*current-locale*
         djula:*current-language*)
     (set-locale ,locale)
     ,@body))

;; (trace _)

;; (trace djula:translate gettext:gettext* gettext::lookup)

;; (_ "hi")

#|
This could technically be just
(mapcan #'djula.locale:file-template-translate-strings
        (djula:list-asdf-system-templates "openbookstore" "src/web/templates"))

But I (fstamour) made it just a bit more complex in order to keep track of the source (just the
filename) of each translatable strings. Hence why the hash-table returned is named `locations`.
|#
(defun extract-translate-strings ()
  "Extract all {_ ... _} string from the djula templates."
  (loop
    :with locations = (make-hash-table :test 'equal)
    :for path :in (djula:list-asdf-system-templates "openbookstore" "src/web/templates")
    :for strings = (djula.locale:file-template-translate-strings path)
    :do (loop :for string :in strings
              :unless (gethash string locations)
                :do (setf (gethash string locations) path))
    :finally (return locations)))


(defun update-djula.pot ()
  "Update djula.pot from *.html files."
  (with-open-file (s (asdf:system-relative-pathname "openbookstore" "locale/templates/LC_MESSAGES/djula.pot")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (let* ((locations (extract-translate-strings))
           (strings (alexandria:hash-table-keys locations)))
      (loop
        :for string :in strings
        :for location = (gethash string locations)
        :do
           (format s "~%#: ~a~%#, lisp-format~%msgid ~s~%msgstr \"\" ~%"
                   (enough-namestring location (asdf:system-relative-pathname "openbookstore" ""))
                   string)))))
