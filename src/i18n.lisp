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

(setf (gettext:textdomain) "bookshops")

(gettext:setup-gettext #.*package* "bookshops")

(unless (deploy:deployed-p)
  (format *debug-io* "~%Reading all *.mo files...")
  (gettext:preload-catalogs
   #.(asdf:system-relative-pathname :bookshops "locale/")))

;; Run this when developping
#+ (or)
(progn
  (clrhash gettext::*catalog-cache*)
  (setf (gettext:textdomaindir "bookshops")
        (asdf:system-relative-pathname :bookshops "locale/")))

(defun list-loaded-locales ()
  (remove-duplicates
   (mapcar #'first
           (alexandria:hash-table-keys
            gettext::*catalog-cache*))
   :test #'string=))

(defun set-locale (locale)
  ;; It is valid to set the locale to nil.
  (when (and locale
             (not (member locale (list-loaded-locales)
                          :test 'string=)))
    (error "Locale not valid or not available: ~s" locale))
  (setf *current-locale* locale))

(defmacro with-locale ((locale) &body body)
  `(let (*current-locale*)
     (set-locale ,locale)
     ,@body))

;; (trace _)

;; (trace djula:translate gettext:gettext* gettext::lookup)

;; (_ "hi")

(defun extract-translate-strings ()
  "Extract all {_ ... _} string from the djula templates."
  (loop
    :with locations = (make-hash-table :test 'equal)
    :for path :in (djula:list-asdf-system-templates "bookshops" "src/web/templates")
    :for strings = (djula.locale:file-template-translate-strings path)
    :do (loop :for string :in strings
              :unless (gethash string locations)
                :do (setf (gethash string locations) path))
    :finally (return locations)))


(defun update-djula.pot ()
  "Update djula.pot from *.html files."
  (with-open-file (s (asdf:system-relative-pathname "bookshops" "locale/templates/LC_MESSAGES/djula.pot")
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
                   (enough-namestring location (asdf:system-relative-pathname "bookshops" ""))
                   string)))))
