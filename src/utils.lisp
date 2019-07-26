(in-package :bookshops)

(export '(isbn-p
          i18n-load
          _))

(defparameter *isbn-accepted-lengths* '(13))

(defun isbn-p (code)
  "Return `t' if the given code (a string) looks like an ISBN (13 digits)."
  (and (member (length code)
               *isbn-accepted-lengths*)
       (str:digitp code)))

(defun _ (a) (cl-i18n:translate a))

(defun i18n-load ()
  (let ((lang (uiop:getenv "LANG"))
        (cl-i18n:*translation-file-root* (asdf:system-relative-pathname :bookshops "")))
    (if (str:starts-with? "fr" lang)
        (setf cl-i18n::*translation-table*
              (cl-i18n:load-language "locale/mo/fr_FR/messages.mo"
                                     :store-hashtable nil
                                     :store-plural-function t
                                     :update-translation-table nil))
        (progn
          ;; Default locale.
          (setf cl-i18n::*translation-table*
                (cl-i18n:load-language "locale/mo/en_GB/messages.mo"
                                       :store-hashtable nil
                                       :store-plural-function t
                                       :update-translation-table nil))))))
