(defpackage :bookshops.utils
  (:use :cl
        :parse-float
        :bookshops.parameters)
  (:export #:isbn-p
           #:extract-float
           #:format-date
           #:i18n-load
           #:_)
  (:documentation "Utilities that do not depend on models."))

(in-package :bookshops.utils)

(defparameter *isbn-accepted-lengths* '(13))

(defun isbn-p (code)
  "Return `t' if the given code (a string) looks like an ISBN (13 digits)."
  (and (member (length code)
               *isbn-accepted-lengths*)
       (str:digitp code)))

(defun extract-float (s)
  "Extract a float from the given string."
  (check-type s string)
  (ignore-errors
    ;; the regexp should be enough, given we parse a known html beforehand.
    (parse-float (ppcre:scan-to-strings "-?\\d+.?\\d*" s))))

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

(defun format-date (date)
  "Format the given date with the default date format (yyyy-mm-dd). Return a string."
  (local-time:format-timestring nil date :format +date-y-m-d+))

(defun asciify (string)
  (str:downcase (slug:asciify string)))
