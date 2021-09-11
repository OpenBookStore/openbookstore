(defpackage :bookshops.utils
  (:use :cl
        :parse-float
        :bookshops.parameters)
  (:export #:isbn-p
           #:asciify
           #:clean-isbn
           #:extract-float
           #:ensure-float
           #:format-date
           #:i18n-load
           #:_
           #:parse-iso-date)
  (:documentation "Utilities that do not depend on models."))

(in-package :bookshops.utils)

(defparameter *isbn-accepted-lengths* '(13))

(defun isbn-p (code)
  "Return `t' if the given code (a string) looks like an ISBN (13 digits)."
  (let ((code (str:replace-all "-" "" code)))
    (and (member (length code)
                 *isbn-accepted-lengths*)
         (str:digitp code))))

#+nil
(progn
  (assert (isbn-p "978-4567-890123")))

(defun clean-isbn (isbn)
  "Clean this ISBN of accepted symbols in the user form, but unwanted in the DB (\"-\")."
  (str:replace-all "-" "" isbn))

(defun extract-float (s)
  "Extract a float from the given string."
  (check-type s string)
  (ignore-errors
    ;; the regexp should be enough, given we parse a known html beforehand.
    (parse-float (ppcre:scan-to-strings "-?\\d+.?\\d*" s))))

(defun ensure-float (param)
  "Return a float from this param, if possible.
  If it is a string, parse it for a float."
  (when param
    (typecase param
      (string (or (ignore-errors (parse-float:parse-float param))
                  0.0))
      (integer (coerce param 'float))
      (float param)
      (t (error (format nil "ensure-float error: the parameter ~a is of type ~a and we don't know how to make it a float." param (type-of param)))))))

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

(defun parse-iso-date (date)
  "Dates returned from javascript are often ISO format"
  (let ((année (parse-integer date :start 0 :end 4))
        (mois (parse-integer date :start 5 :end 7))
        (jour (parse-integer date :start 8 :end 10)))
    (local-time:encode-timestamp 0 0 0 0 jour mois année :timezone local-time:+utc-zone+)))

(defun asciify (string)
  (str:downcase (slug:asciify string)))
