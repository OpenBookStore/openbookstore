(defpackage :bookshops.utils
  (:use :cl
        :mito
        :sxql
        :parse-float
        :bookshops.models
        :bookshops.parameters)
  (:export #:isbn-p
           #:extract-float
           #:format-date
           #:i18n-load
           #:_))

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

(defun cleanup-prices ()
  "When the currency symbol appears in the price, remove it.
  This should be useful during development of the datasources."
  (loop for card in (select-dao 'book
                      (where (:like :price "%€%")))
     ;; XXX: we should run 1 SQL query to update all fields.
     do (setf (price card)
              (str:trim (str:replace-all "€" "" (price card))))
     do (save-dao card)))

(defun asciify (string)
  (str:downcase (slug:asciify string)))

(defun create-ascii-slots ()
  "For all cards, create and save the ascii representations of: title, authors, publisher.
  (needs to migrate the table)"
  (time
   (loop for card in (select-dao 'book)
      for i from 0
      for title-ascii = (asciify (title card))
      for authors-ascii = (asciify (authors card))
      for publisher-ascii = (asciify (publisher card))
      do (format t "- ~a asciify card ~a, \"~a\"~&"
                 i (object-id card) (str:shorten 40 (title card)))
      do (setf (bookshops.models::title-ascii card) title-ascii)
      do (setf (bookshops.models::authors-ascii card) authors-ascii)
      do (setf (bookshops.models::publisher-ascii card) publisher-ascii)
      do (save-dao card))))
