(defpackage bookshops.utils
  (:use :cl)
  (:export :i18n-load
           :_))

(in-package :bookshops.utils)

(defun _ (a) (cl-i18n:translate a))

(defun i18n-load ()
  (setf cl-i18n::*translation-table*
        (cl-i18n:load-language "locale/mo/fr_FR/messages.mo"
                               :store-hashtable nil
                               :store-plural-function t
                               :update-translation-table nil)))

