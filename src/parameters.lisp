(defpackage :bookshops.parameters
  (:use :cl)
  (:export :+date-y-m-d+))

(in-package :bookshops.parameters)

(defparameter +date-y-m-d+ '(:year "-" (:month 2) "-" (:day 2)))
