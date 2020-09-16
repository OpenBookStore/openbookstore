;;;
;;; Light interface around hunchentoot's session to store and retrieve messages
;;; across web pages.
;;;
;;; In a Djula template, use like:
#|
        {% for msg in messages %}
        <div class="notification {{ msg.class }}">
          <button class="delete"></button>
          {{ msg.message }}
        </div>
        {% endfor %}
|#

;;;
;;; ;TODO: handle many messages

(defpackage bookshops.messages
  (:use :cl)
  (:export :add-message
           :get-message-text
           :get-message/status))

(in-package :bookshops.messages)

;; CSS classes. Overwrite, or adapt your stylesheet.
(defparameter +class-success+ "is-success")
(defparameter +class-error+ "is-error")
(defparameter +class-warning+ "is-warning")
(defparameter +class-info+ "is-info")

(defun get-class (status)
  (case status
    (:success +class-success+)
    (:warning +class-warning+)
    (:error +class-error+)
    (t +class-info+)))

(defun add-message (message &key (status :success))
  (setf (hunchentoot:session-value :message-status) status
        (hunchentoot:session-value :message) message))

(defun %delete-data ()
  (hunchentoot:delete-session-value :message)
  (hunchentoot:delete-session-value :message-status) )

(defun get-message-text ()
  "Get only the message's text, discard its status."
  (let ((val (hunchentoot:session-value :message)))
    (%delete-data)
    val))

(defun get-message/status ()
  "Return a plist with :message, :status and :class (CSS class) properties"
  (let ((val (hunchentoot:session-value :message))
        (status (hunchentoot:session-value :message-status)))
    (when val
      (%delete-data)
      `((:message ,val
                  :status ,status
                  :class ,(get-class status))))))
