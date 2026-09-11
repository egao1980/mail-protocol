(in-package #:mail-protocol)

(defvar *mail-backend* nil
  "Current mail send backend.")

(defgeneric backend-send (backend message &key)
  (:documentation "Send MESSAGE via BACKEND."))

(defun send (message &rest keys &key &allow-other-keys)
  "Send MESSAGE via *MAIL-BACKEND*."
  (unless *mail-backend*
    (error 'mail-send-error :message "*mail-backend* is unbound — load a mail-backend-*"))
  (apply #'backend-send *mail-backend* message keys))
