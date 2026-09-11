(in-package #:mail-backend-memory)

(defclass memory-backend (mail-backend)
  ((sent :initform nil :accessor sent-messages)))

(defun make-memory-backend ()
  (make-instance 'memory-backend))

(defun use-memory-backend ()
  (setf *mail-backend* (make-memory-backend)))

(defun clear-sent (&optional (backend *mail-backend*))
  (when (typep backend 'memory-backend)
    (setf (sent-messages backend) nil)))

(defmethod backend-send ((backend memory-backend) message &key)
  (unless (message-p message)
    (error 'mail-send-error :message "send expects a message"))
  (unless (envelope-recipients message)
    (error 'mail-send-error :message "no recipients"))
  (push message (sent-messages backend))
  message)

(use-memory-backend)
