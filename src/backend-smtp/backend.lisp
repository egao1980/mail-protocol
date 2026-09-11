(in-package #:mail-backend-smtp)

(defclass smtp-backend (mail-backend)
  ((host :initarg :host :accessor smtp-host :initform "127.0.0.1")
   (port :initarg :port :accessor smtp-port :initform 25)
   (ehlo-host :initarg :ehlo-host :accessor smtp-ehlo-host :initform "localhost")))

(defun make-smtp-backend (&key (host "127.0.0.1") (port 25) (ehlo-host "localhost"))
  (make-instance 'smtp-backend :host host :port port :ehlo-host ehlo-host))

(defun use-smtp-backend (&rest keys)
  (setf *mail-backend* (apply #'make-smtp-backend keys)))

(defmethod backend-send ((backend smtp-backend) message &key)
  (unless (message-p message)
    (error 'mail-send-error :message "send expects a message"))
  (let ((from (message-from message))
        (rcpts (envelope-recipients message))
        (data (print-message message)))
    (unless from
      (error 'mail-send-error :message "message has no From"))
    (unless rcpts
      (error 'mail-send-error :message "no recipients"))
    (let ((sock (usocket:socket-connect (smtp-host backend) (smtp-port backend)
                                        :element-type 'character)))
      (unwind-protect
           (let ((stream (usocket:socket-stream sock)))
             (smtp-dialogue stream stream
                            :from from
                            :recipients rcpts
                            :data data
                            :ehlo-host (smtp-ehlo-host backend)))
        (ignore-errors (usocket:socket-close sock)))))
  message)
