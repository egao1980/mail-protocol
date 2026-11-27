(in-package #:mail-backend-smtp)

(defclass smtp-backend (mail-backend)
  ((host :initarg :host :accessor smtp-host :initform "127.0.0.1")
   (port :initarg :port :accessor smtp-port :initform 25)
   (ehlo-host :initarg :ehlo-host :accessor smtp-ehlo-host :initform "localhost")
   (username :initarg :username :accessor smtp-username :initform nil)
   (password :initarg :password :initform nil)
   (starttls :initarg :starttls :accessor smtp-starttls :initform nil)
   (tls :initarg :tls :accessor smtp-tls :initform nil)
   (tls-verify :initarg :tls-verify :accessor smtp-tls-verify :initform :required)
   (connect-timeout :initarg :connect-timeout :accessor smtp-connect-timeout
                    :initform 30)
   (read-timeout :initarg :read-timeout :accessor smtp-read-timeout :initform 600
                 :documentation "Seconds to wait for each server reply (NIL: no
limit). 600, the longest of RFC 5321 4.5.3.2's client timeouts (the reply to
the end of DATA), so a slow server that has taken the message is not given
up on and sent the message again."))
  (:documentation
   "SMTP submission (smtplib-shaped). One connection per BACKEND-SEND.
TLS is cl+ssl through cl-stack-ssl: implicit (TLS, port 465) or STARTTLS
(port 587); AUTH PLAIN / LOGIN with USERNAME and PASSWORD."))

(defun make-smtp-backend (&key (host "127.0.0.1") port (ehlo-host "localhost")
                               username password starttls tls (tls-verify :required)
                               (connect-timeout 30) (read-timeout 600))
  "An SMTP backend. PORT defaults to 465 with :TLS (implicit TLS, Python
SMTP_SSL), else 25. :STARTTLS T upgrades after EHLO and refuses to go on
in the clear when the server does not offer it. :USERNAME / :PASSWORD
authenticate (AUTH PLAIN, else LOGIN). TLS-VERIFY is cl+ssl's :VERIFY.
CONNECT-TIMEOUT bounds the connect, READ-TIMEOUT (seconds, NIL for none)
the wait for each reply: a server that stops answering is MAIL-SEND-ERROR."
  (when (and tls starttls)
    (error 'mail-send-error :message ":tls and :starttls are exclusive"))
  (make-instance 'smtp-backend :host host :port (or port (if tls 465 25))
                               :ehlo-host ehlo-host
                               :username username :password password
                               :starttls starttls :tls tls :tls-verify tls-verify
                               :connect-timeout connect-timeout
                               :read-timeout read-timeout))

(defun use-smtp-backend (&rest keys)
  (setf *mail-backend* (apply #'make-smtp-backend keys)))

(defun %endpoint (backend)
  (format nil "~A:~D" (smtp-host backend) (smtp-port backend)))

(defmacro %with-transport-errors ((backend) &body body)
  "Run BODY; a socket, stream or TLS failure becomes MAIL-SEND-ERROR with
the original condition as its CAUSE (the report prints it)."
  `(handler-case (progn ,@body)
     ;; cl+ssl exports no base condition; CL+SSL-ERROR is its root.
     ((or usocket:socket-error usocket:ns-error stream-error cl+ssl::cl+ssl-error) (e)
       (error 'mail-send-error :message (%endpoint ,backend) :cause e))))

(defun %text-stream (binary)
  "SMTP lines over BINARY, one character per octet (ISO-8859-1): the
dialogue writes PRINT-MESSAGE's octets as they are and commands as UTF-8,
so nothing is encoded twice (#5). #\\Newline is a bare LF (the dialogue
writes CR LF itself)."
  (flexi-streams:make-flexi-stream
   binary :external-format (flexi-streams:make-external-format :latin-1 :eol-style :lf)))

(defun %tls-stream (backend binary)
  "A TLS client stream over BINARY, with SNI and hostname verification."
  (cl+ssl:make-ssl-client-stream binary :hostname (smtp-host backend)
                                        :verify (smtp-tls-verify backend)))

(defun %close-quietly (streams socket)
  "Teardown after a send that may have failed half way: the connection is
being dropped, so its own transport errors are of no further use."
  (handler-case
      (progn
        (dolist (s streams) (close s :abort t))
        (usocket:socket-close socket))
    ((or stream-error usocket:socket-error cl+ssl::cl+ssl-error) () nil)))

(defmethod backend-send ((backend smtp-backend) message &key)
  "Send MESSAGE over one SMTP connection. The envelope is the addr-specs of
ENVELOPE-SENDER and ENVELOPE-RECIPIENTS (smtplib's send_message), worked
out before connecting: an address that does not parse signals
MAIL-ADDRESS-ERROR and nothing is sent. Each reply is waited for at most
READ-TIMEOUT seconds."
  (unless (message-p message)
    (error 'mail-send-error :message "send expects a message"))
  (let ((from (envelope-sender message))
        (rcpts (envelope-recipients message))
        (data (print-message message)))
    (unless from
      (error 'mail-send-error :message "message has no From"))
    (unless rcpts
      (error 'mail-send-error :message "no recipients"))
    (let ((socket (%with-transport-errors (backend)
                    (usocket:socket-connect (smtp-host backend) (smtp-port backend)
                                            :element-type '(unsigned-byte 8)
                                            :timeout (smtp-connect-timeout backend))))
          (read-timeout (smtp-read-timeout backend))
          (streams '()))
      (flet ((text (binary)
               (let ((s (%text-stream binary)))
                 (push s streams)
                 s)))
        (unwind-protect
            (%with-transport-errors (backend)
              (when read-timeout
                ;; Where usocket can (SBCL: the stream's own timeout, which
                ;; :TIMEOUT above set to the connect timeout), reads mid-reply
                ;; get the same limit.
                (setf (usocket:socket-option socket :receive-timeout) read-timeout))
              (let* ((raw (usocket:socket-stream socket))
                     (stream (text (if (smtp-tls backend)
                                       (%tls-stream backend raw)
                                       raw))))
                (smtp-dialogue stream stream
                               :from from
                               :recipients rcpts
                               :data data
                               :ehlo-host (smtp-ehlo-host backend)
                               :username (smtp-username backend)
                               :password (slot-value backend 'password)
                               :await-reply (and read-timeout
                                                 (lambda (in)
                                                   (or (listen in)
                                                       (usocket:wait-for-input
                                                        socket :timeout read-timeout
                                                        :ready-only t))))
                               :starttls (and (smtp-starttls backend)
                                              (lambda ()
                                                (let ((tls (text (%tls-stream backend raw))))
                                                  (values tls tls)))))))
          (%close-quietly streams socket)))))
  message)
