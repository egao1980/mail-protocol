(in-package #:mail-protocol)

(define-condition mail-error (error)
  ((message :initarg :message :reader mail-error-message :initform nil))
  (:report (lambda (c s)
             (format s "Mail error~@[: ~A~]" (mail-error-message c)))))

(define-condition mail-parse-error (mail-error) ())

(define-condition mail-address-error (mail-parse-error)
  ((address :initarg :address :reader mail-address-error-address :initform nil
            :documentation "The address text at fault."))
  (:report (lambda (c s)
             (format s "Mail address error~@[ in ~S~]~@[: ~A~]"
                     (mail-address-error-address c)
                     (mail-error-message c))))
  (:documentation
   "An address that is not RFC 5322 syntax where the addr-spec is needed (the
SMTP envelope), or that holds CR, LF or NUL: a line break would end the
header field or the SMTP command and let the rest pose as another one."))
(define-condition mail-send-error (mail-error)
  ((code :initarg :code :reader mail-send-error-code :initform nil
         :documentation "The SMTP reply code, when a server refused.")
   (cause :initarg :cause :reader mail-send-error-cause :initform nil
          :documentation "The transport condition (usocket, stream, TLS), if any."))
  (:report (lambda (c s)
             (format s "Mail send error~@[ (~D)~]~@[: ~A~]~@[ [~A]~]"
                     (mail-send-error-code c)
                     (mail-error-message c)
                     (mail-send-error-cause c)))))

(define-condition mail-auth-error (mail-send-error)
  ()
  (:report (lambda (c s)
             (format s "SMTP authentication failed~@[ (~D)~]~@[: ~A~]"
                     (mail-send-error-code c)
                     (mail-error-message c))))
  (:documentation "The server refused AUTH, or offers no mechanism the client speaks."))

(define-condition imap-error (mail-error)
  ((tag :initarg :tag :reader imap-error-tag :initform nil)
   (status :initarg :status :reader imap-error-status :initform nil))
  (:report (lambda (c s)
             (format s "IMAP error~@[ [~a]~]~@[ ~a~]~@[: ~a~]"
                     (imap-error-tag c)
                     (imap-error-status c)
                     (mail-error-message c)))))

(define-condition imap-auth-error (imap-error) ())

(define-condition imap-argument-error (imap-error)
  ((argument :initarg :argument :reader imap-argument-error-argument :initform nil
             :documentation "The value that cannot go on the wire."))
  (:report (lambda (c s)
             (format s "IMAP argument refused~@[: ~A~]" (mail-error-message c))))
  (:documentation
   "An IMAP command argument the client will not send, signalled before
anything is written. An atom or quoted string cannot hold CR or LF (RFC 3501
§4.3, RFC 9051 §4.3): a line break would end the command and the rest would
reach the server as another tagged command (IMAP command injection). NUL is
not allowed anywhere. CR LF can go only inside a literal, whose size tells
the server where it ends: an IMAP-LITERAL argument, or the message of
IMAP-APPEND. Everywhere else (user, password, mailbox, search key,
sequence, items, flags, dates) CR, LF and NUL are refused outright, as
Python's imaplib does (\"NUL, CR and LF not allowed in commands\")."))

(defclass mail-backend () ()
  (:documentation "Base class for mail-protocol send backends."))
