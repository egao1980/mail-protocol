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

(defclass mail-backend () ()
  (:documentation "Base class for mail-protocol send backends."))
