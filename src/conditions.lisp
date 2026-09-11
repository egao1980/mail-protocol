(in-package #:mail-protocol)

(define-condition mail-error (error)
  ((message :initarg :message :reader mail-error-message :initform nil))
  (:report (lambda (c s)
             (format s "Mail error~@[: ~A~]" (mail-error-message c)))))

(define-condition mail-parse-error (mail-error) ())
(define-condition mail-send-error (mail-error) ())

(defclass mail-backend () ()
  (:documentation "Base class for mail-protocol send backends."))
