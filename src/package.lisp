(defpackage #:mail-protocol
  (:use #:cl)
  (:nicknames #:stack-mail)
  (:export #:mail-error
           #:mail-parse-error
           #:mail-send-error
           #:mail-error-message

           #:*mail-backend*
           #:mail-backend
           #:backend-send
           #:send

           #:message
           #:message-p
           #:message-entity
           #:message-from
           #:message-to
           #:message-cc
           #:message-bcc
           #:message-subject
           #:message-body
           #:make-message
           #:parse-message
           #:print-message
           #:envelope-recipients
           #:address-list))

(in-package #:mail-protocol)
