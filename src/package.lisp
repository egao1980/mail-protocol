(defpackage #:mail-protocol
  (:use #:cl)
  (:nicknames #:stack-mail)
  (:export            #:mail-error
           #:mail-parse-error
           #:mail-send-error
           #:mail-error-message
           #:imap-error
           #:imap-auth-error
           #:imap-error-tag
           #:imap-error-status

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
           #:address-list

           #:imap-client
           #:imap-client-p
           #:make-imap-client
           #:imap-client-host
           #:imap-client-port
           #:imap-client-stream
           #:imap-client-io-fn
           #:imap-client-state
           #:imap-client-selected
           #:imap-client-idle-handler
           #:imap-connect
           #:imap-login
           #:imap-select
           #:imap-fetch
           #:imap-search
           #:imap-idle
           #:encode-imap-command
           #:parse-imap-response
           #:parse-imap-untagged
           #:next-imap-tag))

(in-package #:mail-protocol)
