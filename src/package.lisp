(defpackage #:mail-protocol
  (:use #:cl)
  (:nicknames #:stack-mail)
  (:export            #:mail-error
           #:mail-parse-error
           #:mail-send-error
           #:mail-send-error-code
           #:mail-send-error-cause
           #:mail-auth-error
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
           #:message-date
           #:message-id
           #:message-attachments
           #:add-alternative
           #:add-attachment
           #:make-message-id
           #:format-message-date
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
           #:next-imap-tag
           #:mail-address-error
           #:mail-address-error-address
           #:parse-address
           #:address-mailboxes
           #:envelope-sender))

(in-package #:mail-protocol)
