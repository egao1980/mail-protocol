(defpackage #:mail-backend-memory
  (:use #:cl #:mail-protocol)
  (:export #:memory-backend
           #:make-memory-backend
           #:use-memory-backend
           #:sent-messages
           #:clear-sent))

(in-package #:mail-backend-memory)
