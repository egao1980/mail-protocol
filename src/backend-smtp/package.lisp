(defpackage #:mail-backend-smtp
  (:use #:cl #:mail-protocol)
  (:export #:smtp-backend
           #:make-smtp-backend
           #:use-smtp-backend
           #:smtp-dialogue
           #:dot-stuff))

(in-package #:mail-backend-smtp)
