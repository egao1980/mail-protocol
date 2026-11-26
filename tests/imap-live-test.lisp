(in-package #:mail-protocol/tests)

;;; #9 against a live IMAP server. Skips without one. GreenMail 2.1.3 (no
;;; UTF8=ACCEPT, so names go in modified UTF-7), as run on 2026-11-26:
;;;
;;;   docker run -d --rm --name mailproto-imap -p 127.0.0.1:23143:3143 \
;;;     -p 127.0.0.1:23025:3025 -e GREENMAIL_OPTS="-Dgreenmail.setup.test.imap \
;;;     -Dgreenmail.setup.test.smtp -Dgreenmail.hostname=0.0.0.0 \
;;;     -Dgreenmail.users=alice:secret@x.example,zoe:pässwörd@x.example,long:<1500 p>@x.example" \
;;;     greenmail/standalone:2.1.3
;;;   MAIL_IMAP_TEST_PORT=23143 MAIL_IMAP_TEST_SMTP_PORT=23025 cl-repo test

(defun %imap-port ()
  (parse-integer (%env "MAIL_IMAP_TEST_PORT" "23143")))

(defun %imap-up-p ()
  (handler-case (progn (usocket:socket-close (usocket:socket-connect "127.0.0.1" (%imap-port)
                                                                     :timeout 2))
                       t)
    (error () nil)))

(defmacro %with-live-imap ((client socket) &body body)
  `(let* ((,socket (usocket:socket-connect "127.0.0.1" (%imap-port) :timeout 5
                                                                   :element-type 'character))
          (,client (make-imap-client :stream (usocket:socket-stream ,socket))))
     (unwind-protect (progn (imap-connect ,client) ,@body)
       (usocket:socket-close ,socket))))

(defun %status (parsed)
  (third (find :tagged parsed :key #'first)))

(deftest live-imap-literals-and-injection
  (if (not (%imap-up-p))
      (skip (format nil "no IMAP server on 127.0.0.1:~D (MAIL_IMAP_TEST_PORT)" (%imap-port)))
      (progn
        (%with-live-imap (c s)
          (imap-login c "zoe" (%u "p~Cssw~Crd" #xE4 #xF6))
          (ok (eq :authenticated (imap-client-state c)) "8-bit password as a literal"))
        (%with-live-imap (c s)
          (imap-login c "long" (make-string 1500 :initial-element #\p))
          (ok (eq :authenticated (imap-client-state c)) "1500-octet password as a literal"))
        (%with-live-imap (c s)
          (ok (signals (imap-login c "alice" (%crlf-injection)) 'imap-argument-error))
          (ok (eq :ok (%status (imap-command c "NOOP")))
              "nothing was sent: the next command is answered in step")
          (ok (signals (imap-login c "alice" "wrong") 'imap-auth-error))
          (imap-login c "alice" "secret")
          (ok (eq :authenticated (imap-client-state c)))
          (let ((name (%u "Entw~Crfe & ~C~C" #xFC #x53F0 #x5317)))
            (ok (member (%status (imap-command c "CREATE" (imap-utf7-encode name))) '(:ok :no)))
            (let* ((listed (imap-command c "LIST" "" "*"))
                   (line (find-if (lambda (p) (search "Entw&APw-rfe &- &U,BTFw-"
                                                      (princ-to-string p)))
                                  listed)))
              (ok line "LIST shows the modified UTF-7 name"))
            (ok (eq :ok (%status (imap-select c name))) "SELECT by its text name")
            (ok (signals (imap-select c (format nil "INBOX~C~CA0099 DELETE INBOX"
                                                #\Return #\Newline))
                    'imap-argument-error))
            (ok (eq :ok (%status (imap-select c "INBOX")))
                "INBOX is still there"))
          (ok (listp (imap-search c (list :subject (%u "Gr~C~Ce" #xFC #xDF)))) 
              "SEARCH CHARSET UTF-8 with a literal is accepted")))))

(deftest live-imap-fetch-over-a-stream
  (let ((smtp (parse-integer (%env "MAIL_IMAP_TEST_SMTP_PORT" "23025"))))
    (if (not (%imap-up-p))
        (skip (format nil "no IMAP server on 127.0.0.1:~D (MAIL_IMAP_TEST_PORT)" (%imap-port)))
        (let ((subject (format nil "imap-live ~D" (get-universal-time))))
          (backend-send (mail-backend-smtp:make-smtp-backend :host "127.0.0.1" :port smtp)
                        (make-message :from "s@x.example" :to "alice@x.example" :subject subject
                                      :body (format nil "A0004 OK not the tagged line~%caf~C"
                                                    (code-char #xE9))))
          (%with-live-imap (c s)
            (imap-login c "alice" "secret")
            (imap-select c "INBOX")
            (let ((hits (imap-search c (list :subject subject))))
              (ok (= 1 (length hits)) (format nil "found ~S" hits))
              (when hits
                (let ((got (imap-fetch c (first hits))))
                  (ok (and (= 1 (length got)) (message-p (first got))))
                  (ok (equal subject (message-subject (first got))))
                  (ok (eq :ok (%status (imap-command c "NOOP")))
                      "the stream is in step after the body literal")))))))))
