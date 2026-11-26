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

(defun %live-imap-stream (latin-1)
  "→ (values stream closer). LATIN-1 (SBCL): a Latin-1 character stream on
its own socket, the octet mode, which also reads a server's non-UTF-8
reply text (GreenMail answers a second CREATE with the name in Latin-1)."
  #+sbcl
  (when latin-1
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (sb-bsd-sockets:socket-connect socket #(127 0 0 1) (%imap-port))
      (return-from %live-imap-stream
        (values (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                                          :element-type 'character
                                                          :external-format :latin-1
                                                          :buffering :full)
                (lambda () (sb-bsd-sockets:socket-close socket))))))
  (let ((socket (usocket:socket-connect "127.0.0.1" (%imap-port) :timeout 5
                                                                 :element-type 'character)))
    (values (usocket:socket-stream socket)
            (lambda () (usocket:socket-close socket)))))

(defmacro %with-live-imap ((client socket &key latin-1) &body body)
  "CLIENT on a fresh connection; SOCKET is bound to its closer."
  (let ((stream (gensym "STREAM")))
    `(multiple-value-bind (,stream ,socket) (%live-imap-stream ,latin-1)
       (let ((,client (make-imap-client :stream ,stream)))
         (unwind-protect (progn (imap-connect ,client) ,@body)
           (funcall ,socket))))))

(defun %status (parsed)
  (third (find :tagged parsed :key #'first)))

(deftest live-imap-literals-and-injection
  (if (not (%imap-up-p))
      (skip (format nil "no IMAP server on 127.0.0.1:~D (MAIL_IMAP_TEST_PORT)" (%imap-port)))
      (progn
        (dolist (latin-1 '(nil t))
          (%with-live-imap (c s :latin-1 latin-1)
            (imap-login c "zoe" (%u "p~Cssw~Crd" #xE4 #xF6))
            (ok (eq :authenticated (imap-client-state c))
                (format nil "8-bit password as a literal (~:[UTF-8~;Latin-1~] stream)" latin-1))))
        (%with-live-imap (c s)
          (imap-login c "long" (make-string 1500 :initial-element #\p))
          (ok (eq :authenticated (imap-client-state c)) "1500-octet password as a literal"))
        (%with-live-imap (c s :latin-1 t)
          (ok (signals (imap-login c "alice" (%crlf-injection)) 'imap-argument-error))
          (ok (eq :ok (%status (imap-command c "NOOP")))
              "nothing was sent: the next command is answered in step")
          (ok (signals (imap-search c "FROM {5}") 'imap-argument-error)
              "raw text ending in a literal announcement")
          (ok (eq :ok (%status (imap-command c "NOOP"))) "still in step")
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
        (let ((subject (format nil "imap-live ~D ~D" (get-universal-time) (random 100000))))
          (backend-send (mail-backend-smtp:make-smtp-backend :host "127.0.0.1" :port smtp)
                        (make-message :from "s@x.example" :to "alice@x.example" :subject subject
                                      :body (format nil "A0004 OK not the tagged line~%caf~C ~C"
                                                    (code-char #xE9) (code-char #x2615))))
          (dolist (latin-1 '(nil t))
            (%with-live-imap (c s :latin-1 latin-1)
              (imap-login c "alice" "secret")
              (imap-select c "INBOX")
              (let ((hits (imap-search c (list :subject subject)))
                    (mode (if latin-1 "Latin-1" "UTF-8")))
                (ok (= 1 (length hits)) (format nil "~A: found ~S" mode hits))
                (when hits
                  (let ((got (imap-fetch c (first hits))))
                    (ok (and (= 1 (length got)) (message-p (first got))) mode)
                    (ok (equal subject (message-subject (first got))) mode)
                    (ok (search (format nil "caf~C ~C" (code-char #xE9) (code-char #x2615))
                                (message-body (first got)))
                        (format nil "~A: the body decodes" mode))
                    (ok (eq :ok (%status (imap-command c "NOOP")))
                        (format nil "~A: in step after the body literal" mode)))))))))))
