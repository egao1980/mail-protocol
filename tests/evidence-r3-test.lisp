(in-package #:mail-protocol/tests)

;;; Round-3 evidence against PR #6: the envelope and header injection.
;;;
;;; Python 3.12 (smtplib + email.message.EmailMessage), checked 2026-11-24
;;; against Mailpit:
;;;  - From "Ann <ann@example.com>", To 'Bob <bob@example.com>, "Doe, John"
;;;    <john@example.com>': send_message sends MAIL FROM:<ann@example.com>
;;;    and RCPT TO bob@example.com, john@example.com.
;;;  - msg['Subject'] = 'Hi\r\nBcc: eve@example.com' raises ValueError
;;;    "Header values may not contain linefeed or carriage return characters".
;;;  - sendmail(..., ['bob@example.com>\r\nRCPT TO:<eve@example.com']) raises
;;;    ValueError "command and arguments contain prohibited newline characters".
;;;
;;; Here the From and To header text goes into MAIL FROM / RCPT TO as it
;;; is, a quoted display name with a comma is split in two, and CR LF in a
;;; header value or an address reaches the wire: a header (or a whole SMTP
;;; command) can be injected.

(defun %crlf-text (&rest parts)
  (format nil "~{~A~^~C~C~}"
          (loop for (p . more) on parts
                collect p
                when more append (list #\Return #\Newline))))

(defun %smtp-capture-server ()
  "A one-connection SMTP server on 127.0.0.1 that answers every command with
success and records each line it reads, CR LF kept.
→ (values port thread box); (CAR BOX) is the lines, newest first."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (box (list '())))
    (flet ((serve ()
             (unwind-protect
                 (when (usocket:wait-for-input listener :timeout 5 :ready-only t)
                   (let* ((sock (usocket:socket-accept listener))
                          (s (usocket:socket-stream sock)))
                     (unwind-protect
                         (flet ((reply (text)
                                  (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                                                       (%crlf-text text ""))
                                                  s)
                                  (force-output s))
                                (read-raw ()
                                  (let ((bytes (loop for b = (read-byte s nil nil)
                                                     while b
                                                     collect b
                                                     until (= b 10))))
                                    (and bytes (map 'string #'code-char bytes)))))
                           (handler-case
                               (progn
                                 (reply "220 capture ESMTP")
                                 (loop with in-data = nil
                                       for line = (read-raw)
                                       while line
                                       do (push line (car box))
                                          (let ((cmd (string-right-trim '(#\Return #\Newline) line)))
                                            (cond
                                             (in-data (when (string= cmd ".")
                                                        (setf in-data nil)
                                                        (reply "250 queued")))
                                             ((and (>= (length cmd) 4) (string-equal "EHLO" cmd :end2 4))
                                              (reply "250 capture"))
                                             ((string-equal "DATA" cmd)
                                              (setf in-data t)
                                              (reply "354 go ahead"))
                                             ((string-equal "QUIT" cmd)
                                              (reply "221 bye")
                                              (return))
                                             (t (reply "250 ok"))))))
                             (error () nil)))
                       (usocket:socket-close sock))))
               (usocket:socket-close listener))))
      (values port (bt2:make-thread #'serve :name "smtp-capture") box))))

(defun %send-captured (msg)
  "Send MSG with the SMTP backend to a capture server.
→ (values lines-oldest-first error-or-nil)."
  (multiple-value-bind (port thread box) (%smtp-capture-server)
    (let ((err (handler-case
                   (let ((mail-protocol:*mail-backend*
                           (mail-backend-smtp:make-smtp-backend :host "127.0.0.1" :port port
                                                                :connect-timeout 5)))
                     (send msg)
                     nil)
                 (error (e) e))))
      (bt2:join-thread thread)
      (values (reverse (car box)) err))))

(defun %command-lines (lines prefix)
  (remove-if-not (lambda (l) (and (>= (length l) (length prefix))
                                  (string-equal prefix l :end2 (length prefix))))
                 lines))

(deftest envelope-uses-the-addr-spec-of-display-name-addresses
  (multiple-value-bind (lines err)
      (%send-captured (make-message :from "Ann <ann@example.com>"
                                    :to "Bob <bob@example.com>, \"Doe, John\" <john@example.com>"
                                    :subject "display names" :body "hello"))
    (ok (null err) (format nil "sent (~@[~A~])" err))
    (ok (equal (list (%crlf-text "MAIL FROM:<ann@example.com>" ""))
               (%command-lines lines "MAIL FROM:"))
        (format nil "MAIL FROM carries the addr-spec, as smtplib's send_message (got ~S)"
                (%command-lines lines "MAIL FROM:")))
    (ok (equal (list (%crlf-text "RCPT TO:<bob@example.com>" "")
                     (%crlf-text "RCPT TO:<john@example.com>" ""))
               (%command-lines lines "RCPT TO:"))
        (format nil "one RCPT TO per address, addr-spec only (got ~S)"
                (%command-lines lines "RCPT TO:")))))

(deftest crlf-in-a-header-value-is-refused
  (let* ((subject (%crlf-text "Hi" "Bcc: eve@example.com" "" "Injected body"))
         (wire (handler-case
                   (print-message (make-message :from "ann@example.com" :to "bob@example.com"
                                                :subject subject :body "hello"))
                 (error () :refused))))
    (ok (or (eq :refused wire)
            (and (null (%header wire "Bcc")) (%header wire "Date")))
        (format nil "a Subject holding CR LF is refused (Python: ValueError) or kept in its header; ~
                     the header block was ~S"
                (if (stringp wire) (%header-block wire) wire)))))

(deftest crlf-in-an-envelope-address-is-refused
  (multiple-value-bind (lines err)
      (%send-captured
       (handler-case
           (make-message :from "ann@example.com"
                         :to (%crlf-text "bob@example.com>" "RCPT TO:<eve@example.com")
                         :subject "x" :body "hello")
         (error (e) e)))
    (let ((injected (find (%crlf-text "RCPT TO:<eve@example.com>" "") lines :test #'string=)))
      (ok (and err (null injected))
          (format nil "refused before it reaches the wire (Python: ValueError); error ~S, ~
                       commands received ~S"
                  (and err (princ-to-string err))
                  (remove-if-not (lambda (l) (find #\: l)) lines))))))

(deftest live-display-name-send
  (let ((port (parse-integer (%env "MAIL_SMTP_TEST_PLAIN_PORT" "11025"))))
    (if (not (handler-case (progn (usocket:socket-close (usocket:socket-connect "127.0.0.1" port
                                                                                :timeout 2))
                                  t)
               (error () nil)))
        (skip (format nil "no SMTP server on 127.0.0.1:~D (MAIL_SMTP_TEST_PLAIN_PORT)" port))
        (let ((err (handler-case
                       (let ((mail-protocol:*mail-backend*
                               (mail-backend-smtp:make-smtp-backend :host "127.0.0.1" :port port)))
                         (send (make-message :from "Ann <ann@example.com>"
                                             :to "Bob <bob@example.com>"
                                             :subject "display names" :body "hello"))
                         nil)
                     (error (e) (princ-to-string e)))))
          (ok (null err)
              (format nil "From \"Ann <ann@example.com>\" sends, as with smtplib (got ~A)" err))))))
