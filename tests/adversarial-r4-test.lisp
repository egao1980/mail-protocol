(in-package #:mail-protocol/tests)

;;; Adversarial round against PR #6 head 485a82b (mime-protocol 0.3.2).
;;; Python 3.13 email / smtplib is the reference (checked 2026-11-24).
;;; Every test here fails at 485a82b.

(defun %adv-ascii-p (string)
  (every (lambda (c) (< (char-code c) 128)) string))

;;; 1. A non-ASCII group display name, or the display name of a group member,
;;; goes into To as raw UTF-8. The header block is 8-bit although the
;;; transaction negotiates no SMTPUTF8 (the envelope is ASCII), which the
;;; mailbox case avoids with RFC 2047 (#5). Python's EmailMessage writes
;;;   To: =?utf-8?q?=C3=89quipe?=: a@example.com, b@example.com;
;;;   To: Team: =?utf-8?q?Zo=C3=AB?= <z@example.com>;
;;; and Python's parser reads this library's raw member name as
;;; 'Zo\udcc3\udcab'.

(deftest-parametrize non-ascii-group-display-names-are-encoded-words
    ((to members)
     ((format nil "~Cquipe: a@example.com, b@example.com;" (code-char #xC9))
      '("a@example.com" "b@example.com"))
     ((format nil "Team: Zo~C <z@example.com>, b@example.com;" (code-char #xEB))
      '("z@example.com" "b@example.com")))
  (let ((wire (print-message (make-message :from "a@example.com" :to to :subject "x" :body "y"))))
    (ok (%adv-ascii-p (%header-block wire))
        (format nil "the header block is ASCII, as with a mailbox display name (To: ~S)"
                (%header wire "To")))
    (ok (equal members (envelope-recipients (parse-message wire))))))

;;; 2. add-attachment with a long non-ASCII filename: PRINT-MESSAGE, and so
;;; SEND, signals MIME-HEADER-ERROR "a line of 1057 octets has no space to
;;; fold it". filename*=UTF-8''... is one token of 9 octets per CJK
;;; character, so about 110 characters are enough. Python's add_attachment
;;; splits it into RFC 2231 continuations (filename*0*=utf-8''...;
;;; filename*1*=...), every line within 78, and the message goes out.

(deftest long-non-ascii-attachment-filename-is-sent
  (let* ((filename (concatenate 'string (make-string 115 :initial-element (code-char #x65E5))
                                ".pdf"))
         (m (make-message :from "a@example.com" :to "b@example.com" :body "x"))
         (wire (progn
                 (add-attachment m (coerce #(37 80 68 70) '(vector (unsigned-byte 8)))
                                 :filename filename)
                 (handler-case (print-message m)
                   (error (e) e)))))
    (ok (stringp wire) (format nil "print-message returns the message (got ~A)" wire))
    (when (stringp wire)
      (ok (every (lambda (l) (<= (length l) 998))
                 (uiop:split-string wire :separator '(#\Newline)))
          "no line longer than 998")
      (let ((part (first (message-attachments (parse-message wire)))))
        (ok (equal filename
                   (and part (mime-protocol:disposition-filename
                              (mime-protocol:mime-content-disposition part))))
            "the filename reads back")))))

;;; 3. The reading side of 2: Python (and Thunderbird) write a non-ASCII
;;; filename of more than a few characters as RFC 2231 continuations. Below
;;; is Python 3.13's add_attachment(..., filename="日本語のファイル名.pdf")
;;; (boundary set to b1); its get_filename() gives the name back. Here
;;; message-attachments finds the part, but its filename is NIL:
;;; disposition-filename reads filename and filename*, not filename*0* /
;;; filename*1*.

(deftest python-rfc2231-continued-filename-is-read
  (let* ((wire (%crlf-text "From: a@example.com" "To: b@example.com" "Subject: report"
                           "MIME-Version: 1.0" "Content-Type: multipart/mixed; boundary=\"b1\""
                           "" "--b1" "Content-Type: text/plain; charset=\"utf-8\""
                           "Content-Transfer-Encoding: 7bit" "" "see attached" ""
                           "--b1" "Content-Type: application/pdf"
                           "Content-Transfer-Encoding: base64"
                           "Content-Disposition: attachment;"
                           " filename*0*=utf-8''%E6%97%A5%E6%9C%AC%E8%AA%9E%E3%81%AE%E3%83%95%E3%82%A1;"
                           " filename*1*=%E3%82%A4%E3%83%AB%E5%90%8D.pdf"
                           "MIME-Version: 1.0" "" "JVBERg==" "" "--b1--" ""))
         (expected (concatenate 'string
                                (map 'string #'code-char
                                     '(#x65E5 #x672C #x8A9E #x306E #x30D5 #x30A1 #x30A4 #x30EB
                                       #x540D))
                                ".pdf"))
         (parts (message-attachments (parse-message wire)))
         (name (and parts (mime-protocol:disposition-filename
                           (mime-protocol:mime-content-disposition (first parts))))))
    (ok (= 1 (length parts)) "one attachment")
    (ok (equal expected name)
        (format nil "the filename Python's get_filename() reads (got ~S)" name))))

;;; 4. Regression: smtp-dialogue :from "" sent MAIL FROM:<> on main: the
;;; null reverse-path (RFC 5321 4.5.5, 3.6.3) that bounces, DSNs and RFC 3834
;;; auto-replies are sent with. Python: sendmail("", ...) sends MAIL FROM:<>,
;;; and smtplib.quoteaddr gives "<>" for both "" and "<>". At 485a82b both
;;; signal MAIL-ADDRESS-ERROR "empty local part" and nothing is sent.

(deftest-parametrize smtp-dialogue-sends-a-null-reverse-path
    ((from)
     ("")
     ("<>"))
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from from :recipients '("b@example.com") :data "x")
    (ok (eq t result) (format nil "~S sends (got ~A)" from result))
    (ok (search (%lines "MAIL FROM:<>") out) (format nil "~S → ~S" from out))))

;;; 5. envelope-recipients drops a recipient whose local part differs from
;;; another's only in case: To Bob@example.com, Cc bob@example.com gives one
;;; RCPT TO. RFC 5321 2.4: the local part MUST be treated as case sensitive
;;; (only the domain is not). smtplib's send_message sends both:
;;; ['Bob@example.com', 'bob@example.com']. (main deduplicated the raw
;;; header text with STRING-EQUAL too.)

(deftest envelope-keeps-local-parts-that-differ-in-case
  (let ((rcpts (envelope-recipients
                (make-message :from "a@example.com" :to "Bob@example.com" :cc "bob@example.com"))))
    (ok (and (member "Bob@example.com" rcpts :test #'string=)
             (member "bob@example.com" rcpts :test #'string=))
        (format nil "both mailboxes get the message (got ~S)" rcpts))))

;;; 6. A message the server has accepted ("250 queued") is reported as a
;;; failed send when the server closes the connection instead of answering
;;; QUIT with 221: MAIL-SEND-ERROR "SMTP connection closed", with no code and
;;; nothing saying the message went through, so a caller that retries
;;; delivers it twice. Python's send_message has returned by then, and
;;; SMTP.__exit__ ignores SMTPServerDisconnected on QUIT. (main had the same
;;; (%expect in 221).)

(deftest accepted-message-is-not-a-failed-send-when-quit-gets-no-reply
  (multiple-value-bind (result out)
      (%scripted '("220 hi" "250 mx" "250 ok" "250 ok" "354 go" "250 2.0.0 queued as 1234")
                 :from "a@example.com" :recipients '("b@example.com") :data "x")
    (ok (search (%lines "QUIT") out) "QUIT was sent after the 250")
    (ok (eq t result) (format nil "the server took the message (got ~A)" result))))
