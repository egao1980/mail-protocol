(in-package #:mail-protocol/tests)

;;; egao1980/mail-protocol#9: IMAP command injection. An argument holding
;;; CR, LF or NUL ended the command line and let the rest pose as another
;;; tagged command. Python 3.13.18 imaplib refuses these (_control_chars,
;;; "NUL, CR and LF not allowed in commands") and quotes other values the
;;; way _astring does: an atom when every octet is an ASTRING-CHAR, else
;;; "..." with \ and " escaped. The atom/quoted values and the modified
;;; UTF-7 names below were computed with imaplib and a utf-16-be/base64
;;; codec (checked 2026-11-26).

(defun %crlf-injection ()
  (format nil "pw~C~CA0002 DELETE INBOX" #\Return #\Newline))

(defun %recording-client (&key utf8-accept)
  "→ (values client sent): SENT returns every segment the client handed its
io-fn. A segment ending in {n} gets a + continuation, the rest a tagged OK."
  (let* ((sent '())
         (tag nil)
         (client (make-imap-client
                  :io-fn (lambda (cmd)
                           (cond
                             ((null cmd) '("* OK IMAP4rev1 ready"))
                             (t (push cmd sent)
                                (unless tag
                                  (setf tag (subseq cmd 0 (position #\Space cmd))))
                                (if (and (plusp (length cmd))
                                         (char= #\} (char cmd (1- (length cmd)))))
                                    '("+ go ahead")
                                    (prog1 (list (format nil "~A OK done" tag))
                                      (setf tag nil)))))))))
    (when utf8-accept
      (setf (imap-client-utf8-accept client) t))
    (imap-connect client)
    (values client (lambda () (reverse sent)))))

(deftest-parametrize encode-refuses-line-breaks-and-nul
    ((label value)
     ("CR LF" (%crlf-injection))
     ("bare LF" (format nil "a~Cb" #\Newline))
     ("bare CR" (format nil "a~Cb" #\Return))
     ("NUL" (format nil "a~Cb" (code-char 0))))
  (ok (signals (encode-imap-command "A0001" "LOGIN" "user" value) 'imap-error)
      (format nil "~A in a string argument is refused" label))
  (ok (signals (encode-imap-command "A0001" "LOGIN" value "pw") 'imap-error)
      (format nil "~A in the user is refused" label)))

(deftest-parametrize client-refuses-injection-before-writing
    ((label thunk)
     ("login password" (lambda (c) (imap-login c "alice" (%crlf-injection))))
     ("login user" (lambda (c) (imap-login c (%crlf-injection) "pw")))
     ("select mailbox" (lambda (c) (imap-select c (format nil "INBOX~C~CA0002 DELETE INBOX"
                                                          #\Return #\Newline))))
     ("search criteria string" (lambda (c) (imap-search c (format nil "ALL~C~CA0002 DELETE INBOX"
                                                                  #\Return #\Newline))))
     ("search criteria list" (lambda (c) (imap-search c (list :from (%crlf-injection)))))
     ("search NUL" (lambda (c) (imap-search c (format nil "ALL~C" (code-char 0)))))
     ("fetch sequence" (lambda (c) (imap-fetch c (format nil "1~C~CA0002 DELETE INBOX"
                                                         #\Return #\Newline))))
     ("fetch items" (lambda (c) (imap-fetch c "1" :items (format nil "RFC822~C~CA0002 DELETE INBOX"
                                                                 #\Return #\Newline)))))
  (multiple-value-bind (client sent) (%recording-client)
    (ok (signals (funcall thunk client) 'imap-error)
        (format nil "~A: refused with IMAP-ERROR" label))
    (ok (null (funcall sent)) (format nil "~A: nothing written" label))))

(deftest refusal-is-an-imap-argument-error
  (let ((c (handler-case (encode-imap-command "A0001" "LOGIN" "u" (%crlf-injection))
             (imap-error (c) c))))
    (ok (typep c 'imap-argument-error))
    (ok (typep c 'mail-error))
    (ok (search "CR" (princ-to-string c)))))

(deftest-parametrize astring-quoting-matches-imaplib
    ((value wire)
     ("alice" "alice")
     ("p ass" "\"p ass\"")
     ("" "\"\"")
     ("a\"b" "\"a\\\"b\"")
     ("a\\b" "\"a\\\\b\"")
     ("INBOX" "INBOX")
     ("a%b" "\"a%b\"")
     ("a*b" "\"a*b\"")
     ("x(y" "\"x(y\"")
     ("{3}" "\"{3}\"")
     ("a]b" "a]b")
     ((format nil "tab~Cx" #\Tab) (format nil "\"tab~Cx\"" #\Tab))
     ((format nil "~Cctl" (code-char 1)) (format nil "\"~Cctl\"" (code-char 1))))
  (ok (string= (format nil "A0001 LOGIN ~A x" wire)
               (encode-imap-command "A0001" "LOGIN" value "x"))
      (format nil "~S → ~A" value wire)))

(deftest eight-bit-password-goes-as-a-literal
  (multiple-value-bind (client sent) (%recording-client)
    (imap-login client "alice" (%u "p~Cssword" #xE4))
    (ok (equal (list "A0001 LOGIN alice {9}" (%u "p~Cssword" #xE4))
               (funcall sent))
        "{9}: the UTF-8 octet count, then the value after the + continuation")
    (ok (eq :authenticated (imap-client-state client)))))

(deftest long-password-goes-as-a-literal
  (let ((pw (make-string 2000 :initial-element #\x)))
    (multiple-value-bind (client sent) (%recording-client)
      (imap-login client "alice" pw)
      (ok (equal (list "A0001 LOGIN alice {2000}" pw) (funcall sent))))))

(deftest literal-refused-by-the-server-stops-the-command
  (let* ((sent '())
         (client (make-imap-client
                  :io-fn (lambda (cmd)
                           (if cmd
                               (progn (push cmd sent) '("A0001 NO no literals here"))
                               '("* OK ready"))))))
    (imap-connect client)
    (ok (signals (imap-login client "alice" (%u "p~Cssword" #xE4)) 'imap-auth-error))
    (ok (equal '("A0001 LOGIN alice {9}") sent) "the literal is not sent without a +")))

(deftest-parametrize mailbox-names-use-modified-utf-7
    ((name wire)
     ((%u "Entw~Crfe" #xFC) "Entw&APw-rfe")
     ("A&B" "A&-B")
     ((%u "~C~C~C" #x65E5 #x672C #x8A9E) "&ZeVnLIqe-")
     ((%u "~~peter/mail/~C~C/~C~C~C" #x53F0 #x5317 #x65E5 #x672C #x8A9E)
      "~peter/mail/&U,BTFw-/&ZeVnLIqe-")
     ((%u "~Cn~Cc~Cd~C & ~C x" #xDC #xEF #xF8 #xE9 #x1F389)
      "&ANw-n&AO8-c&APg-d&AOk- &- &2DzfiQ- x")
     ((format nil "Tab~Chere" #\Tab) "Tab&AAk-here"))
  (ok (string= wire (imap-utf7-encode name)) (format nil "~S encodes as ~A" name wire))
  (ok (string= name (imap-utf7-decode wire)) (format nil "~A decodes back" wire)))

(deftest select-sends-modified-utf-7
  (multiple-value-bind (client sent) (%recording-client)
    (imap-select client (%u "Entw~Crfe" #xFC))
    (ok (equal '("A0001 SELECT Entw&APw-rfe") (funcall sent)))
    (ok (string= (%u "Entw~Crfe" #xFC) (imap-client-selected client)))))

(deftest select-with-utf8-accept-sends-utf-8
  (multiple-value-bind (client sent) (%recording-client :utf8-accept t)
    (imap-select client (%u "Entw~Crfe" #xFC))
    (ok (equal (list (%u "A0001 SELECT \"Entw~Crfe\"" #xFC)) (funcall sent))
        "UTF8=ACCEPT (RFC 6855 / 9051): UTF-8 in a quoted string")))

(deftest enable-utf8-accept-records-it
  (let ((client (make-imap-client
                 :io-fn (lambda (cmd)
                          (cond ((null cmd) '("* OK [CAPABILITY IMAP4rev1 ENABLE UTF8=ACCEPT] ready"))
                                ((search "ENABLE" cmd)
                                 '("* ENABLED UTF8=ACCEPT" "A0001 OK enabled"))
                                (t '("A9999 BAD")))))))
    (imap-connect client)
    (ok (member "UTF8=ACCEPT" (imap-client-capabilities client) :test #'string-equal)
        "the greeting's CAPABILITY code is kept")
    (imap-enable client "UTF8=ACCEPT")
    (ok (imap-client-utf8-accept client))))

(deftest search-criteria
  (multiple-value-bind (client sent) (%recording-client)
    (imap-search client "FROM alice UNSEEN")
    (imap-search client (list :from "Zoe Smith" :since "1-Jan-2026"))
    (imap-search client (list :subject (%u "Gr~C~Ce" #xFC #xDF)))
    (ok (equal (list "A0001 SEARCH FROM alice UNSEEN"
                     "A0002 SEARCH FROM \"Zoe Smith\" SINCE 1-Jan-2026"
                     "A0003 SEARCH CHARSET UTF-8 SUBJECT {6}"
                     (%u "Gr~C~Ce" #xFC #xDF))
               (funcall sent))
        "a string is search-key syntax; a list quotes its strings, 8-bit as a literal")))

(deftest search-refuses-eight-bit-in-raw-criteria
  (multiple-value-bind (client sent) (%recording-client)
    (ok (signals (imap-search client (%u "SUBJECT Gr~Cn" #xFC)) 'imap-error))
    (ok (null (funcall sent)))))

(deftest fetch-sequence-and-items-are-not-quoted
  (multiple-value-bind (client sent) (%recording-client)
    (imap-fetch client "1:*" :items "(FLAGS RFC822)")
    (imap-fetch client 7)
    (ok (equal '("A0001 FETCH 1:* (FLAGS RFC822)" "A0002 FETCH 7 RFC822") (funcall sent)))
    (ok (signals (imap-fetch client "1 2") 'imap-error) "a sequence set has no space")))

(deftest literal-over-a-stream-waits-for-the-continuation
  (let* ((in (make-string-input-stream
              (format nil "+ go~C~%A0001 OK LOGIN done~C~%" #\Return #\Return)))
         (out (make-string-output-stream))
         (client (make-imap-client :stream (make-two-way-stream in out))))
    (setf (imap-client-state client) :connected)
    (imap-login client "alice" (make-string 1500 :initial-element #\y))
    (ok (string= (format nil "A0001 LOGIN alice {1500}~C~%~A~C~%"
                         #\Return (make-string 1500 :initial-element #\y) #\Return)
                 (get-output-stream-string out)))
    (ok (eq :authenticated (imap-client-state client)))))

(deftest stream-reads-a-fetch-literal-to-the-tagged-line
  (let* ((body (format nil "From: a@ex.com~C~%Subject: Hi~C~%~C~%A0001 OK is body text~C~%"
                       #\Return #\Return #\Return #\Return))
         (in (make-string-input-stream
              (format nil "* 1 FETCH (RFC822 {~D}~C~%~A)~C~%A0001 OK FETCH done~C~%* 2 EXISTS~C~%"
                      (length body) #\Return body #\Return #\Return #\Return)))
         (client (make-imap-client :stream (make-two-way-stream in (make-broadcast-stream)))))
    (setf (imap-client-state client) :selected)
    (let ((got (imap-fetch client "1")))
      (ok (= 1 (length got)))
      (ok (message-p (first got))))
    (ok (string= (format nil "* 2 EXISTS~C" #\Return) (read-line in))
        "reading stops at the tagged line, not at a body line that looks like one")))
