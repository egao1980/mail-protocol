(in-package #:mail-protocol/tests)

;;; egao1980/mail-protocol#16: leftovers of the #9 adversarial check.
;;; Strict modified UTF-7 (RFC 3501 §5.1.3: the canonical form only, as
;;; Dovecot's imap-utf7 checks it), SELECT state on OK only (Python 3.13
;;; imaplib drops to AUTH on a failed SELECT), binary streams, and APPEND /
;;; STORE. The APPEND and STORE wire forms are what imaplib sent to
;;; GreenMail 2.1.3 on 2026-11-27:
;;;   APPEND INBOX (\Seen \Flagged) {76}   (LF → CRLF, as MapCRLF does)
;;;   STORE 2 +FLAGS (\Deleted)
;;;   STORE 2 -FLAGS.SILENT (\Deleted)

(deftest-parametrize utf7-decode-refuses-non-canonical
    ((label wire)
     ("printable ASCII in base64" "&AGE-")
     ("leftover bits without a unit" "&A-")
     ("literal / inside base64" "&AGEAYgBj/-")
     ("non-zero padding bits" "&AOR-")
     ("pad character" "&AOQ=-")
     ("two adjacent runs" "&AOQ-&APY-")
     ("a CR" "&AA0-")
     ("a LF" "&AAo-")
     ("a NUL" "&AAA-")
     ("a lone high surrogate" "&2Dw-")
     ("a lone low surrogate" "&34k-")
     ("an unclosed run" "&AOQ")
     ("a raw 8-bit character" (%u "Entw~Crfe" #xFC))
     ("a raw control character" (format nil "a~Cb" #\Tab)))
  (let ((c (handler-case (progn (imap-utf7-decode wire) nil)
             (error (e) e))))
    (ok (typep c 'imap-error) (format nil "~A (~S): IMAP-ERROR" label wire))
    (ok (and c (search "modified UTF-7" (princ-to-string c))
             (not (search "assert" (princ-to-string c) :test #'char-equal)))
        (format nil "~A: our own message, not the codec's" label))))

(deftest-parametrize utf7-decode-canonical
    ((wire name)
     ("&AOQA9g-" (%u "~C~C" #xE4 #xF6))
     ("&-" "&")
     ("&AOQ-x&APY-" (%u "~Cx~C" #xE4 #xF6))
     ("&2DzfiQ-" (%u "~C" #x1F389))
     ("" ""))
  (ok (string= name (imap-utf7-decode wire)))
  (ok (string= wire (imap-utf7-encode name)) "and the encoder gives the same form"))

(defun %scripted-client (replies)
  "A client whose io-fn answers each command with the next of REPLIES (a
list of lines, the tag written as ~A)."
  (let ((client (make-imap-client
                 :io-fn (lambda (cmd)
                          (if (null cmd)
                              '("* OK ready")
                              (let ((tag (subseq cmd 0 (position #\Space cmd))))
                                (mapcar (lambda (l) (format nil l tag))
                                        (or (pop replies) (list "~A OK")))))))))
    (imap-connect client)
    client))

(deftest-parametrize select-sets-state-only-on-ok
    ((status)
     ("NO")
     ("BAD"))
  (let ((c (%scripted-client (list (list "* 1 EXISTS" "~A OK [READ-WRITE] done")
                                   (list (format nil "~~A ~A no such mailbox" status))))))
    (setf (imap-client-state c) :authenticated)
    (imap-select c "INBOX")
    (ok (eq :selected (imap-client-state c)))
    (ok (signals (imap-select c "Missing") 'imap-error)
        (format nil "SELECT answered ~A is IMAP-ERROR" status))
    (ok (null (imap-client-selected c)) "no mailbox is selected")
    (ok (eq :authenticated (imap-client-state c))
        "back to authenticated, as imaplib's state AUTH")))

(deftest select-failure-carries-the-status
  (let ((c (%scripted-client (list (list "~A NO [NONEXISTENT] no such mailbox")))))
    (setf (imap-client-state c) :authenticated)
    (let ((e (handler-case (imap-select c "Missing") (imap-error (e) e))))
      (ok (eq :no (imap-error-status e)))
      (ok (search "no such mailbox" (princ-to-string e))))))

;;; Binary streams: octets in, octets out. A reply in Latin-1 reads, and
;;; the client stays in step.

(defun %binary-client (octets)
  "→ (values client out): a client over a binary two-way stream reading
OCTETS; OUT is the in-memory stream the client writes."
  (let ((in (flex:make-in-memory-input-stream (coerce octets '(vector (unsigned-byte 8)))))
        (out (flex:make-in-memory-output-stream)))
    (values (make-imap-client :stream (make-two-way-stream in out)) out)))

(defun %octets (control &rest args)
  (map 'list #'char-code (apply #'format nil control args)))

(deftest binary-stream-reads-a-latin-1-reply
  (multiple-value-bind (c out)
      (%binary-client (append (%octets "A0001 NO [ALREADYEXISTS] Entw")
                              (list #xFC)
                              (%octets "rfe exists~C~%A0002 OK NOOP done~C~%" #\Return #\Return)))
    (setf (imap-client-state c) :authenticated)
    (let ((parsed (imap-command c "CREATE" "x")))
      (ok (eq :no (third (find :tagged parsed :key #'first))))
      (ok (search "exists" (fourth (find :tagged parsed :key #'first)))))
    (ok (eq :authenticated (imap-client-state c)) "not disconnected")
    (ok (eq :ok (third (find :tagged (imap-command c "NOOP") :key #'first)))
        "the next reply is read in step")
    (ok (equal (%octets "A0001 CREATE x~C~%A0002 NOOP~C~%" #\Return #\Return)
               (coerce (flex:get-output-stream-sequence out) 'list)))))

(deftest binary-stream-writes-utf-8-literals
  (multiple-value-bind (c out)
      (%binary-client (%octets "+ go~C~%A0001 OK done~C~%" #\Return #\Return))
    (setf (imap-client-state c) :connected)
    (imap-login c "alice" (%u "p~Cssword" #xE4))
    (ok (eq :authenticated (imap-client-state c)))
    (ok (equal (append (%octets "A0001 LOGIN alice {9}~C~%p" #\Return)
                       (list #xC3 #xA4)
                       (%octets "ssword~C~%" #\Return))
               (coerce (flex:get-output-stream-sequence out) 'list)))))

(deftest binary-stream-counts-literal-octets
  (let* ((body (append (%octets "Subject: x~C~%Content-Type: text/plain; charset=utf-8~C~%~C~%"
                                #\Return #\Return #\Return)
                       (list #xC3 #xBC #x2D #xC3 #xBC #x20)
                       (%octets "end~C~%" #\Return)))
         (wire (append (%octets "* 1 FETCH (RFC822 {~D}~C~%" (length body) #\Return)
                       body
                       (%octets ")~C~%A0001 OK done~C~%" #\Return #\Return))))
    (let ((c (%binary-client wire)))
      (setf (imap-client-state c) :selected)
      (let ((got (imap-fetch c "1")))
        (ok (and (= 1 (length got)) (message-p (first got))))
        (ok (search (%u "~C-~C end" #xFC #xFC) (message-body (first got))))))))

;;; APPEND and STORE.

(defun %crlf (control &rest args)
  "CONTROL with every ~% a CR LF."
  (let ((s (apply #'format nil control args)))
    (with-output-to-string (o)
      (loop for ch across s
            do (when (char= ch #\Newline) (write-char #\Return o))
               (write-char ch o)))))

(deftest append-sends-the-message-as-a-literal
  (multiple-value-bind (client sent) (%recording-client)
    (let ((text (%crlf "Subject: x~%~%line1~%line2~%")))
      (imap-append client "INBOX" (format nil "Subject: x~%~%line1~Cline2~C~%" #\Return #\Return)
                   :flags '("\\Seen" "\\Flagged"))
      (ok (equal (list (format nil "A0001 APPEND INBOX (\\Seen \\Flagged) {~D}" (length text))
                       text)
                 (funcall sent))
          "bare LF and bare CR become CR LF; CR LF stays"))))

(deftest append-mailbox-date-and-utf-8
  (multiple-value-bind (client sent) (%recording-client)
    (let ((text (%crlf "Subject: x~%~%caf~C~%" (code-char #xE9))))
      (imap-append client (%u "Entw~Crfe" #xFC) text :date "27-Nov-2026 10:00:00 +0000")
      (ok (equal (list (format nil "A0001 APPEND Entw&APw-rfe \"27-Nov-2026 10:00:00 +0000\" {~D}"
                               (1+ (length text)))
                       text)
                 (funcall sent))
          "modified UTF-7 name, quoted date, UTF-8 octet count"))))

(deftest append-a-message-object
  (multiple-value-bind (client sent) (%recording-client)
    (let ((msg (make-message :from "a@x.example" :to "b@x.example" :subject "Hi" :body "hello")))
      (imap-append client "INBOX" msg)
      (let ((segments (funcall sent)))
        (ok (= 2 (length segments)))
        (ok (search "Subject: Hi" (second segments)))
        (ok (string= (format nil "A0001 APPEND INBOX {~D}" (length (second segments)))
                     (first segments)))))))

(deftest-parametrize append-still-refuses-injection
    ((label thunk)
     ("mailbox" (lambda (c) (imap-append c (format nil "INBOX~C~CA0002 DELETE INBOX"
                                                   #\Return #\Newline)
                                         "x")))
     ("flag" (lambda (c) (imap-append c "INBOX" "x"
                                      :flags (list (format nil "\\Seen)~C~CA0002 DELETE INBOX"
                                                           #\Return #\Newline)))))
     ("date" (lambda (c) (imap-append c "INBOX" "x" :date (%crlf-injection))))
     ("NUL in the message" (lambda (c) (imap-append c "INBOX" (format nil "a~Cb" (code-char 0)))))
     ("not a message" (lambda (c) (imap-append c "INBOX" 42))))
  (multiple-value-bind (client sent) (%recording-client)
    (ok (signals (funcall thunk client) 'imap-argument-error) label)
    (ok (null (funcall sent)) (format nil "~A: nothing written" label))))

(deftest imap-literal-allows-crlf-only-inside
  (multiple-value-bind (client sent) (%recording-client)
    (imap-command client "X-TEST" (imap-literal (format nil "a~C~Cb" #\Return #\Newline)))
    (ok (equal (list "A0001 X-TEST {4}" (format nil "a~C~Cb" #\Return #\Newline))
               (funcall sent)))
    (ok (signals (imap-command client "X-TEST" (imap-raw (format nil "a~C~Cb" #\Return #\Newline)))
          'imap-argument-error)
        "raw text still refuses CR LF")
    (ok (signals (imap-literal (format nil "a~Cb" (code-char 0))) 'imap-argument-error)
        "a literal holds no NUL (CHAR8)")))

(deftest append-failure-is-an-imap-error
  (let ((c (%scripted-client (list (list "+ go") (list "~A NO [TRYCREATE] no such mailbox")))))
    (setf (imap-client-state c) :authenticated)
    (ok (signals (imap-append c "Missing" "x") 'imap-error))))

(deftest store-flags
  (multiple-value-bind (client sent) (%recording-client)
    (imap-store client 2 '("\\Deleted"))
    (imap-store client "1:3" '("\\Seen" "$Forwarded") :mode :remove :silent t)
    (imap-store client 5 '() :mode :replace)
    (imap-store client 7 '("\\Seen") :uid t)
    (ok (equal '("A0001 STORE 2 +FLAGS (\\Deleted)"
                 "A0002 STORE 1:3 -FLAGS.SILENT (\\Seen $Forwarded)"
                 "A0003 STORE 5 FLAGS ()"
                 "A0004 UID STORE 7 +FLAGS (\\Seen)")
               (funcall sent)))))

(deftest-parametrize store-refuses-bad-flags
    ((label flags mode)
     ("space" '("a b") :add)
     ("bare backslash" '("\\") :add)
     ("bracket" '("x]") :add)
     ("paren" '("(x") :add)
     ("CR LF" (list (%crlf-injection)) :add)
     ("not a string" '(42) :add)
     ("8-bit" (list (%u "\\S~Cen" #xE9)) :add)
     ("bad mode" '("\\Seen") :toggle))
  (multiple-value-bind (client sent) (%recording-client)
    (ok (signals (imap-store client 1 flags :mode mode) 'imap-argument-error) label)
    (ok (null (funcall sent)) (format nil "~A: nothing written" label))))

(deftest store-failure-is-an-imap-error
  (let ((c (%scripted-client (list (list "~A NO not selected")))))
    (ok (signals (imap-store c 1 '("\\Seen")) 'imap-error))))

;;; Live, against GreenMail (see tests/imap-live-test.lisp for the docker line).

(defun %unique ()
  "A suffix no earlier run used: the server keeps what earlier runs made."
  (format nil "~D-~D" (get-universal-time) (random 1000000 (make-random-state t))))

(defun %binary-live-stream ()
  "→ (values stream closer): a binary socket stream to the test server."
  (let ((socket (usocket:socket-connect "127.0.0.1" (%imap-port) :timeout 5
                                                                 :element-type '(unsigned-byte 8))))
    (values (usocket:socket-stream socket)
            (lambda () (usocket:socket-close socket)))))

(deftest live-imap-binary-stream-and-latin-1-reply
  (if (not (%imap-up-p))
      (skip (format nil "no IMAP server on 127.0.0.1:~D (MAIL_IMAP_TEST_PORT)" (%imap-port)))
      (multiple-value-bind (stream close) (%binary-live-stream)
        (unwind-protect
            (let ((c (make-imap-client :stream stream))
                  (name (format nil "~A ~A" (%u "Entw~Crfe" #xFC) (%unique))))
              (imap-connect c)
              (imap-login c "zoe" (%u "p~Cssw~Crd" #xE4 #xF6))
              (ok (eq :authenticated (imap-client-state c)) "8-bit password over octets")
              (ok (eq :ok (%status (imap-command c "CREATE" (imap-utf7-encode name)))))
              (ok (eq :no (%status (imap-command c "CREATE" (imap-utf7-encode name))))
                  "GreenMail answers the second CREATE in Latin-1, and it reads")
              (ok (eq :authenticated (imap-client-state c)))
              (ok (eq :ok (%status (imap-command c "NOOP"))) "still in step"))
          (funcall close)))))

(deftest live-imap-select-failure
  (if (not (%imap-up-p))
      (skip (format nil "no IMAP server on 127.0.0.1:~D (MAIL_IMAP_TEST_PORT)" (%imap-port)))
      (%with-live-imap (c s)
        (imap-login c "alice" "secret")
        (imap-select c "INBOX")
        (ok (eq :selected (imap-client-state c)))
        (ok (signals (imap-select c "NoSuchBox") 'imap-error))
        (ok (eq :authenticated (imap-client-state c)))
        (ok (null (imap-client-selected c)))
        ;; GreenMail 2.1.3: "NO FETCH failed. Command not valid in this state".
        (ok (member (%status (imap-command c "FETCH" 1 (imap-raw "FLAGS"))) '(:no :bad))
            "the server agrees: no mailbox is selected"))))

(deftest live-imap-append-and-store
  (if (not (%imap-up-p))
      (skip (format nil "no IMAP server on 127.0.0.1:~D (MAIL_IMAP_TEST_PORT)" (%imap-port)))
      (dolist (kind '(:utf-8 :latin-1 :binary))
        (let ((subject (format nil "append-~(~A~)-~A" kind (%unique))))
          (multiple-value-bind (stream close)
              (if (eq kind :binary)
                  (%binary-live-stream)
                  (%live-imap-stream (eq kind :latin-1)))
            (unwind-protect
                (let ((c (make-imap-client :stream stream)))
                  (imap-connect c)
                  (imap-login c "alice" "secret")
                  (imap-append c "INBOX"
                               (format nil "From: a@x.example~%Subject: ~A~%Content-Type: text/plain; charset=utf-8~%~%caf~C ~C~%A0009 OK not the tag~%"
                                       subject (code-char #xE9) (code-char #x2615))
                               :flags '("\\Seen"))
                  (imap-append c "INBOX" (make-message :from "a@x.example" :to "alice@x.example"
                                                       :subject (format nil "~A obj" subject)
                                                       :body (%u "Gr~C~Ce" #xFC #xDF)))
                  (imap-select c "INBOX")
                  (let ((hits (imap-search c (list :subject subject))))
                    (ok (= 2 (length hits)) (format nil "~A: both appended" kind))
                    (let ((got (imap-fetch c (first hits))))
                      (ok (search (format nil "caf~C ~C" (code-char #xE9) (code-char #x2615))
                                  (message-body (first got)))
                          (format nil "~A: the UTF-8 body arrived whole" kind)))
                    (let ((got (imap-fetch c (second hits))))
                      (ok (search (%u "Gr~C~Ce" #xFC #xDF) (message-body (first got)))
                          (format nil "~A: the message object arrived" kind)))
                    (imap-store c (first hits) '("\\Flagged" "$Label1"))
                    (imap-store c (first hits) '("\\Seen") :mode :remove :silent t)
                    (let ((raw (princ-to-string (imap-command c "FETCH" (first hits)
                                                              (imap-raw "(FLAGS)")))))
                      (ok (and (search "\\Flagged" raw) (search "$Label1" raw)
                               (not (search "\\Seen" raw)))
                          (format nil "~A: STORE set and cleared flags: ~A" kind raw))))
                  (ok (eq :ok (%status (imap-command c "NOOP"))) (format nil "~A: in step" kind)))
              (funcall close)))))))
