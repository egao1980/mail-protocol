(in-package #:mail-protocol/tests)

;;; PR #6 fixes for #4 (envelope = addr-specs), #8 (no CR/LF in an SMTP
;;; command), #5 (octets on the wire, RFC 2047 headers) and the paths next
;;; to them. Python 3.12's smtplib / EmailMessage is the reference.

(defun %scripted (replies &rest keys)
  "SMTP-DIALOGUE with KEYS against the scripted REPLIES (lines).
→ (values result-or-condition client-output)."
  (let ((out (make-string-output-stream)))
    (with-input-from-string (in (apply #'%lines replies))
      (let ((result (handler-case (apply #'mail-backend-smtp:smtp-dialogue in out keys)
                      (error (e) e))))
        (values result (get-output-stream-string out))))))

(defparameter +plain-replies+
  '("220 hi" "250 mx" "250 ok" "250 ok" "354 go" "250 queued" "221 bye"))

(defun %utf-8-octet-string (string)
  (map 'string #'code-char (encoding-protocol:encode string :encoding :utf-8)))

(defparameter +injected+ (format nil "bob@example.com>~C~CRCPT TO:<eve@example.com" #\Return #\Newline))

;;; --- #8: CR/LF in an address never reaches the wire -----------------------

(deftest-parametrize crlf-in-any-address-field-is-refused
    ((field)
     (:from) (:to) (:cc) (:bcc))
  (ok (signals (make-message field +injected+ :from "ann@example.com" :to "bob@example.com"
                             :subject "x" :body "y")
               'mail-address-error)
      (format nil "~S" field)))

(deftest crlf-in-a-bcc-set-later-is-refused-before-connecting
  (let ((m (make-message :from "ann@example.com" :to "bob@example.com" :body "x")))
    (setf (message-bcc m) (list +injected+))
    (multiple-value-bind (lines err) (%send-captured m)
      (ok (typep err 'mail-address-error) (format nil "~A" err))
      (ok (null lines) "nothing reached the server"))))

(deftest smtp-dialogue-refuses-crlf-before-writing
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from "ann@example.com" :recipients (list +injected+) :data "x")
    (ok (typep result 'mail-address-error) (format nil "~A" result))
    (ok (equal "" out) "not a byte written"))
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from "ann@example.com" :recipients '("bob@example.com")
                                 :data "x"
                                 :ehlo-host (%crlf-text "x" "MAIL FROM:<evil@example.com>"))
    (ok (typep result 'mail-send-error) "a CR LF in any command (EHLO) is refused")
    (ng (search "MAIL FROM" out))))

(deftest-parametrize smtp-path-arguments-are-addr-specs-only
    ((address)
     ("bob@example.com> NOTIFY=NEVER") ("bob@example.com SIZE=1") ("<bob@example.com> x")
     ("bob@example.com>") ("a b@example.com"))
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from "ann@example.com" :recipients (list address) :data "x")
    (ok (typep result 'mail-address-error) (format nil "~S → ~A" address result))
    (ng (search "RCPT" out))))

;;; --- #4: the envelope is the addr-spec ----------------------------------

(deftest-parametrize parse-address-reads-mailboxes
    ((address spec name)
     ("Ann <ann@example.com>" "ann@example.com" "Ann")
     ("\"Doe, John\" <john@example.com>" "john@example.com" "Doe, John")
     ("john@example.com (John Doe)" "john@example.com" nil)
     ("<@relay.example,@r2.example:john@example.com>" "john@example.com" nil)
     ("\"john doe\"@example.com" "\"john doe\"@example.com" nil)
     ("John . Doe @ example . com" "John.Doe@example.com" nil)
     ("=?utf-8?b?Wm/Dqw==?= <zoe@example.com>" "zoe@example.com"
      (format nil "Zo~C" (code-char #xEB)))
     ("J. R. Smith <jr@example.com>" "jr@example.com" "J. R. Smith")
     ("user@[192.0.2.1]" "user@[192.0.2.1]" nil)
     ("postmaster" "postmaster" nil))
  (multiple-value-bind (got-spec got-name) (parse-address address)
    (ok (equal spec got-spec) (format nil "~S → ~S" address got-spec))
    (ok (equal name got-name) (format nil "~S → name ~S" address got-name))))

(deftest-parametrize parse-address-refuses-what-is-no-mailbox
    ((address)
     ("bob@example.com>") ("<bob@example.com") ("\"unterminated <x@example.com>")
     ("a@b@example.com") ("Team: a@example.com;") ("") ("<>") ("a@example.com."))
  (ok (signals (parse-address address) 'mail-address-error) (format nil "~S" address)))

(deftest parse-address-use-value-restart
  (let ((spec (handler-bind ((mail-address-error (lambda (c) (use-value "ok@example.com" c))))
                (parse-address "bad>@example.com"))))
    (ok (equal "ok@example.com" spec))))

(deftest-parametrize address-list-cuts-between-addresses
    ((value addresses)
     ("Bob <bob@example.com>, \"Doe, John\" <john@example.com>"
      '("Bob <bob@example.com>" "\"Doe, John\" <john@example.com>"))
     ("Team: a@example.com, b@example.com;, c@example.com"
      '("Team: a@example.com, b@example.com;" "c@example.com"))
     ("a@example.com (comment, with a comma), b@example.com"
      '("a@example.com (comment, with a comma)" "b@example.com"))
     ("<@r1.example,@r2.example:a@example.com>, b@example.com"
      '("<@r1.example,@r2.example:a@example.com>" "b@example.com"))
     ("a@x, b@y" '("a@x" "b@y")))
  (ok (equal addresses (address-list value)) (format nil "~S → ~S" value (address-list value))))

(deftest envelope-expands-groups-and-strips-names
  ;; a@EXAMPLE.com is a@example.com: the domain is case-insensitive, the
  ;; local part is not (RFC 5321 2.4; see envelope-keeps-local-parts-that-differ-in-case).
  (let ((m (make-message :from "\"Ann A.\" <ann@example.com>"
                         :to "Team: a@example.com, \"B, b\" <b@example.com>;"
                         :cc "undisclosed-recipients:;"
                         :bcc "Carol <c@example.com>, a@EXAMPLE.com"
                         :body "x")))
    (ok (equal '("a@example.com" "b@example.com" "c@example.com") (envelope-recipients m))
        (format nil "~S" (envelope-recipients m)))
    (ok (equal "ann@example.com" (envelope-sender m)))
    (mime-protocol:set-header (message-entity m) "Sender" "Desk <desk@example.com>")
    (ok (equal "desk@example.com" (envelope-sender m)) "Sender wins, as in smtplib")))

(deftest smtp-dialogue-sends-addr-specs
  (multiple-value-bind (result out)
      (%scripted '("220 hi" "250 mx" "250 ok" "250 ok" "250 ok" "354 go" "250 queued" "221 bye")
                 :from "Ann <ann@example.com>"
                 :recipients '("Bob <bob@example.com>" "c@example.com")
                 :data "x")
    (ok (eq t result) (format nil "~A" result))
    (ok (search (%lines "MAIL FROM:<ann@example.com>" "RCPT TO:<bob@example.com>"
                        "RCPT TO:<c@example.com>")
                out)
        out)))

;;; --- #5: non-ASCII headers and bodies ------------------------------------

(deftest non-ascii-subject-and-display-name-are-encoded-words
  (let* ((subject (format nil "Caf~C ~C menu" (code-char #xE9) (code-char #x2615)))
         (from (format nil "Zo~C <zoe@example.com>" (code-char #xEB)))
         (to (format nil "\"Doe, Zo~C\" <z2@example.com>" (code-char #xEB)))
         (m (make-message :from from :to to :subject subject :body "x"))
         (wire (print-message m)))
    (ok (every (lambda (c) (< (char-code c) 128)) (%header-block wire)) "the header block is ASCII")
    (ok (eql 0 (search "=?utf-8?b?" (%header wire "Subject"))) (%header wire "Subject"))
    (ok (search "?= <zoe@example.com>" (%header wire "From")) (%header wire "From"))
    (ok (equal subject (message-subject m)))
    (ok (equal from (message-from m)))
    (ok (equal (list to) (message-to m)) (format nil "~S" (message-to m)))
    (let ((p (parse-message wire)))
      (ok (equal subject (message-subject p)) "the Subject reads back")
      (ok (equal from (message-from p)))
      (ok (equal (list to) (message-to p)))
      (ok (equal "zoe@example.com" (envelope-sender p)))
      (ok (equal '("z2@example.com") (envelope-recipients p))))))

(deftest subject-crlf-is-refused-with-a-mime-header-error
  (ok (signals (make-message :from "a@example.com" :to "b@example.com"
                             :subject (%crlf-text "Hi" "Bcc: eve@example.com"))
               'mime-protocol:mime-header-error))
  (let ((m (make-message :from "a@example.com" :to "b@example.com" :subject "ok")))
    (ok (signals (setf (message-subject m) (format nil "Hi~CBcc: x" #\Newline))
                 'mime-protocol:mime-header-error))
    (ok (equal "ok" (message-subject m)) "the Subject is unchanged")))

(deftest long-header-fields-fold-at-78
  (let* ((subject (format nil "~{~A~^ ~}" (loop for i below 30 collect (format nil "word~D" i))))
         (to (loop for i below 12 collect (format nil "Person ~D <p~D@example.com>" i i)))
         (wire (print-message (make-message :from "a@example.com" :to to :subject subject :body "x"))))
    (ok (every (lambda (l) (<= (length (string-right-trim '(#\Return) l)) 78))
               (uiop:split-string (%header-block wire) :separator '(#\Newline)))
        "every header line within 78")
    (let ((p (parse-message wire)))
      (ok (equal subject (message-subject p)))
      (ok (equal (loop for i below 12 collect (format nil "p~D@example.com" i))
                 (envelope-recipients p))))))

(deftest an-unfoldable-display-name-goes-out-as-encoded-words
  (let* ((from (format nil "~A <ann@example.com>" (make-string 300 :initial-element #\A)))
         (wire (print-message (make-message :from from :to "b@example.com" :body "x"))))
    (ok (every (lambda (l) (<= (length (string-right-trim '(#\Return) l)) 78))
               (uiop:split-string (%header-block wire) :separator '(#\Newline)))
        "every header line within 78")
    (let ((p (parse-message wire)))
      (ok (equal from (message-from p)))
      (ok (equal "ann@example.com" (envelope-sender p))))))

(deftest a-long-body-line-goes-quoted-printable
  (let* ((body (format nil "~A~C~Cend" (make-string 2000 :initial-element #\a) #\Return #\Newline))
         (wire (print-message (make-message :from "a@example.com" :to "b@example.com" :body body))))
    (ok (equal "quoted-printable" (%header wire "Content-Transfer-Encoding")))
    (ok (every (lambda (l) (<= (length l) 998)) (uiop:split-string wire :separator '(#\Newline)))
        "no line longer than SMTP carries")
    (ok (equal body (message-body (parse-message wire))) "the body reads back"))
  (let ((wire (print-message (make-message :from "a@example.com" :to "b@example.com" :body "short"))))
    (ok (equal "8bit" (%header wire "Content-Transfer-Encoding")) "short lines stay 8bit")))

(deftest text-is-encoded-in-its-charset
  (let* ((text (format nil "caf~C" (code-char #xE9)))
         (m (make-message :from "a@example.com" :to "b@example.com" :body "plain")))
    (add-alternative m text :subtype "html" :charset "iso-8859-1")
    (add-attachment m text :content-type "text/plain; charset=iso-8859-1" :filename "a.txt")
    (let ((p (parse-message (print-message m))))
      (ok (equal text (message-body p :subtype "html")) "read back in its charset")
      (ok (equalp #(99 97 102 233) (mime-protocol:mime-content (first (message-attachments p))))
          "ISO-8859-1 octets on the wire"))))

(deftest a-string-attachment-of-another-type-keeps-it
  (let ((m (make-message :from "a@example.com" :to "b@example.com" :body "x")))
    (add-attachment m "{\"a\":1}" :content-type "application/json" :filename "a.json")
    (let ((part (first (message-attachments (parse-message (print-message m))))))
      (ok (equal "application/json"
                 (mime-protocol:media-type-essentials (mime-protocol:mime-content-type part))))
      (ok (equal "{\"a\":1}" (encoding-protocol:decode (mime-protocol:mime-content part)))))))

(deftest smtp-wire-headers-are-ascii-and-read-back
  (let ((subject (format nil "hi ~C~C" (code-char #x41F) (code-char #x2603))))
    (multiple-value-bind (port thread box) (%start-smtp-sink)
      (backend-send (mail-backend-smtp:make-smtp-backend :host "127.0.0.1" :port port)
                    (make-message :from (format nil "Zo~C <zoe@example.com>" (code-char #xEB))
                                  :to "b@example.com" :subject subject :body "x"))
      (bt2:join-thread thread)
      (let* ((wire (car box))
             (head (subseq wire 0 (search #(13 10 13 10) wire))))
        (ok (every (lambda (b) (< b 128)) head) "no 8-bit octet in the header block")
        (ok (equal subject (message-subject (parse-message wire))))))))

(deftest live-non-ascii-send
  ;; The plain Mailpit of live-display-name-send. Its API then shows what
  ;; arrived: curl 127.0.0.1:<http port>/api/v1/messages.
  (let ((port (parse-integer (%env "MAIL_SMTP_TEST_PLAIN_PORT" "11025"))))
    (if (not (handler-case (progn (usocket:socket-close (usocket:socket-connect "127.0.0.1" port
                                                                                :timeout 2))
                             t)
               (error () nil)))
        (skip (format nil "no SMTP server on 127.0.0.1:~D (MAIL_SMTP_TEST_PLAIN_PORT)" port))
        (let* ((m (make-message :from (format nil "Zo~C ~Cnicode <zoe@example.com>"
                                              (code-char #xEB) (code-char #xDC))
                                :to "\"Doe, John\" <john@example.com>, Bob <bob@example.com>"
                                :subject (format nil "Caf~C ~C ~C~C" (code-char #xE9) (code-char #x2615)
                                                 (code-char #x41F) (code-char #x2603))
                                :body (format nil "Caf~C ~C" (code-char #xE9) (code-char #x2615))))
               (err (progn
                      (add-alternative m (format nil "<p>Caf~C ~C</p>" (code-char #xE9) (code-char #x2615)))
                      (handler-case
                          (progn (backend-send (mail-backend-smtp:make-smtp-backend :host "127.0.0.1"
                                                                                    :port port)
                                               m)
                            nil)
                        (error (e) (princ-to-string e))))))
          (ok (null err) (format nil "sent (~@[~A~])" err))))))

;;; --- SMTP parameters and refusals ----------------------------------------

(deftest eight-bit-data-says-body-8bitmime-when-offered
  (let ((data (format nil "Subject: x~C~C~C~Ccaf~C" #\Return #\Newline #\Return #\Newline (code-char #xE9))))
    (multiple-value-bind (result out)
        (%scripted '("220 hi" "250-mx" "250 8BITMIME" "250 ok" "250 ok" "354 go" "250 queued" "221 bye")
                   :from "a@example.com" :recipients '("b@example.com") :data data)
      (ok (eq t result))
      (ok (search (%lines "MAIL FROM:<a@example.com> BODY=8BITMIME") out) out)
      (ok (search (format nil "caf~C" (code-char #xE9)) out) "the octet as it is"))
    (multiple-value-bind (result out) (%scripted +plain-replies+ :from "a@example.com"
                                                                 :recipients '("b@example.com") :data "x")
      (ok (eq t result))
      (ok (search (%lines "MAIL FROM:<a@example.com>") out) "7bit data, no parameter"))))

(deftest non-ascii-envelope-needs-smtputf8
  (let ((from (format nil "zo~C@example.com" (code-char #xEB))))
    (multiple-value-bind (result out)
        (%scripted +plain-replies+ :from from :recipients '("b@example.com") :data "x")
      (ok (typep result 'mail-send-error) "no SMTPUTF8 offered: refused")
      (ng (search "MAIL" out)))
    (multiple-value-bind (result out)
        (%scripted '("220 hi" "250-mx" "250 SMTPUTF8" "250 ok" "250 ok" "354 go" "250 queued" "221 bye")
                   :from from :recipients '("b@example.com") :data "x")
      (ok (eq t result))
      (ok (search (%utf-8-octet-string (format nil "MAIL FROM:<~A> SMTPUTF8" from)) out)
          "UTF-8 octets with SMTPUTF8"))))

(deftest auth-plain-refuses-nul-in-credentials
  (multiple-value-bind (result out)
      (%scripted '("220 hi" "250-mx" "250 AUTH PLAIN")
                 :from "a@example.com" :recipients '("b@example.com") :data "x"
                 :username (format nil "alice~Cadmin" (code-char 0)) :password "p")
    (ok (typep result 'mail-auth-error))
    (ng (search "AUTH" out))))

(deftest data-must-be-octets
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from "a@example.com" :recipients '("b@example.com")
                                 :data (format nil "snow ~C" (code-char #x2603)))
    (ok (typep result 'mail-send-error) (format nil "~A" result))
    (ng (search "MAIL" out)))
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from "a@example.com" :recipients '("b@example.com")
                                 :data (coerce #(104 105 233) '(vector (unsigned-byte 8))))
    (ok (eq t result))
    (ok (search (format nil "hi~C" (code-char 233)) out) "octets go as they are")))

(deftest message-id-domain-falls-back-to-localhost
  (ok (search "@example.com>" (message-id (make-message :from "Ann <ann@example.com>"))))
  (ok (search "@localhost>"
              (message-id (make-message :from (format nil "Ann <ann@ex~Cmple.com>" (code-char #xE4))))))
  (ok (search "@localhost>" (message-id (make-message :from "not an address"))))
  (ok (search "@[192.0.2.1]>" (message-id (make-message :from "a@[192.0.2.1]")))))
