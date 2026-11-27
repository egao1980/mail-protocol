(in-package #:mail-backend-smtp)

(defun %crlf (out)
  (write-char #\Return out)
  (write-char #\Newline out))

(defun %octet-string (string)
  "STRING as its UTF-8 octets, one character each: what the dialogue writes
to its ISO-8859-1 stream. ASCII comes back as it is."
  (if (every (lambda (c) (< (char-code c) 128)) string)
      string
      (map 'string #'code-char (encoding-protocol:encode string :encoding :utf-8))))

(defun %write-line (out line)
  "Write the SMTP command LINE and CRLF. A command holding CR or LF is
refused with MAIL-SEND-ERROR before anything is written: the server would
take the rest as another command (SMTP command injection, as smtplib's
putcmd refuses it). Non-ASCII goes out as UTF-8 (SMTPUTF8)."
  (let ((bad (position-if (lambda (c) (member c '(#\Return #\Newline))) line)))
    (when bad
      (error 'mail-send-error
             :message (format nil "the SMTP command ~S holds ~:[LF~;CR~] at ~D; the rest would be sent as another command"
                              line (char= #\Return (char line bad)) bad))))
  (write-string (%octet-string line) out)
  (%crlf out)
  (finish-output out))

(defvar *await-reply* nil
  "NIL, or a function of the input stream, called before each SMTP reply is
read: true when the reply can be read, NIL when none came within the read
timeout. SMTP-DIALOGUE binds it from its :AWAIT-REPLY.")

(defun %read-reply (in &optional what)
  "Read one SMTP reply, continuation lines included. When *AWAIT-REPLY* is
a function, it is called with IN first; NIL from it (no reply within the
read timeout) is MAIL-SEND-ERROR naming WHAT.
→ (values code text lines): TEXT is the last line's text, LINES every line's."
  (when (and *await-reply* (not (funcall *await-reply* in)))
    (error 'mail-send-error
           :message (format nil "no ~@[~A ~]reply within the read timeout" what)))
  (let ((lines '())
        (code nil))
    (loop
      (let ((line (read-line in nil nil)))
        (unless line
          (error 'mail-send-error :message "SMTP connection closed"))
        (setf line (string-right-trim '(#\Return) line))
        (let ((n (and (>= (length line) 3)
                      (every #'digit-char-p (subseq line 0 3))
                      (parse-integer line :end 3))))
          (unless n
            (error 'mail-send-error :message (format nil "bad SMTP reply: ~S" line)))
          (setf code n)
          (push (if (> (length line) 4) (subseq line 4) "") lines)
          (unless (and (> (length line) 3) (char= (char line 3) #\-))
            (let ((all (reverse lines)))
              (return (values code (first lines) all)))))))))

(defun %expect (in expected &key (condition 'mail-send-error) what)
  "Read a reply; its code must be EXPECTED (an integer or a list of them),
else signal CONDITION with the code and text. → (values code text lines)."
  (multiple-value-bind (code text lines) (%read-reply in what)
    (unless (if (listp expected) (member code expected) (= code expected))
      (error condition
             :code code
             :message (format nil "~@[~A: ~]SMTP wanted ~A got ~A ~A"
                              what expected code text)))
    (values code text lines)))

(defun dot-stuff (body)
  "RFC 5321 DATA payload, as Python's smtplib sends it: every line end, CRLF,
a bare LF or a bare CR (smtplib's _fix_eols), becomes CRLF; a line that
starts with \".\" gets another one; <CRLF>.<CRLF> ends the data. A bare CR
never reaches the wire (RFC 5321 2.3.8), so a body holding <CR>.<CR> cannot
end the message early on a server that takes CR as a line end (SMTP
smuggling)."
  (with-output-to-string (out)
    (let ((len (length body))
          (start 0)
          (i 0))
      (flet ((line (end)
               (when (and (< start end) (char= (char body start) #\.))
                 (write-char #\. out))
               (write-string body out :start start :end end)
               (%crlf out)))
        (loop while (< i len)
              do (case (char body i)
                   (#\Return
                    (line i)
                    (setf i (if (and (< (1+ i) len) (char= (char body (1+ i)) #\Newline))
                                (+ i 2)
                                (1+ i))
                          start i))
                   (#\Newline
                    (line i)
                    (setf i (1+ i)
                          start i))
                   (t (incf i))))
        (when (< start len)
          (line len))))
    (write-char #\. out)
    (%crlf out)))

(defun %ehlo (in out ehlo-host)
  "EHLO; → the extension lines, split into words (\"AUTH\" \"PLAIN\" \"LOGIN\")."
  (%write-line out (format nil "EHLO ~A" ehlo-host))
  (multiple-value-bind (code text lines) (%expect in 250 :what "EHLO")
    (declare (ignore code text))
    ;; The first line is the server's greeting, the rest one extension each.
    (mapcar (lambda (line)
              (uiop:split-string (string-upcase (string-trim " " line))
                                 :separator '(#\Space)))
            (rest lines))))

(defun %extension (extensions keyword)
  "The parameters of extension KEYWORD, or NIL; T when it has none."
  (let ((ext (assoc keyword extensions :test #'string=)))
    (and ext (or (rest ext) t))))

(defun %base64 (string)
  (encoding-protocol:encode (encoding-protocol:encode string) :encoding :base64))

(defun %authenticate (in out extensions username password preference)
  "AUTH with the first mechanism of PREFERENCE the server offers. A
USERNAME or PASSWORD holding NUL is refused with MAIL-AUTH-ERROR before
anything is sent: in AUTH PLAIN, NUL separates the fields (RFC 4616), so
the server would read other credentials."
  (when (or (find (code-char 0) username) (find (code-char 0) (or password "")))
    (error 'mail-auth-error :message "the username or password holds NUL"))
  (let* ((offered (let ((p (%extension extensions "AUTH"))) (and (listp p) p)))
         (mechanism (find-if (lambda (m) (member m offered :test #'string-equal))
                             preference)))
    (unless mechanism
      (error 'mail-auth-error
             :message (format nil "the server offers AUTH ~:[nothing~;~:*~{~A~^ ~}~]; this client speaks ~{~A~^ ~}"
                              offered preference)))
    (cond
     ((string-equal mechanism "PLAIN")
      ;; RFC 4616: authzid NUL authcid NUL passwd.
      (%write-line out (format nil "AUTH PLAIN ~A"
                               (%base64 (format nil "~C~A~C~A" (code-char 0) username
                                                (code-char 0) (or password "")))))
      (%expect in 235 :condition 'mail-auth-error :what "AUTH PLAIN"))
     ((string-equal mechanism "LOGIN")
      (%write-line out "AUTH LOGIN")
      (%expect in 334 :condition 'mail-auth-error :what "AUTH LOGIN")
      (%write-line out (%base64 username))
      (%expect in 334 :condition 'mail-auth-error :what "AUTH LOGIN user")
      (%write-line out (%base64 (or password "")))
      (%expect in 235 :condition 'mail-auth-error :what "AUTH LOGIN password")))
    mechanism))

(defun %data-octets (data)
  "DATA as an octet string, one character per octet: octets, or
PRINT-MESSAGE's string as it is. A string holding a character above U+00FF
is not such a string and signals MAIL-SEND-ERROR rather than being sent
mangled."
  (etypecase data
    ((vector (unsigned-byte 8))
     (map 'string #'code-char data))
    (string
     (let ((wide (position-if (lambda (c) (> (char-code c) 255)) data)))
       (when wide
         (error 'mail-send-error
                :message (format nil "DATA holds U+~4,'0X at ~D; it must be PRINT-MESSAGE's octets, one character each"
                                 (char-code (char data wide)) wide))))
     data)))

(defun smtp-dialogue (in out &key from recipients data (ehlo-host "localhost")
                         username password starttls
                         (auth-mechanisms '("PLAIN" "LOGIN"))
                         await-reply)
  "Drive one SMTP transaction on character streams IN / OUT, one character
per octet (ISO-8859-1): greeting, EHLO, then STARTTLS when STARTTLS is a
function (it is called after the server's 220 and returns the new IN and
OUT, over TLS; EHLO is sent again), then AUTH when USERNAME is given (the
first of AUTH-MECHANISMS the server offers: PLAIN, LOGIN), then MAIL / RCPT
/ DATA / QUIT. A server that does not offer a requested STARTTLS or AUTH
signals MAIL-SEND-ERROR / MAIL-AUTH-ERROR rather than sending in the clear.

AWAIT-REPLY, when given, is called with the input stream before each reply
is read: true when the reply can be read, NIL when none came in time, which
is MAIL-SEND-ERROR naming the reply (no EHLO reply within the read timeout).
The SMTP backend passes one that waits on its socket for :READ-TIMEOUT
seconds.

FROM and each of RECIPIENTS go into MAIL FROM / RCPT TO as their addr-spec
(PARSE-ADDRESS: \"Ann <ann@ex.com>\" → <ann@ex.com>); one that does not
parse, or holds CR or LF, signals MAIL-ADDRESS-ERROR before anything is
sent. FROM \"\", \"<>\" or a mailbox <> (\"Mailer Daemon <>\") is the null
reverse-path, MAIL FROM:<> (RFC 5321 4.5.5: bounces, DSNs, auto-replies);
a recipient <> signals MAIL-ADDRESS-ERROR. DATA is PRINT-MESSAGE's string (one
character per octet) or octets. MAIL FROM gets BODY=8BITMIME when DATA has
8-bit octets and the server offers it, and SMTPUTF8 when an address is not
ASCII (a server without SMTPUTF8 then signals MAIL-SEND-ERROR).

Once the server has taken the message (250 after DATA) the send has
succeeded: QUIT is sent, but a missing or odd reply to it, or a connection
that is already gone, is not an error (smtplib ignores it too), so a caller
never retries a delivered message. → T."
  (unless (and from recipients data)
    (error 'mail-send-error :message "smtp-dialogue needs :from :recipients :data"))
  (let* ((*await-reply* await-reply)
         (from (if (member (string-trim '(#\Space #\Tab) from) '("" "<>") :test #'string=)
                   ""
                   (parse-address from)))
         (recipients (mapcar (lambda (recipient)
                               (let ((spec (parse-address recipient)))
                                 (when (string= "" spec)
                                   (error 'mail-address-error
                                          :address recipient
                                          :message "the null path <> is not a recipient"))
                                 spec))
                             recipients))
         (data (%data-octets data))
         (8bit (find-if (lambda (c) (> (char-code c) 127)) data))
         (utf8 (notevery (lambda (address) (every (lambda (c) (< (char-code c) 128)) address))
                         (cons from recipients))))
    (%expect in 220 :what "greeting")
    (let ((extensions (%ehlo in out ehlo-host)))
      (when starttls
        (unless (%extension extensions "STARTTLS")
          (error 'mail-send-error :message "the server does not offer STARTTLS"))
        (%write-line out "STARTTLS")
        (%expect in 220 :what "STARTTLS")
        (multiple-value-setq (in out) (funcall starttls))
        (setf extensions (%ehlo in out ehlo-host)))
      (when username
        (%authenticate in out extensions username password auth-mechanisms))
      (when (and utf8 (not (%extension extensions "SMTPUTF8")))
        (error 'mail-send-error
               :message "an envelope address is not ASCII and the server does not offer SMTPUTF8"))
      (%write-line out (format nil "MAIL FROM:<~A>~:[~; BODY=8BITMIME~]~:[~; SMTPUTF8~]"
                               from (and 8bit (%extension extensions "8BITMIME")) utf8)))
    (%expect in 250 :what "MAIL FROM")
    (dolist (rcpt recipients)
      (%write-line out (format nil "RCPT TO:<~A>" rcpt))
      (%expect in '(250 251) :what "RCPT TO"))
    (%write-line out "DATA")
    (%expect in 354 :what "DATA")
    (write-string (dot-stuff data) out)
    (finish-output out)
    (%expect in 250 :what "message")
    ;; The message is delivered; the end of the session cannot undo that.
    (handler-case (progn (%write-line out "QUIT")
                    (%expect in 221 :what "QUIT"))
      ((or mail-send-error stream-error cl+ssl::cl+ssl-error) () nil))
    t))
