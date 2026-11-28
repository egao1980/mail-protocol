(in-package #:mail-protocol)

(defun address-list (value)
  "VALUE, an RFC 5322 address-list string or a list of them, as a list of
address strings. The list is cut at the commas between addresses, not at
one inside a quoted display name (\"Doe, John\" <john@ex.com>), a comment,
angle brackets or a group. Signals MAIL-ADDRESS-ERROR when VALUE holds CR,
LF or NUL."
  (cond
   ((null value) nil)
   ((stringp value) (%split-address-list (%refuse-line-breaks value)))
   ((and (consp value) (every #'stringp value))
    (loop for v in value
          append (%split-address-list (%refuse-line-breaks v))))
   (t (error 'mail-parse-error
             :message (format nil "not an address list: ~S" value)))))

(defun %join-addresses (addrs)
  "ADDRS as one header field value, each in its wire form (%WIRE-ADDRESS)."
  (format nil "~{~A~^, ~}" (mapcar #'%wire-address addrs)))

(defclass message ()
  ((entity :initarg :entity :accessor message-entity)
   (bcc :initarg :bcc :accessor message-bcc :initform nil)))

(defun message-p (object)
  (typep object 'message))

(defun %wire-subject (subject)
  "SUBJECT (NIL is \"\") for the Subject field: non-ASCII words as RFC 2047
encoded-words, as Python's EmailMessage sends it. CR, LF or NUL signal
MIME-PROTOCOL:MIME-HEADER-ERROR (restart USE-VALUE): a line break would
add header fields (Python: ValueError)."
  (mime-protocol:encode-header-text (or subject "") :name "Subject"))

(defun %overlong-line-p (octets)
  "Does OCTETS hold a line longer than the 998 octets SMTP carries (RFC 5321
4.5.3.1.6, RFC 5322 2.1.1), or a NUL, which 8bit text may not (RFC 2045 2.8)?"
  (let ((run 0))
    (loop for b across octets
          do (cond
              ((or (= b 10) (= b 13)) (setf run 0))
              ((zerop b) (return t))
              ((> (incf run) 998) (return t))))))

(defun %charset-encoding (charset &optional (errorp t))
  "The encoding-protocol name of MIME CHARSET (\"iso-8859-1\", \"Shift_JIS\",
\"windows-31j\", \"iso-2022-jp\"): ENCODING-PROTOCOL:FIND-ENCODING maps
IANA names and Python's aliases, loads an on-demand charset (big5,
iso-2022-jp, euc-kr, cp949, gb18030) the first time, and never interns an
unknown name. When it names no charset (an unknown name, a charset whose
system does not load, or a transfer encoding such as base64), signal
MAIL-PARSE-ERROR, or return NIL when ERRORP is NIL."
  (let ((system nil))
    (flet ((unknown (&optional reason)
             (if errorp
                 (error 'mail-parse-error
                        :message (format nil "unknown charset ~S~@[: ~A~]~@[ (the ASDF system ~A defines it)~]"
                                         charset reason system))
                 (return-from %charset-encoding nil))))
      (let ((encoding (handler-case (multiple-value-bind (encoding unloaded)
                                        (encoding-protocol:find-encoding charset)
                                      (setf system unloaded)
                                      encoding)
                        (encoding-protocol:encoding-unknown-encoding (e)
                          (setf system (encoding-protocol:encoding-unknown-encoding-system e))
                          (unknown (encoding-protocol:encoding-error-message e))))))
        (unless encoding
          (unknown))
        (when (member encoding '(:base64 :base64url :base32 :base32hex :base16
                                 :quoted-printable :rle))
          (unknown "a transfer encoding"))
        ;; Decoding nothing asks encoding-protocol whether it can decode
        ;; this one (a charset whose system is not loaded cannot).
        (handler-case
            (encoding-protocol:decode (make-array 0 :element-type '(unsigned-byte 8))
                                      :encoding encoding)
          (encoding-protocol:encoding-unknown-encoding (e)
            (unknown (encoding-protocol:encoding-error-message e))))
        encoding))))

(defun %check-token (value what)
  "VALUE, unless it is not an RFC 2045 token: then signal MAIL-PARSE-ERROR
naming WHAT. A ; or = in a subtype or disposition would add parameters to
its field (\"attachment; filename=x.exe\")."
  (unless (and (stringp value)
               (plusp (length value))
               (every (lambda (c)
                        (and (< 32 (char-code c) 127)
                             (not (find c "()<>@,;:\\\"/[]?="))))
                      value))
    (error 'mail-parse-error
           :message (format nil "~A ~S is not an RFC 2045 token" what value)))
  value)

(defun %mail-text-entity (text &rest keys &key (subtype "plain") disposition-type
                               &allow-other-keys)
  "MIME-PROTOCOL:MAKE-TEXT-ENTITY for mail: TEXT in its charset (an unknown
one signals MIME-PROTOCOL:MIME-ENCODE-ERROR). SUBTYPE and DISPOSITION-TYPE
must be tokens (%CHECK-TOKEN). The part is 8bit, or quoted-printable when
the text would not survive 8bit SMTP (a line over 998 octets, a NUL), as
Python's EmailMessage switches to quoted-printable for long lines; a relay
would otherwise break the line."
  (%check-token subtype "subtype")
  (when disposition-type
    (%check-token disposition-type "disposition"))
  (let ((entity (apply #'mime-protocol:make-text-entity text keys)))
    (when (%overlong-line-p (mime-protocol:mime-content entity))
      (setf (mime-protocol:mime-transfer-encoding entity) :quoted-printable))
    entity))

(defun message-from (msg)
  "The From field, encoded-words in display names decoded
(\"Zoë <zoe@ex.com>\"), or NIL."
  (let ((raw (mime-protocol:header-value (message-entity msg) "from")))
    (and raw
         (if (search "=?" raw)
             (format nil "~{~A~^, ~}" (mapcar #'%display-address (address-list raw)))
             raw))))

(defun (setf message-from) (value msg)
  (mime-protocol:set-header (message-entity msg) "from" (%join-addresses (address-list value)))
  value)

(defun message-to (msg)
  "The To addresses, encoded-words in display names decoded."
  (mapcar #'%display-address
          (address-list (mime-protocol:header-value (message-entity msg) "to"))))

(defun (setf message-to) (value msg)
  (let ((addrs (address-list value)))
    (mime-protocol:set-header (message-entity msg) "to" (%join-addresses addrs))
    addrs))

(defun message-cc (msg)
  "The Cc addresses, encoded-words in display names decoded."
  (let ((raw (mime-protocol:header-value (message-entity msg) "cc")))
    (when (and raw (plusp (length raw)))
      (mapcar #'%display-address (address-list raw)))))

(defun (setf message-cc) (value msg)
  (let ((addrs (address-list value)))
    (if addrs
        (mime-protocol:set-header (message-entity msg) "cc" (%join-addresses addrs))
        (setf (mime-protocol:mime-headers (message-entity msg))
              (remove "cc" (mime-protocol:mime-headers (message-entity msg))
                      :key #'car :test #'string-equal)))
    addrs))

(defun message-subject (msg)
  "The Subject, RFC 2047 encoded-words decoded, or \"\"."
  (mime-protocol:decode-header-text
   (or (mime-protocol:header-value (message-entity msg) "subject") "")))

(defun (setf message-subject) (value msg)
  (mime-protocol:set-header (message-entity msg) "subject" (%wire-subject value))
  value)

(defun %entity-text (entity)
  "ENTITY's content as text, decoded in its Content-Type charset (UTF-8 when
it names none encoding-protocol knows)."
  (let ((content (mime-protocol:mime-content entity))
        (type (mime-protocol:mime-content-type entity)))
    (cond
     ((stringp content) content)
     ((vectorp content)
      (encoding-protocol:decode
       content
       :encoding (or (let ((charset (and type (mime-protocol:media-type-charset type))))
                       (and charset (%charset-encoding charset nil)))
                     :utf-8)))
     (t ""))))

(defun %media-type-p (entity type &optional subtype)
  (let ((mt (mime-protocol:mime-content-type entity)))
    (and mt
         (string-equal type (mime-protocol:media-type-type mt))
         (or (null subtype)
             (string-equal subtype (mime-protocol:media-type-subtype mt))))))

(defun %attachment-p (entity)
  (let ((cd (mime-protocol:mime-content-disposition entity)))
    (and cd (string-equal "attachment" (mime-protocol:content-disposition-type cd)))))

(defun %walk-entities (entity fn)
  "Call FN on ENTITY and every part below it, depth first."
  (funcall fn entity)
  (dolist (part (mime-protocol:mime-parts entity))
    (%walk-entities part fn)))

(defun message-body (msg &key (subtype "plain"))
  "The body text. A single-part message: its content, whatever its
subtype. A multipart one: the first text/SUBTYPE part that is not an
attachment (Python EmailMessage.get_body), else \"\"."
  (let ((entity (message-entity msg)))
    (if (not (mime-protocol:multipart-p entity))
        (%entity-text entity)
        (block found
          (%walk-entities entity
                          (lambda (e)
                            (when (and (%media-type-p e "text" subtype)
                                       (not (%attachment-p e)))
                              (return-from found (%entity-text e)))))
          ""))))

(defun message-attachments (msg)
  "The attachment entities of MSG (Content-Disposition: attachment), in order."
  (let ((found '()))
    (%walk-entities (message-entity msg)
                    (lambda (e) (when (%attachment-p e) (push e found))))
    (nreverse found)))

(defun message-date (msg)
  "The Date header text, or NIL."
  (mime-protocol:header-value (message-entity msg) "date"))

(defun message-id (msg)
  "The Message-ID header text (\"<...@...>\"), or NIL."
  (mime-protocol:header-value (message-entity msg) "message-id"))

(defun format-message-date (&optional (instant (datetime-protocol:now)))
  "INSTANT (default: now) as an RFC 5322 date-time in UTC,
\"Mon, 23 Nov 2026 21:04:05 +0000\" (Python email.utils.formatdate)."
  ;; An RFC 7231 IMF-fixdate is this date-time with the obsolete zone "GMT"
  ;; (RFC 5322 4.3), which must not be generated; UTC is "+0000".
  (let ((imf (datetime-protocol:print-http-date instant)))
    (concatenate 'string (subseq imf 0 (- (length imf) 3)) "+0000")))

(defvar *message-id-nonce* nil
  "64 random bits drawn from the OS on first use in each process; they keep
two processes (or two runs of one saved image) apart. A saved image starts
with NIL again, and a forked child draws its own (see *MESSAGE-ID-PID*).")

(defvar *message-id-pid* nil
  "The process id *MESSAGE-ID-NONCE* was drawn in. A process forked after
its first Message-ID (a pre-fork server, a job runner that forks per task)
inherits the nonce and the counter; it sees another pid and draws its own.")

(defun %current-pid ()
  "This process's id, or NIL where the implementation cannot tell."
  #+sbcl
  (let ((getpid (find-symbol "UNIX-GETPID" "SB-UNIX")))
    (and getpid (fboundp getpid) (funcall getpid)))
  #-sbcl nil)

(defvar *message-id-counter* 0
  "IDs made so far in this process; each Message-ID takes the next value.")

(defvar *message-id-lock* (bt2:make-lock :name "mail-protocol message-id")
  "Guards *MESSAGE-ID-COUNTER* and the first draw of *MESSAGE-ID-NONCE*:
senders on several threads must never get the same pair.")

(defun %next-message-id-parts ()
  "→ (values sequence pid nonce), a triple no other call returns. On first
use, after an image restore, and in a forked child (the pid changed) the
nonce is drawn again and the sequence starts over."
  (bt2:with-lock-held (*message-id-lock*)
    (let ((pid (%current-pid)))
      (unless (and *message-id-nonce* (eql pid *message-id-pid*))
        (setf *message-id-nonce* (random (expt 2 64) (make-random-state t))
              *message-id-pid* pid
              *message-id-counter* 0))
      (values (incf *message-id-counter*) pid *message-id-nonce*))))

(uiop:register-image-restore-hook
 (lambda () (setf *message-id-nonce* nil))
 nil)

(defun %address-domain (address)
  "The domain of the first addr-spec in ADDRESS (\"Alice <a@ex.com>\" →
\"ex.com\"), when it can stand in a msg-id (RFC 5322 3.6.4: ASCII dot-atom
text or a domain literal); else NIL."
  (when address
    (let* ((first (first (address-list address)))
           (spec (and first
                      (handler-case (car (first (%address-mailboxes first)))
                        (mail-address-error () nil))))
           (at (and spec (position #\@ spec :from-end t)))
           (domain (and at (subseq spec (1+ at)))))
      (and domain
           (plusp (length domain))
           (every (lambda (c)
                    (and (< 32 (char-code c) 127)
                         (not (find c "()<>:;@\\,\""))))
                  domain)
           domain))))

(defun make-message-id (&key domain)
  "A new Message-ID \"<time.sequence.pid.nonce@DOMAIN>\", in base 36, like
Python's email.utils.make_msgid (time, pid and a random number). SEQUENCE
counts the IDs of this process under a lock, so no two calls, on any
thread, return the same ID; PID and NONCE (64 random bits, drawn again in a
forked child or a restored image) keep processes apart (RFC 5322 3.6.4:
globally unique). PID is left out where the implementation cannot tell it.
DOMAIN defaults to \"localhost\"; MAKE-MESSAGE passes the From address's
domain."
  (multiple-value-bind (sequence pid nonce) (%next-message-id-parts)
    (format nil "<~(~36R.~36R.~@[~36R.~]~36R~)@~A>"
            (get-universal-time) sequence pid nonce (or domain "localhost"))))

(defun make-message (&key from to cc bcc subject (body "") (subtype "plain")
                          (date t) (message-id t))
  "A new message. DATE is T (now), an RFC 5322 date string, a
datetime-protocol INSTANT, or NIL (no Date header). MESSAGE-ID is T (a new
one from MAKE-MESSAGE-ID with the From domain), a string, or NIL.

FROM, TO, CC and BCC are address lists (see ADDRESS-LIST); a non-ASCII
display name and a non-ASCII SUBJECT go out as RFC 2047 encoded-words.
CR, LF or NUL in an address signals MAIL-ADDRESS-ERROR, and in the
subject or another header MIME-PROTOCOL:MIME-HEADER-ERROR: a line break
would add header fields or SMTP commands. The body is 8bit text, or
quoted-printable when a line is longer than SMTP carries."
  (unless from
    (error 'mail-parse-error :message "make-message requires :from"))
  (let* ((from-list (address-list from))
         (to-list (address-list to))
         (cc-list (address-list cc))
         (bcc-list (address-list bcc))
         (entity (%mail-text-entity body :subtype subtype)))
    (mime-protocol:set-header entity "from" (%join-addresses from-list))
    (when to-list
      (mime-protocol:set-header entity "to" (%join-addresses to-list)))
    (when cc-list
      (mime-protocol:set-header entity "cc" (%join-addresses cc-list)))
    (mime-protocol:set-header entity "subject" (%wire-subject subject))
    (when date
      (mime-protocol:set-header entity "date"
                                (etypecase date
                                  ((eql t) (format-message-date))
                                  (string date)
                                  (datetime-protocol:instant (format-message-date date)))))
    (when message-id
      (mime-protocol:set-header entity "message-id"
                                (etypecase message-id
                                  ((eql t) (make-message-id :domain (%address-domain from)))
                                  (string message-id))))
    (mime-protocol:set-header entity "mime-version" "1.0")
    (make-instance 'message :entity entity :bcc bcc-list)))

(defun %content-header-p (name)
  (eql 0 (search "content-" name)))

(defun %multipart-of-p (entity subtype)
  (and (mime-protocol:multipart-p entity)
       (string-equal subtype (mime-protocol:media-type-subtype
                              (mime-protocol:mime-content-type entity)))))

(defun %wrap-in-multipart (msg subtype)
  "Make MSG's entity the first part of a new multipart/SUBTYPE. The message
headers (From, Subject, Date, ...) move to the new entity with
MIME-PROTOCOL:COPY-HEADERS, so a parsed field that was not UTF-8 keeps its
octets; the old one keeps its Content-* headers. → the new multipart."
  (let* ((old (message-entity msg))
         (new (mime-protocol:make-multipart-entity (list old) :subtype subtype)))
    (mime-protocol:copy-headers old new
                                :test (complement #'%content-header-p)
                                :remove t)
    (setf (message-entity msg) new)))

(defun %append-part (multipart part)
  (setf (mime-protocol:mime-parts multipart)
        (append (mime-protocol:mime-parts multipart) (list part)))
  multipart)

(defun %as-alternative (body part)
  "BODY (a part) with PART as another alternative: appended to a
multipart/alternative, else both in a new one."
  (if (%multipart-of-p body "alternative")
      (%append-part body part)
      (mime-protocol:make-multipart-entity (list body part) :subtype "alternative")))

(defun add-alternative (msg content &key (subtype "html") (charset "utf-8"))
  "Add CONTENT, a string, as another rendering of the body (HTML next to
the plain text by default; Python EmailMessage.add_alternative). The body
becomes multipart/alternative, inside the multipart/mixed when there are
attachments. CONTENT is encoded in CHARSET. → MSG."
  (check-type content string)
  (let ((part (%mail-text-entity content :subtype subtype :charset charset))
        (top (message-entity msg)))
    (cond
     ((%multipart-of-p top "mixed")
      (let ((parts (mime-protocol:mime-parts top)))
        (setf (first parts) (%as-alternative (first parts) part))))
     ((%multipart-of-p top "alternative")
      (%append-part top part))
     (t
      (%append-part (%wrap-in-multipart msg "alternative") part)))
    msg))

(defun add-attachment (msg data &key content-type filename (disposition "attachment"))
  "Attach DATA, octets or a string (a text attachment) (Python
EmailMessage.add_attachment). CONTENT-TYPE defaults from FILENAME's
extension, else application/octet-stream (text/plain for a string).
A string is encoded in the Content-Type's charset (default UTF-8); a string
of a type other than text/* is sent as its UTF-8 octets, keeping the type.
DISPOSITION must be a token (\"attachment\", \"inline\"); NIL is \"attachment\",
and one with \";\" or \"=\" signals MAIL-PARSE-ERROR rather than add
parameters. A long non-ASCII FILENAME goes out as RFC 2231 continuations
(mime-protocol). The message becomes multipart/mixed: the body first, then
attachments in the order added; binary parts are base64. → MSG."
  (check-type data (or string (vector (unsigned-byte 8))))
  (setf disposition (%check-token (or disposition "attachment") "disposition"))
  (let* ((ct (or content-type
                 (and filename (mime-protocol:lookup-mime filename nil))
                 (if (stringp data) "text/plain" "application/octet-stream")))
         (mt (mime-protocol:parse-media-type ct))
         (text-p (and (stringp data) (string= "text" (mime-protocol:media-type-type mt))))
         (part (if text-p
                   (%mail-text-entity
                    data :subtype (mime-protocol:media-type-subtype mt)
                    :charset (or (mime-protocol:media-type-charset mt) "utf-8")
                    :disposition-type disposition :filename filename)
                   (mime-protocol:make-binary-entity
                    (if (stringp data) (encoding-protocol:encode data :encoding :utf-8) data)
                    :type (mime-protocol:media-type-type mt)
                    :subtype (mime-protocol:media-type-subtype mt)
                    :disposition-type disposition :filename filename)))
         (top (message-entity msg)))
    (%append-part (if (%multipart-of-p top "mixed")
                      top
                      (%wrap-in-multipart msg "mixed"))
                  part)
    msg))

(defun parse-message (source)
  (handler-case
      (let* ((entity (mime-protocol:parse-mime source))
             (bcc (mime-protocol:header-value entity "bcc")))
        (when bcc
          (setf (mime-protocol:mime-headers entity)
                (remove "bcc" (mime-protocol:mime-headers entity)
                        :key #'car :test #'string-equal)))
        (make-instance 'message
                       :entity entity
                       :bcc (when (and bcc (plusp (length bcc)))
                              (address-list bcc))))
    (mime-protocol:mime-error (e)
      (error 'mail-parse-error :message (princ-to-string e)))))

(defun print-message (msg &key stream)
  "Serialize MSG (Bcc omitted — envelope only). The text is octets, one
character per octet (MIME-PROTOCOL:PRINT-MIME): write it to the wire
through ISO-8859-1. Header lines are folded at 78 (RFC 5322 2.1.1)."
  (unless (message-p msg)
    (error 'mail-parse-error :message "print-message expects a message"))
  (mime-protocol:print-mime (message-entity msg) :stream stream :max-line-length 78))

(defun %addr-specs (addresses &key recipients)
  "The addr-specs of ADDRESSES (address strings), groups expanded. With
RECIPIENTS, the null path (<>) signals MAIL-ADDRESS-ERROR: RFC 5321 4.5.5
names it for MAIL FROM only, and RCPT TO:<> is no mailbox."
  (loop for address in addresses
        append (let ((specs (mapcar #'car (address-mailboxes address))))
                 (when (and recipients (member "" specs :test #'string=))
                   (%address-fail address "the null path <> is not a recipient"))
                 specs)))

(defun %mailbox-key (spec)
  "SPEC with its domain in lowercase: the local part is case-sensitive, the
domain is not (RFC 5321 2.4)."
  (let ((at (position #\@ spec :from-end t)))
    (if at
        (concatenate 'string (subseq spec 0 at) (string-downcase (subseq spec at)))
        spec)))

(defun envelope-recipients (msg)
  "The SMTP envelope recipients of MSG: the addr-spec of every To, Cc and
Bcc mailbox, group members included, each once, as smtplib's send_message
takes them (\"Bob <bob@ex.com>\" → \"bob@ex.com\"). Two addresses are the
same mailbox when they differ only in the case of the domain: the local part
is case-sensitive (RFC 5321 2.4). Signals MAIL-ADDRESS-ERROR for an address
that does not parse, or for the null path <>."
  (let ((entity (message-entity msg)))
    (delete-duplicates
     (%addr-specs (append (address-list (mime-protocol:header-value entity "to"))
                          (address-list (mime-protocol:header-value entity "cc"))
                          (message-bcc msg))
                  :recipients t)
     :test #'string= :key #'%mailbox-key :from-end t)))

(defun envelope-sender (msg)
  "The SMTP envelope sender of MSG (MAIL FROM): the addr-spec of the Sender
field, else of the first From mailbox, as smtplib's send_message. \"\" (the
null reverse-path, MAIL FROM:<>, RFC 5321 4.5.5) when that field is empty
or its mailbox is <> (\"Mailer Daemon <>\", \"<> (bounce)\"). NIL when there is
neither field. Signals MAIL-ADDRESS-ERROR for an address that does not
parse."
  (let* ((entity (message-entity msg))
         (field (or (mime-protocol:header-value entity "sender")
                    (mime-protocol:header-value entity "from"))))
    (cond
      ((null field) nil)
      ((member (string-trim '(#\Space #\Tab) field) '("" "<>") :test #'string=) "")
      (t (first (%addr-specs (address-list field)))))))
