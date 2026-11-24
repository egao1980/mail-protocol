(in-package #:mail-protocol)

(defun %split-commas (s)
  (loop for start = 0 then (1+ pos)
        for pos = (position #\, s :start start)
        for tok = (string-trim '(#\Space #\Tab) (subseq s start (or pos (length s))))
        unless (zerop (length tok))
          collect tok
        while pos))

(defun address-list (value)
  "Normalize VALUE to a list of address strings."
  (cond
    ((null value) nil)
    ((stringp value) (%split-commas value))
    ((and (consp value) (every #'stringp value)) (copy-list value))
    (t (error 'mail-parse-error
              :message (format nil "not an address list: ~S" value)))))

(defun %join-addresses (addrs)
  (format nil "~{~A~^, ~}" addrs))

(defclass message ()
  ((entity :initarg :entity :accessor message-entity)
   (bcc :initarg :bcc :accessor message-bcc :initform nil)))

(defun message-p (object)
  (typep object 'message))

(defun message-from (msg)
  (mime-protocol:header-value (message-entity msg) "from"))

(defun (setf message-from) (value msg)
  (mime-protocol:set-header (message-entity msg) "from" value)
  value)

(defun message-to (msg)
  (address-list (mime-protocol:header-value (message-entity msg) "to")))

(defun (setf message-to) (value msg)
  (let ((addrs (address-list value)))
    (mime-protocol:set-header (message-entity msg) "to" (%join-addresses addrs))
    addrs))

(defun message-cc (msg)
  (let ((raw (mime-protocol:header-value (message-entity msg) "cc")))
    (when (and raw (plusp (length raw)))
      (address-list raw))))

(defun (setf message-cc) (value msg)
  (let ((addrs (address-list value)))
    (if addrs
        (mime-protocol:set-header (message-entity msg) "cc" (%join-addresses addrs))
        (setf (mime-protocol:mime-headers (message-entity msg))
              (remove "cc" (mime-protocol:mime-headers (message-entity msg))
                      :key #'car :test #'string-equal)))
    addrs))

(defun message-subject (msg)
  (or (mime-protocol:header-value (message-entity msg) "subject") ""))

(defun (setf message-subject) (value msg)
  (mime-protocol:set-header (message-entity msg) "subject" (or value ""))
  value)

(defun %entity-text (entity)
  (let ((content (mime-protocol:mime-content entity)))
    (cond
     ((stringp content) content)
     ((vectorp content) (encoding-protocol:decode content))
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
  "The domain of the addr-spec in ADDRESS (\"Alice <a@ex.com>\" → \"ex.com\"), or NIL."
  (when address
    (let* ((lt (position #\< address :from-end t))
           (spec (if lt
                     (subseq address (1+ lt) (or (position #\> address :start lt)
                                                 (length address)))
                     address))
           (at (position #\@ spec :from-end t))
           (domain (and at (string-trim '(#\Space #\Tab #\>) (subseq spec (1+ at))))))
      (and domain (plusp (length domain)) domain))))

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
one from MAKE-MESSAGE-ID with the From domain), a string, or NIL."
  (unless from
    (error 'mail-parse-error :message "make-message requires :from"))
  (let* ((to-list (address-list to))
         (cc-list (address-list cc))
         (bcc-list (address-list bcc))
         (entity (mime-protocol:make-text-entity body :subtype subtype)))
    (mime-protocol:set-header entity "from" from)
    (when to-list
      (mime-protocol:set-header entity "to" (%join-addresses to-list)))
    (when cc-list
      (mime-protocol:set-header entity "cc" (%join-addresses cc-list)))
    (mime-protocol:set-header entity "subject" (or subject ""))
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
headers (From, Subject, Date, ...) move to the new entity; the old one
keeps its Content-* headers. → the new multipart."
  (let* ((old (message-entity msg))
         (headers (mime-protocol:mime-headers old))
         (new (mime-protocol:make-multipart-entity (list old) :subtype subtype)))
    (setf (mime-protocol:mime-headers new)
          (remove-if #'%content-header-p headers :key #'car)
          (mime-protocol:mime-headers old)
          (remove-if-not #'%content-header-p headers :key #'car)
          (message-entity msg) new)))

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
attachments. → MSG."
  (check-type content string)
  (let ((part (mime-protocol:make-text-entity content :subtype subtype :charset charset))
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
The message becomes multipart/mixed: the body first, then attachments in
the order added; binary parts are base64. → MSG."
  (check-type data (or string (vector (unsigned-byte 8))))
  (let* ((ct (or content-type
                 (and filename (mime-protocol:lookup-mime filename nil))
                 (if (stringp data) "text/plain" "application/octet-stream")))
         (mt (mime-protocol:parse-media-type ct))
         (part (if (stringp data)
                   (mime-protocol:make-text-entity
                    data :subtype (mime-protocol:media-type-subtype mt)
                    :charset (or (mime-protocol:media-type-charset mt) "utf-8")
                    :disposition-type disposition :filename filename)
                   (mime-protocol:make-binary-entity
                    data :type (mime-protocol:media-type-type mt)
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
  "Serialize MSG (Bcc omitted — envelope only)."
  (unless (message-p msg)
    (error 'mail-parse-error :message "print-message expects a message"))
  (mime-protocol:print-mime (message-entity msg) :stream stream))

(defun envelope-recipients (msg)
  (delete-duplicates
   (append (message-to msg)
           (or (message-cc msg) '())
           (or (message-bcc msg) '()))
   :test #'string-equal))
