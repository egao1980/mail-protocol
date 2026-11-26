(in-package #:mail-protocol)

(defclass imap-client ()
  ((host :initarg :host :accessor imap-client-host :initform "localhost")
   (port :initarg :port :accessor imap-client-port :initform 143)
   (stream :initarg :stream :accessor imap-client-stream :initform nil)
   (io-fn :initarg :io-fn :accessor imap-client-io-fn :initform nil)
   (state :initarg :state :accessor imap-client-state :initform :disconnected)
   (selected :initarg :selected :accessor imap-client-selected :initform nil)
   (idle-handler :initarg :idle-handler :accessor imap-client-idle-handler
                 :initform nil)
   (tag-counter :initform 0 :accessor imap-client-tag-counter)
   (greeting :initform nil :accessor imap-client-greeting)
   (capabilities :initform nil :accessor imap-client-capabilities
                 :documentation "The server's last CAPABILITY list (strings),
from a CAPABILITY response or a [CAPABILITY ...] response code.")
   (utf8-accept :initarg :utf8-accept :initform nil :accessor imap-client-utf8-accept
                :documentation "True once the server has ENABLEd UTF8=ACCEPT
(RFC 6855, RFC 9051). Non-ASCII strings and mailbox names then go as UTF-8
quoted strings; without it, mailbox names go in modified UTF-7 and other
non-ASCII strings as literals.")))

(defun imap-client-p (x)
  (typep x 'imap-client))

(defun make-imap-client (&key (host "localhost") (port 143) stream io-fn utf8-accept)
  "An IMAP client over STREAM or IO-FN.
STREAM: a character stream to the server. Literal sizes are counted in UTF-8
octets, unless the stream's external format is Latin-1: then the client
writes UTF-8 octets one per character, as the SMTP backend does.
IO-FN: called with each command segment (a string, no CRLF; NIL for the
greeting) and returns the response lines. A command with a literal comes in
more than one segment: the first ends in {n}, and the next is sent only
after a + continuation line came back."
  (make-instance 'imap-client :host host :port port :stream stream :io-fn io-fn
                              :utf8-accept utf8-accept))

(defun next-imap-tag (client)
  (format nil "A~4,'0d" (incf (imap-client-tag-counter client))))

(defstruct (imap-raw (:constructor imap-raw (text)))
  "Command syntax sent as it is, never quoted: a search key such as
FROM \"a b\" UNSEEN, or FETCH items such as (FLAGS RFC822). It is still
checked: no CR, LF or NUL, and no 8-bit text unless UTF8=ACCEPT is on."
  (text "" :type string))

(defparameter *imap-quoted-max-octets* 1024
  "A string longer than this many octets goes as a literal, not a quoted
string: servers cap the command line length (RFC 7162 §4 suggests 8192
octets for a whole line).")

(defstruct (%imap-literal (:constructor %imap-literal (text)))
  "A string sent as an IMAP literal: {n} CRLF, the server's + continuation,
then the N octets."
  (text "" :type string))

(defun %octet-length (string)
  "STRING's length in UTF-8 octets."
  (if (%ascii-p string)
      (length string)
      (length (encoding-protocol:encode string :encoding :utf-8))))

(defun %check-line-safe (value what)
  "VALUE, unless it holds CR, LF or NUL: then IMAP-ARGUMENT-ERROR. WHAT names
the argument in the message."
  (let ((bad (position-if (lambda (c)
                            (or (char= c #\Return) (char= c #\Newline)
                                (char= c (code-char 0))))
                          value)))
    (when bad
      (error 'imap-argument-error
             :argument value
             :message (format nil "the ~A ~S holds ~A at ~D; the rest would reach the server as another command"
                              what value
                              (case (char value bad)
                                (#\Return "CR")
                                (#\Newline "LF")
                                (t "NUL"))
                              bad)))
    value))

(defun %astring-atom-p (string)
  "True when STRING can go as an atom: every character an ASTRING-CHAR. The
same set as Python imaplib's _non_astring_char: no space, CTL, 8-bit,
( ) { % * \\ or \"."
  (and (plusp (length string))
       (every (lambda (c)
                (and (< 31 (char-code c) 127)
                     (not (find c "(){ %*\\\""))))
              string)))

(defun %imap-quoted (string)
  "STRING as an IMAP quoted string, \\ and \" escaped."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across string
          do (when (or (char= c #\") (char= c #\\))
               (write-char #\\ o))
             (write-char c o))
    (write-char #\" o)))

(defun %imap-string (value utf8)
  "VALUE as an astring piece: an atom, a quoted string, or a %IMAP-LITERAL.
Quoted strings carry 7-bit text, or UTF-8 when UTF8 (UTF8=ACCEPT) is on;
8-bit text without it, or more than *IMAP-QUOTED-MAX-OCTETS* of anything,
goes as a literal (RFC 3501 §4.3)."
  (%check-line-safe value "string")
  (cond
    ((> (%octet-length value) *imap-quoted-max-octets*) (%imap-literal value))
    ((%astring-atom-p value) value)
    ((or utf8 (%ascii-p value)) (%imap-quoted value))
    (t (%imap-literal value))))

(defun %imap-atom (symbol)
  "SYMBOL's name as an atom: a search key (:from → FROM) or a flag (\\Seen)."
  (let* ((name (symbol-name symbol))
         (body (if (and (plusp (length name)) (char= #\\ (char name 0)))
                   (subseq name 1)
                   name)))
    (%check-line-safe name "atom")
    (unless (%astring-atom-p body)
      (error 'imap-argument-error
             :argument symbol
             :message (format nil "~S is not an IMAP atom" symbol)))
    name))

(defun %imap-emit (value utf8 emit)
  "Call EMIT with the wire pieces of argument VALUE: strings, or a
%IMAP-LITERAL. NIL → NIL; a string → atom, quoted or literal; IMAP-RAW → its
text; a non-negative integer → its digits; a symbol → an atom; a list → a
parenthesized list."
  (typecase value
    (null (funcall emit "NIL"))
    (string (funcall emit (%imap-string value utf8)))
    (imap-raw
     (let ((text (%check-line-safe (imap-raw-text value) "command text")))
       (unless (or utf8 (%ascii-p text))
         (error 'imap-argument-error
                :argument text
                :message (format nil "the command text ~S is not 7-bit; pass non-ASCII strings in a list, where they go as literals"
                                 text)))
       (funcall emit text)))
    ((integer 0) (funcall emit (princ-to-string value)))
    (symbol (funcall emit (%imap-atom value)))
    (cons
     (funcall emit "(")
     (loop for (x . more) on value
           do (%imap-emit x utf8 emit)
              (when more (funcall emit " ")))
     (funcall emit ")"))
    (t (error 'imap-argument-error
              :argument value
              :message (format nil "cannot send ~S in an IMAP command" value)))))

(defun %imap-segments (tag verb args utf8)
  "The wire segments of TAG VERB ARGS (no CRLFs). Every segment but the
last ends in {n}; the next one starts with that literal's text and goes
only after the server's + continuation. Every argument is checked before
anything is returned, so a refused command writes nothing."
  (let ((segments '())
        (current (make-string-output-stream))
        (verb (string-upcase (string verb))))
    (unless (%astring-atom-p (%check-line-safe tag "tag"))
      (error 'imap-argument-error :argument tag
                                  :message (format nil "the tag ~S is not an atom" tag)))
    (unless (every #'%astring-atom-p (uiop:split-string (%check-line-safe verb "command")
                                                        :separator " "))
      (error 'imap-argument-error :argument verb
                                  :message (format nil "the command ~S is not atoms" verb)))
    (flet ((emit (piece)
             (etypecase piece
               (string (write-string piece current))
               (%imap-literal
                (let ((text (%imap-literal-text piece)))
                  (format current "{~D}" (%octet-length text))
                  (push (get-output-stream-string current) segments)
                  (write-string text current))))))
      (emit tag)
      (emit " ")
      (emit verb)
      (dolist (arg args)
        (emit " ")
        (%imap-emit arg utf8 #'emit))
      (push (get-output-stream-string current) segments)
      (nreverse segments))))

(defun imap-utf7-encode (name)
  "Mailbox NAME in modified UTF-7 (RFC 3501 §5.1.3): printable ASCII as it
is, & as &-, every other run as & + modified base64 of its UTF-16BE + -."
  (with-output-to-string (o)
    (let ((run '()))
      (flet ((flush ()
               (when run
                 (let ((b64 (encoding-protocol:encode
                             (encoding-protocol:encode (coerce (reverse run) 'string)
                                                       :encoding :utf-16be)
                             :encoding :base64 :pad nil)))
                   (write-char #\& o)
                   (write-string (substitute #\, #\/ b64) o)
                   (write-char #\- o))
                 (setf run '()))))
        (loop for c across name
              do (if (<= #x20 (char-code c) #x7E)
                     (progn (flush)
                       (if (char= c #\&)
                           (write-string "&-" o)
                           (write-char c o)))
                     (push c run)))
        (flush)))))

(defun imap-utf7-decode (wire)
  "The mailbox name that modified UTF-7 WIRE encodes. A malformed name →
IMAP-ERROR."
  (with-output-to-string (o)
    (let ((i 0)
          (n (length wire)))
      (loop while (< i n)
            do (let ((c (char wire i)))
                 (if (char/= c #\&)
                     (progn (write-char c o) (incf i))
                     (let ((end (position #\- wire :start (1+ i))))
                       (unless end
                         (error 'imap-error
                                :message (format nil "modified UTF-7 ~S: & at ~D has no closing -"
                                                 wire i)))
                       (if (= end (1+ i))
                           (write-char #\& o)
                           (write-string
                            (handler-case
                                (encoding-protocol:decode
                                 (encoding-protocol:decode
                                  (substitute #\/ #\, (subseq wire (1+ i) end))
                                  :encoding :base64 :pad nil)
                                 :encoding :utf-16be)
                              (encoding-protocol:encoding-error (e)
                                (error 'imap-error
                                       :message (format nil "modified UTF-7 ~S: ~A" wire e))))
                            o))
                       (setf i (1+ end)))))))))

(defun encode-imap-command (tag verb &rest args)
  "Encode TAG VERB ARGS as one IMAP command (no final CRLF).
A string argument goes as an atom when it can, else quoted, else (8-bit or
long) as a literal; see %IMAP-EMIT for the other argument types. An argument
holding CR, LF or NUL → IMAP-ARGUMENT-ERROR.
→ (values wire segments): WIRE is the whole command, literals in place;
SEGMENTS are the pieces a client sends one by one, each after the server's +
continuation. For a command without a literal, WIRE is its only segment."
  (let ((segments (%imap-segments tag verb args nil)))
    (values (with-output-to-string (o)
              (loop for (segment . more) on segments
                    do (write-string segment o)
                       (when more
                         (write-char #\Return o)
                         (write-char #\Newline o))))
            segments)))

(defun %split-ws (s)
  (loop for start = 0 then (1+ pos)
        for pos = (position-if (lambda (c) (find c '(#\Space #\Tab))) s :start start)
        for tok = (string-trim '(#\Space #\Tab) (subseq s start (or pos (length s))))
        unless (zerop (length tok))
          collect tok
        while pos))

(defun parse-imap-untagged (line)
  "Parse an untagged (`* …`) IMAP line into a plist-like list."
  (let* ((rest (string-trim '(#\Space #\Tab #\Return #\Newline)
                            (if (and (>= (length line) 2)
                                     (char= (char line 0) #\*)
                                     (find (char line 1) '(#\Space #\Tab)))
                                (subseq line 2)
                                line)))
         (tokens (%split-ws rest))
         (head (and tokens (string-upcase (first tokens)))))
    (cond
      ((null tokens) (list :untagged :unknown rest))
      ((member head '("OK" "NO" "BAD" "BYE" "PREAUTH") :test #'string=)
       (list :untagged (intern head :keyword)
             (string-trim '(#\Space) (subseq rest (length (first tokens))))))
      ((string= head "SEARCH")
       (list :untagged :search
             (mapcar (lambda (tok) (parse-integer tok :junk-allowed t))
                     (rest tokens))))
      ((string= head "CAPABILITY")
       (list :untagged :capability (rest tokens)))
      ((string= head "ENABLED")
       (list :untagged :enabled (rest tokens)))
      ((and (every #'digit-char-p head)
            (rest tokens)
            (string-equal "FETCH" (second tokens)))
       (list :untagged :fetch
             :seq (parse-integer head)
             :raw rest
             :body (%extract-fetch-literal rest)))
      ((and (every #'digit-char-p head)
            (rest tokens)
            (string-equal "EXISTS" (second tokens)))
       (list :untagged :exists (parse-integer head)))
      (t (list :untagged :unknown rest)))))

(defun parse-imap-response (line)
  "Parse one IMAP response line. Tagged, untagged (`*`), or continuation (`+`)."
  (let ((line (string-right-trim '(#\Return #\Newline) line)))
    (cond
      ((zerop (length line)) (list :empty))
      ((char= (char line 0) #\*)
       (parse-imap-untagged line))
      ((char= (char line 0) #\+)
       (list :continuation (string-trim '(#\Space) (subseq line 1))))
      (t
       (let* ((sp (or (position #\Space line) (length line)))
              (tag (subseq line 0 sp))
              (rest (if (< sp (length line)) (subseq line (1+ sp)) ""))
              (sp2 (or (position #\Space rest) (length rest)))
              (status (string-upcase (subseq rest 0 sp2)))
              (text (if (< sp2 (length rest))
                        (string-trim '(#\Space) (subseq rest (1+ sp2)))
                        "")))
         (list :tagged tag (intern status :keyword) text))))))

(defun %extract-fetch-literal (rest)
  "Pull RFC822 / BODY[] literal text after `{n}` if it is inline in REST.
N counts UTF-8 octets; the text starts after the CRLF (or LF) that ends the
{n} line."
  (let ((pos (or (search "RFC822 {" rest :test #'char-equal)
                 (search "BODY[] {" rest :test #'char-equal)
                 (search "BODY.PEEK[] {" rest :test #'char-equal))))
    (when pos
      (let* ((brace (position #\{ rest :start pos))
             (end (and brace (position #\} rest :start brace))))
        (when (and brace end)
          (let ((n (parse-integer rest :start (1+ brace) :end end :junk-allowed t))
                (start (1+ end)))
            (when (and (< start (length rest)) (char= #\Return (char rest start)))
              (incf start))
            (when (and (< start (length rest)) (char= #\Newline (char rest start)))
              (incf start))
            (when (and n (< start (length rest)))
              (let ((stop start)
                    (octets 0))
                (loop while (and (< stop (length rest)) (< octets n))
                      do (incf octets (%char-octets (char rest stop) nil))
                         (incf stop))
                (subseq rest start stop)))))))))

(defun %char-octets (c octet-mode)
  "Octets C takes on the wire: 1 in OCTET-MODE (a Latin-1 stream carrying
UTF-8 octets as characters), else its UTF-8 length."
  (let ((code (char-code c)))
    (cond (octet-mode 1)
          ((< code #x80) 1)
          ((< code #x800) 2)
          ((< code #x10000) 3)
          (t 4))))

(defun %octet-mode-p (stream)
  "True when STREAM's external format is Latin-1: the client then writes
UTF-8 octets one per character."
  (let* ((format (ignore-errors (stream-external-format stream)))
         (name (if (consp format) (first format) format)))
    (and (symbolp name)
         (member (symbol-name name) '("LATIN-1" "LATIN1" "ISO-8859-1" "ISO8859-1")
                 :test #'string-equal)
         t)))

(defun %wire-string (string octet-mode)
  (if (or (not octet-mode) (%ascii-p string))
      string
      (map 'string #'code-char (encoding-protocol:encode string :encoding :utf-8))))

(defun %literal-size (line)
  "N when LINE (its CR trimmed) ends in {N}: a literal follows."
  (let ((end (length (string-right-trim '(#\Return) line))))
    (when (and (> end 2) (char= #\} (char line (1- end))))
      (let ((open (position #\{ line :end end :from-end t)))
        (when open
          (let ((digits (subseq line (1+ open) (1- end))))
            (and (plusp (length digits))
                 (every (lambda (c) (char<= #\0 c #\9)) digits)
                 (parse-integer digits))))))))

(defun %read-literal (stream n octet-mode out)
  "Copy an N-octet literal from STREAM to OUT, its line breaks kept.
→ the text after it on its last line (the rest of the response line), or
NIL at end of file."
  (let ((remaining n))
    (loop
      (let ((line (read-line stream nil nil)))
        (unless line
          (return nil))
        (let ((pos 0)
              (len (length line)))
          (loop while (and (< pos len) (plusp remaining))
                do (decf remaining (%char-octets (char line pos) octet-mode))
                   (incf pos))
          (cond
            ((< pos len)
             (write-string line out :end pos)
             (return (subseq line pos)))
            ((not (plusp remaining))
             (write-string line out)
             (return ""))
            (t
             (write-string line out)
             (write-char #\Newline out)
             (decf remaining)
             (unless (plusp remaining)
               (return (or (read-line stream nil nil) ""))))))))))

(defun %read-response-line (stream octet-mode)
  "One response line from STREAM, any literals in it folded in (so a body
line is never taken for a tagged reply), or NIL at end of file."
  (let ((tail (read-line stream nil nil)))
    (when tail
      (with-output-to-string (o)
        (loop
          (write-string tail o)
          (let ((n (%literal-size tail)))
            (unless n
              (return))
            (write-char #\Newline o)
            (setf tail (%read-literal stream n octet-mode o))
            (unless tail
              (return))))))))

(defun %continuation-line-p (line)
  (and (plusp (length line)) (char= #\+ (char line 0))))

(defun %tagged-line-p (line tag)
  (let ((n (length tag)))
    (and (> (length line) n)
         (string= tag line :end2 n)
         (char= #\Space (char line n)))))

(defun %io (client tag segments)
  "Send command SEGMENTS (NIL: read the greeting) and return the response
lines. Each segment but the last waits for a + continuation; a tagged reply
instead (the server refused the literal) ends the command there."
  (let ((fn (imap-client-io-fn client))
        (s (imap-client-stream client)))
    (cond
      (fn
       (if (null segments)
           (funcall fn nil)
           (loop for (segment . more) on segments
                 for lines = (%ensure-list (funcall fn segment))
                 append lines into all
                 while (and more
                            (some #'%continuation-line-p lines)
                            (notany (lambda (l) (%tagged-line-p l tag)) lines))
                 finally (return all))))
      (s
       (let ((octet-mode (%octet-mode-p s)))
         (flet ((read-one ()
                  (or (%read-response-line s octet-mode)
                      (error 'imap-error :tag tag :message "IMAP connection closed"))))
           (if (null segments)
               (list (read-one))
               (let ((all '()))
                 (loop for (segment . more) on segments
                       do (write-string (%wire-string segment octet-mode) s)
                          (write-char #\Return s)
                          (write-char #\Newline s)
                          (force-output s)
                          (loop for line = (read-one)
                                do (push line all)
                                until (or (%tagged-line-p line tag)
                                          (and more (%continuation-line-p line))))
                       until (%tagged-line-p (first all) tag))
                 (nreverse all))))))
      (t (error 'imap-error :message "imap-client has no stream or io-fn")))))

(defun %ensure-list (x)
  (if (and (consp x) (not (keywordp (car x))))
      x
      (list x)))

(defun %parse-lines (lines)
  (mapcar #'parse-imap-response (%ensure-list lines)))

(defun %tagged-status (parsed)
  (find :tagged parsed :key #'car))

(defun %response-code-capabilities (text)
  "The atoms of a [CAPABILITY ...] response code at the start of TEXT."
  (when (and text (>= (length text) 12) (string-equal "[CAPABILITY " text :end2 12))
    (let ((end (position #\] text)))
      (when end
        (%split-ws (subseq text 12 end))))))

(defun %note-capabilities (client parsed)
  "Keep the last CAPABILITY list PARSED carries, as a response or a
[CAPABILITY ...] response code."
  (dolist (p parsed)
    (let ((caps (cond ((and (eq (first p) :untagged) (eq (second p) :capability))
                       (third p))
                      ((and (eq (first p) :untagged) (eq (second p) :ok))
                       (%response-code-capabilities (third p)))
                      ((and (eq (first p) :tagged) (eq (third p) :ok))
                       (%response-code-capabilities (fourth p))))))
      (when caps
        (setf (imap-client-capabilities client) caps)))))

(defun imap-command (client verb &rest args)
  "Send VERB ARGS as one tagged command and read the reply. Every argument
is checked and encoded (see ENCODE-IMAP-COMMAND) before anything is written;
literals wait for the server's + continuation.
→ (values parsed-lines tag raw-lines)."
  (let* ((n (1+ (imap-client-tag-counter client)))
         (tag (format nil "A~4,'0d" n))
         (segments (%imap-segments tag verb args (imap-client-utf8-accept client))))
    (setf (imap-client-tag-counter client) n)
    (let* ((lines (%ensure-list (%io client tag segments)))
           (parsed (%parse-lines lines)))
      (%note-capabilities client parsed)
      (values parsed tag lines))))

(defun %check-tagged (parsed tag verb)
  "Signal IMAP-ERROR when PARSED ends in NO or BAD."
  (let ((tagged (%tagged-status parsed)))
    (when (and tagged (member (third tagged) '(:no :bad)))
      (error 'imap-error :tag tag :status (third tagged)
                         :message (or (fourth tagged) (format nil "~A failed" verb))))
    parsed))

(defun %sequence-set (sequence)
  "SEQUENCE (a positive integer or a sequence-set string such as 1:*,7) as
a command argument; anything else → IMAP-ARGUMENT-ERROR."
  (typecase sequence
    ((integer 1) sequence)
    (string
     (%check-line-safe sequence "sequence set")
     (unless (and (plusp (length sequence))
                  (every (lambda (c) (or (char<= #\0 c #\9) (find c ":*,$"))) sequence))
       (error 'imap-argument-error
              :argument sequence
              :message (format nil "~S is not an IMAP sequence set" sequence)))
     (imap-raw sequence))
    (t (error 'imap-argument-error
              :argument sequence
              :message (format nil "~S is not an IMAP sequence set" sequence)))))

(defun %non-ascii-string-in-p (tree)
  (typecase tree
    (string (not (%ascii-p tree)))
    (cons (or (%non-ascii-string-in-p (car tree))
              (%non-ascii-string-in-p (cdr tree))))
    (t nil)))

(defgeneric imap-connect (client &key)
  (:documentation "Read the server greeting (`* OK`). Sets state :connected."))

(defgeneric imap-login (client username password &key)
  (:documentation "LOGIN. Failure → IMAP-AUTH-ERROR."))

(defgeneric imap-select (client mailbox &key)
  (:documentation "SELECT MAILBOX. Sets IMAP-CLIENT-SELECTED.
MAILBOX is the name as text (Entwürfe, not Entw&APw-rfe): it goes in
modified UTF-7 (RFC 3501 §5.1.3), or as UTF-8 once UTF8=ACCEPT is enabled.
CR, LF or NUL → IMAP-ARGUMENT-ERROR."))

(defgeneric imap-fetch (client sequence &key items)
  (:documentation "FETCH SEQUENCE. ITEMS defaults RFC822.
   Returns a list of MESSAGE (via PARSE-MESSAGE) or raw strings."))

(defgeneric imap-search (client criteria &key)
  (:documentation "SEARCH CRITERIA. → list of sequence numbers.
CRITERIA is a string of search-key syntax sent as it is (FROM alice UNSEEN),
or a list whose keywords are search keys and whose strings are values,
quoted as needed: (:from \"Zoe Smith\" :since \"1-Jan-2026\"). A non-ASCII
string in the list goes as a literal, with CHARSET UTF-8 in front unless
UTF8=ACCEPT is on. CR, LF or NUL anywhere → IMAP-ARGUMENT-ERROR, and so does
8-bit text in a criteria string."))

(defgeneric imap-idle (client &key handler)
  (:documentation "Register HANDLER for untagged EXISTS/EXPUNGE.
   Stub: does not block on a live server."))

(defgeneric imap-capability (client &key)
  (:documentation "CAPABILITY. → the server's capability strings, also kept
in IMAP-CLIENT-CAPABILITIES."))

(defgeneric imap-enable (client &rest capabilities)
  (:documentation "ENABLE CAPABILITIES (RFC 5161). → the ones the server
ENABLED. UTF8=ACCEPT among them sets IMAP-CLIENT-UTF8-ACCEPT. NO or BAD →
IMAP-ERROR."))

(defmethod imap-connect ((client imap-client) &key)
  (let* ((lines (%io client nil nil))
         (parsed (%parse-lines (or lines '("* OK IMAP4rev1 ready")))))
    (%note-capabilities client parsed)
    (setf (imap-client-greeting client) parsed
          (imap-client-state client) :connected)
    parsed))

(defmethod imap-login ((client imap-client) username password &key)
  (dolist (v (list username password))
    (unless (stringp v)
      (error 'imap-argument-error :argument v
                                  :message (format nil "LOGIN wants strings, got ~S" v))))
  (multiple-value-bind (parsed tag) (imap-command client "LOGIN" username password)
    (let ((tagged (%tagged-status parsed)))
      (when (and tagged (eq (third tagged) :no))
        (error 'imap-auth-error :tag tag :status :no
                                :message (or (fourth tagged) "LOGIN failed")))
      (when (and tagged (eq (third tagged) :bad))
        (error 'imap-error :tag tag :status :bad
                           :message (or (fourth tagged) "LOGIN BAD"))))
    (setf (imap-client-state client) :authenticated)
    parsed))

(defmethod imap-select ((client imap-client) mailbox &key)
  (unless (stringp mailbox)
    (error 'imap-argument-error :argument mailbox
                                :message (format nil "SELECT wants a mailbox name, got ~S" mailbox)))
  ;; Check before modified UTF-7, which would hide a CR as &AA0-.
  (%check-line-safe mailbox "mailbox name")
  (let ((parsed (imap-command client "SELECT"
                              (if (imap-client-utf8-accept client)
                                  mailbox
                                  (imap-utf7-encode mailbox)))))
    (setf (imap-client-selected client) mailbox
          (imap-client-state client) :selected)
    parsed))

(defmethod imap-fetch ((client imap-client) sequence &key (items "RFC822"))
  (multiple-value-bind (parsed tag lines)
      (imap-command client "FETCH" (%sequence-set sequence)
                    (if (stringp items) (imap-raw items) items))
    (declare (ignore tag))
    (let ((bodies (loop for p in parsed
                        when (and (eq (first p) :untagged)
                                  (eq (second p) :fetch)
                                  (getf (cddr p) :body))
                          collect (getf (cddr p) :body))))
      (unless bodies
        (let ((lit (%extract-fetch-literal (format nil "~{~a~%~}" lines))))
          (when lit (push lit bodies))))
      (mapcar (lambda (b)
                (if (plusp (length b))
                    (handler-case (parse-message b)
                      (mail-parse-error () b))
                    b))
              bodies))))

(defmethod imap-search ((client imap-client) criteria &key)
  (let* ((args (typecase criteria
                 (string (list (imap-raw criteria)))
                 (imap-raw (list criteria))
                 (cons (if (and (not (imap-client-utf8-accept client))
                                (%non-ascii-string-in-p criteria)
                                (not (member (first criteria) '(:charset "CHARSET")
                                             :test #'equalp)))
                           (list* "CHARSET" "UTF-8" criteria)
                           criteria))
                 (t (error 'imap-argument-error
                           :argument criteria
                           :message (format nil "SEARCH wants a string or a list, got ~S"
                                            criteria)))))
         (parsed (apply #'imap-command client "SEARCH" args)))
    (or (loop for p in parsed
              when (and (eq (first p) :untagged) (eq (second p) :search))
                return (third p))
        nil)))

(defmethod imap-capability ((client imap-client) &key)
  (multiple-value-bind (parsed tag) (imap-command client "CAPABILITY")
    (%check-tagged parsed tag "CAPABILITY")
    (imap-client-capabilities client)))

(defmethod imap-enable ((client imap-client) &rest capabilities)
  (multiple-value-bind (parsed tag) (apply #'imap-command client "ENABLE" capabilities)
    (%check-tagged parsed tag "ENABLE")
    (let ((enabled (loop for p in parsed
                         when (and (eq (first p) :untagged) (eq (second p) :enabled))
                           append (third p))))
      (when (member "UTF8=ACCEPT" enabled :test #'string-equal)
        (setf (imap-client-utf8-accept client) t))
      enabled)))

(defmethod imap-idle ((client imap-client) &key handler)
  (when handler
    (setf (imap-client-idle-handler client) handler))
  (let ((h (imap-client-idle-handler client)))
    (when h
      (funcall h client :registered))
    h))
