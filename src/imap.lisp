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

(defstruct (%imap-literal (:constructor %imap-literal (text &optional octets)))
  "A string sent as an IMAP literal: {n} CRLF, the server's + continuation,
then the N octets. OCTETS true: TEXT is octets, one character each (as
PRINT-MESSAGE writes them), else characters sent as UTF-8."
  (text "" :type string)
  (octets nil))

(defun %octet-length (string)
  "STRING's length in UTF-8 octets."
  (if (%ascii-p string)
      (length string)
      (length (encoding-protocol:encode string :encoding :utf-8))))

(defun %check-line-safe (value what)
  "VALUE, unless it holds CR, LF or NUL, or a lone UTF-16 surrogate (no
UTF-8 or modified UTF-7 form): then IMAP-ARGUMENT-ERROR. WHAT names the
argument in the message."
  (let ((bad (position-if (lambda (c)
                            (or (char= c #\Return) (char= c #\Newline)
                                (char= c (code-char 0))))
                          value))
        (surrogate (position-if (lambda (c) (<= #xD800 (char-code c) #xDFFF)) value)))
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
    (when surrogate
      (error 'imap-argument-error
             :argument value
             :message (format nil "the ~A ~S holds a lone surrogate U+~4,'0X at ~D"
                              what value (char-code (char value surrogate)) surrogate)))
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
text; an IMAP-LITERAL → itself (the only piece that may hold CR LF); a
non-negative integer → its digits; a symbol → an atom; a list → a
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
    (%imap-literal (funcall emit value))
    ((integer 0) (funcall emit (princ-to-string value)))
    (symbol (funcall emit (%imap-atom value)))
    (cons
     (unless (null (cdr (last value)))
       (error 'imap-argument-error
              :argument value
              :message (format nil "cannot send the dotted list ~S" value)))
     (funcall emit "(")
     (loop for (x . more) on value
           do (%imap-emit x utf8 emit)
              (when more (funcall emit " ")))
     (funcall emit ")"))
    (t (error 'imap-argument-error
              :argument value
              :message (format nil "cannot send ~S in an IMAP command" value)))))

(defun %announces-literal-p (text)
  "True when TEXT ends in {n} or {n+} (also ~{n}, literal8): a server
reading it as a command line would wait for N octets."
  (let ((end (length text)))
    (when (and (> end 2) (char= #\} (char text (1- end))))
      (let* ((stop (if (char= #\+ (char text (- end 2))) (- end 2) (1- end)))
             (open (position #\{ text :end stop :from-end t)))
        (and open
             (< (1+ open) stop)
             (every (lambda (c) (char<= #\0 c #\9)) (subseq text (1+ open) stop)))))))

(defun imap-literal (data)
  "DATA as an IMAP literal argument for IMAP-COMMAND: {n}, the server's +
continuation, then the octets. DATA is a string (sent as UTF-8) or an octet
vector, and may hold CR and LF: a literal is the one place they can go, as
its size, not a line end, tells the server where it stops. NUL (no CHAR8,
RFC 3501 §4.3) or a lone surrogate → IMAP-ARGUMENT-ERROR."
  (flet ((refuse (what at)
           (error 'imap-argument-error
                  :argument data
                  :message (format nil "a literal cannot hold ~A (at ~D)" what at))))
    (typecase data
      (string
       (let ((nul (position (code-char 0) data))
             (surrogate (position-if (lambda (c) (<= #xD800 (char-code c) #xDFFF)) data)))
         (when nul (refuse "NUL" nul))
         (when surrogate (refuse "a lone surrogate" surrogate))
         (%imap-literal data)))
      ((vector (unsigned-byte 8))
       (let ((nul (position 0 data)))
         (when nul (refuse "NUL" nul))
         (%imap-literal (map 'string #'code-char data) t)))
      (t (error 'imap-argument-error
                :argument data
                :message (format nil "a literal is a string or octets, not ~S" data))))))

(defun %literal-octet-count (literal)
  (if (%imap-literal-octets literal)
      (length (%imap-literal-text literal))
      (%octet-length (%imap-literal-text literal))))

(defun %literal-wire-text (literal octet-mode)
  "LITERAL's text as segment text: octets, one character each, in
OCTET-MODE; characters otherwise. Octets that are not UTF-8 cannot go
through a character stream → IMAP-ARGUMENT-ERROR."
  (let ((text (%imap-literal-text literal)))
    (cond
      ((not (%imap-literal-octets literal)) (%wire-string text octet-mode))
      ((or octet-mode (%ascii-p text)) text)
      (t (handler-case
             (encoding-protocol:decode-utf-8-strict (map '(vector (unsigned-byte 8)) #'char-code text))
           (encoding-protocol:encoding-error (e)
             (error 'imap-argument-error
                    :argument text
                    :message (format nil "the literal's octets are not UTF-8 (~A); use a Latin-1 or binary stream to send them"
                                     e))))))))

(defun %imap-segments (tag verb args utf8 &optional octet-mode)
  "The wire segments of TAG VERB ARGS (no final CRLFs). Every segment but the
last ends in {n}; the next one starts with that literal's text and goes
only after the server's + continuation. Every argument is checked before
anything is returned, so a refused command writes nothing. A command line
whose own text ends in {n} or {n+} (raw text such as FROM {5}) is refused:
the server would wait for a literal and take the next command as it.
OCTET-MODE (a Latin-1 or binary stream): the segments are octets, one
character each; otherwise characters, to go out as UTF-8."
  (let ((segments '())
        (current (make-string-output-stream))
        (tail (make-string-output-stream))
        (verb (string-upcase (string verb))))
    (unless (and (stringp tag)
                 (%astring-atom-p (%check-line-safe tag "tag"))
                 (not (find #\+ tag)))
      (error 'imap-argument-error :argument tag
                                  :message (format nil "the tag ~S is not an IMAP tag" tag)))
    (unless (every #'%astring-atom-p (uiop:split-string (%check-line-safe verb "command")
                                                        :separator " "))
      (error 'imap-argument-error :argument verb
                                  :message (format nil "the command ~S is not atoms" verb)))
    (flet ((emit (piece)
             (etypecase piece
               (string (write-string (%wire-string piece octet-mode) current)
                       (write-string piece tail))
               (%imap-literal
                (format current "{~D}" (%literal-octet-count piece))
                (push (get-output-stream-string current) segments)
                (get-output-stream-string tail)
                (write-string (%literal-wire-text piece octet-mode) current)))))
      (emit tag)
      (emit " ")
      (emit verb)
      (dolist (arg args)
        (emit " ")
        (%imap-emit arg utf8 #'emit))
      (let ((own (get-output-stream-string tail)))
        (when (%announces-literal-p own)
          (error 'imap-argument-error
                 :argument own
                 :message (format nil "the command ends in a literal announcement (~S); the server would take the next command as the literal"
                                  own))))
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

(defun %utf7-error (wire control &rest args)
  (error 'imap-error
         :message (format nil "modified UTF-7 ~S: ~?" wire control args)))

(defun %utf7-run-units (wire start end)
  "The UTF-16 code units of the modified base64 run WIRE[START,END), checked
to be canonical (RFC 3501 §5.1.3): only modified base64 characters (, for
/, no = padding), whole units, and fewer than six leftover bits, all zero.
Anything else → IMAP-ERROR."
  (let ((bits 0)
        (nbits 0)
        (units '()))
    (loop for i from start below end
          for c = (char wire i)
          for v = (cond ((char<= #\A c #\Z) (- (char-code c) 65))
                        ((char<= #\a c #\z) (+ 26 (- (char-code c) 97)))
                        ((char<= #\0 c #\9) (+ 52 (- (char-code c) 48)))
                        ((char= c #\+) 62)
                        ((char= c #\,) 63))
          do (unless v
               (%utf7-error wire "~S at ~D is not a modified base64 character" c i))
             (setf bits (logior (ash bits 6) v))
             (incf nbits 6)
             (when (>= nbits 16)
               (decf nbits 16)
               (push (ldb (byte 16 nbits) bits) units)
               (setf bits (ldb (byte nbits 0) bits))))
    (when (or (>= nbits 6) (null units))
      (%utf7-error wire "the run at ~D does not end on a UTF-16 unit" (1- start)))
    (unless (zerop bits)
      (%utf7-error wire "the run at ~D has non-zero padding bits" (1- start)))
    (nreverse units)))

(defun %utf7-units-string (wire units at)
  "The characters UTF-16 UNITS encode. A lone surrogate, a character that
must stand as itself (printable ASCII), or CR, LF or NUL → IMAP-ERROR."
  (let ((octets (make-array (* 2 (length units)) :element-type '(unsigned-byte 8))))
    (loop for (u . more) on units
          for k from 0 by 2
          for prev = nil then high
          for high = (<= #xD800 u #xDBFF)
          do (when (or (and high (not (and more (<= #xDC00 (first more) #xDFFF))))
                       (and (<= #xDC00 u #xDFFF) (not prev)))
               (%utf7-error wire "the run at ~D holds a lone surrogate ~4,'0X" at u))
             (setf (aref octets k) (ash u -8)
                   (aref octets (1+ k)) (logand u #xFF)))
    (let ((text (encoding-protocol:decode octets :encoding :utf-16be)))
      (loop for ch across text
            do (cond ((<= #x20 (char-code ch) #x7E)
                      (%utf7-error wire "the run at ~D encodes ~S, which must stand as itself"
                                   at ch))
                     ((member (char-code ch) '(0 10 13))
                      (%utf7-error wire "the run at ~D encodes ~A, which no mailbox name holds"
                                   at (case (char-code ch) (0 "NUL") (10 "LF") (t "CR"))))))
      text)))

(defun imap-utf7-decode (wire)
  "The mailbox name that modified UTF-7 WIRE encodes (RFC 3501 §5.1.3).
Only the canonical form decodes, the one IMAP-UTF7-ENCODE writes: printable
ASCII stands as itself (& as &-), every other run is one &...- of modified
base64 with no leftover bits. Anything else → IMAP-ERROR: a raw 8-bit or
control character, printable ASCII in base64 (&AGE- for a), two adjacent
runs, a / or = in a run, leftover or non-zero padding bits, a lone
surrogate, or a CR, LF or NUL in the result."
  (with-output-to-string (o)
    (let ((i 0)
          (n (length wire))
          (after-run nil))
      (loop while (< i n)
            do (let ((c (char wire i)))
                 (cond
                   ((char/= c #\&)
                    (unless (<= #x20 (char-code c) #x7E)
                      (%utf7-error wire "~S at ~D must be in base64" c i))
                    (write-char c o)
                    (setf after-run nil)
                    (incf i))
                   (t
                    (let ((end (position #\- wire :start (1+ i))))
                      (unless end
                        (%utf7-error wire "& at ~D has no closing -" i))
                      (cond
                        ((= end (1+ i))
                         (write-char #\& o)
                         (setf after-run nil))
                        (after-run
                         (%utf7-error wire "the run at ~D follows another one; canonical form joins them"
                                      i))
                        (t
                         (write-string (%utf7-units-string wire (%utf7-run-units wire (1+ i) end) i)
                                       o)
                         (setf after-run t)))
                      (setf i (1+ end))))))))))

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

(defun %extract-fetch-literal (rest &optional octet-mode)
  "Pull RFC822 / BODY[] literal text after `{n}` if it is inline in REST.
N counts octets: UTF-8 octets of the text, or one per character in
OCTET-MODE (a Latin-1 stream). The text starts after the CRLF (or LF) that
ends the {n} line."
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
                      do (incf octets (%char-octets (char rest stop) octet-mode))
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

(defun %binary-stream-p (stream)
  "True when STREAM's elements are octets."
  (let ((type (ignore-errors (stream-element-type stream))))
    (and type
         (not (eq type :default))
         (ignore-errors (subtypep type '(unsigned-byte 8))))))

(defun %octet-mode-p (stream)
  "How the client reads and writes STREAM. :BINARY for a stream of octets
(read-byte / write-byte): octets in and out, so any reply text reads and
literal sizes are exact. T when its external format is Latin-1: UTF-8
octets one per character, the same. NIL for a strict UTF-8 stream (or one
with no external format, as a string stream). A replacement character or
another charset would make the client miscount literal octets, so that is
IMAP-ERROR. A two-way stream is judged by its two sides."
  (cond
    ((typep stream 'two-way-stream)
     (let ((in (%octet-mode-p (two-way-stream-input-stream stream)))
           (out (%octet-mode-p (two-way-stream-output-stream stream))))
       (if (or (eq in :binary) (eq out :binary))
           :binary
           (or in out))))
    ((%binary-stream-p stream) :binary)
    (t
     (let* ((format (ignore-errors (stream-external-format stream)))
            (name (if (consp format) (first format) format))
            (options (if (consp format) (rest format) nil))
            (label (and (symbolp name) name (symbol-name name))))
       (cond
         ((and label (member label '("LATIN-1" "LATIN1" "ISO-8859-1" "ISO8859-1")
                             :test #'string-equal))
          t)
         ((and (or (null label)
                   (member label '("DEFAULT" "UTF-8" "UTF8") :test #'string-equal))
               (not (and (listp options) (getf options :replacement))))
          nil)
         (t (error 'imap-error
                   :message (format nil "IMAP stream external format ~S: use octets, Latin-1 or strict UTF-8, or literal sizes go wrong"
                                    format))))))))

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

(defun %read-wire-line (stream octet-mode)
  "One line from STREAM without its LF (a CR before it stays, as READ-LINE
leaves it), or NIL at end of file. On a binary stream (OCTET-MODE :BINARY)
each octet is one character of the same code, as on a Latin-1 stream."
  (if (eq octet-mode :binary)
      (let ((first (read-byte stream nil nil)))
        (when first
          (with-output-to-string (o)
            (loop for b = first then (read-byte stream nil nil)
                  until (or (null b) (= b 10))
                  do (write-char (code-char b) o)))))
      (read-line stream nil nil)))

(defun %read-literal (stream n octet-mode out)
  "Copy an N-octet literal from STREAM to OUT, its line breaks kept.
→ the text after it on its last line (the rest of the response line), or
NIL at end of file."
  (let ((remaining n))
    (loop
      (let ((line (%read-wire-line stream octet-mode)))
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
               (return (or (%read-wire-line stream octet-mode) ""))))))))))

(defun %read-response-line (stream octet-mode)
  "One response line from STREAM, or NIL at end of file. On an untagged
(`* `) line, literals are folded in, so a body line is never taken for a
tagged reply. A tagged or + line is resp-text: a {n} at its end is text
(a server may echo a name), as Python imaplib reads it."
  (let ((tail (%read-wire-line stream octet-mode)))
    (when tail
      (let ((untagged (and (>= (length tail) 2) (string= "* " tail :end2 2))))
        (with-output-to-string (o)
          (loop
            (write-string tail o)
            (let ((n (and untagged (%literal-size tail))))
              (unless n
                (return))
              (write-char #\Newline o)
              (setf tail (%read-literal stream n octet-mode o))
              (unless tail
                (return)))))))))

(defun %continuation-line-p (line)
  (and (plusp (length line)) (char= #\+ (char line 0))))

(defun %tagged-line-p (line tag)
  (let ((n (length tag)))
    (and (> (length line) n)
         (string= tag line :end2 n)
         (char= #\Space (char line n)))))

(defun %io (client tag segments)
  "Send command SEGMENTS (NIL: read the greeting) and return the response
lines. SEGMENTS are already in the stream's wire form (see %IMAP-SEGMENTS).
Each segment but the last waits for a + continuation; a tagged reply
instead (the server refused the literal) ends the command there. A stream
error (a closed socket, an undecodable octet in strict UTF-8) leaves the
stream out of step: it is IMAP-ERROR, and the client is :disconnected."
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
                      (error 'imap-error :tag tag :message "IMAP connection closed")))
                (send (segment)
                  (let ((line (concatenate 'string segment (coerce '(#\Return #\Newline) 'string))))
                    (if (eq octet-mode :binary)
                        (write-sequence (map '(vector (unsigned-byte 8)) #'char-code line) s)
                        (write-string line s)))
                  (force-output s)))
           (handler-case
               (if (null segments)
                   (list (read-one))
                   (let ((all '()))
                     (loop for (segment . more) on segments
                           do (send segment)
                              (loop for line = (read-one)
                                    do (push line all)
                                    until (or (%tagged-line-p line tag)
                                              (and more (%continuation-line-p line))))
                           until (%tagged-line-p (first all) tag))
                     (nreverse all)))
             (imap-error (e)
               (setf (imap-client-state client) :disconnected)
               (error e))
             (stream-error (e)
               (setf (imap-client-state client) :disconnected)
               (error 'imap-error :tag tag
                                  :message (format nil "IMAP stream failed; the connection is out of step: ~A"
                                                   e)))))))
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
literals wait for the server's + continuation. An IMAP-LITERAL argument is
the one place CR and LF may go.
→ (values parsed-lines tag raw-lines)."
  (let* ((n (1+ (imap-client-tag-counter client)))
         (tag (format nil "A~4,'0d" n))
         (stream (and (null (imap-client-io-fn client)) (imap-client-stream client)))
         (segments (%imap-segments tag verb args (imap-client-utf8-accept client)
                                   (and stream (%octet-mode-p stream)))))
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
  (:documentation "SELECT MAILBOX. On OK, sets IMAP-CLIENT-SELECTED and state
:selected. NO or BAD → IMAP-ERROR, with no mailbox selected and a selected
client back to :authenticated (RFC 3501 §6.3.1: SELECT deselects first).
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

(defgeneric imap-append (client mailbox message &key flags date)
  (:documentation "APPEND MESSAGE to MAILBOX (RFC 3501 §6.3.11). MESSAGE is a
MESSAGE (sent as PRINT-MESSAGE writes it), a string (sent as UTF-8) or
octets; it goes as a literal, its bare CRs and LFs made CR LF as imaplib
does. FLAGS: a list of flag strings (\"\\\\Seen\"). DATE: an IMAP
date-time string (\"27-Nov-2026 10:00:00 +0000\"). CR, LF or NUL in the
mailbox, a flag or the date, or NUL in the message → IMAP-ARGUMENT-ERROR
before anything is written; NO or BAD → IMAP-ERROR. → the parsed reply."))

(defgeneric imap-store (client sequence flags &key mode silent uid)
  (:documentation "STORE FLAGS on SEQUENCE (RFC 3501 §6.4.6). MODE :add (+FLAGS,
the default), :remove (-FLAGS) or :replace (FLAGS); SILENT adds .SILENT; UID
sends UID STORE. FLAGS: a list of flag strings (\"\\\\Seen\" \"$Forwarded\").
A malformed flag → IMAP-ARGUMENT-ERROR before anything is written; NO or
BAD → IMAP-ERROR. → the parsed reply, its untagged FETCH lines carrying the
new flags."))

(defun %mailbox-argument (client mailbox verb)
  "MAILBOX (its text name) as VERB's argument: modified UTF-7, or itself once
UTF8=ACCEPT is on. Not a string, or CR, LF or NUL → IMAP-ARGUMENT-ERROR."
  (unless (stringp mailbox)
    (error 'imap-argument-error :argument mailbox
                                :message (format nil "~A wants a mailbox name, got ~S" verb mailbox)))
  ;; Check before modified UTF-7, which would hide a CR as &AA0-.
  (%check-line-safe mailbox "mailbox name")
  (if (imap-client-utf8-accept client)
      mailbox
      (imap-utf7-encode mailbox)))

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
  (let ((arg (%mailbox-argument client mailbox "SELECT")))
    ;; SELECT deselects first (RFC 3501 §6.3.1): a failed one leaves no
    ;; mailbox selected, as imaplib drops to state AUTH.
    (setf (imap-client-selected client) nil)
    (when (eq (imap-client-state client) :selected)
      (setf (imap-client-state client) :authenticated))
    (multiple-value-bind (parsed tag) (imap-command client "SELECT" arg)
      (let ((tagged (%tagged-status parsed)))
        (unless (and tagged (eq (third tagged) :ok))
          (error 'imap-error :tag tag :status (and tagged (third tagged))
                             :message (or (and tagged (fourth tagged))
                                          "SELECT got no tagged reply"))))
      (setf (imap-client-selected client) mailbox
            (imap-client-state client) :selected)
      parsed)))

(defmethod imap-fetch ((client imap-client) sequence &key (items "RFC822"))
  (multiple-value-bind (parsed tag lines)
      (imap-command client "FETCH" (%sequence-set sequence)
                    (if (stringp items) (imap-raw items) items))
    (declare (ignore tag))
    (let* ((octet-mode (and (null (imap-client-io-fn client))
                            (imap-client-stream client)
                            (%octet-mode-p (imap-client-stream client))))
           (bodies (loop for p in parsed
                         for body = (and (eq (first p) :untagged)
                                         (eq (second p) :fetch)
                                         (if octet-mode
                                             (%extract-fetch-literal (getf (cddr p) :raw) t)
                                             (getf (cddr p) :body)))
                         when body
                           collect body)))
      (unless bodies
        (let ((lit (%extract-fetch-literal (format nil "~{~a~%~}" lines) octet-mode)))
          (when lit (push lit bodies))))
      (mapcar (lambda (b)
                (if (plusp (length b))
                    (handler-case
                        ;; Parse the body's octets, so its charset decodes: on a
                        ;; Latin-1 stream they are its characters, one each;
                        ;; otherwise the characters' UTF-8.
                        (parse-message (cond (octet-mode
                                              (map '(vector (unsigned-byte 8)) #'char-code b))
                                             ((every (lambda (c) (< (char-code c) 128)) b) b)
                                             (t (encoding-protocol:encode b :encoding :utf-8))))
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

(defun %imap-flag (flag)
  "FLAG (a string: \\Seen, \\Deleted, $Forwarded, a keyword) as command text.
A system flag is \\ and an atom, a keyword an atom (RFC 3501 §2.3.2, flag
syntax §9); anything else → IMAP-ARGUMENT-ERROR."
  (let ((body (and (stringp flag)
                   (if (and (plusp (length flag)) (char= #\\ (char flag 0)))
                       (subseq flag 1)
                       flag))))
    (unless (and body
                 (%astring-atom-p (%check-line-safe body "flag"))
                 (not (find #\] body)))
      (error 'imap-argument-error
             :argument flag
             :message (format nil "~S is not an IMAP flag" flag)))
    (imap-raw flag)))

(defun %imap-flag-list (flags)
  (unless (listp flags)
    (error 'imap-argument-error
           :argument flags
           :message (format nil "flags are a list of strings, not ~S" flags)))
  (mapcar #'%imap-flag flags))

(defun %crlf-line-ends (text)
  "TEXT with every bare CR or bare LF a CR LF, as imaplib's MapCRLF does
for APPEND."
  (with-output-to-string (o)
    (loop with n = (length text)
          for i from 0 below n
          for c = (char text i)
          do (cond ((char= c #\Return)
                    (write-char #\Return o)
                    (write-char #\Newline o)
                    (when (and (< (1+ i) n) (char= #\Newline (char text (1+ i))))
                      (incf i)))
                   ((char= c #\Newline)
                    (write-char #\Return o)
                    (write-char #\Newline o))
                   (t (write-char c o))))))

(defun %append-literal (message)
  "MESSAGE (a string, octets, or a MESSAGE) as an APPEND literal, its line
ends CR LF."
  (flet ((octets (text)
           (map '(vector (unsigned-byte 8)) #'char-code (%crlf-line-ends text))))
    (typecase message
      (string (imap-literal (%crlf-line-ends message)))
      ((vector (unsigned-byte 8)) (imap-literal (octets (map 'string #'code-char message))))
      (message (imap-literal (octets (print-message message))))
      (t (error 'imap-argument-error
                :argument message
                :message (format nil "APPEND wants a message, a string or octets, not ~S"
                                 message))))))

(defmethod imap-append ((client imap-client) mailbox message &key flags date)
  (let* ((args (append (list (%mailbox-argument client mailbox "APPEND"))
                       (when flags (list (%imap-flag-list flags)))
                       (when date
                         (unless (stringp date)
                           (error 'imap-argument-error
                                  :argument date
                                  :message (format nil "APPEND wants a date-time string, not ~S"
                                                   date)))
                         (list date))
                       (list (%append-literal message)))))
    (multiple-value-bind (parsed tag) (apply #'imap-command client "APPEND" args)
      (%check-tagged parsed tag "APPEND"))))

(defmethod imap-store ((client imap-client) sequence flags &key (mode :add) silent uid)
  (let ((item (format nil "~A~:[~;.SILENT~]"
                      (case mode
                        (:add "+FLAGS")
                        (:remove "-FLAGS")
                        (:replace "FLAGS")
                        (t (error 'imap-argument-error
                                  :argument mode
                                  :message (format nil "STORE :mode is :add, :remove or :replace, not ~S"
                                                   mode))))
                      silent))
        (set (%sequence-set sequence))
        (flags (or (%imap-flag-list flags) (imap-raw "()"))))
    (multiple-value-bind (parsed tag)
        (imap-command client (if uid "UID STORE" "STORE") set (imap-raw item) flags)
      (%check-tagged parsed tag "STORE"))))
