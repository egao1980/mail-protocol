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
   (greeting :initform nil :accessor imap-client-greeting)))

(defun imap-client-p (x)
  (typep x 'imap-client))

(defun make-imap-client (&key (host "localhost") (port 143) stream io-fn)
  (make-instance 'imap-client :host host :port port :stream stream :io-fn io-fn))

(defun next-imap-tag (client)
  (format nil "A~4,'0d" (incf (imap-client-tag-counter client))))

(defun %imap-quote (value)
  (cond
    ((null value) "NIL")
    ((and (stringp value)
          (or (find #\Space value) (find #\" value) (find #\\ value)))
     (with-output-to-string (o)
       (write-char #\" o)
       (loop for c across value
             do (when (or (char= c #\") (char= c #\\))
                  (write-char #\\ o))
                (write-char c o))
       (write-char #\" o)))
    (t (princ-to-string value))))

(defun encode-imap-command (tag verb &rest args)
  "Encode TAG VERB args as one IMAP command line (no CRLF)."
  (let ((parts (cons (string-upcase (string verb))
                     (mapcar #'%imap-quote args))))
    (format nil "~a ~{~a~^ ~}" tag parts)))

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
  "Pull RFC822 / BODY[] literal text after `{n}` if it is inline in REST."
  (let ((pos (or (search "RFC822 {" rest :test #'char-equal)
                 (search "BODY[] {" rest :test #'char-equal)
                 (search "BODY.PEEK[] {" rest :test #'char-equal))))
    (when pos
      (let* ((brace (position #\{ rest :start pos))
             (end (and brace (position #\} rest :start brace))))
        (when (and brace end)
          (let ((n (parse-integer rest :start (1+ brace) :end end :junk-allowed t))
                (after (and (< (1+ end) (length rest))
                            (subseq rest (1+ end)))))
            (when (and n after)
              (let ((payload (string-left-trim '(#\Newline #\Return #\Space) after)))
                (subseq payload 0 (min n (length payload)))))))))))

(defun %io (client command)
  (let ((fn (imap-client-io-fn client)))
    (cond
      (fn (funcall fn command))
      ((imap-client-stream client)
       (let ((s (imap-client-stream client)))
         (when command
           (write-string command s)
           (write-char #\Return s)
           (write-char #\Newline s)
           (force-output s))
         (loop for line = (read-line s nil nil)
               while line
               collect line
               until (and (plusp (length line))
                          (char/= (char line 0) #\*)
                          (char/= (char line 0) #\+)))))
      (t (error 'imap-error :message "imap-client has no stream or io-fn")))))

(defun %ensure-list (x)
  (if (and (consp x) (not (keywordp (car x))))
      x
      (list x)))

(defun %parse-lines (lines)
  (mapcar #'parse-imap-response (%ensure-list lines)))

(defun %tagged-status (parsed)
  (find :tagged parsed :key #'car))

(defgeneric imap-connect (client &key)
  (:documentation "Read the server greeting (`* OK`). Sets state :connected."))

(defgeneric imap-login (client username password &key)
  (:documentation "LOGIN. Failure → IMAP-AUTH-ERROR."))

(defgeneric imap-select (client mailbox &key)
  (:documentation "SELECT MAILBOX. Sets IMAP-CLIENT-SELECTED."))

(defgeneric imap-fetch (client sequence &key items)
  (:documentation "FETCH SEQUENCE. ITEMS defaults RFC822.
   Returns a list of MESSAGE (via PARSE-MESSAGE) or raw strings."))

(defgeneric imap-search (client criteria &key)
  (:documentation "SEARCH CRITERIA. → list of sequence numbers."))

(defgeneric imap-idle (client &key handler)
  (:documentation "Register HANDLER for untagged EXISTS/EXPUNGE.
   Stub: does not block on a live server."))

(defmethod imap-connect ((client imap-client) &key)
  (let* ((lines (%io client nil))
         (parsed (%parse-lines (or lines '("* OK IMAP4rev1 ready")))))
    (setf (imap-client-greeting client) parsed
          (imap-client-state client) :connected)
    parsed))

(defmethod imap-login ((client imap-client) username password &key)
  (let* ((tag (next-imap-tag client))
         (cmd (encode-imap-command tag "LOGIN" username password))
         (parsed (%parse-lines (%io client cmd)))
         (tagged (%tagged-status parsed)))
    (when (and tagged (eq (third tagged) :no))
      (error 'imap-auth-error :tag tag :status :no
             :message (or (fourth tagged) "LOGIN failed")))
    (when (and tagged (eq (third tagged) :bad))
      (error 'imap-error :tag tag :status :bad
             :message (or (fourth tagged) "LOGIN BAD")))
    (setf (imap-client-state client) :authenticated)
    parsed))

(defmethod imap-select ((client imap-client) mailbox &key)
  (let* ((tag (next-imap-tag client))
         (cmd (encode-imap-command tag "SELECT" mailbox))
         (parsed (%parse-lines (%io client cmd))))
    (setf (imap-client-selected client) mailbox
          (imap-client-state client) :selected)
    parsed))

(defmethod imap-fetch ((client imap-client) sequence &key (items "RFC822"))
  (let* ((tag (next-imap-tag client))
         (cmd (encode-imap-command tag "FETCH" sequence items))
         (raw (%io client cmd))
         (lines (%ensure-list raw))
         (joined (format nil "~{~a~%~}" lines))
         (parsed (%parse-lines lines))
         (bodies (loop for p in parsed
                       when (and (eq (first p) :untagged)
                                 (eq (second p) :fetch))
                         collect (getf (cddr p) :body))))
    (unless bodies
      (let ((lit (%extract-fetch-literal joined)))
        (when lit (push lit bodies))))
    (mapcar (lambda (b)
              (if (and b (plusp (length b)))
                  (handler-case (parse-message b)
                    (mail-parse-error () b))
                  b))
            (remove nil bodies))))

(defmethod imap-search ((client imap-client) criteria &key)
  (let* ((tag (next-imap-tag client))
         (cmd (encode-imap-command tag "SEARCH" criteria))
         (parsed (%parse-lines (%io client cmd))))
    (or (loop for p in parsed
              when (and (eq (first p) :untagged) (eq (second p) :search))
                return (third p))
        nil)))

(defmethod imap-idle ((client imap-client) &key handler)
  (when handler
    (setf (imap-client-idle-handler client) handler))
  (let ((h (imap-client-idle-handler client)))
    (when h
      (funcall h client :registered))
    h))
