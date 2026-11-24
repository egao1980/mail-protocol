(in-package #:mail-protocol)

;;; RFC 5322 3.4 addresses. An address list is cut at the commas between
;;; addresses (not inside a quoted display name, a comment, angle brackets
;;; or a group), and a mailbox gives its addr-spec and display name, which
;;; is what the SMTP envelope needs (smtplib's send_message reads From, To,
;;; Cc and Bcc with email.utils.getaddresses).

(defun %address-fail (address format-control &rest args)
  (error 'mail-address-error :address address
                             :message (apply #'format nil format-control args)))

(defun %refuse-line-breaks (address)
  "ADDRESS, unless it holds CR, LF or NUL: then signal MAIL-ADDRESS-ERROR."
  (let ((bad (find-if (lambda (c) (member (char-code c) '(0 10 13))) address)))
    (when bad
      (%address-fail address "the address holds ~A; a line break would end the header field or the SMTP command, and the rest would pose as another one"
                     (case (char-code bad) (0 "NUL") (10 "LF") (t "CR"))))
    address))

(defun %map-top-level (fn string)
  "Call FN with the index, character and angle-bracket depth of each
character of STRING outside quoted strings, comments and domain literals.
The depth of < and > is the depth outside them."
  (let ((quoted nil)
        (escaped nil)
        (comment 0)
        (literal nil)
        (angle 0))
    (dotimes (i (length string))
      (let ((c (char string i)))
        (cond
         (escaped
          (setf escaped nil))
         ((and (or quoted literal (plusp comment)) (char= c #\\))
          (setf escaped t))
         (quoted
          (when (char= c #\") (setf quoted nil)))
         ((plusp comment)
          (case c
            (#\( (incf comment))
            (#\) (decf comment))))
         (literal
          (when (char= c #\]) (setf literal nil)))
         ((char= c #\")
          (setf quoted t))
         ((char= c #\()
          (incf comment))
         ((char= c #\[)
          (setf literal t))
         (t
          (when (char= c #\>)
            (setf angle (max 0 (1- angle))))
          (funcall fn i c angle)
          (when (char= c #\<)
            (incf angle))))))))

(defun %split-address-list (string)
  "STRING, an RFC 5322 address-list, cut at the commas between addresses.
→ the addresses, blanks trimmed, empty ones dropped."
  (let ((cuts '())
        (group nil))
    (%map-top-level (lambda (i c angle)
                      (when (zerop angle)
                        (case c
                          (#\: (setf group t))
                          (#\; (setf group nil))
                          (#\, (unless group (push i cuts))))))
                    string)
    (loop for start = 0 then (1+ end)
          for end in (append (nreverse cuts) (list (length string)))
          for piece = (string-trim '(#\Space #\Tab) (subseq string start end))
          when (plusp (length piece))
            collect piece)))

(defun %atom-char-p (c)
  "RFC 5322 atext, RFC 6532 non-ASCII included."
  (let ((code (char-code c)))
    (and (> code 32) (/= code 127)
         (not (find c "()<>[]:;@\\,.\"")))))

(defun %address-tokens (address)
  "ADDRESS (one address) as tokens: (:atom . text), (:quoted . content with
quoted-pairs undone), (:literal . \"[...]\"), or one of the characters
< > @ . , : ;. Blanks and comments are dropped. Signals MAIL-ADDRESS-ERROR
on an unterminated quoted string, comment or literal, or a stray character."
  (let ((tokens '())
        (i 0)
        (n (length address)))
    (loop while (< i n)
          do (let ((c (char address i)))
               (cond
                ((member c '(#\Space #\Tab))
                 (incf i))
                ((char= c #\()
                 (let ((depth 1))
                   (incf i)
                   (loop while (and (< i n) (plusp depth))
                         do (case (char address i)
                              (#\\ (incf i))
                              (#\( (incf depth))
                              (#\) (decf depth)))
                            (incf i))
                   (when (plusp depth)
                     (%address-fail address "unterminated comment"))))
                ((char= c #\")
                 (let ((out (make-string-output-stream)))
                   (incf i)
                   (loop
                     (when (>= i n)
                       (%address-fail address "unterminated quoted string"))
                     (let ((d (char address i)))
                       (cond
                        ((char= d #\\)
                         (when (>= (1+ i) n)
                           (%address-fail address "unterminated quoted string"))
                         (write-char (char address (1+ i)) out)
                         (incf i 2))
                        ((char= d #\")
                         (incf i)
                         (return))
                        (t
                         (write-char d out)
                         (incf i)))))
                   (push (cons :quoted (get-output-stream-string out)) tokens)))
                ((char= c #\[)
                 (let ((end (position #\] address :start i)))
                   (unless end
                     (%address-fail address "unterminated domain literal"))
                   (when (find-if (lambda (d) (member d '(#\[ #\\ #\Space #\Tab)))
                                  address :start (1+ i) :end end)
                     (%address-fail address "a domain literal holds [, \\ or a blank"))
                   (push (cons :literal (subseq address i (1+ end))) tokens)
                   (setf i (1+ end))))
                ((find c "<>@.,:;")
                 (push c tokens)
                 (incf i))
                ((%atom-char-p c)
                 (let ((end (or (position-if-not #'%atom-char-p address :start i) n)))
                   (push (cons :atom (subseq address i end)) tokens)
                   (setf i end)))
                (t
                 (%address-fail address "stray character U+~4,'0X" (char-code c))))))
    (nreverse tokens)))

(defun %quoted-string (text)
  "TEXT as an RFC 5322 quoted-string."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across text
          do (when (member c '(#\\ #\"))
               (write-char #\\ o))
             (write-char c o))
    (write-char #\" o)))

(defun %dot-words (tokens address what &key (quoted t))
  "TOKENS, word *(\".\" word), as text: a word is an atom, or a quoted
string when QUOTED (quoted again on output). WHAT names the part for the
error."
  (when (null tokens)
    (%address-fail address "empty ~A" what))
  (with-output-to-string (o)
    (loop for (token . rest) on tokens
          for word-p = t then (not word-p)
          do (cond
              ((and word-p (consp token) (eq :atom (car token)))
               (write-string (cdr token) o))
              ((and word-p quoted (consp token) (eq :quoted (car token)))
               (write-string (%quoted-string (cdr token)) o))
              ((and (not word-p) (eql token #\.))
               (write-char #\. o)
               (unless rest
                 (%address-fail address "the ~A ends in a dot" what)))
              (t
               (%address-fail address "unexpected ~A in the ~A"
                              (if (consp token) (cdr token) token) what))))))

(defun %addr-spec-text (tokens address)
  "TOKENS of local-part [\"@\" domain] as addr-spec text. A local part
alone (\"postmaster\") is kept, as smtplib sends it."
  (let ((at (position #\@ tokens)))
    (if (null at)
        (%dot-words tokens address "local part")
        (let ((local (%dot-words (subseq tokens 0 at) address "local part"))
              (domain-tokens (subseq tokens (1+ at))))
          (concatenate 'string local "@"
                       (if (and (= 1 (length domain-tokens))
                                (consp (first domain-tokens))
                                (eq :literal (car (first domain-tokens))))
                           (cdr (first domain-tokens))
                           (%dot-words domain-tokens address "domain" :quoted nil)))))))

(defun %phrase-text (tokens address)
  "The display name of phrase TOKENS: atoms with their RFC 2047
encoded-words decoded, quoted strings as written, joined by spaces. NIL
for no tokens."
  (when tokens
    (let ((parts '())
          (atoms '()))
      (flet ((flush ()
               (when atoms
                 (push (mime-protocol:decode-header-text
                        (format nil "~{~A~^ ~}" (reverse atoms)))
                       parts)
                 (setf atoms '()))))
        (dolist (token tokens)
          (cond
           ((eql token #\.)
            (if atoms
                (setf (first atoms) (concatenate 'string (first atoms) "."))
                (push "." atoms)))
           ((and (consp token) (eq :atom (car token)))
            (push (cdr token) atoms))
           ((and (consp token) (eq :quoted (car token)))
            (flush)
            (push (cdr token) parts))
           (t
            (%address-fail address "unexpected ~A in the display name"
                           (if (consp token) (cdr token) token)))))
        (flush))
      (format nil "~{~A~^ ~}" (reverse parts)))))

(defun %parse-mailbox (address)
  "Mailbox ADDRESS: name-addr ([display-name] <addr-spec>, an obsolete
route allowed) or a bare addr-spec, comments anywhere.
→ (values addr-spec display-name)."
  (let* ((tokens (%address-tokens (%refuse-line-breaks address)))
         (lt (position #\< tokens)))
    (if (null lt)
        (values (%addr-spec-text tokens address) nil)
        (let ((gt (position #\> tokens :start lt)))
          (unless gt
            (%address-fail address "no > after <"))
          (when (nthcdr (1+ gt) tokens)
            (%address-fail address "text after >"))
          (let ((inner (subseq tokens (1+ lt) gt)))
            ;; obs-route: <@a.example,@b.example:user@c.example>
            (when (eql #\@ (first inner))
              (let ((colon (position #\: inner)))
                (unless colon
                  (%address-fail address "a route without a colon"))
                (setf inner (subseq inner (1+ colon)))))
            (values (%addr-spec-text inner address)
                    (%phrase-text (subseq tokens 0 lt) address)))))))

(defun %group-bounds (address)
  "→ (values colon semicolon) when ADDRESS is a group (display-name \":\"
[mailbox-list] \";\"), else NIL."
  (let ((colon nil)
        (semicolon nil))
    (%map-top-level (lambda (i c angle)
                      (when (zerop angle)
                        (cond
                         ((and (char= c #\:) (null colon)) (setf colon i))
                         ((and (char= c #\;) colon (null semicolon)) (setf semicolon i)))))
                    address)
    (when colon
      (values colon semicolon))))

(defun %address-mailboxes (address)
  "The mailboxes of one ADDRESS: itself, or a group's members. Signals
MAIL-ADDRESS-ERROR. → list of (addr-spec . display-name)."
  (multiple-value-bind (colon semicolon) (%group-bounds address)
    (if (null colon)
        (multiple-value-bind (spec name) (%parse-mailbox address)
          (list (cons spec name)))
        (progn
          (when (and semicolon (%address-tokens (subseq address (1+ semicolon))))
            (%address-fail address "text after the group's ;"))
          (loop for member in (%split-address-list
                               (subseq address (1+ colon) (or semicolon (length address))))
                collect (multiple-value-bind (spec name) (%parse-mailbox member)
                          (cons spec name)))))))

(defun %read-address ()
  (format *query-io* "~&Address: ")
  (force-output *query-io*)
  (list (read-line *query-io*)))

(defun address-mailboxes (address)
  "The mailboxes of ADDRESS, one address (a mailbox, or a group
\"Team: a@ex.com, b@ex.com;\"), as (addr-spec . display-name) pairs. The
display name has its RFC 2047 encoded-words decoded. Signals
MAIL-ADDRESS-ERROR when ADDRESS is not RFC 5322 address syntax or holds CR,
LF or NUL; restart USE-VALUE takes another address, which is read again."
  (loop
    (restart-case (return-from address-mailboxes (%address-mailboxes address))
      (use-value (new-address)
        :report "Use another address."
        :interactive %read-address
        (setf address new-address)))))

(defun parse-address (address)
  "The addr-spec and display name of mailbox ADDRESS, as smtplib's
send_message reads it (email.utils.getaddresses): \"Ann <ann@ex.com>\" →
\"ann@ex.com\" \"Ann\"; quoted display names, comments and an obsolete
route are read, encoded-words in the display name decoded. A group is
refused here (see ADDRESS-MAILBOXES). Signals MAIL-ADDRESS-ERROR (restart
USE-VALUE). → (values addr-spec display-name)."
  (let ((mailboxes (address-mailboxes address)))
    (unless (and (= 1 (length mailboxes)) (null (%group-bounds address)))
      (%address-fail address "a group, not one mailbox"))
    (values (car (first mailboxes)) (cdr (first mailboxes)))))

(defun %ascii-p (string)
  (every (lambda (c) (< (char-code c) 128)) string))

(defun %display-phrase (name)
  "NAME as a phrase: as it is when it is atoms and blanks, else quoted."
  (if (and (plusp (length name))
           (not (search "=?" name))
           (every (lambda (c) (or (char= c #\Space) (%atom-char-p c))) name))
      name
      (%quoted-string name)))

(defun %long-word-p (text)
  "Does TEXT hold a run of more than 76 non-blank characters, which no
folding can fit on a line?"
  (let ((run 0))
    (loop for c across text
            thereis (if (member c '(#\Space #\Tab))
                        (progn (setf run 0) nil)
                        (> (incf run) 76)))))

(defun %address-comments (address)
  "The comments of ADDRESS outside quoted strings and domain literals, each
as written between its outer parentheses, in order."
  (let ((comments '())
        (quoted nil)
        (literal nil)
        (escaped nil)
        (depth 0)
        (start 0))
    (dotimes (i (length address))
      (let ((c (char address i)))
        (cond
          (escaped (setf escaped nil))
          ((and (or quoted literal (plusp depth)) (char= c #\\)) (setf escaped t))
          (quoted (when (char= c #\") (setf quoted nil)))
          (literal (when (char= c #\]) (setf literal nil)))
          ((plusp depth)
           (case c
             (#\( (incf depth))
             (#\) (decf depth)
              (when (zerop depth)
                (push (subseq address (1+ start) i) comments)))))
          ((char= c #\") (setf quoted t))
          ((char= c #\[) (setf literal t))
          ((char= c #\() (setf depth 1 start i)))))
    (nreverse comments)))

(defun %needs-encoded-words-p (text)
  (or (not (%ascii-p text)) (%long-word-p text)))

(defun %wire-phrase (name)
  "Display name NAME for the wire: encoded-words when it needs them, else
a phrase (quoted where it needs it)."
  (if (%needs-encoded-words-p name)
      (mime-protocol:encode-encoded-words name)
      (%display-phrase name)))

(defun %wire-mailbox (address)
  "Mailbox ADDRESS for the wire: display-name <addr-spec> (comments), or
addr-spec (comments) without a display name, the display name and comments
as encoded-words where they need them (RFC 2047 5 (2), (3)). ADDRESS comes
back as written when neither does."
  (multiple-value-bind (spec name) (%parse-mailbox address)
    (let ((comments (%address-comments address)))
      (if (and (or (null name) (not (%needs-encoded-words-p name)))
               (notany #'%needs-encoded-words-p comments))
          address
          (format nil "~:[~*~A~;~A <~A>~]~{ (~A)~}"
                  (and name (plusp (length name)))
                  (and name (plusp (length name)) (%wire-phrase name))
                  spec
                  (mapcar (lambda (comment)
                            (if (%needs-encoded-words-p comment)
                                (mime-protocol:encode-encoded-words comment)
                                comment))
                          comments))))))

(defun %wire-group (address colon semicolon)
  "Group ADDRESS (its \":\" at COLON, \";\" at SEMICOLON) for the wire: the
group's display name and each member as %WIRE-ADDRESS writes them."
  (let ((name (%phrase-text (%address-tokens (subseq address 0 colon)) address))
        (members (%split-address-list (subseq address (1+ colon) (or semicolon (length address))))))
    (if (null name)
        address
        (format nil "~A: ~{~A~^, ~};" (%wire-phrase name) (mapcar #'%wire-address members)))))

(defun %wire-address (address)
  "ADDRESS for a header field, as Python's EmailMessage writes it: a display
name (of a mailbox or a group) or a comment that is not ASCII, or holds a
word too long to fold, becomes RFC 2047 encoded-words (RFC 2047 5 (2),
(3)); a group's members are written the same way. An ASCII address, and
one that does not parse, is kept as written."
  (if (and (%ascii-p address) (not (%long-word-p address)))
      address
      (handler-case
          (multiple-value-bind (colon semicolon) (%group-bounds address)
            (if colon
                (%wire-group address colon semicolon)
                (%wire-mailbox address)))
        (mail-address-error () address))))

(defun %display-address (address)
  "ADDRESS for a reader: encoded-words in a mailbox's or a group's display
name decoded (a group's members too), each name quoted again where it needs
it. Anything else as written."
  (if (not (search "=?" address))
      address
      (handler-case
          (multiple-value-bind (colon semicolon) (%group-bounds address)
            (if colon
                (let ((name (%phrase-text (%address-tokens (subseq address 0 colon)) address)))
                  (if (null name)
                      address
                      (format nil "~A: ~{~A~^, ~};"
                              (%display-phrase name)
                              (mapcar #'%display-address
                                      (%split-address-list
                                       (subseq address (1+ colon) (or semicolon (length address))))))))
                (multiple-value-bind (spec name) (%parse-mailbox address)
                  (if name
                      (format nil "~A <~A>" (%display-phrase name) spec)
                      address))))
        (mail-address-error () address))))
