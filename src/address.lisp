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

(defun %address-lexemes (address)
  "ADDRESS (one address) as lexemes, in order, each (kind start end value)
over ADDRESS[START,END): :BLANK (a run of SP / HTAB), :COMMENT (outer
parentheses included), :QUOTED (VALUE its content, quoted-pairs undone),
:LITERAL ([...]), :ATOM, or :SPECIAL (VALUE one of < > @ . , : ;).
Signals MAIL-ADDRESS-ERROR on an unterminated quoted string, comment or
literal, or a stray character."
  (let ((lexemes '())
        (i 0)
        (n (length address)))
    (flet ((lexeme (kind start end &optional value)
             (push (list kind start end value) lexemes)))
      (loop while (< i n)
            do (let ((c (char address i))
                     (start i))
                 (cond
                   ((member c '(#\Space #\Tab))
                    (setf i (or (position-if-not (lambda (d) (member d '(#\Space #\Tab)))
                                                 address :start i)
                                n))
                    (lexeme :blank start i))
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
                        (%address-fail address "unterminated comment"))
                      (lexeme :comment start i)))
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
                      (lexeme :quoted start i (get-output-stream-string out))))
                   ((char= c #\[)
                    (let ((end (position #\] address :start i)))
                      (unless end
                        (%address-fail address "unterminated domain literal"))
                      (when (find-if (lambda (d) (member d '(#\[ #\\ #\Space #\Tab)))
                                     address :start (1+ i) :end end)
                        (%address-fail address "a domain literal holds [, \\ or a blank"))
                      (setf i (1+ end))
                      (lexeme :literal start i)))
                   ((find c "<>@.,:;")
                    (incf i)
                    (lexeme :special start i c))
                   ((%atom-char-p c)
                    (setf i (or (position-if-not #'%atom-char-p address :start i) n))
                    (lexeme :atom start i))
                   (t
                    (%address-fail address "stray character U+~4,'0X" (char-code c)))))))
    (nreverse lexemes)))

(defun %address-tokens (address &key blanks)
  "ADDRESS (one address) as tokens: (:atom . text), (:quoted . content with
quoted-pairs undone), (:literal . \"[...]\"), or one of the characters
< > @ . , : ;. Blanks and comments are dropped; with BLANKS each run of
blanks is one :BLANK token, so a phrase keeps its spacing (\"J.R.R.
Tolkien\"), and each comment is a :COMMENT token, which separates words as a
blank does. Signals MAIL-ADDRESS-ERROR on an unterminated quoted string,
comment or literal, or a stray character."
  (let ((tokens '()))
    (loop for (kind start end value) in (%address-lexemes address)
          do (case kind
               (:blank
                (when (and blanks (not (eq :blank (first tokens))))
                  (push :blank tokens)))
               (:comment
                (when blanks (push :comment tokens)))
               (:quoted (push (cons :quoted value) tokens))
               (:literal (push (cons :literal (subseq address start end)) tokens))
               (:atom (push (cons :atom (subseq address start end)) tokens))
               (:special (push value tokens))))
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
  "The display name of phrase TOKENS (%ADDRESS-TOKENS with :BLANKS): a run
of atoms and dots as written, a blank inside it one space (\"J.R.R.
Tolkien\", \"Zoë . Smith\", as email.utils.getaddresses reads them), its RFC
2047 encoded-words decoded; a quoted string as written; the pieces joined
by spaces. A comment ends a run, as whitespace that separates encoded-words
(=?utf-8?q?a?= (c) =?utf-8?q?b?= reads as \"a b\", as Python's
headerregistry reads it). NIL when TOKENS hold no word."
  (let ((parts '())
        (run (make-string-output-stream))
        (in-run nil)
        (blank nil))
    (flet ((flush ()
             (when in-run
               (push (mime-protocol:decode-header-text (get-output-stream-string run)) parts)
               (setf in-run nil))))
      (dolist (token tokens)
        (cond
          ((eq token :blank)
           (setf blank t))
          ((eq token :comment)
           (flush)
           (setf blank t))
          ((or (eql token #\.) (and (consp token) (eq :atom (car token))))
           (when (and in-run blank)
             (write-char #\Space run))
           (write-string (if (consp token) (cdr token) ".") run)
           (setf in-run t blank nil))
          ((and (consp token) (eq :quoted (car token)))
           (flush)
           (push (cdr token) parts)
           (setf blank nil))
          (t
           (%address-fail address "unexpected ~A in the display name"
                          (if (consp token) (cdr token) token)))))
      (flush))
    (when parts
      (format nil "~{~A~^ ~}" (reverse parts)))))

(defun %parse-mailbox (address)
  "Mailbox ADDRESS: name-addr ([display-name] <addr-spec>, an obsolete
route allowed) or a bare addr-spec, comments anywhere. <> is the null path,
addr-spec \"\" (\"Mailer Daemon <>\", \"<> (bounce)\"), as getaddresses
reads it. → (values addr-spec display-name route), ROUTE the obsolete
route's tokens or NIL."
  (let* ((spaced (%address-tokens (%refuse-line-breaks address) :blanks t))
         (tokens (remove-if (lambda (token) (member token '(:blank :comment))) spaced))
         (lt (position #\< tokens)))
    (if (null lt)
        (values (%addr-spec-text tokens address) nil nil)
        (let ((gt (position #\> tokens :start lt))
              (route nil))
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
                (setf route (subseq inner 0 colon)
                      inner (subseq inner (1+ colon)))))
            (values (if (and (null inner) (null route))
                        ""
                        (%addr-spec-text inner address))
                    (%phrase-text (subseq spaced 0 (position #\< spaced)) address)
                    route))))))

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
  "The mailboxes of one ADDRESS: itself, or a group's members. A group
whose display name is not a phrase (\"Bob <bob@x>: eve@y;\") is refused:
getaddresses reads no mailbox there, so its members are no recipients.
Signals MAIL-ADDRESS-ERROR. → list of (addr-spec . display-name)."
  (multiple-value-bind (colon semicolon) (%group-bounds address)
    (if (null colon)
        (multiple-value-bind (spec name) (%parse-mailbox address)
          (list (cons spec name)))
        (progn
          (when (intersection '(#\< #\> #\@) (%address-tokens (subseq address 0 colon)))
            (%address-fail address "the group's display name is an address"))
          (%phrase-text (%address-tokens (subseq address 0 colon) :blanks t) address)
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

(defun %wire-ctext (text)
  "TEXT, one comment level's text with quoted-pairs undone, for the wire:
encoded-words when it needs them, the blanks at its ends kept outside; else
with ( ) and \\ escaped."
  (let* ((blank-p (lambda (c) (member c '(#\Space #\Tab))))
         (start (position-if-not blank-p text))
         (end (and start (1+ (position-if-not blank-p text :from-end t)))))
    (cond
      ((null start) text)
      ((%needs-encoded-words-p text)
       (concatenate 'string
                    (subseq text 0 start)
                    (mime-protocol:encode-encoded-words (subseq text start end))
                    (subseq text end)))
      (t
       (with-output-to-string (o)
         (loop for c across text
               do (when (find c "()\\")
                    (write-char #\\ o))
                  (write-char c o)))))))

(defun %wire-comment (comment)
  "COMMENT's text (between its outer parentheses, as written) for the wire.
As written when it needs no encoding. Otherwise each nesting level on its
own: its text, quoted-pairs undone, as encoded-words where it needs them
(blanks at its ends kept outside them), else with ( ) \\ escaped again; a
nested comment in its own parentheses. So (c\\(ö) decodes as c(ö, and
(Zoë (nested \\) ü) x) keeps its nesting (RFC 2047 5 (2))."
  (if (not (%needs-encoded-words-p comment))
      comment
      (with-output-to-string (o)
        (let ((text (make-string-output-stream))
              (i 0)
              (n (length comment)))
          (flet ((flush-text ()
                   (write-string (%wire-ctext (get-output-stream-string text)) o)))
            (loop while (< i n)
                  do (let ((c (char comment i)))
                       (cond
                         ((char= c #\\)
                          (when (< (1+ i) n)
                            (write-char (char comment (1+ i)) text))
                          (incf i 2))
                         ((char= c #\()
                          (flush-text)
                          (let ((depth 1)
                                (j (1+ i)))
                            (loop while (and (< j n) (plusp depth))
                                  do (case (char comment j)
                                       (#\\ (incf j))
                                       (#\( (incf depth))
                                       (#\) (decf depth)))
                                     (incf j))
                            (format o "(~A)" (%wire-comment (subseq comment (1+ i) (max (1+ i) (1- j)))))
                            (setf i j)))
                         (t
                          (write-char c text)
                          (incf i)))))
            (flush-text))))))

(defun %ends-encoded-p (text)
  "Does TEXT end in an encoded-word (?=), which wants a blank before a
following special?"
  (let ((n (length text)))
    (and (>= n 2) (string= "?=" text :start2 (- n 2)))))

(defun %lexeme-text (address lexeme)
  (subseq address (second lexeme) (third lexeme)))

(defun %wire-comment-lexeme (address lexeme)
  (format nil "(~A)" (%wire-comment (subseq address (1+ (second lexeme)) (1- (third lexeme))))))

(defun %wire-phrase-lexemes (address lexemes)
  "Phrase LEXEMES of ADDRESS for the wire, comments and blanks in place:
each run of words between comments (blanks at its ends left out) as
encoded-words when its display text needs them (%WIRE-PHRASE), else as
written; an encoded run gets a blank between it and a comment that touches
it."
  (let ((pieces '())
        (segment '()))
    (flet ((piece (text &optional encoded)
             (push (cons text encoded) pieces))
           (blank-p (l) (eq :blank (first l))))
      (flet ((flush ()
               (let* ((seg (nreverse segment))
                      (first (position-if-not #'blank-p seg))
                      (last (position-if-not #'blank-p seg :from-end t)))
                 (setf segment '())
                 (if (null first)
                     (dolist (l seg) (piece (%lexeme-text address l)))
                     (let* ((core (subseq address (second (nth first seg)) (third (nth last seg))))
                            (display (%phrase-text (%address-tokens core :blanks t) address)))
                       (dolist (l (subseq seg 0 first)) (piece (%lexeme-text address l)))
                       (if (and display (%needs-encoded-words-p display))
                           (piece (%wire-phrase display) t)
                           (piece core))
                       (dolist (l (subseq seg (1+ last))) (piece (%lexeme-text address l))))))))
        (dolist (l lexemes)
          (if (eq :comment (first l))
              (progn (flush) (piece (%wire-comment-lexeme address l)))
              (push l segment)))
        (flush)))
    (with-output-to-string (o)
      (loop for (piece . more) on (nreverse pieces)
            for previous = nil then last
            for last = piece
            do (when (and previous
                          (or (cdr previous) (cdr piece))
                          (not (find (char (car previous) (1- (length (car previous))))
                                     '(#\Space #\Tab)))
                          (not (find (char (car piece) 0) '(#\Space #\Tab))))
                 (write-char #\Space o))
               (write-string (car piece) o)))))

(defun %wire-rest-lexemes (address lexemes drop-route)
  "LEXEMES of ADDRESS after its display name (<addr-spec>, or a bare
addr-spec) for the wire: as written, each comment as %WIRE-COMMENT writes
it. DROP-ROUTE: leave out an obsolete route (<@a,@b:...>) up to its colon."
  (let* ((skip-from (and drop-route
                         (position-if (lambda (l) (and (eq :special (first l)) (eql #\@ (fourth l))))
                                      lexemes)))
         (skip-to (and skip-from
                       (position-if (lambda (l) (and (eq :special (first l)) (eql #\: (fourth l))))
                                    lexemes :start skip-from))))
    (with-output-to-string (o)
      (loop for l in lexemes
            for i from 0
            unless (and skip-to (<= skip-from i skip-to))
              do (write-string (if (eq :comment (first l))
                                   (%wire-comment-lexeme address l)
                                   (%lexeme-text address l))
                               o)))))

(defun %wire-mailbox (address)
  "Mailbox ADDRESS for the wire, with its comments and blanks where they
were (as Python's EmailMessage writes it): each run of display-name words
between comments as encoded-words when it needs them, each comment as
%WIRE-COMMENT writes it, the rest as written. A non-ASCII obsolete route is
dropped: it has no encoded form, and a reader ignores it (RFC 5322 4.4).
ADDRESS comes back as written when nothing needs encoding."
  (multiple-value-bind (spec name route) (%parse-mailbox address)
    (declare (ignore spec))
    (let ((comments (%address-comments address))
          (ascii-route (every (lambda (token)
                                (or (characterp token) (%ascii-p (cdr token))))
                              route)))
      (if (and (or (null name) (not (%needs-encoded-words-p name)))
               (notany #'%needs-encoded-words-p comments)
               ascii-route)
          address
          (let* ((lexemes (%address-lexemes address))
                 (lt (position-if (lambda (l) (and (eq :special (first l)) (eql #\< (fourth l))))
                                  lexemes))
                 (phrase (if lt (%wire-phrase-lexemes address (subseq lexemes 0 lt)) "")))
            (concatenate 'string
                         phrase
                         (if (%ends-encoded-p phrase) " " "")
                         (%wire-rest-lexemes address (if lt (subseq lexemes lt) lexemes)
                                             (not ascii-route))))))))

(defun %wire-group (address colon semicolon)
  "Group ADDRESS (its \":\" at COLON, \";\" at SEMICOLON) for the wire: the
group's display name (empty allowed) with its comments in place, as
%WIRE-PHRASE-LEXEMES writes a phrase, each member as %WIRE-ADDRESS writes
it, then the comments after the \";\" where they were. An encoded-word name
gets a blank before the \":\" (RFC 2047 5 (3)).
Signals MAIL-ADDRESS-ERROR for a group ADDRESS-MAILBOXES refuses, such as
one with an address after its \";\": writing only the group would drop that
address from To and from the envelope."
  (%address-mailboxes address)
  (let* ((head (string-right-trim '(#\Space #\Tab)
                                  (%wire-phrase-lexemes address
                                                        (%address-lexemes (subseq address 0 colon)))))
         (members (%split-address-list
                   (subseq address (1+ colon) (or semicolon (length address)))))
         (tail (if semicolon
                   (let ((after (subseq address (1+ semicolon))))
                     (%wire-rest-lexemes after (%address-lexemes after) nil))
                   "")))
    (format nil "~A~:[~; ~]: ~{~A~^, ~};~A"
            head
            (%ends-encoded-p head)
            (mapcar #'%wire-address members)
            (string-right-trim '(#\Space #\Tab) tail))))

(defun %wire-address (address)
  "ADDRESS for a header field, as Python's EmailMessage writes it: a display
name (of a mailbox or a group) or a comment that is not ASCII, or holds a
word too long to fold, becomes RFC 2047 encoded-words (RFC 2047 5 (2),
(3)), comments and blanks kept in place; a group's members are written the
same way, a non-ASCII obsolete route dropped. An address that needs none of
this is kept as written. One rule for every address, ASCII or not: one that
does not parse signals MAIL-ADDRESS-ERROR, so a message is never made with
a recipient no reader can find (Python's getaddresses reads ('', '') for
\"Zoe Smith\" or \"Zoe <z@x.example>;\")."
  (multiple-value-bind (colon semicolon) (%group-bounds address)
    (cond
      ((and (%ascii-p address) (not (%long-word-p address)))
       (%address-mailboxes address)
       address)
      (colon (%wire-group address colon semicolon))
      (t (%wire-mailbox address)))))

(defun %display-address (address)
  "ADDRESS for a reader: encoded-words in a mailbox's or a group's display
name decoded (a group's members too, also under an empty group name), each
name quoted again where it needs it. Anything else as written."
  (if (not (search "=?" address))
      address
      (handler-case
          (multiple-value-bind (colon semicolon) (%group-bounds address)
            (if colon
                (let* ((head (subseq address 0 colon))
                       (name (%phrase-text (%address-tokens head :blanks t) address)))
                  (if (and (null name) (%address-comments head))
                      address
                      (format nil "~@[~A~]: ~{~A~^, ~};"
                              (and name (plusp (length name)) (%display-phrase name))
                              (mapcar #'%display-address
                                      (%split-address-list
                                       (subseq address (1+ colon) (or semicolon (length address))))))))
                (multiple-value-bind (spec name) (%parse-mailbox address)
                  (if name
                      (format nil "~A <~A>" (%display-phrase name) spec)
                      address))))
        (mail-address-error () address))))
