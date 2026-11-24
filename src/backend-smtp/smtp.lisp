(in-package #:mail-backend-smtp)

(defun %crlf (out)
  (write-char #\Return out)
  (write-char #\Newline out))

(defun %write-line (out line)
  (write-string line out)
  (%crlf out)
  (finish-output out))

(defun %read-reply (in)
  "Read one SMTP reply, continuation lines included.
→ (values code text lines): TEXT is the last line's text, LINES every line's."
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
  (multiple-value-bind (code text lines) (%read-reply in)
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
  "AUTH with the first mechanism of PREFERENCE the server offers."
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

(defun smtp-dialogue (in out &key from recipients data (ehlo-host "localhost")
                         username password starttls
                         (auth-mechanisms '("PLAIN" "LOGIN")))
  "Drive one SMTP transaction on character streams IN / OUT: greeting,
EHLO, then STARTTLS when STARTTLS is a function (it is called after the
server's 220 and returns the new IN and OUT, over TLS; EHLO is sent again),
then AUTH when USERNAME is given (the first of AUTH-MECHANISMS the server
offers: PLAIN, LOGIN), then MAIL / RCPT / DATA / QUIT. A server that does
not offer a requested STARTTLS or AUTH signals MAIL-SEND-ERROR /
MAIL-AUTH-ERROR rather than sending in the clear. → T."
  (unless (and from recipients data)
    (error 'mail-send-error :message "smtp-dialogue needs :from :recipients :data"))
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
      (%authenticate in out extensions username password auth-mechanisms)))
  (%write-line out (format nil "MAIL FROM:<~A>" from))
  (%expect in 250 :what "MAIL FROM")
  (dolist (rcpt recipients)
    (%write-line out (format nil "RCPT TO:<~A>" rcpt))
    (%expect in '(250 251) :what "RCPT TO"))
  (%write-line out "DATA")
  (%expect in 354 :what "DATA")
  (write-string (dot-stuff data) out)
  (finish-output out)
  (%expect in 250 :what "message")
  (%write-line out "QUIT")
  (%expect in 221 :what "QUIT")
  t)
