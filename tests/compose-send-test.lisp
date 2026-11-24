(in-package #:mail-protocol/tests)

;;; Evidence for egao1980/mail-protocol#3 (smtplib + email.message jobs).
;;;
;;; Spec:
;;;  1. print-message writes Date (RFC 5322 date-time) and Message-ID
;;;     (<id@host>, unique per message);
;;;  2. header names come out in canonical case (From, Subject, MIME-Version);
;;;  3. add-alternative / add-attachment (Python EmailMessage's
;;;     add_alternative / add_attachment) build multipart/alternative and
;;;     multipart/mixed through mime-protocol;
;;;  4. make-smtp-backend takes :username :password (AUTH), :starttls t
;;;     (port 587 style) and :tls t (implicit TLS, port 465 style).
;;;
;;; 1-3 need no server. 4 uses Mailpit and skips without it:
;;;   docker run -d -p 127.0.0.1:11026:1025 -v $PWD/certs:/certs:ro axllent/mailpit \
;;;     --smtp-auth-file /certs/auth.txt --smtp-auth-allow-insecure   # auth.txt: evidence:s3cret
;;;   docker run -d -p 127.0.0.1:11027:1025 -v $PWD/certs:/certs:ro axllent/mailpit \
;;;     --smtp-tls-cert /certs/cert.pem --smtp-tls-key /certs/key.pem --smtp-require-starttls
;;;   docker run -d -p 127.0.0.1:11028:1025 -v $PWD/certs:/certs:ro axllent/mailpit \
;;;     --smtp-tls-cert /certs/cert.pem --smtp-tls-key /certs/key.pem --smtp-require-tls
;;; cert.pem: CN=127.0.0.1, signed by a CA whose PEM is in MAIL_SMTP_TEST_CA
;;; (TLS tests skip without it; the CA is added to cl+ssl's global context).
;;; Ports: MAIL_SMTP_TEST_AUTH_PORT / _STARTTLS_PORT / _TLS_PORT.

(defun %env (name default)
  (let ((v (uiop:getenv name)))
    (if (and v (plusp (length v))) v default)))

(defun %mail-fn (name)
  "The function named by NAME exported from mail-protocol, or a failure."
  (multiple-value-bind (sym status) (find-symbol name "MAIL-PROTOCOL")
    (if (and (eq status :external) (fboundp sym))
        (fdefinition sym)
        (error "mail-protocol does not export ~A" name))))

(defun %sample ()
  (make-message :from "a@ex.com" :to "b@ex.com" :subject "Hi" :body "hello"))

(defun %header-block (wire)
  (subseq wire 0 (or (search (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline)
                             wire)
                     (search (format nil "~%~%") wire)
                     (length wire))))

(defun %header-names (wire)
  "Header field names as printed (case kept), top level only."
  (loop for line in (uiop:split-string (%header-block wire) :separator '(#\Newline))
        for l = (string-right-trim '(#\Return) line)
        for colon = (position #\: l)
        when (and colon (plusp (length l))
                  (not (member (char l 0) '(#\Space #\Tab))))
          collect (subseq l 0 colon)))

(defun %header (wire name)
  (loop for line in (uiop:split-string (%header-block wire) :separator '(#\Newline))
        for l = (string-right-trim '(#\Return) line)
        for colon = (position #\: l)
        when (and colon (string-equal name (subseq l 0 colon)))
          return (string-trim " " (subseq l (1+ colon)))))

(defun %rfc5322-date-p (s)
  ;; "Mon, 23 Nov 2026 21:04:05 +0000"
  (let ((parts (and s (uiop:split-string s :separator '(#\Space)))))
    (and (= 6 (length parts))
         (member (first parts) '("Mon," "Tue," "Wed," "Thu," "Fri," "Sat," "Sun,")
                 :test #'string=)
         (parse-integer (second parts) :junk-allowed t)
         (member (third parts) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                 :test #'string=)
         (= 4 (length (fourth parts)))
         (= 8 (length (fifth parts)))
         (= 5 (length (sixth parts)))
         (find (char (sixth parts) 0) "+-")
         t)))

(defun %canonical (name)
  (cond ((string-equal name "mime-version") "MIME-Version")
        ((string-equal name "message-id") "Message-ID")
        (t (format nil "~{~:(~A~)~^-~}" (uiop:split-string name :separator '(#\-))))))

(deftest printed-message-has-date-and-message-id
  (let* ((wire (print-message (%sample)))
         (date (%header wire "Date"))
         (id (%header wire "Message-ID")))
    (ok (%rfc5322-date-p date) (format nil "Date is an RFC 5322 date-time (got ~S)" date))
    (ok (and id (> (length id) 4) (char= #\< (char id 0))
             (char= #\> (char id (1- (length id)))) (find #\@ id))
        (format nil "Message-ID is <left@right> (got ~S)" id))
    (ok (not (equal id (%header (print-message (%sample)) "Message-ID")))
        "two messages get different Message-IDs")))

(deftest header-names-are-canonical
  (let ((names (%header-names (print-message (%sample)))))
    (ok (member "Subject" names :test #'string=)
        (format nil "Subject: is printed as Subject (names ~S)" names))
    (ok (every (lambda (n) (string= n (%canonical n))) names)
        (format nil "every header name is canonical (names ~S)" names))))

(defun %type (entity)
  (let ((mt (mime-protocol:mime-content-type entity)))
    (string-downcase (format nil "~A/~A" (mime-protocol:media-type-type mt)
                             (mime-protocol:media-type-subtype mt)))))

(defun %text (entity)
  (let ((c (mime-protocol:mime-content entity)))
    (string-right-trim '(#\Return #\Newline)
                       (if (stringp c) c (encoding-protocol:decode c)))))

(deftest add-alternative-builds-multipart-alternative
  (let ((msg (%sample)))
    (funcall (%mail-fn "ADD-ALTERNATIVE") msg "<p>hello</p>" :subtype "html")
    (let* ((e (mime-protocol:parse-mime (print-message msg)))
           (parts (mime-protocol:mime-parts e)))
      (ok (equal "multipart/alternative" (%type e)) "top level is multipart/alternative")
      (ok (equal '("text/plain" "text/html") (mapcar #'%type parts)) "text then html")
      (ok (equal '("hello" "<p>hello</p>") (mapcar #'%text parts)) "both bodies kept"))))

(deftest add-attachment-builds-multipart-mixed
  (let ((msg (%sample))
        (data (coerce #(0 1 2 13 10 255) '(vector (unsigned-byte 8)))))
    (funcall (%mail-fn "ADD-ATTACHMENT") msg data
             :content-type "application/octet-stream" :filename "data.bin")
    (let* ((e (mime-protocol:parse-mime (print-message msg)))
           (parts (mime-protocol:mime-parts e))
           (att (second parts))
           (disp (and att (mime-protocol:mime-content-disposition att))))
      (ok (equal "multipart/mixed" (%type e)) "top level is multipart/mixed")
      (ok (equal '("text/plain" "application/octet-stream") (mapcar #'%type parts))
          "body then attachment")
      (ok (and disp (string-equal "attachment" (mime-protocol:content-disposition-type disp))
               (equal "data.bin" (mime-protocol:disposition-filename disp)))
          "Content-Disposition: attachment; filename=data.bin")
      (ok (and att (equalp data (mime-protocol:mime-content att)))
          "binary content round-trips"))))

(deftest alternative-plus-attachment-nests
  (let ((msg (%sample)))
    (funcall (%mail-fn "ADD-ALTERNATIVE") msg "<p>hello</p>" :subtype "html")
    (funcall (%mail-fn "ADD-ATTACHMENT") msg (coerce #(1 2 3) '(vector (unsigned-byte 8)))
             :content-type "application/pdf" :filename "r.pdf")
    (let* ((e (mime-protocol:parse-mime (print-message msg)))
           (parts (mime-protocol:mime-parts e)))
      (ok (equal "multipart/mixed" (%type e)) "mixed at the top")
      (ok (equal '("multipart/alternative" "application/pdf") (mapcar #'%type parts))
          "alternative(text, html) then the attachment"))))

;;; --- SMTP AUTH / STARTTLS / implicit TLS (Mailpit) ---

(defun %smtp-host () (%env "MAIL_SMTP_TEST_HOST" "127.0.0.1"))

(defun %port-open-p (port)
  (let ((s (handler-case (usocket:socket-connect (%smtp-host) port :timeout 2)
             (error () nil))))
    (when s (usocket:socket-close s) t)))

(defun %smtp-backend (&rest args)
  "MAKE-SMTP-BACKEND through APPLY: an unknown keyword fails at run time."
  (apply #'mail-backend-smtp:make-smtp-backend args))

(defvar *ca-loaded* nil)

(defun %trust-test-ca ()
  (let ((ca (uiop:getenv "MAIL_SMTP_TEST_CA")))
    (when (and ca (plusp (length ca)) (probe-file ca))
      (unless *ca-loaded*
        (cl+ssl:ssl-load-global-verify-locations ca)
        (setf *ca-loaded* t))
      t)))

(defmacro with-mailpit ((port-var env default &key tls) &body body)
  `(let ((,port-var (parse-integer (%env ,env ,default))))
     (cond ((not (%port-open-p ,port-var))
            (skip (format nil "no Mailpit at ~A:~D (see tests/compose-send-test.lisp)"
                          (%smtp-host) ,port-var)))
           ((and ,tls (not (%trust-test-ca)))
            (skip "MAIL_SMTP_TEST_CA is not set (see tests/compose-send-test.lisp)"))
           (t ,@body))))

(defun %to-send (tag)
  (make-message :from "a@ex.com" :to "b@ex.com"
                :subject (format nil "evidence ~A" tag) :body "hello"))

(deftest smtp-auth-plain
  (with-mailpit (port "MAIL_SMTP_TEST_AUTH_PORT" "11026")
    (ok (signals (backend-send (mail-backend-smtp:make-smtp-backend
                                :host (%smtp-host) :port port)
                               (%to-send "no-auth"))
                 'mail-send-error)
        "the server really requires AUTH")
    (let ((b (%smtp-backend :host (%smtp-host) :port port
                            :username (%env "MAIL_SMTP_TEST_USER" "evidence")
                            :password (%env "MAIL_SMTP_TEST_PASSWORD" "s3cret"))))
      (ok (backend-send b (%to-send "auth")) "send with :username / :password"))))

(deftest smtp-starttls
  (with-mailpit (port "MAIL_SMTP_TEST_STARTTLS_PORT" "11027" :tls t)
    (ok (signals (backend-send (mail-backend-smtp:make-smtp-backend
                                :host (%smtp-host) :port port)
                               (%to-send "plain"))
                 'mail-send-error)
        "the server really requires STARTTLS")
    (ok (backend-send (%smtp-backend :host (%smtp-host) :port port :starttls t)
                      (%to-send "starttls"))
        "send with :starttls t")))

(deftest smtp-implicit-tls
  (with-mailpit (port "MAIL_SMTP_TEST_TLS_PORT" "11028" :tls t)
    (ok (backend-send (%smtp-backend :host (%smtp-host) :port port :tls t)
                      (%to-send "tls"))
        "send with :tls t (implicit TLS)")))
