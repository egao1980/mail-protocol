(in-package #:mail-protocol/tests)

;;; Unit tests for #3 (no server): Date / Message-ID options, nesting of
;;; alternatives and attachments in either order, the parsed view
;;; (message-body / message-attachments), and the SMTP dialogue for AUTH
;;; and STARTTLS on scripted streams.

(defun %instant (http-date)
  (datetime-protocol:parse-http-date http-date))

(deftest format-message-date-is-rfc-5322-utc
  (ok (equal "Mon, 23 Nov 2026 21:04:05 +0000"
             (format-message-date (%instant "Mon, 23 Nov 2026 21:04:05 GMT"))))
  (ok (equal "Sun, 06 Nov 1994 08:49:37 +0000"
             (format-message-date (%instant "Sun, 06 Nov 1994 08:49:37 GMT")))))

(deftest make-message-date-and-id-options
  (let ((m (datetime-protocol:with-fixed-clock ((%instant "Tue, 01 Dec 2026 00:00:00 GMT"))
             (make-message :from "Alice <alice@ex.com>" :to "b@ex.com" :body "x"))))
    (ok (equal "Tue, 01 Dec 2026 00:00:00 +0000" (message-date m)) "Date from the clock")
    (ok (let ((id (message-id m)))
          (and (char= #\< (char id 0))
               (search "@ex.com>" id)))
        (format nil "Message-ID uses the From domain: ~A" (message-id m))))
  (let ((m (make-message :from "a@ex.com" :to "b@ex.com" :date nil :message-id nil)))
    (ok (null (message-date m)) ":date nil → no Date")
    (ok (null (message-id m)) ":message-id nil → no Message-ID")
    (ok (not (search "Date:" (print-message m)))))
  (let ((m (make-message :from "a@ex.com" :to "b@ex.com"
                         :date "Wed, 02 Dec 2026 10:00:00 +0100"
                         :message-id "<fixed@ex.com>")))
    (ok (equal "Wed, 02 Dec 2026 10:00:00 +0100" (message-date m)))
    (ok (equal "<fixed@ex.com>" (message-id m)))))

(deftest message-ids-are-unique
  (let ((ids (loop repeat 2000 collect (make-message-id :domain "ex.com"))))
    (ok (= 2000 (length (remove-duplicates ids :test #'string=))))
    (ok (every (lambda (id) (and (char= #\< (char id 0)) (search "@ex.com>" id))) ids))
    (ok (search "@localhost>" (make-message-id)) "default domain")))

(defun %top-type (msg)
  (let ((mt (mime-protocol:mime-content-type (message-entity msg))))
    (format nil "~(~A/~A~)" (mime-protocol:media-type-type mt)
            (mime-protocol:media-type-subtype mt))))

(defun %part-types (entity)
  (mapcar (lambda (p)
            (let ((mt (mime-protocol:mime-content-type p)))
              (format nil "~(~A/~A~)" (mime-protocol:media-type-type mt)
                      (mime-protocol:media-type-subtype mt))))
          (mime-protocol:mime-parts entity)))

(deftest attachment-then-alternative-nests-the-same
  (let ((m (make-message :from "a@ex.com" :to "b@ex.com" :subject "S" :body "plain")))
    (add-attachment m (coerce #(1 2 3) '(vector (unsigned-byte 8))) :filename "r.pdf")
    (add-alternative m "<p>html</p>")
    (let ((e (mime-protocol:parse-mime (print-message m))))
      (ok (equal '("multipart/alternative" "application/pdf") (%part-types e))
          "r.pdf gets application/pdf from its extension")
      (ok (equal '("text/plain" "text/html")
                 (%part-types (first (mime-protocol:mime-parts e))))))))

(deftest message-headers-move-to-the-top
  (let ((m (make-message :from "a@ex.com" :to "b@ex.com" :subject "S" :body "plain")))
    (add-alternative m "<p>html</p>")
    (let* ((top (message-entity m))
           (body (first (mime-protocol:mime-parts top))))
      (ok (equal "S" (message-subject m)) "Subject still reads from the message")
      (ok (equal "a@ex.com" (message-from m)))
      (ok (null (mime-protocol:header-value body "subject")) "not on the inner part")
      (ok (null (mime-protocol:header-value body "mime-version")))
      (ok (equal "1.0" (mime-protocol:header-value top "mime-version"))))))

(deftest parsed-multipart-view
  (let ((m (make-message :from "a@ex.com" :to "b@ex.com" :subject "S"
                         :body (format nil "plain~C~C" #\Return #\Newline))))
    (add-alternative m "<p>html</p>")
    (add-attachment m "a,b" :content-type "text/csv" :filename "t.csv")
    (add-attachment m (coerce #(0 255) '(vector (unsigned-byte 8))) :filename "x.bin")
    (let ((p (parse-message (print-message m))))
      (ok (equal (format nil "plain~C~C" #\Return #\Newline) (message-body p))
          "message-body finds text/plain, CRLF intact")
      (ok (equal "<p>html</p>" (message-body p :subtype "html")))
      (ok (equal '("t.csv" "x.bin")
                 (mapcar (lambda (e) (mime-protocol:disposition-filename
                                      (mime-protocol:mime-content-disposition e)))
                         (message-attachments p))))
      (ok (equal "a,b" (encoding-protocol:decode
                        (mime-protocol:mime-content (first (message-attachments p)))))
          "a string attachment is text")
      (ok (equalp #(0 255) (mime-protocol:mime-content (second (message-attachments p))))))))

(deftest add-attachment-rejects-other-data
  (ok (signals (add-attachment (make-message :from "a@ex.com") '(1 2 3)) 'type-error)))

;;; --- SMTP dialogue on scripted streams -------------------------------------

(defun %lines (&rest lines)
  (format nil "~{~A~C~C~}"
          (loop for l in lines append (list l #\Return #\Newline))))

(defun %dialogue (replies &rest keys)
  "Run SMTP-DIALOGUE against REPLIES. → (values result client-output)."
  (let ((out (make-string-output-stream)))
    (with-input-from-string (in replies)
      (let ((result (apply #'mail-backend-smtp:smtp-dialogue in out
                           :from "a@ex.com" :recipients '("b@ex.com") :data "x"
                           keys)))
        (values result (get-output-stream-string out))))))

(defparameter +after-auth+ '("250 ok" "250 ok" "354 go" "250 queued" "221 bye"))

(deftest smtp-auth-plain-wire
  (multiple-value-bind (ok wire)
      (apply #'%dialogue
             (list (apply #'%lines "220 hi" "250-mx" "250-AUTH LOGIN PLAIN" "250 SIZE 1000"
                          "235 ok" +after-auth+)
                   :username "alice" :password "s3cret"))
    (ok ok)
    ;; base64("\0alice\0s3cret")
    (ok (search "AUTH PLAIN AGFsaWNlAHMzY3JldA==" wire) "PLAIN preferred over LOGIN")
    (ok (< (search "AUTH" wire) (search "MAIL FROM" wire)))))

(deftest smtp-auth-login-wire
  (multiple-value-bind (ok wire)
      (apply #'%dialogue
             (list (apply #'%lines "220 hi" "250-mx" "250 AUTH LOGIN"
                          "334 VXNlcm5hbWU6" "334 UGFzc3dvcmQ6" "235 ok" +after-auth+)
                   :username "alice" :password "s3cret"))
    (ok ok)
    (ok (search (%lines "AUTH LOGIN" "YWxpY2U=" "czNjcmV0") wire))))

(deftest smtp-auth-refused-is-mail-auth-error
  (let ((e (handler-case
               (%dialogue (%lines "220 hi" "250-mx" "250 AUTH PLAIN"
                                  "535 5.7.8 Authentication credentials invalid")
                          :username "alice" :password "wrong")
             (mail-auth-error (e) e))))
    (ok (typep e 'mail-auth-error))
    (ok (and e (eql 535 (mail-send-error-code e))) "the reply code is kept")))

(deftest smtp-auth-not-offered
  (ok (signals (%dialogue (%lines "220 hi" "250 mx") :username "alice" :password "pw")
               'mail-auth-error)
      "no AUTH extension: refuse rather than send unauthenticated"))

(deftest smtp-starttls-not-offered
  (ok (signals (%dialogue (%lines "220 hi" "250-mx" "250 AUTH PLAIN")
                          :starttls (lambda () (error "must not upgrade")))
               'mail-send-error)
      "a requested STARTTLS the server lacks is an error, not a plain send"))

(deftest smtp-starttls-upgrades-and-says-ehlo-again
  (let* ((out1 (make-string-output-stream))
         (out2 (make-string-output-stream))
         (upgraded nil))
    (with-input-from-string (in1 (%lines "220 hi" "250-mx" "250-STARTTLS" "250 SIZE 10"
                                         "220 go ahead"))
      (with-input-from-string (in2 (apply #'%lines "250-mx" "250 AUTH PLAIN" "235 ok"
                                          +after-auth+))
        (ok (mail-backend-smtp:smtp-dialogue
             in1 out1 :from "a@ex.com" :recipients '("b@ex.com") :data "x"
                      :username "u" :password "p"
                      :starttls (lambda () (setf upgraded t) (values in2 out2))))))
    (let ((plain (get-output-stream-string out1))
          (tls (get-output-stream-string out2)))
      (ok upgraded)
      (ok (equal (%lines "EHLO localhost" "STARTTLS") plain)
          "only EHLO and STARTTLS go out in the clear")
      (ok (eql 0 (search "EHLO localhost" tls)) "EHLO again over TLS")
      (ok (search "AUTH PLAIN" tls) "AUTH after the upgrade"))))

(deftest smtp-multiline-reply-codes
  (ok (signals (%dialogue (%lines "220 hi" "250-mx" "250 ok" "550-no" "550 such user"))
               'mail-send-error))
  (let ((e (handler-case (%dialogue (%lines "220 hi" "250 mx" "550 5.1.1 no"))
             (mail-send-error (e) e))))
    (ok (and e (eql 550 (mail-send-error-code e))))))

(deftest smtp-connect-refused-is-mail-send-error
  ;; egao1980/mail-protocol#4, second part.
  (let ((e (handler-case
               (progn (backend-send (mail-backend-smtp:make-smtp-backend :port 1)
                                    (make-message :from "a@ex.com" :to "b@ex.com"))
                      nil)
             (mail-send-error (e) e))))
    (ok (typep e 'mail-send-error) "a refused connection is a mail-send-error")
    (ok (and e (typep (mail-send-error-cause e) 'usocket:socket-error))
        "with the usocket condition as cause")))

(deftest smtp-backend-defaults
  (ok (= 465 (mail-backend-smtp::smtp-port (mail-backend-smtp:make-smtp-backend :tls t))))
  (ok (= 25 (mail-backend-smtp::smtp-port (mail-backend-smtp:make-smtp-backend))))
  (ok (= 587 (mail-backend-smtp::smtp-port
              (mail-backend-smtp:make-smtp-backend :port 587 :starttls t))))
  (ok (signals (mail-backend-smtp:make-smtp-backend :tls t :starttls t) 'mail-send-error)))
