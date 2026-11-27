(in-package #:mail-protocol/tests)

;;; egao1980/mail-protocol#13: leftovers of the 0.3.1 adversarial check.
;;; Python 3.13.18 (email.headerregistry, EmailMessage, checked 2026-11-27):
;;; - "=?utf-8?q?a?= (c) =?utf-8?q?b?= <z@x.example>" reads as 'a b': a
;;;   comment is whitespace that separates the encoded-words;
;;; - EmailMessage keeps comments where they were: "Zoë (c) Smith <a@x>"
;;;   → "=?utf-8?q?Zo=C3=AB?= (c) Smith <a@x>", "Zoë <a(c)@x>" keeps
;;;   "<a(c)@x>", "Team (t): Zoë <a@x>; (after)" keeps both comments;
;;; - a non-ASCII comment decodes without its quoted-pairs ("c(ö"). Python
;;;   drops the comment's parentheses; this client keeps them and encodes
;;;   each nesting level on its own (RFC 2047 5 (2)).

(defun %enc (text)
  (mime-protocol:encode-encoded-words text))

(defun %to-wire (to)
  "The To field PRINT-MESSAGE writes for TO."
  (%header (print-message (make-message :from "s@x.example" :to to :subject "s" :body "b"))
           "To"))

(deftest-parametrize comment-separates-encoded-words
    ((address name)
     ("=?utf-8?q?a?= (c) =?utf-8?q?b?= <z@x.example>" "a b")
     ("=?utf-8?q?a?=(c)=?utf-8?q?b?= <z@x.example>" "a b")
     ("=?utf-8?q?a?= =?utf-8?q?b?= <z@x.example>" "ab")
     ("J.R.R. (the) Tolkien <j@x.example>" "J.R.R. Tolkien"))
  (ok (equal name (nth-value 1 (parse-address address)))
      (format nil "~A reads as ~S" address name)))

(deftest-parametrize comments-are-encoded-level-by-level
    ((label to wire)
     ("quoted-pair undone" (%u "Zo~C <a@x.example> (c\\(~C)" #xEB #xF6)
      (format nil "~A <a@x.example> (~A)" (%enc (%u "Zo~C" #xEB)) (%enc (%u "c(~C" #xF6))))
     ("nested levels" (%u "a@x.example (Zo~C (nested \\) ~C) x)" #xEB #xFC)
      (format nil "a@x.example (~A (~A) x)" (%enc (%u "Zo~C" #xEB)) (%enc (%u "nested ) ~C" #xFC))))
     ("ASCII nesting kept" (%u "Zo~C <a@x.example> (a (b) c)" #xEB)
      (format nil "~A <a@x.example> (a (b) c)" (%enc (%u "Zo~C" #xEB)))))
  (ok (equal wire (%to-wire to)) (format nil "~A: ~A" label wire)))

(deftest-parametrize comments-stay-in-place
    ((label to wire)
     ("in the phrase" (%u "Zo~C (c) Smith <a@x.example>" #xEB)
      (format nil "~A (c) Smith <a@x.example>" (%enc (%u "Zo~C" #xEB))))
     ("before <" (%u "Zo~C (c) <a@x.example>" #xEB)
      (format nil "~A (c) <a@x.example>" (%enc (%u "Zo~C" #xEB))))
     ("inside <>" (%u "Zo~C <a(c)@x.example>" #xEB)
      (format nil "~A <a(c)@x.example>" (%enc (%u "Zo~C" #xEB))))
     ("group head and tail" (%u "Team (t): Zo~C <a@x.example>; (after)" #xEB)
      (format nil "Team (t): ~A <a@x.example>; (after)" (%enc (%u "Zo~C" #xEB))))
     ("encoded group name, comment after" (%u "Zo~C Team (t): a@x.example;" #xEB)
      (format nil "~A (t): a@x.example;" (%enc (%u "Zo~C Team" #xEB))))
     ("non-ASCII comment before <" (%u "Ann (~C) <a@x.example>" #xFC)
      (format nil "Ann (~A) <a@x.example>" (%enc (%u "~C" #xFC)))))
  (ok (equal wire (%to-wire to)) (format nil "~A: ~A" label wire)))

(deftest re-encoded-address-reads-back
  (let ((wire (%to-wire (%u "Zo~C (c) Smith <a@x.example>" #xEB))))
    (ok (equal (%u "Zo~C Smith" #xEB) (nth-value 1 (parse-address wire)))
        "the display name reads as Python reads it")))

;;; One rule for addresses that do not parse: refused when the message is
;;; made, ASCII or not. Python's getaddresses reads ('', '') for each, so
;;; none of them is a recipient.

(deftest-parametrize malformed-addresses-are-refused-when-made
    ((address)
     ("Zoe Smith")
     ((%u "Zo~C Smith" #xEB))
     ("Zoe <z@x.example>;")
     ((%u "Zo~C <z@x.example>;" #xEB))
     ("Zoe: a@x.example;;")
     ((%u "Zo~C: a@x.example;;" #xEB))
     ("Zoe: a@x.example, (u);")
     ((%u "Zo~C: a@x.example, (~C);" #xEB #xFC)))
  (ok (signals (make-message :from "s@x.example" :to address :subject "s" :body "b")
          'mail-address-error)
      (format nil "~S is refused by make-message" address))
  (let ((m (make-message :from "s@x.example" :to "ok@x.example" :subject "s" :body "b")))
    (ok (signals (setf (message-to m) address) 'mail-address-error)
        (format nil "~S is refused by (setf message-to)" address))))

;;; SMTP read timeout.

(defun %silent-smtp-server ()
  "A one-connection server that sends a 220 greeting and then never
answers. → (values port thread)."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener)))
    (flet ((serve ()
             (unwind-protect
                  (when (usocket:wait-for-input listener :timeout 5 :ready-only t)
                    (let* ((sock (usocket:socket-accept listener))
                           (s (usocket:socket-stream sock)))
                      (unwind-protect
                           (handler-case
                               (progn
                                 (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                                                      (%crlf-text "220 silent ESMTP" ""))
                                                 s)
                                 (force-output s)
                                 ;; Read until the client hangs up; never reply.
                                 (loop while (read-byte s nil nil)))
                             (error () nil))
                        (usocket:socket-close sock))))
               (usocket:socket-close listener))))
      (values port (bt2:make-thread #'serve :name "smtp-silent")))))

(deftest smtp-read-timeout
  (multiple-value-bind (port thread) (%silent-smtp-server)
    (let* ((start (get-internal-real-time))
           (err (handler-case
                    (progn
                      (backend-send (mail-backend-smtp:make-smtp-backend
                                     :host "127.0.0.1" :port port
                                     :connect-timeout 5 :read-timeout 1)
                                    (make-message :from "s@x.example" :to "r@x.example"
                                                  :subject "s" :body "b"))
                      nil)
                  (error (e) e)))
           (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
      (bt2:join-thread thread)
      (ok (typep err 'mail-send-error) (format nil "a silent server is MAIL-SEND-ERROR (~A)" err))
      (ok (< seconds 10) (format nil "after the read timeout, not forever (~,1Fs)" seconds))
      (ok (and err (search "EHLO" (princ-to-string err)))
          "the error says which reply did not come"))))

(deftest smtp-dialogue-awaits-each-reply
  (let* ((calls 0)
         (err (handler-case
                  (progn (%dialogue (%lines "220 hi" "250 mx")
                                    :await-reply (lambda (in)
                                                   (declare (ignore in))
                                                   (/= 3 (incf calls))))
                    nil)
                (mail-send-error (e) e))))
    (ok err "AWAIT-REPLY NIL (no reply in time) is MAIL-SEND-ERROR")
    (ok (and err (search "MAIL FROM" (princ-to-string err))) "naming the reply it waited for")
    (ok (= 3 calls) "it ran before the greeting, EHLO, then MAIL FROM")))
