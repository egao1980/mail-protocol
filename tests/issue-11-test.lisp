(in-package #:mail-protocol/tests)

;;; egao1980/mail-protocol#11: the breaks found by the adversarial review of
;;; #10 (0.3.0). Python 3.13 for the same inputs (email.utils.getaddresses,
;;; EmailMessage with policy.SMTP):
;;;   "Équipe: a@x; b@x"     getaddresses [('', '')], To defect: invalid address
;;;   "Équipe: a@x; (c)"     [('', 'a@x')], no defect
;;;   ": Zoë <z@x>;"         [('Zoë', 'z@x')], To: : =?utf-8?q?Zo=C3=AB?= <z@x>;
;;;   "<@bücher.example:a@x>" [('', 'a@x')]
;;;   "J.R.R. Tolkiën <t@x>" [('J.R.R. Tolkiën', 't@x')]
;;;   "Zoë . Smith <z@x>"    [('Zoë . Smith', 'z@x')]
;;;   "Mailer Daemon <>"     [('Mailer Daemon', '')]  (MAIL FROM:<>)
;;;   "<> (bounce)"          [('', '')]               (MAIL FROM:<>)
;;; Break 3 (long or non-ASCII filenames) waits for mime-protocol#19.

(defun %u (control &rest codes)
  "CONTROL with each ~C filled by the character of one of CODES."
  (apply #'format nil control (mapcar #'code-char codes)))

(defun %ascii-block-p (wire)
  (every (lambda (c) (< (char-code c) 128)) (%header-block wire)))

;;; 1. Text after a group's ";" lost the rest of the list.

(deftest-parametrize text-after-a-group-is-refused
    ((address)
     ((%u "~Cquipe: a@x.example; b@x.example" #xC9))
     ("Equipe: a@x.example; b@x.example")
     ((%u "~Cquipe: a@x.example;b@x.example" #xC9))
     ((%u "~Cquipe: Zo~C <z@x.example>; junk" #xC9 #xEB)))
  (ok (signals (envelope-recipients
                (make-message :from "s@x.example" :to address :body "x"))
               'mail-address-error)
      "make-message or the envelope refuses it")
  (ok (signals (address-mailboxes address) 'mail-address-error)))

(deftest no-recipient-of-a-group-is-dropped
  (let ((m (handler-case
               (make-message :from "s@x.example"
                             :to (%u "~Cquipe: a@x.example; b@x.example" #xC9) :body "x")
             (mail-address-error () nil))))
    (when m
      (ok (member "b@x.example"
                  (handler-case (envelope-recipients m) (mail-address-error () '("b@x.example")))
                  :test #'string=)
          "b@x.example is not silently gone")
      (ok (search "b@x.example" (%header-block (print-message m)))
          "b@x.example is still in To"))
    (ok t)))

(deftest a-comment-after-a-group-is-fine
  (let* ((m (make-message :from "s@x.example"
                          :to (%u "~Cquipe: a@x.example; (note)" #xC9) :body "x"))
         (wire (print-message m)))
    (ok (equal '("a@x.example") (envelope-recipients m)))
    (ok (%ascii-block-p wire) (%header wire "To"))))

;;; 2. Raw UTF-8 reached the header.

(deftest-parametrize non-ascii-group-and-route-parts-are-encoded
    ((address recipients read-back)
     ((%u ": Zo~C <z@x.example>;" #xEB) '("z@x.example") (%u ": Zo~C <z@x.example>;" #xEB))
     ((%u "(B~Cro): a@x.example;" #xFC) '("a@x.example") nil)
     ((%u "Team (B~Cro): a@x.example;" #xFC) '("a@x.example") nil)
     ((%u "<@b~Ccher.example:a@x.example>" #xFC) '("a@x.example") nil)
     ((%u "Zo~C <@b~Ccher.example:z@x.example>" #xEB #xFC) '("z@x.example")
      (%u "Zo~C <z@x.example>" #xEB)))
  (let* ((m (make-message :from "s@x.example" :to address :body "x"))
         (wire (print-message m))
         (p (parse-message wire)))
    (ok (%ascii-block-p wire) (format nil "ASCII header block: To: ~A" (%header wire "To")))
    (ok (equal recipients (envelope-recipients p)) (format nil "~S" (envelope-recipients p)))
    (when read-back
      (ok (equal (list read-back) (message-to p)) (format nil "~S" (message-to p))))))

;;; 4. :disposition nil is the default, not an error.

(deftest-parametrize a-nil-disposition-is-an-attachment
    ((data filename)
     ((coerce #(1 2 3) '(vector (unsigned-byte 8))) "r.pdf")
     ("note" "n.txt"))
  (let ((m (make-message :from "s@x.example" :to "b@x.example" :body "x")))
    (ok (add-attachment m data :filename filename :disposition nil))
    (ok (search (format nil "Content-Disposition: attachment; filename=~A" filename)
                (print-message m)))))

;;; 5. "Name <>" and "<> (comment)" are the null reverse-path.

(deftest-parametrize a-named-or-commented-null-path-is-the-null-path
    ((from)
     ("<>")
     ("Mailer Daemon <>")
     ("<> (bounce)")
     ("\"Mail Delivery\" <> (auto)"))
  (let ((m (make-message :from from :to "b@x.example" :body "bounce")))
    (ok (equal "" (envelope-sender m)) (format nil "~S" (handler-case (envelope-sender m)
                                                         (error (e) (princ-to-string e)))))
    (ok (equal "" (parse-address from)))
    (multiple-value-bind (lines err) (%send-captured m)
      (ok (null err) (format nil "~@[~A~]" err))
      (ok (member (%crlf-text "MAIL FROM:<>" "") lines :test #'string=)
          (format nil "~S" (%command-lines lines "MAIL"))))))

(deftest-parametrize the-null-path-is-no-recipient
    ((to)
     ("Mailer Daemon <>")
     ("<>")
     ("Team: <>;"))
  (ok (signals (envelope-recipients (make-message :from "s@x.example" :to to :body "x"))
               'mail-address-error)))

(deftest smtp-dialogue-takes-a-named-null-path-but-no-null-recipient
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from "Mailer Daemon <>" :recipients '("b@x.example") :data "x")
    (ok (eq t result) (format nil "~A" result))
    (ok (search (%lines "MAIL FROM:<>") out)))
  (multiple-value-bind (result out)
      (%scripted +plain-replies+ :from "a@x.example" :recipients '("Nobody <>") :data "x")
    (ok (typep result 'mail-address-error) (format nil "~A" result))
    (ok (zerop (length out)) "nothing is sent")))

;;; 6. "J.R.R." kept its dots and spaces.

(deftest-parametrize a-phrase-keeps-its-dots-as-written
    ((address name)
     ((%u "J.R.R. Tolki~Cn <t@x.example>" #xEB) (%u "J.R.R. Tolki~Cn" #xEB))
     ("J.R.R. Tolkien <t@x.example>" "J.R.R. Tolkien")
     ((%u "Dr. Zo~C <z@x.example>" #xEB) (%u "Dr. Zo~C" #xEB))
     ((%u "Zo~C . Smith <z@x.example>" #xEB) (%u "Zo~C . Smith" #xEB))
     ("=?utf-8?q?Zo=C3=AB?= =?utf-8?q?_Smith?= <z@x.example>" (%u "Zo~C Smith" #xEB)))
  (ok (equal name (nth-value 1 (parse-address address)))
      (format nil "~S" (nth-value 1 (parse-address address))))
  (let ((p (parse-message (print-message (make-message :from "s@x.example" :to address
                                                       :body "x")))))
    (ok (equal name (nth-value 1 (parse-address (first (message-to p)))))
        (format nil "~S" (message-to p)))))

;;; Adversarial round on the fix: a group head that is an address made its
;;; members recipients (getaddresses: [('', '')], nothing sent), and an
;;; encoded group name sat against the ":" (RFC 2047 5 (3)).

(deftest-parametrize a-group-head-that-is-an-address-is-refused
    ((address)
     ("Bob <bob@x.example>: eve@evil.example;")
     ("<a@x.example>: eve@evil.example;")
     ("Bob@x.example: eve@evil.example;")
     ((%u "\"Zo~C\" <z@x.example>: eve@evil.example;" #xEB)))
  (ok (signals (address-mailboxes address) 'mail-address-error) address)
  (ok (signals (envelope-recipients
                (make-message :from "s@x.example" :to address :body "x"))
        'mail-address-error)))

(deftest an-encoded-group-name-is-followed-by-a-blank
  (let ((to (%header (print-message (make-message :from "s@x.example"
                                                  :to (%u "~Cquipe: a@x.example;" #xC9)
                                                  :body "x"))
                     "To")))
    (ok (search "?= : a@x.example;" to) to)
    (ok (equal (list (%u "~Cquipe: a@x.example;" #xC9))
               (message-to (parse-message (format nil "To: ~A~C~C~C~Cx" to #\Return #\Newline
                                                  #\Return #\Newline)))))))

(deftest live-issue-11-addresses
  ;; The plain Mailpit of live-non-ascii-send (MAIL_SMTP_TEST_PLAIN_PORT).
  (let ((port (parse-integer (%env "MAIL_SMTP_TEST_PLAIN_PORT" "11025"))))
    (if (not (%port-open-p port))
        (skip (format nil "no SMTP server on 127.0.0.1:~D (MAIL_SMTP_TEST_PLAIN_PORT)" port))
        (let ((b (mail-backend-smtp:make-smtp-backend :host (%smtp-host) :port port))
              (m (make-message :from "Mailer Daemon <>" :subject "issue 11"
                               :to (%u ": Zo~C <z@x.example>;, <@b~Ccher.example:r@x.example>"
                                       #xEB #xFC)
                               :cc (%u "J.R.R. Tolki~Cn <t@x.example>" #xEB)
                               :body "x")))
          (add-attachment m (coerce #(1 2 3) '(vector (unsigned-byte 8)))
                          :filename "r.pdf" :disposition nil)
          (ok (backend-send b m) "null path, empty group name, route, dotted name")
          (ok (signals (backend-send b (make-message
                                        :from "a@x.example"
                                        :to (%u "~Cquipe: a@x.example; b@x.example" #xC9)
                                        :body "x"))
                       'mail-address-error)
              "text after a group's ; is refused before anything is sent")))))
