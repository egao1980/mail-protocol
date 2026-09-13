(in-package #:mail-protocol/tests)

(deftest encode-login
  (ok (string= "A0001 LOGIN alice secret"
               (encode-imap-command "A0001" "LOGIN" "alice" "secret")))
  (ok (search "\"p ass\"" (encode-imap-command "A0002" "LOGIN" "u" "p ass"))))

(deftest parse-ok-greeting
  (let ((p (parse-imap-response "* OK IMAP4rev1 ready")))
    (ok (eq :untagged (first p)))
    (ok (eq :ok (second p)))
    (ok (search "IMAP4rev1" (third p)))))

(deftest parse-search
  (let ((p (parse-imap-response "* SEARCH 1 2")))
    (ok (eq :search (second p)))
    (ok (equal '(1 2) (third p)))))

(deftest parse-tagged
  (let ((p (parse-imap-response "A0001 OK LOGIN completed")))
    (ok (eq :tagged (first p)))
    (ok (string= "A0001" (second p)))
    (ok (eq :ok (third p)))))

(deftest parse-fetch-body
  (let* ((raw (format nil "* 1 FETCH (RFC822 {32}~%From: a@ex.com~%~%hello)"))
         (p (parse-imap-untagged raw)))
    (ok (eq :fetch (second p)))
    (ok (= 1 (getf (cddr p) :seq)))
    (ok (search "hello" (getf (cddr p) :body)))))

(deftest imap-client-scripted
  (let* ((inbox (make-hash-table :test #'equal))
         (msg (print-message (make-message :from "a@ex.com" :to "b@ex.com"
                                           :subject "Hi" :body "hello")))
         (client (make-imap-client
                  :io-fn
                  (lambda (cmd)
                    (cond
                      ((null cmd)
                       '("* OK IMAP4rev1 ready"))
                      ((search "LOGIN" cmd)
                       (if (search "badpass" cmd)
                           '("A0001 NO [AUTHENTICATIONFAILED] denied")
                           '("A0001 OK LOGIN completed")))
                      ((search "SELECT" cmd)
                       '("* 1 EXISTS" "A0002 OK [READ-WRITE] SELECT completed"))
                      ((search "SEARCH" cmd)
                       '("* SEARCH 1 2" "A0003 OK SEARCH completed"))
                      ((search "FETCH" cmd)
                       (list (format nil "* 1 FETCH (RFC822 {~d}~%~a)"
                                     (length msg) msg)
                             "A0004 OK FETCH completed"))
                      (t '("A9999 BAD unknown")))))))
    (setf (gethash 1 inbox) msg)
    (imap-connect client)
    (ok (eq :connected (imap-client-state client)))
    (imap-login client "alice" "secret")
    (ok (eq :authenticated (imap-client-state client)))
    (imap-select client "INBOX")
    (ok (string= "INBOX" (imap-client-selected client)))
    (ok (equal '(1 2) (imap-search client "ALL")))
    (let ((fetched (imap-fetch client "1")))
      (ok (plusp (length fetched)))
      (ok (or (message-p (first fetched))
              (search "hello" (princ-to-string (first fetched))))))
    (ok (imap-idle client :handler (lambda (c ev)
                                     (declare (ignore c ev))
                                     :ok)))))

(deftest imap-auth-error
  (let ((client (make-imap-client
                 :io-fn (lambda (cmd)
                          (if cmd
                              '("A0001 NO denied")
                              '("* OK ready"))))))
    (imap-connect client)
    (ok (signals (imap-login client "u" "badpass") 'imap-auth-error))))
