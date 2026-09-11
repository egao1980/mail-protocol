(in-package #:mail-protocol/tests)

(deftest make-and-print
  (let* ((msg (make-message :from "a@ex.com"
                            :to '("b@ex.com" "c@ex.com")
                            :cc "d@ex.com"
                            :bcc "secret@ex.com"
                            :subject "Hi"
                            :body "hello"))
         (wire (print-message msg)))
    (ok (string= "a@ex.com" (message-from msg)))
    (ok (equal '("b@ex.com" "c@ex.com") (message-to msg)))
    (ok (equal '("d@ex.com") (message-cc msg)))
    (ok (equal '("secret@ex.com") (message-bcc msg)))
    (ok (string= "Hi" (message-subject msg)))
    (ok (string= "hello" (message-body msg)))
    (ok (search "hello" wire))
    (ok (search "subject: Hi" wire))
    (ng (search "secret@ex.com" wire))
    (ok (equal '("b@ex.com" "c@ex.com" "d@ex.com" "secret@ex.com")
               (envelope-recipients msg)))))

(deftest parse-roundtrip
  (let* ((msg (make-message :from "a@ex.com" :to "b@ex.com" :subject "S" :body "z"))
         (again (parse-message (print-message msg))))
    (ok (string= "a@ex.com" (message-from again)))
    (ok (equal '("b@ex.com") (message-to again)))
    (ok (string= "S" (message-subject again)))
    (ok (string= "z" (message-body again)))))

(deftest address-list-splits
  (ok (equal '("a@x" "b@y") (address-list "a@x, b@y")))
  (ok (null (address-list "")))
  (ok (null (address-list nil))))
