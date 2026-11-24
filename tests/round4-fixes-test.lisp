(in-package #:mail-protocol/tests)

;;; Fixes for the round-1 review of #6 (tests/adversarial-r4-test.lisp),
;;; and the paths next to them.

(deftest non-ascii-comments-and-groups-read-back
  (let* ((to (list (format nil "~Cquipe: a@example.com, Zo~C <z@example.com>;" (code-char #xC9) (code-char #xEB))
                   (format nil "c@example.com (B~Cro)" (code-char #xFC))))
         (m (make-message :from "a@example.com" :to to :body "x"))
         (wire (print-message m))
         (p (parse-message wire)))
    (ok (every (lambda (c) (< (char-code c) 128)) (%header-block wire)) "ASCII header block")
    (ok (equal (list (format nil "~Cquipe: a@example.com, Zo~C <z@example.com>;" (code-char #xC9) (code-char #xEB))
                     "c@example.com (=?utf-8?b?QsO8cm8=?=)")
               (message-to p))
        (format nil "group names decode; comments stay encoded (~S)" (message-to p)))
    (ok (equal '("a@example.com" "z@example.com" "c@example.com") (envelope-recipients p)))))

(deftest ascii-addresses-are-left-as-written
  (let* ((to "Team: a@example.com, \"B, b\" <b@example.com>;, c@example.com (desk)")
         (wire (print-message (make-message :from "a@example.com" :to to :body "x"))))
    (ok (search to (%header-block wire)) (%header wire "To"))))

(deftest a-message-with-an-empty-from-goes-out-with-the-null-reverse-path
  (let ((m (make-message :from "<>" :to "b@example.com" :body "bounce")))
    (ok (equal "" (envelope-sender m)))
    (multiple-value-bind (lines err) (%send-captured m)
      (ok (null err) (format nil "~@[~A~]" err))
      (ok (member (%crlf-text "MAIL FROM:<>" "") lines :test #'string=)
          (format nil "~S" (%command-lines lines "MAIL"))))))

(deftest domain-case-is-one-mailbox
  (ok (equal '("bob@Example.com" "Bob@example.com")
             (envelope-recipients
              (make-message :from "a@example.com" :to "bob@Example.com, Bob@example.com"
                            :cc "bob@example.com")))))

(deftest a-rejected-quit-or-a-dead-connection-after-250-is-a-success
  (multiple-value-bind (result out)
      (%scripted '("220 hi" "250 mx" "250 ok" "250 ok" "354 go" "250 queued" "500 what")
                 :from "a@example.com" :recipients '("b@example.com") :data "x")
    (ok (eq t result) (format nil "~A" result))
    (ok (search (%lines "QUIT") out)))
  (multiple-value-bind (result out)
      (%scripted '("220 hi" "250 mx" "250 ok" "250 ok" "354 go")
                 :from "a@example.com" :recipients '("b@example.com") :data "x")
    (declare (ignore out))
    (ok (typep result 'mail-send-error) "no 250 for the message: a failed send")))

(deftest-parametrize subtypes-and-dispositions-must-be-tokens
    ((thunk)
     ((lambda () (make-message :from "a@example.com" :to "b@example.com" :subtype "html; charset=evil")))
     ((lambda () (add-alternative (make-message :from "a@example.com" :to "b@example.com")
                                  "<p>x</p>" :subtype "html charset")))
     ((lambda () (add-attachment (make-message :from "a@example.com" :to "b@example.com")
                                 (coerce #(1) '(vector (unsigned-byte 8)))
                                 :disposition "attachment; filename=x.exe")))
     ((lambda () (add-attachment (make-message :from "a@example.com" :to "b@example.com")
                                 "text" :disposition "inline=1"))))
  (ok (signals (funcall thunk) 'mail-parse-error)))

(deftest a-good-disposition-still-works
  (let ((m (make-message :from "a@example.com" :to "b@example.com" :body "x")))
    (add-attachment m "note" :filename "n.txt" :disposition "inline")
    (ok (search "Content-Disposition: inline; filename=n.txt" (print-message m)))))
