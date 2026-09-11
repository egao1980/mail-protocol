(in-package #:mail-protocol/tests)

(deftest dot-stuff
  (let ((out (mail-backend-smtp:dot-stuff (format nil "hello~%.dotx~%end"))))
    (ok (search "hello" out))
    (ok (search "..dotx" out))
    (ok (char= #\. (char out (- (length out) 3))))))

(deftest smtp-dialogue-happy
  (let ((replies (format nil "220 hi~%250 ehlo~%250 mail~%250 rcpt~%354 go~%250 queued~%221 bye~%")))
    (ok (with-input-from-string (in replies)
          (with-output-to-string (out)
            (mail-backend-smtp:smtp-dialogue in out
                                             :from "a@ex.com"
                                             :recipients '("b@ex.com")
                                             :data (format nil "Subject: x~%~%hi")))))))

(deftest smtp-dialogue-bad-code
  (ok (signals
       (with-input-from-string (in (format nil "554 nope~%"))
         (with-output-to-string (out)
           (mail-backend-smtp:smtp-dialogue in out
                                            :from "a@ex.com"
                                            :recipients '("b@ex.com")
                                            :data "x")))
       'mail-send-error)))
