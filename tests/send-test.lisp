(in-package #:mail-protocol/tests)

(deftest memory-send
  (let ((mail-protocol:*mail-backend* (mail-backend-memory:make-memory-backend)))
    (let ((msg (make-message :from "a@ex.com" :to "b@ex.com" :body "x")))
      (ok (eq msg (send msg)))
      (ok (= 1 (length (mail-backend-memory:sent-messages mail-protocol:*mail-backend*))))
      (ok (eq msg (first (mail-backend-memory:sent-messages mail-protocol:*mail-backend*)))))))

(deftest memory-send-needs-recipients
  (let ((mail-protocol:*mail-backend* (mail-backend-memory:make-memory-backend)))
    (ok (signals (send (make-message :from "a@ex.com" :body "x"))
                 'mail-send-error))))
