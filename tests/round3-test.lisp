(in-package #:mail-protocol/tests)

;;; Round 3 (#3, #7): a Message-ID carries the pid, and a forked child
;;; draws its own nonce; the DATA payload has CRLF line ends only.

(deftest message-id-carries-the-pid
  (let ((pid (mail-protocol::%current-pid))
        (id (make-message-id :domain "ex.com")))
    (if pid
        (ok (search (format nil ".~(~36R~)." pid) id)
            (format nil "~A holds the pid ~D in base 36" id pid))
        (ok (search "@ex.com>" id) "no pid on this implementation"))))

(deftest a-new-pid-draws-a-new-nonce
  ;; What a forked child sees: the parent's nonce under another pid.
  (make-message-id)
  (let ((nonce mail-protocol::*message-id-nonce*))
    (let ((mail-protocol::*message-id-pid* -1)
          (mail-protocol::*message-id-nonce* nonce)
          (mail-protocol::*message-id-counter* 41))
      (make-message-id)
      (ok (/= nonce mail-protocol::*message-id-nonce*) "a new nonce for the new pid")
      (ok (= 1 mail-protocol::*message-id-counter*) "the sequence starts over")
      (ok (eql (mail-protocol::%current-pid) mail-protocol::*message-id-pid*)
          "the new pid is recorded"))))

(deftest-parametrize dot-stuff-sends-crlf-only
    ((body wire)
     ((format nil "hi~C.~CMAIL FROM:<x@evil>~C~Cend" #\Return #\Return #\Return #\Newline)
      (format nil "hi~C~C..~C~CMAIL FROM:<x@evil>~C~Cend~C~C.~C~C"
              #\Return #\Newline #\Return #\Newline #\Return #\Newline
              #\Return #\Newline #\Return #\Newline))
     ((format nil "a~Cb~C~Cc~Cd" #\Return #\Return #\Newline #\Newline)
      (format nil "a~C~Cb~C~Cc~C~Cd~C~C.~C~C"
              #\Return #\Newline #\Return #\Newline #\Return #\Newline
              #\Return #\Newline #\Return #\Newline))
     ((format nil "x~C" #\Return)
      (format nil "x~C~C.~C~C" #\Return #\Newline #\Return #\Newline))
     ("" (format nil ".~C~C" #\Return #\Newline)))
  (let ((got (mail-backend-smtp:dot-stuff body)))
    (ok (equal wire got) (format nil "~S → ~S" body got))
    (ok (loop for i from 0 below (length got)
              always (or (char/= (char got i) #\Return)
                         (and (< (1+ i) (length got)) (char= (char got (1+ i)) #\Newline))))
        "no bare CR on the wire")))
