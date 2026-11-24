(in-package #:mail-protocol/tests)

;;; Evidence against PR #6 (fix for #3), two checks.
;;;
;;; 1. Non-ASCII text on the SMTP wire. make-text-entity (body,
;;;    add-alternative, a string add-attachment) stores UTF-8 octets with
;;;    Content-Transfer-Encoding 8bit and charset=utf-8. print-mime turns
;;;    those octets into a string one char per octet (ISO-8859-1), and
;;;    mail-backend-smtp writes that string through a UTF-8 flexi-stream
;;;    (%TEXT-STREAM), so every octet >= #x80 is encoded a second time:
;;;    "Café" arrives as "CafÃ©". A one-connection SMTP sink records the
;;;    DATA octets; parsing them back must give the text that was sent.
;;;
;;; 2. Message-ID uniqueness across threads. MAKE-MESSAGE-ID draws from one
;;;    global *MESSAGE-ID-RANDOM-STATE* with no lock; SBCL random states are
;;;    not thread-safe, so concurrent senders can get the same 64 bits in
;;;    the same second, i.e. the same Message-ID (RFC 5322 3.6.4: MUST be
;;;    unique).

(defun %octets-of (string)
  (encoding-protocol:encode string :encoding :utf-8))

(defun %read-octet-line (stream)
  "Octets up to and including LF, or NIL at EOF."
  (let ((line (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil)
          do (cond ((null b) (return (and (plusp (length line)) line)))
                   (t (vector-push-extend b line)
                      (when (= b 10) (return line)))))))

(defun %reply (stream text)
  (write-sequence (%octets-of (format nil "~A~C~C" text #\Return #\Newline)) stream)
  (force-output stream))

(defun %start-smtp-sink ()
  "Serve one SMTP session on 127.0.0.1 and keep the DATA octets.
→ (values port thread box); after the thread ends, (CAR BOX) is the octets."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (box (list nil))
         (thread
           (bt2:make-thread
            (lambda ()
              (unwind-protect
                   (let* ((sock (usocket:socket-accept listener))
                          (s (usocket:socket-stream sock))
                          (data (make-array 0 :element-type '(unsigned-byte 8)
                                              :adjustable t :fill-pointer 0)))
                     (unwind-protect
                          (progn
                            (%reply s "220 sink")
                            (loop for line = (%read-octet-line s)
                                  while line
                                  do (let ((cmd (string-upcase
                                                 (map 'string #'code-char
                                                      (subseq line 0 (min 4 (length line)))))))
                                       (cond
                                         ((string= cmd "EHLO") (%reply s "250-sink")
                                          (%reply s "250 8BITMIME"))
                                         ((string= cmd "DATA")
                                          (%reply s "354 go")
                                          (loop for l = (%read-octet-line s)
                                                until (or (null l) (equalp l #(46 13 10)))
                                                do (loop for b across l
                                                         do (vector-push-extend b data)))
                                          (setf (car box) (coerce data '(vector (unsigned-byte 8))))
                                          (%reply s "250 queued"))
                                         ((string= cmd "QUIT") (%reply s "221 bye") (return))
                                         (t (%reply s "250 ok"))))))
                       (usocket:socket-close sock)))
                (usocket:socket-close listener)))
            :name "smtp-sink")))
    (values port thread box)))

(deftest smtp-sends-non-ascii-text-as-utf-8
  (let ((text (format nil "Caf~C ~C" (code-char #xE9) (code-char #x2615)))
        (html (format nil "<p>Caf~C ~C</p>" (code-char #xE9) (code-char #x2615))))
    (multiple-value-bind (port thread box) (%start-smtp-sink)
      (let ((msg (make-message :from "a@ex.com" :to "b@ex.com" :subject "menu" :body text)))
        (funcall (%mail-fn "ADD-ALTERNATIVE") msg html :subtype "html")
        (backend-send (mail-backend-smtp:make-smtp-backend :host "127.0.0.1" :port port) msg)
        (bt2:join-thread thread)
        (let* ((wire (car box))
               (got (and wire (parse-message wire))))
          (ok (and wire (search (%octets-of text) wire))
              "the DATA octets contain the UTF-8 encoding of the text body")
          (ok (and got (equal text (message-body got)))
              (format nil "text/plain received as sent (got ~S)" (and got (message-body got))))
          (ok (and got (equal html (message-body got :subtype "html")))
              (format nil "text/html received as sent (got ~S)"
                      (and got (message-body got :subtype "html")))))))))

(deftest message-ids-are-unique-across-threads
  (let* ((per-thread 20000)
         (threads (loop repeat 8
                        collect (bt2:make-thread
                                 (lambda ()
                                   (loop repeat per-thread
                                         collect (make-message-id :domain "ex.com"))))))
         (ids (loop for th in threads append (bt2:join-thread th)))
         (seen (make-hash-table :test #'equal))
         (dups (loop for id in ids
                     count (prog1 (gethash id seen) (setf (gethash id seen) t)))))
    (ok (zerop dups)
        (format nil "~D Message-IDs from 8 threads, ~D duplicates" (length ids) dups))))
