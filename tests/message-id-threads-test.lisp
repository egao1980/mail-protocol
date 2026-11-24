(in-package #:mail-protocol/tests)

;;; Evidence against PR #6 (fix for #3, item 1): Message-ID uniqueness
;;; across threads. MAKE-MESSAGE-ID draws its 64 random bits from one
;;; global *MESSAGE-ID-RANDOM-STATE* with no lock. SBCL random states are
;;; not thread-safe, so senders on several threads get the same bits in
;;; the same second, i.e. the same Message-ID (RFC 5322 3.6.4: "MUST be a
;;; globally unique identifier"). Python's make_msgid adds the pid and
;;; draws under the GIL.

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

(deftest make-message-ids-are-unique-across-threads
  ;; The same through MAKE-MESSAGE, which is what a pool of senders calls.
  (let* ((threads (loop repeat 8
                        collect (bt2:make-thread
                                 (lambda ()
                                   (loop repeat 5000
                                         collect (message-id
                                                  (make-message :from "a@ex.com" :to "b@ex.com"
                                                                :subject "s" :body "b")))))))
         (ids (loop for th in threads append (bt2:join-thread th))))
    (ok (= (length ids) (length (remove-duplicates ids :test #'equal)))
        (format nil "~D messages, ~D distinct Message-IDs"
                (length ids) (length (remove-duplicates ids :test #'equal))))))
