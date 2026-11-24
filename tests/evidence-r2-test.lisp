(in-package #:mail-protocol/tests)

;;; Round-2 evidence against PR #6 (0fd3841). MAKE-MESSAGE-ID's docstring:
;;; "NONCE is 64 random bits per process, which keeps processes apart
;;; (RFC 5322 3.6.4: globally unique)", and it is compared with Python's
;;; email.utils.make_msgid. The nonce and the sequence are drawn once and
;;; then live in the image's memory, so a process forked after its first
;;; Message-ID (a pre-fork server such as Woo's :worker-num, or a job
;;; runner that forks per task) starts with the parent's nonce and counter:
;;; parent and child make the same next ID in the same second. make_msgid
;;; puts os.getpid() in every ID; the image-restore hook covers a saved
;;; image, not a fork.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl unix) (require :sb-posix))

(defun %same-second-start ()
  "Wait for the next universal-time second, so a fork right after falls in
one second."
  (let ((now (get-universal-time)))
    (loop while (= now (get-universal-time)) do (sleep 0.01))))

(deftest message-ids-differ-across-a-fork
  #-(and sbcl unix) (skip "needs sb-posix:fork")
  #+(and sbcl unix)
  (let ((path (format nil "/tmp/mail-msgid-fork-~D-~D.txt"
                      (sb-posix:getpid) (random 1000000000 (make-random-state t)))))
    ;; The parent has sent mail before forking its workers.
    (make-message-id :domain "ex.com")
    (%same-second-start)
    (let ((pid (sb-posix:fork)))
      (if (zerop pid)
          (unwind-protect
              (with-open-file (o path :direction :output :if-exists :supersede)
                (write-string (make-message-id :domain "ex.com") o))
            (sb-ext:exit :code 0 :abort t))
          (let ((mine (make-message-id :domain "ex.com")))
            (sb-posix:waitpid pid 0)
            (let ((child (and (probe-file path) (uiop:read-file-string path))))
              (when (probe-file path) (delete-file path))
              (ok (and child (string/= mine child))
                  (format nil "parent and forked child make different Message-IDs (parent ~A, child ~A)"
                          mine child))))))))
