(in-package #:mail-protocol/tests)

;;; egao1980/mail-protocol#11 break 3, fixed by mime-protocol 0.3.4: long or
;;; non-ASCII attachment names. Python 3.13 EmailMessage.add_attachment
;;; writes the same shape: filename*=utf-8''... alone for a non-ASCII name,
;;; RFC 2231 continuations (filename*0*=...) for a long one, every line
;;; within 78; its get_filename() (compat32 and policy.default) reads each
;;; name below back from PRINT-MESSAGE's output (checked 2026-11-26).

(defun %lines-of (block)
  (mapcar (lambda (line) (string-right-trim '(#\Return) line))
          (uiop:split-string block :separator '(#\Newline))))

(defun %attachment-wire (filename)
  (let ((m (make-message :from "s@x.example" :to "r@x.example" :subject "files" :body "see")))
    (add-attachment m (coerce #(1 2 3) '(vector (unsigned-byte 8))) :filename filename)
    (print-message m)))

(deftest-parametrize attachment-names-are-written-and-read-back
    ((label filename)
     ("400 CJK" (make-string 400 :initial-element (code-char #x4E2D)))
     ("1100 ASCII" (concatenate 'string (make-string 1096 :initial-element #\a) ".bin"))
     ("short Japanese" (%u "~C~C~C~C~C~C~C~C~C.pdf"
                           #x65E5 #x672C #x8A9E #x306E #x30D5 #x30A1 #x30A4 #x30EB #x540D))
     ("short Latin" (%u "r~Csum~C.pdf" #xE9 #xE9))
     ("long ASCII with spaces" (format nil "~{~A~^ ~}" (loop repeat 40 collect "quarterly"))))
  (let* ((wire (handler-case (%attachment-wire filename)
                 (error (c) c))))
    (ok (stringp wire) (format nil "~A: printed~:[ (got ~A)~;~]" label (stringp wire) wire))
    (when (stringp wire)
      (let* ((part-headers (subseq wire (search "Content-Disposition" wire)))
             (block (%header-block part-headers)))
        (ok (every (lambda (line) (<= (length line) 78)) (%lines-of block))
            (format nil "~A: every Content-Disposition line within 78" label))
        (ok (%ascii-block-p part-headers) (format nil "~A: no raw octets in the part header" label))
        (ok (not (search "filename=\"" block))
            (format nil "~A: no raw filename= next to filename*" label)))
      (let ((parts (message-attachments (parse-message wire))))
        (ok (= 1 (length parts)))
        (ok (equal filename
                   (and parts (mime-protocol:disposition-filename
                               (mime-protocol:mime-content-disposition (first parts)))))
            (format nil "~A: the name reads back" label))))))

(deftest a-short-ascii-name-stays-plain
  (let ((wire (%attachment-wire "report.pdf")))
    (ok (search "filename=report.pdf" wire))
    (ok (not (search "filename*" wire)))))

(deftest live-long-attachment-names-send
  ;; The plain Mailpit of live-non-ascii-send (MAIL_SMTP_TEST_PLAIN_PORT).
  ;; Its API then lists each attachment's FileName as Mailpit read it:
  ;; curl 127.0.0.1:<http port>/api/v1/message/<id>.
  (let ((port (parse-integer (%env "MAIL_SMTP_TEST_PLAIN_PORT" "11025"))))
    (if (not (%port-open-p port))
        (skip (format nil "no SMTP server on 127.0.0.1:~D (MAIL_SMTP_TEST_PLAIN_PORT)" port))
        (let ((backend (mail-backend-smtp:make-smtp-backend :host (%smtp-host) :port port)))
          (loop for (tag filename) in
                (list (list "cjk-400" (make-string 400 :initial-element (code-char #x4E2D)))
                      (list "ascii-1100"
                            (concatenate 'string (make-string 1096 :initial-element #\a) ".bin"))
                      (list "short-ja" (%u "~C~C~C.pdf" #x65E5 #x672C #x8A9E)))
                do (let ((m (make-message :from "a@ex.com" :to "b@ex.com"
                                          :subject (format nil "attachment ~A" tag) :body "see")))
                     (add-attachment m (coerce #(1 2 3) '(vector (unsigned-byte 8)))
                                     :filename filename)
                     (ok (backend-send backend m) (format nil "sent ~A" tag))))))))

(deftest a-parsed-raw-subject-keeps-its-octets-through-add-attachment
  ;; mime-protocol#19 break 3: %WRAP-IN-MULTIPART moved the fields without
  ;; their raw state, so the windows-1251 Subject went out as UTF-8 of its
  ;; ISO-8859-1 reading. Python 3.13 parse → add_attachment → as_bytes keeps
  ;; the octets.
  (let* ((subject (map 'string #'code-char '(#xCF #xF0 #xE8 #xE2 #xE5 #xF2)))
         (m (parse-message (%crlf-text "From: a@x.example" "To: b@x.example"
                                       (concatenate 'string "Subject: " subject)
                                       "Content-Type: text/plain; charset=windows-1251"
                                       "" "body"))))
    (add-attachment m (coerce #(1 2 3) '(vector (unsigned-byte 8))) :filename "a.bin")
    (let ((wire (print-message m)))
      (ok (search (concatenate 'string "Subject: " subject (string #\Return)) wire)
          "the Subject goes out as the octets it came in")
      (ok (= 1 (length (message-attachments (parse-message wire))))))))
