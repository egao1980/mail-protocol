(in-package #:mail-protocol)

(defun %split-commas (s)
  (loop for start = 0 then (1+ pos)
        for pos = (position #\, s :start start)
        for tok = (string-trim '(#\Space #\Tab) (subseq s start (or pos (length s))))
        unless (zerop (length tok))
          collect tok
        while pos))

(defun address-list (value)
  "Normalize VALUE to a list of address strings."
  (cond
    ((null value) nil)
    ((stringp value) (%split-commas value))
    ((and (consp value) (every #'stringp value)) (copy-list value))
    (t (error 'mail-parse-error
              :message (format nil "not an address list: ~S" value)))))

(defun %join-addresses (addrs)
  (format nil "~{~A~^, ~}" addrs))

(defclass message ()
  ((entity :initarg :entity :accessor message-entity)
   (bcc :initarg :bcc :accessor message-bcc :initform nil)))

(defun message-p (object)
  (typep object 'message))

(defun message-from (msg)
  (mime-protocol:header-value (message-entity msg) "from"))

(defun (setf message-from) (value msg)
  (mime-protocol:set-header (message-entity msg) "from" value)
  value)

(defun message-to (msg)
  (address-list (mime-protocol:header-value (message-entity msg) "to")))

(defun (setf message-to) (value msg)
  (let ((addrs (address-list value)))
    (mime-protocol:set-header (message-entity msg) "to" (%join-addresses addrs))
    addrs))

(defun message-cc (msg)
  (let ((raw (mime-protocol:header-value (message-entity msg) "cc")))
    (when (and raw (plusp (length raw)))
      (address-list raw))))

(defun (setf message-cc) (value msg)
  (let ((addrs (address-list value)))
    (if addrs
        (mime-protocol:set-header (message-entity msg) "cc" (%join-addresses addrs))
        (setf (mime-protocol:mime-headers (message-entity msg))
              (remove "cc" (mime-protocol:mime-headers (message-entity msg))
                      :key #'car :test #'string-equal)))
    addrs))

(defun message-subject (msg)
  (or (mime-protocol:header-value (message-entity msg) "subject") ""))

(defun (setf message-subject) (value msg)
  (mime-protocol:set-header (message-entity msg) "subject" (or value ""))
  value)

(defun message-body (msg)
  (let* ((entity (message-entity msg))
         (content (mime-protocol:mime-content entity)))
    (cond
      ((stringp content) content)
      ((vectorp content)
       (encoding-protocol:decode content))
      (t ""))))

(defun make-message (&key from to cc bcc subject (body "") (subtype "plain"))
  (unless from
    (error 'mail-parse-error :message "make-message requires :from"))
  (let* ((to-list (address-list to))
         (cc-list (address-list cc))
         (bcc-list (address-list bcc))
         (entity (mime-protocol:make-text-entity body :subtype subtype)))
    (mime-protocol:set-header entity "from" from)
    (when to-list
      (mime-protocol:set-header entity "to" (%join-addresses to-list)))
    (when cc-list
      (mime-protocol:set-header entity "cc" (%join-addresses cc-list)))
    (mime-protocol:set-header entity "subject" (or subject ""))
    (mime-protocol:set-header entity "mime-version" "1.0")
    (make-instance 'message :entity entity :bcc bcc-list)))

(defun parse-message (source)
  (handler-case
      (let* ((entity (mime-protocol:parse-mime source))
             (bcc (mime-protocol:header-value entity "bcc")))
        (when bcc
          (setf (mime-protocol:mime-headers entity)
                (remove "bcc" (mime-protocol:mime-headers entity)
                        :key #'car :test #'string-equal)))
        (make-instance 'message
                       :entity entity
                       :bcc (when (and bcc (plusp (length bcc)))
                              (address-list bcc))))
    (mime-protocol:mime-error (e)
      (error 'mail-parse-error :message (princ-to-string e)))))

(defun print-message (msg &key stream)
  "Serialize MSG (Bcc omitted — envelope only)."
  (unless (message-p msg)
    (error 'mail-parse-error :message "print-message expects a message"))
  (mime-protocol:print-mime (message-entity msg) :stream stream))

(defun envelope-recipients (msg)
  (delete-duplicates
   (append (message-to msg)
           (or (message-cc msg) '())
           (or (message-bcc msg) '()))
   :test #'string-equal))
