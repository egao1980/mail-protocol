(defsystem "mail-protocol"
  :version "0.2.0"
  :description "Email message + send GFs + IMAP (stack-mail); MIME via mime-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("mime-protocol" "encoding-protocol")
  :properties (:cl-repo (:ci (:with ("mail-backend-memory" "mail-backend-smtp"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "message")
               (:file "protocol")
               (:file "imap"))
  :in-order-to ((test-op (test-op "mail-protocol/tests"))))

(defsystem "mail-protocol/tests"
  :depends-on ("mail-protocol" "mail-backend-memory" "mail-backend-smtp" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "message-test")
               (:file "send-test")
               (:file "smtp-test")
               (:file "imap-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
