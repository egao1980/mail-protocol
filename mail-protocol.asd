(defsystem "mail-protocol"
  :version "0.3.4"
  :description "Email message + send GFs + IMAP (stack-mail); MIME via mime-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "mime-protocol" "0.3.4") (:version "encoding-protocol" "0.3.0")
               "datetime-protocol" "bordeaux-threads")
  :properties (:cl-repo (:ci (:with ("mail-backend-memory" "mail-backend-smtp"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "address")
               (:file "message")
               (:file "protocol")
               (:file "imap"))
  :in-order-to ((test-op (test-op "mail-protocol/tests"))))

(defsystem "mail-protocol/tests"
  :depends-on ("mail-protocol" "mail-backend-memory" "mail-backend-smtp" "rove"
               "usocket" "cl-stack-ssl" "datetime-protocol" "flexi-streams")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "message-test")
               (:file "send-test")
               (:file "smtp-test")
               (:file "imap-test")
               (:file "compose-send-test")
               (:file "compose-unit-test")
               (:file "message-id-threads-test")
               (:file "evidence-r2-test")
               (:file "round3-test")
               (:file "evidence-r3-test")
               (:file "utf8-wire-test")
               (:file "envelope-header-test")
               (:file "adversarial-r4-test")
               (:file "round4-fixes-test")
               (:file "issue-11-test")
               (:file "issue-11-break-3-test")
               (:file "imap-injection-test")
               (:file "imap-live-test")
               (:file "imap-leftovers-test"))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "tests failed for ~A" (component-name c)))))
