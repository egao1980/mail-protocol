(defsystem "mail-backend-smtp"
  :version "0.3.6"
  :description "mail-protocol backend — SMTP over usocket, AUTH PLAIN/LOGIN, STARTTLS and implicit TLS through cl-stack-ssl (smtplib-shaped)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "mail-protocol" "0.3.6") "usocket" "cl-stack-ssl" "flexi-streams"
               "encoding-protocol")
  :serial t
  :pathname "src/backend-smtp"
  :components ((:file "package")
               (:file "smtp")
               (:file "backend")))
