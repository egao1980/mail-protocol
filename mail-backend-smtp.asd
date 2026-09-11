(defsystem "mail-backend-smtp"
  :version "0.1.0"
  :description "mail-protocol backend — SMTP over usocket (smtplib-shaped)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("mail-protocol" "usocket")
  :serial t
  :pathname "src/backend-smtp"
  :components ((:file "package")
               (:file "smtp")
               (:file "backend")))
