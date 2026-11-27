(defsystem "mail-backend-memory"
  :version "0.3.6"
  :description "mail-protocol backend — record sends in memory"
  :author "egao1980"
  :license "MIT"
  :depends-on ("mail-protocol")
  :serial t
  :pathname "src/backend-memory"
  :components ((:file "package")
               (:file "backend")))
