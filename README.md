# mail-protocol

Email **compose / parse / send** for [cl-stack](https://github.com/egao1980/cl-stack). MIME wire format is [`mime-protocol`](https://github.com/egao1980/mime-protocol). **Not** IMAP / MUA.

| System | Role | OCI |
|--------|------|-----|
| `mail-protocol` (`stack-mail`) | `make-message` / `parse-message` / `send` | **0.1.0** |
| `mail-backend-memory` | Default in tests — records sent messages | **0.1.0** |
| `mail-backend-smtp` | SMTP (EHLO / MAIL / RCPT / DATA / QUIT); no AUTH/STARTTLS yet | **0.1.0** |

Bcc is envelope-only (stripped from `print-message`).

```lisp
(asdf:load-system "mail-backend-memory")

(let ((msg (stack-mail:make-message
            :from "a@ex.com" :to "b@ex.com"
            :subject "Hi" :body "hello")))
  (stack-mail:send msg)
  (stack-mail:print-message msg))
```

```lisp
(asdf:load-system "mail-backend-smtp")
(mail-backend-smtp:use-smtp-backend :host "127.0.0.1" :port 25)
(stack-mail:send msg)
```

## License

MIT — see [LICENSE](LICENSE).
