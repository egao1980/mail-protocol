# mail-protocol

Email **compose / parse / send / IMAP** for [cl-stack](https://github.com/egao1980/cl-stack). MIME wire format is [`mime-protocol`](https://github.com/egao1980/mime-protocol).

| System | Role | OCI |
|--------|------|-----|
| `mail-protocol` (`stack-mail`) | `make-message` / `parse-message` / `send` / IMAP | **0.2.0** |
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

IMAP (`imap-client`): `imap-connect` `imap-login` `imap-select` `imap-fetch` `imap-search` `imap-idle`. Inject a stream or `io-fn` — no live server required. FETCH RFC822 bodies go through `parse-message`. Conditions: `imap-error`, `imap-auth-error`.

```lisp
(let ((c (stack-mail:make-imap-client
          :io-fn (lambda (cmd) …))))
  (stack-mail:imap-connect c)
  (stack-mail:imap-login c "user" "pass")
  (stack-mail:imap-select c "INBOX")
  (stack-mail:imap-search c "ALL")
  (stack-mail:imap-fetch c "1" :items "RFC822"))
```

## License

MIT — see [LICENSE](LICENSE).
