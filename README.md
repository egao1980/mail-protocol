# mail-protocol

Email **compose / parse / send / IMAP** for [cl-stack](https://github.com/egao1980/cl-stack). MIME wire format is [`mime-protocol`](https://github.com/egao1980/mime-protocol).

| System | Role | OCI |
|--------|------|-----|
| `mail-protocol` (`stack-mail`) | `make-message` / `add-alternative` / `add-attachment` / `parse-message` / `send` / IMAP | **0.3.0** |
| `mail-backend-memory` | Default in tests — records sent messages | **0.3.0** |
| `mail-backend-smtp` | SMTP: AUTH PLAIN / LOGIN, STARTTLS, implicit TLS (cl-stack-ssl) | **0.3.0** |

Bcc is envelope-only (stripped from `print-message`).

## Compose (Python `email.message.EmailMessage`)

```lisp
(asdf:load-system "mail-backend-memory")

(let ((msg (stack-mail:make-message
            :from "alice@ex.com" :to "b@ex.com"
            :subject "Report" :body "See the attached report.")))
  (stack-mail:add-alternative msg "<p>See the attached <b>report</b>.</p>")    ; add_alternative
  (stack-mail:add-attachment msg (alexandria:read-file-into-byte-vector "r.pdf") ; add_attachment
                             :filename "r.pdf")                              ; → application/pdf
  (stack-mail:send msg)
  (stack-mail:print-message msg))
```

- `make-message` writes `Date` (RFC 5322, UTC: `Mon, 23 Nov 2026 21:04:05 +0000`) and `Message-ID` (`<time.sequence.pid.nonce@domain-of-From>`: a per-process counter taken under a lock, so IDs never repeat across threads, the process id and 64 random bits per process, drawn again in a forked child or a restored image). `:date` takes `t` (now, from datetime-protocol's clock), a string, an `instant` or `nil`; `:message-id` takes `t`, a string or `nil`. `format-message-date` / `make-message-id` are `email.utils.formatdate` / `make_msgid`.
- Header names print in canonical case (`From`, `Subject`, `MIME-Version`, `Message-ID`).
- `add-alternative content &key (subtype "html") charset` makes the body `multipart/alternative`. `add-attachment data &key content-type filename disposition` makes the message `multipart/mixed` (body first). Octets are base64; a string is a text attachment. The content type defaults from the filename. Either order nests as `mixed(alternative(text, html), attachments…)`.
- Reading: `message-body` (`:subtype "html"` for the HTML part), `message-attachments`, `message-date`, `message-id`.

## Send over SMTP (Python `smtplib`)

```lisp
(asdf:load-system "mail-backend-smtp")

;; smtplib.SMTP("smtp.example.com", 587); starttls(); login(user, pw)
(mail-backend-smtp:use-smtp-backend :host "smtp.example.com" :port 587 :starttls t
                                    :username "me@example.com" :password "app-password")
;; smtplib.SMTP_SSL("smtp.example.com")            ; port 465
(mail-backend-smtp:use-smtp-backend :host "smtp.example.com" :tls t
                                    :username "me@example.com" :password "app-password")
(stack-mail:send msg)
```

| keyword | default | |
|---|---|---|
| `:host` `:port` | `"127.0.0.1"`, 465 with `:tls`, else 25 | |
| `:tls` | `nil` | implicit TLS (SMTPS) |
| `:starttls` | `nil` | upgrade after EHLO; a server that does not offer STARTTLS is an error, never a plain send |
| `:username` `:password` | `nil` | AUTH PLAIN, else LOGIN, whichever the server offers first |
| `:tls-verify` | `:required` | cl+ssl's `:verify`; SNI + hostname check, OS trust store from cl-stack-ssl |
| `:ehlo-host` `:connect-timeout` | `"localhost"` `30` | |

Conditions: `mail-send-error` (`code` = SMTP reply code, `cause` = usocket / stream / cl+ssl condition, e.g. a refused connection) and its subclass `mail-auth-error` (535, or no common AUTH mechanism). `smtp-dialogue` drives the protocol on any pair of character streams (tests script it). The DATA payload is sent as smtplib sends it: every line end (CRLF, a bare LF or a bare CR) becomes CRLF before dot-stuffing, so a bare CR never reaches the wire and a body holding `<CR>.<CR>` cannot end the message early (SMTP smuggling, #7).

## IMAP

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

## Tests

`cl-repo test --system mail-protocol`. The SMTP tests in `tests/compose-send-test.lisp` run against Mailpit (AUTH on 11026, STARTTLS on 11027, implicit TLS on 11028, a test CA in `MAIL_SMTP_TEST_CA`) and skip without it; the file header has the `docker run` lines. Everything else needs no server.

Not yet: RFC 5322 address parsing for the envelope (#4) and RFC 2047 / 8-bit bodies over SMTP (#5).

## License

MIT — see [LICENSE](LICENSE).
