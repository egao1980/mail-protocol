# mail-protocol

Email **compose / parse / send / IMAP** for [cl-stack](https://github.com/egao1980/cl-stack). MIME wire format is [`mime-protocol`](https://github.com/egao1980/mime-protocol).

| System | Role | OCI |
|--------|------|-----|
| `mail-protocol` (`stack-mail`) | `make-message` / `add-alternative` / `add-attachment` / `parse-message` / `send` / IMAP | **0.3.2** |
| `mail-backend-memory` | Default in tests — records sent messages | **0.3.2** |
| `mail-backend-smtp` | SMTP: AUTH PLAIN / LOGIN, STARTTLS, implicit TLS (cl-stack-ssl) | **0.3.2** |

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
- Header names print in canonical case (`From`, `Subject`, `MIME-Version`, `Message-ID`). Header lines fold at 78 (RFC 5322 §2.1.1).
- Addresses (`:from` `:to` `:cc` `:bcc`, a string or a list) are RFC 5322 address lists: `address-list` cuts them at the commas between addresses, not inside `"Doe, John" <john@ex.com>`, a comment or a group (`Team: a@ex.com, b@ex.com;`). `parse-address` gives the addr-spec and display name of a mailbox (`"Ann <ann@ex.com>"` → `"ann@ex.com"` `"Ann"`), `address-mailboxes` the members of a group, as `email.utils.getaddresses`. A display name keeps its dots and spacing (`J.R.R. Tolkien`). An address after a group's `;` without a comma (`Team: a@ex.com; b@ex.com`) is refused with `mail-address-error`, never dropped; a non-ASCII address that does not parse is refused when the message is made, since it has no form for the header.
- Non-ASCII goes out as Python's `EmailMessage` sends it: the Subject's non-ASCII words, and a non-ASCII (or unfoldable) display name of a mailbox or a group or a comment (an empty group name too), as RFC 2047 encoded-words, a non-ASCII obsolete route dropped (`=?utf-8?b?...?=`); `message-subject`, `message-from`, `message-to` and `message-cc` decode the display names again. A non-ASCII attachment filename goes out as `filename*=utf-8''…` alone, and a long one (a 400-character CJK or 1100-character ASCII name) as RFC 2231 continuations within 78 octets a line, as Python writes them; continuations from Python or Thunderbird read back (mime-protocol 0.3.4). `add-attachment` and `add-alternative` on a parsed message keep a raw (non-UTF-8) Subject or other field as the octets it came in. The body is 8bit text in its charset (`add-alternative … :charset`, a `text/*; charset=` attachment), or quoted-printable when a line is longer than the 998 octets SMTP carries. A string attachment of another type (`application/json`) keeps that type.
- A subtype (`:subtype`) and a disposition (`add-attachment … :disposition`) must be tokens: `"attachment; filename=x.exe"` signals `mail-parse-error` rather than add a parameter.
- CR, LF or NUL in an address signals `mail-address-error` (a `mail-parse-error`, restart `use-value`); in the Subject or another header, mime-protocol's `mime-header-error`. A line break would add header fields or SMTP commands (Python: `ValueError`).
- `print-message` returns octets, one character per octet (mime-protocol's `print-mime`): write it to the wire through ISO-8859-1.
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

The envelope is what smtplib's `send_message` uses: `envelope-sender` is the addr-spec of `Sender`, else of the first `From` mailbox (`""`, the null reverse-path `MAIL FROM:<>` for bounces, when that field is empty or its mailbox is `<>`, as in `Mailer Daemon <>` or `<> (bounce)`), and `envelope-recipients` the addr-specs of every `To`, `Cc` and `Bcc` mailbox, group members included, each once, `<>` refused (the local part is case-sensitive, the domain is not, RFC 5321 §2.4). `From: Ann <ann@ex.com>` goes out as `MAIL FROM:<ann@ex.com>` (#4). Both are worked out before connecting, so a bad address sends nothing.

No SMTP command carries CR or LF: an address that holds one is refused when the message is made, `smtp-dialogue` reduces every path argument to its addr-spec, and any command line with CR or LF is refused with `mail-send-error` before it is written, as smtplib's `putcmd` does (#8). AUTH PLAIN refuses a NUL in the credentials. Once the server has taken the message (250 after DATA), the send has succeeded: a missing or odd reply to QUIT, or a dropped connection, is not an error, so a retry never delivers twice. `MAIL FROM` gets `BODY=8BITMIME` when the data has 8-bit octets and the server offers it, and `SMTPUTF8` for a non-ASCII envelope address (a server without SMTPUTF8 is an error).

Conditions: `mail-send-error` (`code` = SMTP reply code, `cause` = usocket / stream / cl+ssl condition, e.g. a refused connection) and its subclass `mail-auth-error` (535, or no common AUTH mechanism). `smtp-dialogue` drives the protocol on any pair of character streams, one character per octet (tests script it); the backend's streams are ISO-8859-1, so `print-message`'s octets reach the wire as they are (#5). The DATA payload is sent as smtplib sends it: every line end (CRLF, a bare LF or a bare CR) becomes CRLF before dot-stuffing, so a bare CR never reaches the wire and a body holding `<CR>.<CR>` cannot end the message early (SMTP smuggling, #7).

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

`cl-repo test --system mail-protocol`. The SMTP tests in `tests/compose-send-test.lisp` run against Mailpit (AUTH on 11026, STARTTLS on 11027, implicit TLS on 11028, a test CA in `MAIL_SMTP_TEST_CA`) and skip without it; the file header has the `docker run` lines. `live-display-name-send` and `live-non-ascii-send` use a plain Mailpit (`MAIL_SMTP_TEST_PLAIN_PORT`, default 11025). Everything else needs no server: capture servers on an ephemeral port record the wire.

## License

MIT — see [LICENSE](LICENSE).
