# mail-protocol

Email **compose / parse / send / IMAP** for [cl-stack](https://github.com/egao1980/cl-stack). MIME wire format is [`mime-protocol`](https://github.com/egao1980/mime-protocol).

| System | Role | OCI |
|--------|------|-----|
| `mail-protocol` (`stack-mail`) | `make-message` / `add-alternative` / `add-attachment` / `parse-message` / `send` / IMAP | **0.3.6** |
| `mail-backend-memory` | Default in tests — records sent messages | **0.3.6** |
| `mail-backend-smtp` | SMTP: AUTH PLAIN / LOGIN, STARTTLS, implicit TLS (cl-stack-ssl) | **0.3.6** |

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
- Addresses (`:from` `:to` `:cc` `:bcc`, a string or a list) are RFC 5322 address lists: `address-list` cuts them at the commas between addresses, not inside `"Doe, John" <john@ex.com>`, a comment or a group (`Team: a@ex.com, b@ex.com;`). `parse-address` gives the addr-spec and display name of a mailbox (`"Ann <ann@ex.com>"` → `"ann@ex.com"` `"Ann"`), `address-mailboxes` the members of a group, as `email.utils.getaddresses`. A display name keeps its dots and spacing (`J.R.R. Tolkien`). An address after a group's `;` without a comma (`Team: a@ex.com; b@ex.com`) is refused with `mail-address-error`, never dropped; an address that does not parse, ASCII or not (`Zoe Smith`, `Zoe <z@x.example>;`, `Zoe: a@x.example;;`), is refused when the message is made (#13): Python's `getaddresses` reads `('', '')` for these, so no reader would find a recipient there. A comment separates words as a blank does: `=?utf-8?q?a?= (c) =?utf-8?q?b?= <z@x.example>` reads as `a b`, as Python's headerregistry reads it.
- Non-ASCII goes out as Python's `EmailMessage` sends it: the Subject's non-ASCII words, and a non-ASCII (or unfoldable) display name of a mailbox or a group or a comment (an empty group name too), as RFC 2047 encoded-words, a non-ASCII obsolete route dropped (`=?utf-8?b?...?=`). Comments and blanks stay where they were, as `EmailMessage` keeps them (`Zoë (c) Smith <a@x.example>` → `=?utf-8?b?Wm/Dqw==?= (c) Smith <a@x.example>`, `<a(c)@x.example>` kept); a non-ASCII comment is encoded one nesting level at a time with its quoted-pairs undone, so `(c\(ö)` decodes as `c(ö` and `(Zoë (nested \) ü) x)` keeps its nesting; `message-subject`, `message-from`, `message-to` and `message-cc` decode the display names again. A non-ASCII attachment filename goes out as `filename*=utf-8''…` alone, and a long one (a 400-character CJK or 1100-character ASCII name) as RFC 2231 continuations within 78 octets a line, as Python writes them; continuations from Python or Thunderbird read back (mime-protocol 0.3.4). `add-attachment` and `add-alternative` on a parsed message keep a raw (non-UTF-8) Subject or other field as the octets it came in. A parsed body decodes in its `charset=` through `encoding-protocol:normalize-encoding` (0.4.0), so IANA names and Python's aliases work (`Shift_JIS`, `windows-31j`, `euc-jp`, `gb2312`, `KOI8-R`), and with `encoding-protocol/cjk` (a dependency) also `iso-2022-jp`, `big5`, `euc-kr`, `cp949` and `gb18030`; a charset nothing defines reads as UTF-8 (#17). The body is 8bit text in its charset (`add-alternative … :charset`, a `text/*; charset=` attachment), or quoted-printable when a line is longer than the 998 octets SMTP carries. A string attachment of another type (`application/json`) keeps that type.
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
| `:read-timeout` | `600` | seconds to wait for each reply (`nil`: no wait); a server that stops answering is `mail-send-error` naming the reply. 600 is RFC 5321 §4.5.3.2's longest client timeout (the reply to the end of DATA), so a slow server that has taken the message is not given up on |

The envelope is what smtplib's `send_message` uses: `envelope-sender` is the addr-spec of `Sender`, else of the first `From` mailbox (`""`, the null reverse-path `MAIL FROM:<>` for bounces, when that field is empty or its mailbox is `<>`, as in `Mailer Daemon <>` or `<> (bounce)`), and `envelope-recipients` the addr-specs of every `To`, `Cc` and `Bcc` mailbox, group members included, each once, `<>` refused (the local part is case-sensitive, the domain is not, RFC 5321 §2.4). `From: Ann <ann@ex.com>` goes out as `MAIL FROM:<ann@ex.com>` (#4). Both are worked out before connecting, so a bad address sends nothing.

No SMTP command carries CR or LF: an address that holds one is refused when the message is made, `smtp-dialogue` reduces every path argument to its addr-spec, and any command line with CR or LF is refused with `mail-send-error` before it is written, as smtplib's `putcmd` does (#8). AUTH PLAIN refuses a NUL in the credentials. Once the server has taken the message (250 after DATA), the send has succeeded: a missing or odd reply to QUIT, or a dropped connection, is not an error, so a retry never delivers twice. `MAIL FROM` gets `BODY=8BITMIME` when the data has 8-bit octets and the server offers it, and `SMTPUTF8` for a non-ASCII envelope address (a server without SMTPUTF8 is an error).

Conditions: `mail-send-error` (`code` = SMTP reply code, `cause` = usocket / stream / cl+ssl condition, e.g. a refused connection) and its subclass `mail-auth-error` (535, or no common AUTH mechanism). `smtp-dialogue` drives the protocol on any pair of character streams, one character per octet (tests script it), its `:await-reply` function called before each reply (the backend's waits on the socket for `:read-timeout`); the backend's streams are ISO-8859-1, so `print-message`'s octets reach the wire as they are (#5). The DATA payload is sent as smtplib sends it: every line end (CRLF, a bare LF or a bare CR) becomes CRLF before dot-stuffing, so a bare CR never reaches the wire and a body holding `<CR>.<CR>` cannot end the message early (SMTP smuggling, #7).

## IMAP

IMAP (`imap-client`): `imap-connect` `imap-login` `imap-select` `imap-fetch` `imap-search` `imap-append` `imap-store` `imap-capability` `imap-enable` `imap-idle`, and `imap-command` for any other command. Inject a stream or `io-fn` — no live server required. FETCH RFC822 bodies go through `parse-message`. Conditions: `imap-error`, `imap-auth-error`, `imap-argument-error`.

Every argument is checked before anything is written (#9). CR, LF or NUL in a user, password, mailbox name, search criteria, sequence set or FETCH items is refused with `imap-argument-error`, as Python's `imaplib` refuses it: a line break would end the command and the rest would reach the server as another tagged command. A string goes as an atom when every character is an ASTRING-CHAR, else quoted (`\` and `"` escaped), as `imaplib`'s `_astring` does, with one deliberate difference: `_astring` passes a value that already starts and ends with `"` through as it is, while this client quotes it again (`"\"x\""`), so a value is always a value and never command syntax (use `imap-raw` for syntax); 8-bit text, or more than 1024 octets, goes as a literal (`{n}`, then the value after the server's `+` continuation; a tagged NO instead ends the command there). Mailbox names are text (`"Entwürfe"`) and go in modified UTF-7 (`Entw&APw-rfe`, RFC 3501 §5.1.3; `imap-utf7-encode` / `imap-utf7-decode`), or as UTF-8 quoted strings once `(imap-enable c "UTF8=ACCEPT")` succeeds. `imap-search` takes search-key syntax as a string (`"FROM alice UNSEEN"`, 7-bit) or a list whose strings are quoted for you (`'(:from "Zoe Smith" :subject "Grüße")`, non-ASCII as literals under `CHARSET UTF-8`). Response literals on untagged lines are read by size, so a body line never passes for the tagged reply; raw text that would end the command in `{n}` is refused. Give the client a binary stream (`(unsigned-byte 8)`, e.g. `usocket:socket-connect … :element-type '(unsigned-byte 8)`; the portable choice), a Latin-1 character stream (both are octet mode: UTF-8 octets out, exact literal sizes, and any reply text reads, one character per octet, such as GreenMail's Latin-1 answer to a second `CREATE` of `Entwürfe`), or a strict UTF-8 one; a replacement character or another charset is refused, and an undecodable octet on a UTF-8 stream is an `imap-error` that leaves the client `:disconnected`. Fetched bodies are parsed from their octets.

`imap-select` sets `imap-client-selected` and state `:selected` only on OK; NO or BAD is an `imap-error` and leaves no mailbox selected (a selected client goes back to `:authenticated`, as `imaplib` drops to `AUTH`; RFC 3501 §6.3.1). `imap-utf7-decode` takes only the canonical form `imap-utf7-encode` writes (RFC 3501 §5.1.3): printable ASCII in base64 (`&AGE-`), leftover or non-zero padding bits (`&A-`, `&AOR-`), a `/` or `=` in a run, adjacent runs, a lone surrogate, raw 8-bit or control characters, and a CR, LF or NUL in the result are each an `imap-error` (#16).

`imap-append` sends a message (a `message`, as `print-message` writes it; a string, as UTF-8; or octets) as a literal, its bare CRs and LFs made CRLF as `imaplib`'s `append` does, with optional `:flags` (`'("\\Seen")`) and `:date` (`"27-Nov-2026 10:00:00 +0000"`). `imap-store` sets flags: `(imap-store c 2 '("\\Deleted"))` sends `STORE 2 +FLAGS (\Deleted)`; `:mode :remove` / `:replace`, `:silent t` (`.SILENT`) and `:uid t` (`UID STORE`). A literal is the one place CR LF may go: `imap-literal` makes one for `imap-command` (a string or octets, no NUL), while every other argument, flags and dates included, still refuses CR, LF and NUL. NO or BAD to either is an `imap-error`.

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

`cl-repo test --system mail-protocol`. The SMTP tests in `tests/compose-send-test.lisp` run against Mailpit (AUTH on 11026, STARTTLS on 11027, implicit TLS on 11028, a test CA in `MAIL_SMTP_TEST_CA`) and skip without it; the file header has the `docker run` lines. `live-display-name-send` and `live-non-ascii-send` use a plain Mailpit (`MAIL_SMTP_TEST_PLAIN_PORT`, default 11025). The IMAP live tests in `tests/imap-live-test.lisp` run against GreenMail (`MAIL_IMAP_TEST_PORT`, default 23143; `MAIL_IMAP_TEST_SMTP_PORT`, default 23025) and skip without it; the file header has the `docker run` line. Everything else needs no server: capture servers on an ephemeral port record the wire.

## License

MIT — see [LICENSE](LICENSE).
