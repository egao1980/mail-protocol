(in-package #:mail-protocol/tests)

;;; egao1980/mail-protocol#17: a body's MIME charset name goes through
;;; encoding-protocol:normalize-encoding (0.4.0), so IANA names and Python's
;;; aliases decode. The octets are Python 3.13's codecs of the same name
;;; (checked 2026-11-27). The CJK multibyte charsets come from
;;; encoding-protocol/cjk, which mail-protocol loads.

(defun %body-in-charset (charset octets)
  "PARSE-MESSAGE of a text/plain message whose 8bit body is OCTETS in CHARSET."
  (parse-message
   (coerce (append (map 'list #'char-code
                        (format nil "From: a@x.example~C~%To: b@x.example~C~%Subject: c~C~%MIME-Version: 1.0~C~%Content-Type: text/plain; charset=~A~C~%Content-Transfer-Encoding: 8bit~C~%~C~%"
                                #\Return #\Return #\Return #\Return charset #\Return #\Return #\Return))
                   octets)
           '(vector (unsigned-byte 8)))))

(deftest-parametrize body-charsets-decode
    ((charset octets text)
     ("Shift_JIS" '(#x93 #xFA #x96 #x7B #x8C #xEA) (%u "~C~C~C" #x65E5 #x672C #x8A9E))
     ("shift_jis" '(#x93 #xFA #x96 #x7B #x8C #xEA) (%u "~C~C~C" #x65E5 #x672C #x8A9E))
     ("windows-31j" '(#x93 #xFA #x96 #x7B #x8C #xEA) (%u "~C~C~C" #x65E5 #x672C #x8A9E))
     ("euc-jp" '(#xC6 #xFC #xCB #xDC #xB8 #xEC) (%u "~C~C~C" #x65E5 #x672C #x8A9E))
     ("gb2312" '(#xD6 #xD0 #xCE #xC4) (%u "~C~C" #x4E2D #x6587))
     ("windows-1251" '(#xCF #xF0 #xE8 #xE2 #xE5 #xF2) (%u "~C~C~C~C~C~C" #x41F #x440 #x438 #x432 #x435 #x442))
     ("KOI8-R" '(#xF0 #xD2 #xC9 #xD7 #xC5 #xD4) (%u "~C~C~C~C~C~C" #x41F #x440 #x438 #x432 #x435 #x442))
     ("iso-2022-jp" '(#x1B #x24 #x42 #x46 #x7C #x4B #x5C #x38 #x6C #x1B #x28 #x42)
      (%u "~C~C~C" #x65E5 #x672C #x8A9E))
     ("big5" '(#xA4 #xA4 #xA4 #xE5) (%u "~C~C" #x4E2D #x6587))
     ("euc-kr" '(#xC7 #xD1 #xB1 #xB9 #xBE #xEE) (%u "~C~C~C" #xD55C #xAD6D #xC5B4)))
  (ok (equal text (string-right-trim '(#\Return #\Newline)
                                     (message-body (%body-in-charset charset octets))))
      (format nil "charset=~A decodes as Python's codec does" charset)))

(deftest unknown-charset-falls-back-to-utf-8
  (ok (equal "plain"
             (string-right-trim '(#\Return #\Newline)
                                (message-body (%body-in-charset "x-no-such-charset"
                                                                (map 'list #'char-code "plain")))))
      "a charset nothing defines reads as UTF-8, as before, not an error")
  (ok (null (mail-protocol::%charset-encoding "x-no-such-charset" nil)))
  (ok (signals (mail-protocol::%charset-encoding "x-no-such-charset") 'mail-parse-error)
      "the error form names the charset as MAIL-PARSE-ERROR")
  (ok (null (mail-protocol::%charset-encoding "base64" nil))
      "a transfer encoding is no charset"))

;;; egao1980/mail-protocol#21: encoding-protocol (>= 0.5.0) loads a CJK
;;; charset's system the first time a body needs it, so mail-protocol no
;;; longer depends on encoding-protocol/cjk. Charset names resolve through
;;; find-encoding (0.6.0), which never interns an unknown name.

(deftest cjk-charsets-load-on-demand
  (ok (not (member "encoding-protocol/cjk"
                   (mapcar (lambda (spec) (if (consp spec) (second spec) spec))
                           (asdf:system-depends-on (asdf:find-system "mail-protocol")))
                   :test #'equalp))
      "mail-protocol does not depend on encoding-protocol/cjk")
  (ok (equal (%u "~C~C" #x4E2D #x6587)
             (string-right-trim '(#\Return #\Newline)
                                (message-body (%body-in-charset "big5" '(#xA4 #xA4 #xA4 #xE5))))))
  (ok (asdf:component-loaded-p "encoding-protocol/big5")
      "the big5 body loaded encoding-protocol/big5"))

(deftest an-unknown-charset-name-is-not-interned
  (let ((charset (format nil "x-no-such-charset-~36R" (random (expt 36 8)))))
    (ok (equal "plain"
               (string-right-trim '(#\Return #\Newline)
                                  (message-body (%body-in-charset charset (map 'list #'char-code "plain"))))))
    (ok (null (mail-protocol::%charset-encoding charset nil)))
    (ok (null (find-symbol (string-upcase charset) :keyword)))))
