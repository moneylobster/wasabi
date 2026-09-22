;;; wasabi-replies-test.el --- Tests for replies  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-replies-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Sending is stubbed: there is no wuzapi to talk to.

;;; Code:

(require 'ert)
(require 'wasabi)

(defmacro wasabi-replies-test--isolated (&rest body)
  "Run BODY with no remembered JIDs or sent quotes, and a scratch data dir."
  (declare (indent 0))
  `(let ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (wasabi-chat--sent-quotes nil)
         (wasabi-chat--own-jids '())
         (wasabi--own-jid nil))
     ,@body))

(defun wasabi-replies-test--row (id text &rest info)
  "A stored history row with ID and TEXT; INFO is a plist of Info overrides.
:quote (STANZA PARTICIPANT QUOTED-TEXT) makes it a reply."
  (let* ((quote-spec (plist-get info :quote))
         (message (if quote-spec
                      `((extendedTextMessage
                         . ((text . ,text)
                            (contextInfo
                             . ((stanzaID . ,(nth 0 quote-spec))
                                (participant . ,(nth 1 quote-spec))
                                (quotedMessage
                                 . ((conversation . ,(nth 2 quote-spec)))))))))
                    `((conversation . ,text)))))
    `((message_id . ,id)
      (chat_jid . "447123456789@s.whatsapp.net")
      (sender_jid . ,(or (plist-get info :sender) "447123456789@s.whatsapp.net"))
      (timestamp . "2026-09-22T12:00:00Z")
      (data_json
       . ,(json-encode
           `((Info . ((ID . ,id)
                      (Chat . "447123456789@s.whatsapp.net")
                      (Sender . ,(or (plist-get info :sender)
                                     "447123456789@s.whatsapp.net"))
                      (IsFromMe . ,(if (plist-get info :from-me) t :json-false))
                      (Timestamp . ,(or (plist-get info :time)
                                        "2026-09-22T12:00:00Z"))))
             (Message . ,message)))))))

(defmacro wasabi-replies-test--in-chat (rows &rest body)
  "Run BODY in a chat buffer showing the stored ROWS."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer "*wasabi-replies-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (wasabi-chat-mode)
           (setq wasabi-chat--chat
                 (wasabi-chat--make-chat :chat-jid "447123456789@s.whatsapp.net"
                                         :contact-name "John"))
           (wasabi-chat--refresh
            (wasabi-chat--parse-messages ,rows
                                         :chat-jid "447123456789@s.whatsapp.net"
                                         :contact-name "John"))
           ,@body)
       (kill-buffer buffer))))

;;; Reading a quote

(ert-deftest wasabi-replies-test-quote-from-text ()
  (let ((quote-info (wasabi-chat--quote
                     '((extendedTextMessage
                        . ((text . "sure")
                           (contextInfo . ((stanzaID . "ORIG1")
                                           (participant . "447123456789@s.whatsapp.net")
                                           (quotedMessage . ((conversation . "lunch?")))))))))))
    (should (equal (map-elt quote-info :id) "ORIG1"))
    (should (equal (map-elt quote-info :participant) "447123456789@s.whatsapp.net"))
    (should (equal (map-elt quote-info :text) "lunch?"))))

(ert-deftest wasabi-replies-test-quote-from-media ()
  ;; A captioned photo can be a reply as well as text can.
  (let ((quote-info (wasabi-chat--quote
                     '((imageMessage
                        . ((caption . "this one")
                           (contextInfo . ((stanzaID . "ORIG2")
                                           (participant . "1@lid")
                                           (quotedMessage
                                            . ((imageMessage . ((caption . "which?")))))))))))))
    (should (equal (map-elt quote-info :id) "ORIG2"))
    (should (equal (map-elt quote-info :text) "[image] which?"))))

(ert-deftest wasabi-replies-test-no-quote ()
  (should-not (wasabi-chat--quote '((conversation . "hi"))))
  (should-not (wasabi-chat--quote '((extendedTextMessage . ((text . "hi"))))))
  ;; Context without a quoted ID, as forwards and mentions carry.
  (should-not (wasabi-chat--quote
               '((extendedTextMessage . ((text . "hi")
                                         (contextInfo . ((isForwarded . t))))))))
  (should-not (wasabi-chat--quote nil)))

(ert-deftest wasabi-replies-test-summaries ()
  (should (equal (wasabi-chat--summarize '((conversation . "hello"))) "hello"))
  (should (equal (wasabi-chat--summarize '((extendedTextMessage . ((text . "a\nb")))))
                 "a b"))
  (should (equal (wasabi-chat--summarize '((stickerMessage . ((URL . "x"))))) "[sticker]"))
  (should (equal (wasabi-chat--summarize '((documentMessage . ((fileName . "cv.pdf")))))
                 "[document] cv.pdf"))
  ;; What wuzapi stores for a quote it had no text for.
  (should (equal (wasabi-chat--summarize '((conversation . ""))) "[message]"))
  (should (equal (wasabi-chat--summarize nil) "[message]"))
  (should (<= (string-width (wasabi-chat--summarize
                             `((conversation . ,(make-string 200 ?x)))))
              80)))

;;; Showing it

(ert-deftest wasabi-replies-test-reply-shows-its-quote ()
  (wasabi-replies-test--isolated
    (wasabi-replies-test--in-chat
        (list (wasabi-replies-test--row "ORIG1" "lunch?")
              (wasabi-replies-test--row "REPLY1" "sure"
                                        :sender "999@s.whatsapp.net" :from-me t
                                        :time "2026-09-22T12:05:00Z"
                                        :quote '("ORIG1" "447123456789@s.whatsapp.net" "lunch?")))
      ;; The author comes from the quoted message itself.
      (should (string-match-p "│ John: lunch\\?" (buffer-string)))
      ;; Above the reply's own text.
      (should (< (string-match "│ John" (buffer-string))
                 (string-match "sure" (buffer-string)))))))

(ert-deftest wasabi-replies-test-quoting-yourself ()
  (wasabi-replies-test--isolated
    (wasabi-replies-test--in-chat
        (list (wasabi-replies-test--row "MINE1" "coming?"
                                        :sender "999@s.whatsapp.net" :from-me t)
              (wasabi-replies-test--row "REPLY2" "yes"
                                        :time "2026-09-22T12:05:00Z"
                                        :quote '("MINE1" "999@s.whatsapp.net" "coming?")))
      (should (string-match-p "│ You: coming\\?" (buffer-string))))))

(ert-deftest wasabi-replies-test-quote-of-something-not-loaded ()
  (wasabi-replies-test--isolated
    (let ((wasabi--own-jid "999@s.whatsapp.net"))
      (wasabi-replies-test--in-chat
          (list (wasabi-replies-test--row "REPLY3" "yes"
                                          :quote '("OLD1" "999:12@s.whatsapp.net" "ancient")))
        ;; Not in the chat, but known to be ours from the JID.
        (should (string-match-p "│ You: ancient" (buffer-string)))))))

(ert-deftest wasabi-replies-test-goto-quoted ()
  (wasabi-replies-test--isolated
    (wasabi-replies-test--in-chat
        (list (wasabi-replies-test--row "ORIG1" "lunch?")
              (wasabi-replies-test--row "REPLY1" "sure"
                                        :time "2026-09-22T12:05:00Z"
                                        :quote '("ORIG1" "447123456789@s.whatsapp.net" "lunch?")))
      (goto-char (point-max))
      (wasabi-chat-goto-quoted "ORIG1")
      (should (equal (get-text-property (point) 'wasabi-message-id) "ORIG1"))
      (goto-char (point-max))
      (wasabi-chat-goto-quoted "NOT-LOADED")
      ;; Stays put when there is nowhere to go.
      (should (equal (point) (point-max))))))

;;; Replying

(ert-deftest wasabi-replies-test-reply-at-point ()
  (wasabi-replies-test--isolated
    (wasabi-replies-test--in-chat
        (list (wasabi-replies-test--row "ORIG1" "lunch?")
              (wasabi-replies-test--row "LATER" "also"
                                        :time "2026-09-22T12:05:00Z"))
      ;; On the first message's text.
      (goto-char (point-min))
      (search-forward "lunch?")
      (wasabi-chat-reply)
      (should (equal (map-elt (map-elt wasabi-chat--chat :reply-to) :message-id) "ORIG1"))
      ;; Off to the prompt to write it.
      (should (wasabi-chat--in-input-area-p))
      (should (string-match-p "replying to John: lunch\\?"
                              (substring-no-properties header-line-format)))
      (wasabi-chat-cancel-reply)
      (should-not (map-elt wasabi-chat--chat :reply-to))
      (should-not (string-match-p "replying to"
                                  (substring-no-properties header-line-format))))))

(ert-deftest wasabi-replies-test-reply-from-prompt-takes-the-latest ()
  (wasabi-replies-test--isolated
    (wasabi-replies-test--in-chat
        (list (wasabi-replies-test--row "ORIG1" "lunch?")
              (wasabi-replies-test--row "LATER" "also"
                                        :time "2026-09-22T12:05:00Z"))
      (goto-char (point-max))
      (wasabi-chat-reply)
      (should (equal (map-elt (map-elt wasabi-chat--chat :reply-to) :message-id) "LATER")))))

(ert-deftest wasabi-replies-test-r-types-in-the-input ()
  (wasabi-replies-test--isolated
    (wasabi-replies-test--in-chat
        (list (wasabi-replies-test--row "ORIG1" "lunch?"))
      (goto-char (point-max))
      (let ((last-command-event ?r))
        (wasabi-chat-reply-or-insert))
      (should (equal (wasabi-chat--get-prompt-input) "r"))
      (should-not (map-elt wasabi-chat--chat :reply-to)))))

(ert-deftest wasabi-replies-test-participant ()
  (wasabi-replies-test--isolated
    (let ((wasabi--own-jid "999@s.whatsapp.net"))
      ;; Theirs: whoever sent it, without a device suffix.
      (should (equal (wasabi-chat--reply-participant
                      '((:from-me) (:sender-jid . "447123456789:3@s.whatsapp.net")))
                     "447123456789@s.whatsapp.net"))
      ;; Ours, including one sent from here, stored as "me".
      (should (equal (wasabi-chat--reply-participant
                      '((:from-me . t) (:sender-jid . "me")))
                     "999@s.whatsapp.net")))))

(ert-deftest wasabi-replies-test-sends-as-a-reply ()
  (wasabi-replies-test--isolated
    (let ((wasabi-buffer (get-buffer-create "*Wasabi*"))
          (sent nil)
          (appended nil))
      (unwind-protect
          (cl-letf (((symbol-function 'wasabi--send-chat-send-text-request)
                     (lambda (&rest args)
                       (setq sent args)
                       (funcall (plist-get args :on-success)
                                '((Details . "Sent") (Timestamp . 1790000000)
                                  (Id . "3EBREPLY")))))
                    ((symbol-function 'wasabi-chat--append-message)
                     (lambda (message) (setq appended message))))
            (wasabi-replies-test--in-chat
                (list (wasabi-replies-test--row "ORIG1" "lunch?"))
              (goto-char (point-min))
              (search-forward "lunch?")
              (wasabi-chat-reply)
              (goto-char (point-max))
              (insert "sure")
              (wasabi-chat-send-input)
              ;; Out as a reply to it.
              (should (equal (map-nested-elt sent '(:context-info StanzaID)) "ORIG1"))
              (should (equal (map-nested-elt sent '(:context-info Participant))
                             "447123456789@s.whatsapp.net"))
              (should (equal (plist-get sent :quoted-text) "lunch?"))
              ;; Shown with its quote, and done replying.
              (should (equal (map-nested-elt appended '(:quote :id)) "ORIG1"))
              (should (equal (map-elt appended :message-id) "3EBREPLY"))
              (should-not (map-elt wasabi-chat--chat :reply-to))
              ;; Kept, since wuzapi keeps only the text of what we send.
              (should (equal (map-elt (gethash "3EBREPLY" (wasabi-chat--sent-quotes)) :id)
                             "ORIG1"))))
        (kill-buffer wasabi-buffer)))))

(ert-deftest wasabi-replies-test-plain-send-is-no-reply ()
  (wasabi-replies-test--isolated
    (let ((wasabi-buffer (get-buffer-create "*Wasabi*"))
          (sent nil))
      (unwind-protect
          (cl-letf (((symbol-function 'wasabi--send-chat-send-text-request)
                     (lambda (&rest args) (setq sent args)))
                    ((symbol-function 'wasabi-chat--append-message) #'ignore))
            (wasabi-replies-test--in-chat
                (list (wasabi-replies-test--row "ORIG1" "lunch?"))
              (goto-char (point-max))
              (insert "hello")
              (wasabi-chat-send-input)
              (should-not (plist-get sent :context-info))
              (should-not (plist-get sent :quoted-text))))
        (kill-buffer wasabi-buffer)))))

(ert-deftest wasabi-replies-test-sent-reply-keeps-its-quote-on-reload ()
  (wasabi-replies-test--isolated
    (wasabi-chat--remember-sent-quote
     "3EBREPLY" '((:id . "ORIG1") (:participant . "447123456789@s.whatsapp.net")
                  (:text . "lunch?")))
    ;; Survives a restart.
    (setq wasabi-chat--sent-quotes nil)
    ;; What wuzapi stored for it: the text, and nothing more.
    (wasabi-replies-test--in-chat
        (list (wasabi-replies-test--row "ORIG1" "lunch?")
              `((message_id . "3EBREPLY") (sender_jid . "me") (message_type . "text")
                (text_content . "sure") (timestamp . "2026-09-22T12:05:00Z")
                (data_json . "")))
      (should (string-match-p "│ John: lunch\\?" (buffer-string))))))

(ert-deftest wasabi-replies-test-own-jids ()
  (wasabi-replies-test--isolated
    (wasabi--remember-own-jid "999:47@s.whatsapp.net")
    (should (equal wasabi--own-jid "999@s.whatsapp.net"))
    ;; Learned from history too, in whatever addressing it used.
    (wasabi-chat--reply-fields (wasabi-replies-test--row "MINE" "x"
                                                          :sender "555@lid" :from-me t))
    (should (wasabi-chat--own-jid-p "555@lid"))
    (should (wasabi-chat--own-jid-p "999:3@s.whatsapp.net"))
    (should-not (wasabi-chat--own-jid-p "447123456789@s.whatsapp.net"))))

(ert-deftest wasabi-replies-test-keys ()
  (should (eq (lookup-key wasabi-chat-mode-map (kbd "r")) #'wasabi-chat-reply-or-insert))
  (should (eq (lookup-key wasabi-chat-mode-map (kbd "C-c C-r")) #'wasabi-chat-reply))
  (should (eq (lookup-key wasabi-chat-mode-map (kbd "C-c C-k")) #'wasabi-chat-cancel-reply)))

(provide 'wasabi-replies-test)
;;; wasabi-replies-test.el ends here
