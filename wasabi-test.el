;;; wasabi-test.el --- Tests for wasabi  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wasabi)

(defmacro wasabi-test--with-clean-jids (&rest body)
  "Run BODY with an empty, throwaway JID alias store."
  (declare (indent 0))
  `(let ((wasabi--jid-canonical-table (make-hash-table :test 'equal))
         (wasabi--jid-variants-table (make-hash-table :test 'equal))
         (wasabi--push-names-table (make-hash-table :test 'equal))
         (wasabi--chat-times-table (make-hash-table :test 'equal))
         (wasabi-data-dir (make-temp-file "wasabi-test" t)))
     ,@body))

;;; JID normalization

(ert-deftest wasabi-test-normalize-jid ()
  (should (equal (wasabi--normalize-jid "447123456789@s.whatsapp.net")
                 "447123456789@s.whatsapp.net"))
  ;; Device and agent suffixes are dropped.
  (should (equal (wasabi--normalize-jid "447123456789:12@s.whatsapp.net")
                 "447123456789@s.whatsapp.net"))
  (should (equal (wasabi--normalize-jid "447123456789.0:5@s.whatsapp.net")
                 "447123456789@s.whatsapp.net"))
  (should (equal (wasabi--normalize-jid "120363000000000000@g.us")
                 "120363000000000000@g.us"))
  ;; Symbols (JSON object keys) are accepted.
  (should (equal (wasabi--normalize-jid (intern "99988877@lid")) "99988877@lid"))
  (should (equal (wasabi--normalize-jid nil) nil))
  (should (equal (wasabi--normalize-jid "") nil)))

(ert-deftest wasabi-test-jid-identifier ()
  (should (equal (wasabi--jid-identifier "447123456789@s.whatsapp.net")
                 "447123456789"))
  (should (equal (wasabi--jid-identifier nil) nil)))

;;; Learning LID/phone-number pairings

(ert-deftest wasabi-test-learn-jid-alias-prefers-phone-number ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    ;; The phone number JID wins: that is what the send API expects.
    (should (equal (wasabi--canonical-jid "99988877@lid")
                   "447123456789@s.whatsapp.net"))
    (should (equal (wasabi--canonical-jid "447123456789@s.whatsapp.net")
                   "447123456789@s.whatsapp.net"))
    (should (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))
    ;; Device suffixes do not break the match.
    (should (wasabi--same-chat-p "99988877:3@lid" "447123456789@s.whatsapp.net"))
    (should-not (wasabi--same-chat-p "99988877@lid" "447999999999@s.whatsapp.net"))))

(ert-deftest wasabi-test-learn-jid-alias-never-merges-groups ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "120363000000000000@g.us" "99988877@lid")
    (should-not (wasabi--same-chat-p "120363000000000000@g.us" "99988877@lid"))))

(ert-deftest wasabi-test-learn-jid-alias-is-transitive ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (should (equal (length (wasabi--jid-variants "99988877@lid")) 2))
    ;; Nothing new to learn the second time around.
    (should-not (wasabi--learn-jid-alias "99988877@lid"
                                         "447123456789@s.whatsapp.net"))))

(ert-deftest wasabi-test-learn-from-incoming-message-info ()
  (wasabi-test--with-clean-jids
    ;; An incoming one-to-one message addressed by LID.
    (wasabi--learn-from-message-info
     '((Chat . "99988877@lid")
       (Sender . "99988877@lid")
       (SenderAlt . "447123456789@s.whatsapp.net")
       (IsFromMe . nil)
       (IsGroup . nil)))
    (should (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))))

(ert-deftest wasabi-test-learn-from-outgoing-message-info ()
  (wasabi-test--with-clean-jids
    ;; A message we sent: the peer's other addressing is in RecipientAlt.
    (wasabi--learn-from-message-info
     '((Chat . "99988877@lid")
       (Sender . "1111@lid")
       (RecipientAlt . "447123456789@s.whatsapp.net")
       (IsFromMe . t)
       (IsGroup . nil)))
    (should (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))))

(ert-deftest wasabi-test-learn-from-group-message-info ()
  (wasabi-test--with-clean-jids
    ;; In a group, only the participant is aliased, never the group.
    (wasabi--learn-from-message-info
     '((Chat . "120363000000000000@g.us")
       (Sender . "99988877@lid")
       (SenderAlt . "447123456789@s.whatsapp.net")
       (IsFromMe . nil)
       (IsGroup . t)))
    (should (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))
    (should (equal (wasabi--canonical-jid "120363000000000000@g.us")
                   "120363000000000000@g.us"))))

(ert-deftest wasabi-test-jid-aliases-round-trip-on-disk ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (wasabi--save-jid-aliases)
    (clrhash wasabi--jid-canonical-table)
    (clrhash wasabi--jid-variants-table)
    (should-not (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))
    (wasabi--load-jid-aliases)
    (should (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))))

;;; Contact lookup

(ert-deftest wasabi-test-find-contact-across-jid-variants ()
  (wasabi-test--with-clean-jids
    (let ((contacts (list (cons (intern "99988877@lid")
                                '((:full-name . "John Smith")
                                  (:push-name . "Johnny"))))))
      ;; Contacts are keyed by LID; the chat index uses the phone number.
      (should-not (wasabi--contact-display-name "447123456789@s.whatsapp.net"
                                                contacts))
      (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
      (should (equal (wasabi--contact-display-name "447123456789@s.whatsapp.net"
                                                   contacts)
                     "John Smith")))))

;;; Timestamps

(ert-deftest wasabi-test-parse-timestamp ()
  ;; ISO 8601, as message events use.
  (should (wasabi--parse-timestamp "2025-11-11T12:00:00Z"))
  ;; Go's default format, as the chat index uses.
  (should (wasabi--parse-timestamp "2025-11-11 12:00:00.000000 +0000 GMT"))
  (should-not (wasabi--parse-timestamp nil))
  (should-not (wasabi--parse-timestamp ""))
  (should-not (wasabi--parse-timestamp "not a timestamp")))

(ert-deftest wasabi-test-timestamp-ordering-across-formats ()
  (let ((iso "2025-11-11T12:00:00Z")
        (go "2025-11-12 12:00:00.000000 +0000 GMT"))
    (should (wasabi--timestamp-older-p iso go))
    (should-not (wasabi--timestamp-older-p go iso))
    (should (wasabi--timestamp-newer-p go iso))
    ;; An undated chat sorts to the bottom of the list.
    (should (wasabi--timestamp-newer-p iso nil))
    (should-not (wasabi--timestamp-newer-p nil iso))
    ;; An undated message sorts to the top of a conversation, rather
    ;; than to the bottom where it would read as the latest thing said.
    (should (wasabi--timestamp-older-p nil iso))
    (should-not (wasabi--timestamp-older-p iso nil))))

;;; Chat index

(defun wasabi-test--index-entry (jid updated)
  "Build a protocol chat index entry for JID updated at UPDATED."
  (cons jid `((chat_jid . ,jid) (last_updated . ,updated))))

(ert-deftest wasabi-test-chat-index-merges-lid-and-phone-number ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (let* ((contacts (list (cons (intern "99988877@lid")
                                 '((:full-name . "John Smith")))))
           (index (wasabi--parse-chat-index
                   (list (wasabi-test--index-entry "447123456789@s.whatsapp.net"
                                                   "2025-11-11T10:00:00Z")
                         (wasabi-test--index-entry "99988877@lid"
                                                   "2025-11-11T12:00:00Z"))
                   contacts nil)))
      ;; One person, one row.
      (should (equal (length index) 1))
      (let ((chat (car index)))
        ;; The most recent JID wins: that is where new messages land.
        (should (equal (map-elt chat :chat-jid) "99988877@lid"))
        (should (equal (map-elt chat :display-name) "John Smith"))
        (should (equal (map-elt chat :alt-jids)
                       '("447123456789@s.whatsapp.net")))))))

(ert-deftest wasabi-test-chat-index-keeps-the-resolved-name ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (let* ((contacts (list (cons (intern "99988877@lid")
                                 '((:full-name . "John Smith")))))
           ;; The newest row is the phone number one, which cannot resolve a
           ;; name on its own; the merged row should still be named.
           (index (wasabi--parse-chat-index
                   (list (wasabi-test--index-entry "447123456789@s.whatsapp.net"
                                                   "2025-11-11T12:00:00Z")
                         (wasabi-test--index-entry "99988877@lid"
                                                   "2025-11-11T10:00:00Z"))
                   contacts nil)))
      (should (equal (length index) 1))
      (should (equal (map-elt (car index) :chat-jid)
                     "447123456789@s.whatsapp.net"))
      (should (equal (map-elt (car index) :display-name) "John Smith")))))

(ert-deftest wasabi-test-chat-index-keeps-distinct-people-apart ()
  (wasabi-test--with-clean-jids
    (let ((index (wasabi--parse-chat-index
                  (list (wasabi-test--index-entry "447123456789@s.whatsapp.net"
                                                  "2025-11-11T10:00:00Z")
                        (wasabi-test--index-entry "447999999999@s.whatsapp.net"
                                                  "2025-11-11T12:00:00Z"))
                  nil nil)))
      (should (equal (length index) 2)))))

(ert-deftest wasabi-test-chat-index-drops-status-broadcast ()
  (wasabi-test--with-clean-jids
    (should (equal (wasabi--parse-chat-index
                    (list (wasabi-test--index-entry "status@broadcast"
                                                    "2025-11-11T10:00:00Z"))
                    nil nil)
                   nil))))

(ert-deftest wasabi-test-chat-index-sorts-newest-first ()
  (wasabi-test--with-clean-jids
    (let ((index (wasabi--parse-chat-index
                  (list (wasabi-test--index-entry "1@s.whatsapp.net"
                                                  "2025-11-11 10:00:00.000000 +0000 GMT")
                        (wasabi-test--index-entry "2@s.whatsapp.net"
                                                  "2025-11-11 12:00:00.000000 +0000 GMT")
                        (wasabi-test--index-entry "3@s.whatsapp.net" nil))
                  nil nil)))
      (should (equal (mapcar (lambda (chat) (map-elt chat :chat-jid)) index)
                     '("2@s.whatsapp.net" "1@s.whatsapp.net" "3@s.whatsapp.net"))))))

;;; Contact picker

(ert-deftest wasabi-test-dedupe-contact-entries-prefers-indexed-jid ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (let* ((entries (list (list (cons :display-name "John Smith")
                                (cons :jid "99988877@lid")
                                (cons :is-group nil))
                          (list (cons :display-name "John Smith")
                                (cons :jid "447123456789@s.whatsapp.net")
                                (cons :is-group nil))))
           (index (list (list (cons :chat-jid "447123456789@s.whatsapp.net")
                              (cons :alt-jids nil))))
           (deduped (wasabi--dedupe-contact-entries entries index)))
      ;; One entry, addressed by the JID the chat list already uses, so
      ;; picking it opens the existing chat instead of starting a new one.
      (should (equal (length deduped) 1))
      (should (equal (map-elt (car deduped) :jid)
                     "447123456789@s.whatsapp.net")))))

(ert-deftest wasabi-test-dedupe-contact-entries-keeps-unrelated ()
  (wasabi-test--with-clean-jids
    (let ((entries (list (list (cons :display-name "John") (cons :jid "1@lid"))
                         (list (cons :display-name "Jane") (cons :jid "2@lid")))))
      (should (equal (length (wasabi--dedupe-contact-entries entries nil)) 2)))))

(ert-deftest wasabi-test-disambiguate-entries ()
  (let ((entries (list (list (cons :display-name "John Smith")
                             (cons :jid "447123456789@s.whatsapp.net"))
                       (list (cons :display-name "John Smith")
                             (cons :jid "447999999999@s.whatsapp.net"))
                       (list (cons :display-name "Jane Doe")
                             (cons :jid "447000000000@s.whatsapp.net")))))
    (wasabi--disambiguate-entries entries)
    (should (equal (mapcar (lambda (entry) (map-elt entry :display-name)) entries)
                   '("John Smith <447123456789>"
                     "John Smith <447999999999>"
                     "Jane Doe")))))

;;; Chat buffers

(ert-deftest wasabi-test-chat-buffer-found-by-identity-not-name ()
  (wasabi-test--with-clean-jids
    (let ((buffer (generate-new-buffer "*wasabi-test-chat*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (wasabi-chat-mode)
              (setq wasabi-chat--chat
                    (wasabi-chat--make-chat :chat-jid "99988877@lid"
                                            :contact-name "John Smith")))
            (should (eq (wasabi-chat--find-buffer "99988877@lid") buffer))
            ;; Not yet known to be the same person.
            (should-not (wasabi-chat--find-buffer "447123456789@s.whatsapp.net"))
            (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
            ;; Now the phone-number addressing resolves to the same buffer, so
            ;; a reply cannot open a second chat.
            (should (eq (wasabi-chat--find-buffer "447123456789@s.whatsapp.net")
                        buffer))
            (should-not (wasabi-chat--find-buffer "447999999999@s.whatsapp.net")))
        (kill-buffer buffer)))))

(ert-deftest wasabi-test-chat-buffer-name-not-shared-between-chats ()
  (wasabi-test--with-clean-jids
    (let ((buffer (generate-new-buffer "*Wasabi: John Smith*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (wasabi-chat-mode)
              (setq wasabi-chat--chat
                    (wasabi-chat--make-chat :chat-jid "447123456789@s.whatsapp.net"
                                            :contact-name "John Smith")))
            ;; Same chat: same buffer name.
            (should (equal (wasabi-chat--buffer-name "447123456789@s.whatsapp.net"
                                                     "John Smith")
                           "*Wasabi: John Smith*"))
            ;; A different person with the same name gets their own buffer
            ;; rather than taking this one over.
            (should-not (equal (wasabi-chat--buffer-name "447999999999@s.whatsapp.net"
                                                         "John Smith")
                               "*Wasabi: John Smith*")))
        (kill-buffer buffer)))))

;;; Sender naming

(ert-deftest wasabi-test-notification-sender-in-a-group ()
  (wasabi-test--with-clean-jids
    (let ((parsed (wasabi-chat--parse-notification
                   :p-message '((conversation . "Hello"))
                   :p-info '((Chat . "120363000000000000@g.us")
                             (Sender . "99988877@lid")
                             (PushName . "Johnny")
                             (IsGroup . t)
                             (IsFromMe . nil)
                             (Timestamp . "2025-11-11T12:00:00Z"))
                   :contact-name "Family"
                   :chat-jid "120363000000000000@g.us"
                   :contacts nil)))
      ;; The group's name is not the sender's name.
      (should (equal (map-elt parsed :sender-name) "~Johnny"))
      (should (equal (map-elt parsed :content) "Hello")))))

(ert-deftest wasabi-test-notification-sender-from-contacts ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (let ((parsed (wasabi-chat--parse-notification
                   :p-message '((conversation . "Hello"))
                   :p-info '((Chat . "447123456789@s.whatsapp.net")
                             (Sender . "447123456789@s.whatsapp.net")
                             (PushName . "Johnny")
                             (IsFromMe . nil))
                   :contact-name nil
                   :chat-jid "447123456789@s.whatsapp.net"
                   :contacts (list (cons (intern "99988877@lid")
                                         '((:full-name . "John Smith")))))))
      ;; The stored contact name wins over the push name, even though the
      ;; contact is stored under the other addressing.
      (should (equal (map-elt parsed :sender-name) "John Smith")))))

(ert-deftest wasabi-test-notification-reaction ()
  (wasabi-test--with-clean-jids
    (let ((parsed (wasabi-chat--parse-notification
                   :p-message '((reactionMessage . ((key . ((ID . "ABC")))
                                                    (text . "❤️"))))
                   :p-info '((Chat . "447123456789@s.whatsapp.net")
                             (Sender . "447123456789@s.whatsapp.net")
                             (PushName . "Johnny")
                             (IsFromMe . nil))
                   :contact-name "John Smith"
                   :chat-jid "447123456789@s.whatsapp.net"
                   :contacts nil)))
      (should (map-elt parsed :is-reaction))
      (should (equal (map-elt parsed :target-id) "ABC"))
      (should (equal (map-elt parsed :emoji) "❤️"))
      (should (equal (map-elt parsed :sender-name) "~Johnny")))))

(ert-deftest wasabi-test-notification-from-me ()
  (wasabi-test--with-clean-jids
    (let ((parsed (wasabi-chat--parse-notification
                   :p-message '((conversation . "Hello"))
                   :p-info '((Chat . "447123456789@s.whatsapp.net")
                             (Sender . "447000000000@s.whatsapp.net")
                             (IsFromMe . t))
                   :contact-name "John Smith"
                   :chat-jid "447123456789@s.whatsapp.net"
                   :contacts nil)))
      (should (equal (map-elt parsed :sender-name) "Me")))))

(ert-deftest wasabi-test-chat-start-reuses-buffer-across-addressings ()
  (wasabi-test--with-clean-jids
    (let ((buffers '()))
      (unwind-protect
          (progn
            ;; Open the chat as the chat list knows it: by LID.
            (wasabi-chat--start :chat-jid "99988877@lid"
                                :messages nil
                                :contact-name "John Smith")
            (push (current-buffer) buffers)
            (should (equal (map-elt wasabi-chat--chat :chat-jid) "99988877@lid"))
            ;; A reply arrives addressed by phone number.  Once the pairing
            ;; is known it must land in this chat, not open a second one.
            (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
            (wasabi-chat--start :chat-jid "447123456789@s.whatsapp.net"
                                :messages nil
                                :contact-name "John Smith")
            (push (current-buffer) buffers)
            (should (equal (length (seq-uniq buffers)) 1))
            ;; The buffer now follows the JID we were last handed, so the
            ;; next message we send goes where the reply came from.
            (should (equal (map-elt wasabi-chat--chat :chat-jid)
                           "447123456789@s.whatsapp.net"))
            (should (equal (wasabi--canonical-jid
                            (map-elt wasabi-chat--chat :chat-jid))
                           "447123456789@s.whatsapp.net")))
        (dolist (buffer (seq-uniq buffers))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest wasabi-test-chat-start-separates-same-named-contacts ()
  (wasabi-test--with-clean-jids
    (let ((buffers '()))
      (unwind-protect
          (progn
            (wasabi-chat--start :chat-jid "447123456789@s.whatsapp.net"
                                :messages nil
                                :contact-name "John Smith")
            (push (current-buffer) buffers)
            ;; A different person who happens to share the name must not
            ;; take over the first one's buffer.
            (wasabi-chat--start :chat-jid "447999999999@s.whatsapp.net"
                                :messages nil
                                :contact-name "John Smith")
            (push (current-buffer) buffers)
            (should (equal (length (seq-uniq buffers)) 2))
            (should (equal (map-elt wasabi-chat--chat :chat-jid)
                           "447999999999@s.whatsapp.net")))
        (dolist (buffer (seq-uniq buffers))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest wasabi-test-saved-name-wins-across-jid-variants ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    ;; The usual split: the LID entry knows only what they call
    ;; themselves, the phone number entry carries the name we saved.
    (let ((contacts (list (cons (intern "99988877@lid")
                                '((:push-name . "Johnny")))
                          (cons (intern "447123456789@s.whatsapp.net")
                                '((:full-name . "John Smith"))))))
      (should (equal (wasabi--contact-display-name "99988877@lid" contacts)
                     "John Smith"))
      (should (equal (wasabi--contact-display-name "447123456789@s.whatsapp.net"
                                                   contacts)
                     "John Smith")))))

(ert-deftest wasabi-test-push-name-marked-when-nothing-saved ()
  (wasabi-test--with-clean-jids
    (let ((contacts (list (cons (intern "99988877@lid")
                                '((:push-name . "Johnny"))))))
      ;; Nobody we saved, so show what they call themselves, marked.
      (should (equal (wasabi--contact-display-name "99988877@lid" contacts)
                     "~Johnny")))))

(ert-deftest wasabi-test-push-name-marking ()
  (should (equal (wasabi--push-name "Johnny") "~Johnny"))
  ;; Already marked names are left alone.
  (should (equal (wasabi--push-name "~Johnny") "~Johnny"))
  (should-not (wasabi--push-name ""))
  (should-not (wasabi--push-name nil)))

(ert-deftest wasabi-test-chat-index-prefers-saved-name-over-push-name ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (let* ((contacts (list (cons (intern "99988877@lid")
                                 '((:push-name . "Johnny")))
                           (cons (intern "447123456789@s.whatsapp.net")
                                 '((:full-name . "John Smith")))))
           (index (wasabi--parse-chat-index
                   (list (wasabi-test--index-entry "99988877@lid"
                                                   "2025-11-11T12:00:00Z"))
                   contacts nil)))
      (should (equal (map-elt (car index) :display-name) "John Smith")))))

(ert-deftest wasabi-test-chat-index-falls-back-to-marked-push-name ()
  (wasabi-test--with-clean-jids
    (let* ((contacts (list (cons (intern "447123456789@s.whatsapp.net")
                                 '((:push-name . "Johnny")))))
           (index (wasabi--parse-chat-index
                   (list (wasabi-test--index-entry "447123456789@s.whatsapp.net"
                                                   "2025-11-11T12:00:00Z"))
                   contacts nil)))
      ;; Someone we never saved: their own name beats a bare number.
      (should (equal (map-elt (car index) :display-name) "~Johnny")))))

;;; Gathering a chat recorded under more than one JID

(ert-deftest wasabi-test-dedupe-messages ()
  (let ((messages (list '((message_id . "A") (text_content . "one"))
                        '((message_id . "B") (text_content . "two"))
                        '((message_id . "A") (text_content . "one again")))))
    (should (equal (mapcar (lambda (m) (map-elt m 'message_id))
                           (wasabi--dedupe-messages messages))
                   '("A" "B")))))

(ert-deftest wasabi-test-dedupe-messages-keeps-unidentified ()
  ;; No message_id is no reason to drop a message.
  (let ((messages (list '((text_content . "one"))
                        '((text_content . "two")))))
    (should (equal (length (wasabi--dedupe-messages messages)) 2))))

(ert-deftest wasabi-test-dedupe-messages-accepts-a-vector ()
  (should (equal (length (wasabi--dedupe-messages
                          (vector '((message_id . "A"))
                                  '((message_id . "B")))))
                 2)))

(ert-deftest wasabi-test-index-prefers-real-message-times ()
  (wasabi-test--with-clean-jids
    ;; Both rows were written by the same first sync, so last_updated
    ;; says nothing about when either chat last saw a message.
    (let* ((synced-at "2025-11-20T09:00:00Z")
           ;; Remembered off the messages as the history loaded: wuzapi's
           ;; own row timestamps record the sync, not the conversation.
           (times (list (cons "1@s.whatsapp.net" "2025-11-19T18:00:00Z")))
           (index (wasabi--parse-chat-index
                   (list (wasabi-test--index-entry "1@s.whatsapp.net" synced-at)
                         (wasabi-test--index-entry "2@s.whatsapp.net" synced-at))
                   nil nil times)))
      ;; The chat we have messages for is dated by its newest one.
      (should (equal (map-elt (seq-find (lambda (chat)
                                          (equal (map-elt chat :chat-jid)
                                                 "1@s.whatsapp.net"))
                                        index)
                              :last-updated)
                     "2025-11-19T18:00:00Z"))
      ;; The one we have not loaded keeps what the index said.
      (should (equal (map-elt (seq-find (lambda (chat)
                                          (equal (map-elt chat :chat-jid)
                                                 "2@s.whatsapp.net"))
                                        index)
                              :last-updated)
                     synced-at))
      ;; And the sync time sorts ahead of the older real one.
      (should (equal (map-elt (car index) :chat-jid) "2@s.whatsapp.net")))))

(ert-deftest wasabi-test-latest-message-timestamp-spans-jid-variants ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    ;; Remembered under whichever JID loaded, found under either: one
    ;; conversation, one time.
    (let ((times (list (cons "447123456789@s.whatsapp.net"
                             "2025-11-19T18:00:00Z"))))
      (should (equal (wasabi--latest-message-timestamp "99988877@lid" times)
                     "2025-11-19T18:00:00Z"))
      (should (equal (wasabi--latest-message-timestamp
                      "447123456789@s.whatsapp.net" times)
                     "2025-11-19T18:00:00Z")))))

(ert-deftest wasabi-test-latest-message-timestamp-without-history ()
  (wasabi-test--with-clean-jids
    (should-not (wasabi--latest-message-timestamp "1@s.whatsapp.net" nil))
    (should-not (wasabi--latest-message-timestamp
                 "1@s.whatsapp.net"
                 (list (cons "2@s.whatsapp.net" "2025-11-19T18:00:00Z"))))))

(ert-deftest wasabi-test-remember-chat-time-keeps-the-newest ()
  (wasabi-test--with-clean-jids
    (wasabi--remember-chat-time "1@s.whatsapp.net" "2025-11-19T18:00:00Z")
    (should (equal (wasabi--latest-message-timestamp
                    "1@s.whatsapp.net" wasabi--chat-times-table)
                   "2025-11-19T18:00:00Z"))
    ;; An older message does not un-date the chat.
    (wasabi--remember-chat-time "1@s.whatsapp.net" "2025-11-11T10:00:00Z")
    (should (equal (wasabi--latest-message-timestamp
                    "1@s.whatsapp.net" wasabi--chat-times-table)
                   "2025-11-19T18:00:00Z"))
    ;; Nor does one we cannot read.
    (wasabi--remember-chat-time "1@s.whatsapp.net" "nonsense")
    (should (equal (wasabi--latest-message-timestamp
                    "1@s.whatsapp.net" wasabi--chat-times-table)
                   "2025-11-19T18:00:00Z"))))

(ert-deftest wasabi-test-remembered-under-one-jid-found-under-the-other ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (wasabi--remember-chat-time "99988877@lid" "2025-11-19T18:00:00Z")
    (should (equal (wasabi--latest-message-timestamp
                    "447123456789@s.whatsapp.net" wasabi--chat-times-table)
                   "2025-11-19T18:00:00Z"))))

(ert-deftest wasabi-test-cached-identities-round-trip ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-jid-alias "99988877@lid" "447123456789@s.whatsapp.net")
    (wasabi--learn-push-name "99988877@lid" "Johnny")
    (wasabi--remember-chat-time "99988877@lid" "2025-11-19T18:00:00Z")
    (wasabi--save-jid-aliases)
    (clrhash wasabi--jid-canonical-table)
    (clrhash wasabi--jid-variants-table)
    (clrhash wasabi--push-names-table)
    (clrhash wasabi--chat-times-table)
    (wasabi--load-jid-aliases)
    ;; All three survive a restart, so a chat keeps its date and its
    ;; name without being opened again.
    (should (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))
    (should (equal (wasabi--known-push-name "99988877@lid") "Johnny"))
    (should (equal (wasabi--latest-message-timestamp
                    "99988877@lid" wasabi--chat-times-table)
                   "2025-11-19T18:00:00Z"))))

(ert-deftest wasabi-test-cached-identities-read-the-old-format ()
  (wasabi-test--with-clean-jids
    ;; A cache written before push names and chat times were kept holds
    ;; the bare list of pairings.
    (with-temp-file (wasabi--jid-aliases-file)
      (prin1 (list (cons "447123456789@s.whatsapp.net"
                         (list "447123456789@s.whatsapp.net" "99988877@lid")))
             (current-buffer)))
    (wasabi--load-jid-aliases)
    (should (wasabi--same-chat-p "99988877@lid" "447123456789@s.whatsapp.net"))))

;;; Names taken from the messages themselves

(ert-deftest wasabi-test-push-name-learned-from-a-message ()
  (wasabi-test--with-clean-jids
    ;; Nobody saved, and no contact entry at all: all we have to go on
    ;; is what they called themselves when they wrote.
    (should-not (wasabi--contact-display-name "99988877@lid" nil))
    (wasabi--learn-from-message-info
     (list (cons 'Chat "99988877@lid")
           (cons 'Sender "99988877@lid")
           (cons 'PushName "Johnny")
           (cons 'IsFromMe nil)
           (cons 'IsGroup nil)))
    (should (equal (wasabi--known-push-name "99988877@lid") "Johnny"))
    (should (equal (wasabi--contact-display-name "99988877@lid" nil) "~Johnny"))))

(ert-deftest wasabi-test-push-name-not-learned-from-ourselves ()
  (wasabi-test--with-clean-jids
    ;; Our own name on our own message says nothing about anyone else.
    (wasabi--learn-from-message-info
     (list (cons 'Chat "99988877@lid")
           (cons 'Sender "1111@lid")
           (cons 'PushName "Me Myself")
           (cons 'IsFromMe t)
           (cons 'IsGroup nil)))
    (should-not (wasabi--known-push-name "1111@lid"))))

(ert-deftest wasabi-test-saved-name-still-beats-a-learned-push-name ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-from-message-info
     (list (cons 'Sender "447123456789@s.whatsapp.net")
           (cons 'PushName "Johnny")
           (cons 'IsFromMe nil)))
    (let ((contacts (list (cons (intern "447123456789@s.whatsapp.net")
                                (list (cons :full-name "John Smith"))))))
      (should (equal (wasabi--contact-display-name
                      "447123456789@s.whatsapp.net" contacts)
                     "John Smith")))))

(ert-deftest wasabi-test-push-name-found-across-jid-variants ()
  (wasabi-test--with-clean-jids
    ;; Learned from a LID-addressed message, wanted for a chat the index
    ;; keys by phone number.
    (wasabi--learn-from-message-info
     (list (cons 'Chat "99988877@lid")
           (cons 'Sender "99988877@lid")
           (cons 'SenderAlt "447123456789@s.whatsapp.net")
           (cons 'PushName "Johnny")
           (cons 'IsFromMe nil)
           (cons 'IsGroup nil)))
    (should (equal (wasabi--contact-display-name
                    "447123456789@s.whatsapp.net" nil)
                   "~Johnny"))))

(ert-deftest wasabi-test-chat-index-names-from-a-learned-push-name ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-from-message-info
     (list (cons 'Chat "447123456789@s.whatsapp.net")
           (cons 'Sender "447123456789@s.whatsapp.net")
           (cons 'PushName "Johnny")
           (cons 'IsFromMe nil)
           (cons 'IsGroup nil)))
    (let ((index (wasabi--parse-chat-index
                  (list (wasabi-test--index-entry "447123456789@s.whatsapp.net"
                                                  "2025-11-11T12:00:00Z"))
                  nil nil nil)))
      ;; A bare phone number becomes a name, marked as their own.
      (should (equal (map-elt (car index) :display-name) "~Johnny")))))

(ert-deftest wasabi-test-parse-epoch-timestamps ()
  ;; Compared as instants: Emacs has several representations of one.
  (let ((expected (parse-iso8601-time-string "2025-11-11T12:00:00Z")))
    ;; Seconds, as a number and as the string of digits it can arrive as.
    (should (time-equal-p (wasabi--parse-timestamp 1762862400) expected))
    (should (time-equal-p (wasabi--parse-timestamp "1762862400") expected))
    ;; Milliseconds.
    (should (time-equal-p (wasabi--parse-timestamp 1762862400000) expected)))
  (should-not (wasabi--parse-timestamp t))
  (should-not (wasabi--parse-timestamp '(1 2))))

(ert-deftest wasabi-test-epoch-and-iso-order-together ()
  ;; A chat's history can mix the two, and did: the epoch ones used to
  ;; fail to parse and so sorted as though they were the newest.
  (let ((older 1762862400)
        (newer "2025-11-19T18:00:00Z"))
    (should (wasabi--timestamp-older-p older newer))
    (should-not (wasabi--timestamp-older-p newer older))
    (should (wasabi--timestamp-newer-p newer older))))

(ert-deftest wasabi-test-index-dates-chats-from-epoch-messages ()
  (wasabi-test--with-clean-jids
    (let* ((times (list (cons "1@s.whatsapp.net" 1763380800)))
           (index (wasabi--parse-chat-index
                   (list (wasabi-test--index-entry "1@s.whatsapp.net"
                                                   "2025-11-20T09:00:00Z"))
                   nil nil times)))
      (should (time-equal-p (wasabi--parse-timestamp
                             (map-elt (car index) :last-updated))
                            (wasabi--parse-timestamp 1763380800))))))

(ert-deftest wasabi-test-silent-refresh-flag-round-trips ()
  ;; `map-put!' can only update a key an alist already has: it signals
  ;; map-not-inplace otherwise, which used to abort the HistorySync and
  ;; OfflineSyncCompleted handlers before they re-fetched anything.
  (let ((buffer (generate-new-buffer "*wasabi-state-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (wasabi-mode)
          (setq wasabi--state (wasabi--make-state :wasabi-buffer buffer))
          (map-put! wasabi--state :silent-refresh t)
          (should (map-elt wasabi--state :silent-refresh))
          ;; Reaching ready clears it without removing the key, so the
          ;; next sync can set it again.
          (wasabi--set-status :type 'ready :message nil)
          (should-not (map-elt wasabi--state :silent-refresh))
          (map-put! wasabi--state :silent-refresh t)
          (should (map-elt wasabi--state :silent-refresh)))
      (kill-buffer buffer))))

(ert-deftest wasabi-test-state-declares-every-key-it-puts ()
  ;; Same trap, for every key the code writes with `map-put!'.
  (let ((buffer (generate-new-buffer "*wasabi-state-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (wasabi-mode)
          (setq wasabi--state (wasabi--make-state :wasabi-buffer buffer))
          (dolist (key '(:client :status :connected :contacts :chats-index
                                 :p-chat-index :chats :groups :silent-refresh))
            (should (assq key wasabi--state))))
      (kill-buffer buffer))))

(ert-deftest wasabi-test-check-request-shape ()
  (let ((request (wasabi--make-user-check-request
                  :token "tok" :phones '("447123456789" "447999999999"))))
    (should (equal (map-elt request :method) "user.check"))
    (should (equal (map-nested-elt request '(:params token)) "tok"))
    ;; A vector, since it goes out as a JSON array.
    (should (equal (map-nested-elt request '(:params Phone))
                   ["447123456789" "447999999999"])))
  (should-error (wasabi--make-user-check-request :token "tok"))
  (should-error (wasabi--make-user-check-request :phones '("1"))))

(ert-deftest wasabi-test-learn-pairings-from-a-check-response ()
  (wasabi-test--with-clean-jids
    ;; What WhatsApp answers: the number asked about, and the JID it
    ;; belongs to, which is now a linked identity.
    (let ((response '((Users . [((IsInWhatsapp . t)
                                 (JID . "99988877@lid")
                                 (Query . "447123456789")
                                 (VerifiedName . ""))
                                ((IsInWhatsapp . t)
                                 (JID . "11122233@lid")
                                 (Query . "+447999999999")
                                 (VerifiedName . ""))]))))
      (should (equal (wasabi--learn-from-check-response response) 2))
      (should (wasabi--same-chat-p "447123456789@s.whatsapp.net" "99988877@lid"))
      ;; A leading + on the query is not part of the JID.
      (should (wasabi--same-chat-p "447999999999@s.whatsapp.net" "11122233@lid"))
      ;; The phone number stays canonical, so that is what we send to.
      (should (equal (wasabi--canonical-jid "99988877@lid")
                     "447123456789@s.whatsapp.net"))
      ;; Asking twice teaches nothing twice.
      (should (equal (wasabi--learn-from-check-response response) 0)))))

(ert-deftest wasabi-test-check-response-without-a-pairing ()
  (wasabi-test--with-clean-jids
    ;; Someone not on WhatsApp, and an answer with nothing in it.
    (should (equal (wasabi--learn-from-check-response
                    '((Users . [((IsInWhatsapp . :false)
                                 (JID . "")
                                 (Query . "447123456789"))])))
                   0))
    (should (equal (wasabi--learn-from-check-response '((Users . []))) 0))
    (should (equal (wasabi--learn-from-check-response nil) 0))))

(ert-deftest wasabi-test-check-response-pairs-the-chat-index ()
  (wasabi-test--with-clean-jids
    (wasabi--learn-from-check-response
     '((Users . [((JID . "99988877@lid") (Query . "447123456789"))])))
    ;; The two halves of one conversation become one row, keeping the
    ;; most recent JID and noting the other.
    (let ((index (wasabi--parse-chat-index
                  (list (wasabi-test--index-entry "447123456789@s.whatsapp.net"
                                                  "2026-07-26T01:50:06Z")
                        (wasabi-test--index-entry "99988877@lid"
                                                  "2026-09-21T00:12:28Z"))
                  nil nil nil)))
      (should (equal (length index) 1))
      (should (equal (map-elt (car index) :chat-jid) "99988877@lid"))
      (should (equal (map-elt (car index) :alt-jids)
                     '("447123456789@s.whatsapp.net"))))))

(ert-deftest wasabi-test-history-request-limit ()
  ;; wuzapi defaults to 50 rows ordered by when it wrote them, which is
  ;; an arbitrary slice of a conversation rather than its recent end.
  (let ((request (wasabi--make-chat-history-request
                  :token "tok" :chat-jid "1@s.whatsapp.net" :limit 1000)))
    (should (equal (map-elt request :method) "chat.history"))
    (should (equal (map-nested-elt request '(:params chat_jid)) "1@s.whatsapp.net"))
    (should (equal (map-nested-elt request '(:params limit)) 1000)))
  ;; The index takes no limit, and asking without one is still valid.
  (let ((request (wasabi--make-chat-history-request
                  :token "tok" :chat-jid "index")))
    (should-not (map-elt (map-elt request :params) 'limit))
    (should (equal (map-nested-elt request '(:params chat_jid)) "index")))
  (should-error (wasabi--make-chat-history-request :token "tok"))
  (should-error (wasabi--make-chat-history-request :chat-jid "index")))

(ert-deftest wasabi-test-messages-sorted-by-their-own-times ()
  ;; The rows arrive ordered by when wuzapi wrote them, so what came
  ;; back last is not what was said last.  Sorting has to use the time
  ;; inside each message.
  (let* ((messages (list '((:timestamp . "2026-08-18T20:08:11Z") (:content . "last"))
                         '((:timestamp . "2026-06-29T21:02:39Z") (:content . "first"))
                         '((:timestamp . "2026-07-26T22:50:08Z") (:content . "middle"))))
         (sorted (sort (copy-sequence messages)
                       (lambda (a b)
                         (wasabi--timestamp-older-p (map-elt a :timestamp)
                                                    (map-elt b :timestamp))))))
    (should (equal (mapcar (lambda (m) (map-elt m :content)) sorted)
                   '("first" "middle" "last")))))

(provide 'wasabi-test)
;;; wasabi-test.el ends here
