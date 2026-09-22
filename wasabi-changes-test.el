;;; wasabi-changes-test.el --- Tests for edits and deletions  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-changes-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wasabi)

;; Declared rather than required: read receipts may or may not be part
;; of the wasabi under test, and these tests are not about them.
(defvar wasabi-send-read-receipts)

(defmacro wasabi-changes-test--isolated (&rest body)
  "Run BODY with no recorded changes, and a scratch data dir."
  (declare (indent 0))
  `(let ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (wasabi-chat--changes nil)
         (wasabi-chat--changes-dirty nil)
         (wasabi-send-read-receipts nil))
     ,@body))

(defun wasabi-changes-test--row (id message &optional time)
  "A stored history row with ID carrying protocol MESSAGE, sent at TIME."
  `((message_id . ,id)
    (chat_jid . "447123456789@s.whatsapp.net")
    (sender_jid . "447123456789@s.whatsapp.net")
    (message_type . "text")
    (timestamp . "2026-09-22T12:00:00Z")
    (data_json
     . ,(json-encode
         `((Info . ((ID . ,id)
                    (Chat . "447123456789@s.whatsapp.net")
                    (Sender . "447123456789@s.whatsapp.net")
                    (IsFromMe . :json-false)
                    (Timestamp . ,(or time "2026-09-22T12:00:00Z"))))
           (Message . ,message))))))

(defun wasabi-changes-test--revoke (target &optional type)
  "A protocol message deleting TARGET, its type given as TYPE."
  `((protocolMessage . ((key . ((ID . ,target)))
                        (type . ,(or type 0))))))

(defun wasabi-changes-test--edit (target text &optional type)
  "A protocol message editing TARGET to TEXT, its type given as TYPE."
  `((protocolMessage . ((key . ((ID . ,target)))
                        (type . ,(or type 14))
                        (editedMessage . ((conversation . ,text)))))))

(defmacro wasabi-changes-test--in-chat (rows &rest body)
  "Run BODY in a chat buffer showing the stored ROWS."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer "*wasabi-changes-test*")))
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

;;; Recognising them

(ert-deftest wasabi-changes-test-deletion ()
  ;; Whether the type comes as its number or its name.
  (dolist (type '(0 "REVOKE"))
    (let ((change (wasabi-chat--change (wasabi-changes-test--revoke "ORIG1" type)
                                       '((Timestamp . "2026-09-22T12:05:00Z")))))
      (should (equal (map-elt change :target) "ORIG1"))
      (should (eq (map-elt change :kind) 'deleted))
      (should (equal (map-elt change :time) "2026-09-22T12:05:00Z")))))

(ert-deftest wasabi-changes-test-edit ()
  (dolist (type '(14 "MESSAGE_EDIT"))
    (let ((change (wasabi-chat--change (wasabi-changes-test--edit "ORIG1" "fixed" type)
                                       '((Timestamp . "2026-09-22T12:05:00Z")))))
      (should (eq (map-elt change :kind) 'edited))
      (should (equal (map-elt change :text) "fixed"))))
  ;; An edit to a longer message arrives as extended text.
  (should (equal (map-elt (wasabi-chat--change
                           '((protocolMessage
                              . ((key . ((ID . "ORIG1")))
                                 (type . 14)
                                 (editedMessage
                                  . ((extendedTextMessage . ((text . "longer")))))))) nil)
                          :text)
                 "longer")))

(ert-deftest wasabi-changes-test-not-a-change ()
  (should-not (wasabi-chat--change '((conversation . "hi")) nil))
  (should-not (wasabi-chat--change '((reactionMessage . ((key . ((ID . "X"))) (text . "x")))) nil))
  ;; Other protocol messages, such as disappearing message settings.
  (should-not (wasabi-chat--change '((protocolMessage . ((key . ((ID . "X"))) (type . 3)))) nil))
  (should-not (wasabi-chat--change '((protocolMessage . ((type . 0)))) nil))
  (should-not (wasabi-chat--change nil nil)))

(ert-deftest wasabi-changes-test-stored-deletion-row ()
  ;; How wuzapi files a deletion: a row of its own, the ID as its text.
  (let ((change (wasabi-chat--stored-change
                 '((message_id . "DEL1") (message_type . "delete")
                   (text_content . "ORIG1") (timestamp . "2026-09-22T12:05:00Z")
                   (data_json . "")))))
    (should (equal (map-elt change :target) "ORIG1"))
    (should (eq (map-elt change :kind) 'deleted)))
  (should-not (wasabi-chat--stored-change
               (wasabi-changes-test--row "M1" '((conversation . "hi"))))))

;;; Recording them

(ert-deftest wasabi-changes-test-record-and-keep ()
  (wasabi-changes-test--isolated
    (wasabi-chat--record-change '((:target . "ORIG1") (:kind . edited)
                                  (:time . "2026-09-22T12:10:00Z") (:text . "second")))
    (wasabi-chat--record-change '((:target . "ORIG1") (:kind . edited)
                                  (:time . "2026-09-22T12:05:00Z") (:text . "first")))
    ;; The same edit seen twice is one edit.
    (wasabi-chat--record-change '((:target . "ORIG1") (:kind . edited)
                                  (:time . "2026-09-22T12:05:00Z") (:text . "first")))
    (wasabi-chat--record-change '((:target . "ORIG1") (:kind . deleted)
                                  (:time . "2026-09-22T12:20:00Z")))
    (let ((changes (gethash "ORIG1" (wasabi-chat--changes))))
      (should (equal (mapcar #'cdr (cdr (assq :edits changes))) '("first" "second")))
      (should (assq :deleted changes)))
    ;; Kept across a restart: wuzapi never stores edits.
    (wasabi-chat--save-changes)
    (setq wasabi-chat--changes nil)
    (should (equal (length (cdr (assq :edits (gethash "ORIG1" (wasabi-chat--changes)))))
                   2))))

(ert-deftest wasabi-changes-test-nothing-new-nothing-written ()
  (wasabi-changes-test--isolated
    (wasabi-chat--record-change '((:target . "ORIG1") (:kind . deleted) (:time . "t")))
    (wasabi-chat--save-changes)
    (should-not wasabi-chat--changes-dirty)
    (wasabi-chat--record-change '((:target . "ORIG1") (:kind . deleted) (:time . "t")))
    (should-not wasabi-chat--changes-dirty)))

;;; Showing them

(ert-deftest wasabi-changes-test-deleted-message-stays-annotated ()
  (wasabi-changes-test--isolated
    (wasabi-changes-test--in-chat
        (list (wasabi-changes-test--row "ORIG1" '((conversation . "secret plan")))
              (wasabi-changes-test--row "DEL1" (wasabi-changes-test--revoke "ORIG1")
                                        "2026-09-22T12:05:00Z"))
      ;; Still there, marked, and the deletion is no message of its own.
      (should (string-match-p "secret plan (DELETED)" (buffer-string)))
      (should (equal (length (map-elt wasabi-chat--chat :messages)) 1))
      (goto-char (point-min))
      (search-forward "(DELETED)")
      (should (get-text-property (1- (point)) 'wasabi-annotation))
      (should (eq (get-text-property (1- (point)) 'face) 'wasabi-chat-deleted))
      ;; While the message's own text is no annotation.
      (search-backward "secret")
      (should-not (get-text-property (point) 'wasabi-annotation)))))

(ert-deftest wasabi-changes-test-edited-message-keeps-its-original ()
  (wasabi-changes-test--isolated
    (wasabi-chat--record-change '((:target . "ORIG1") (:kind . edited)
                                  (:time . "2026-09-22T12:05:00Z") (:text . "see you at 6")))
    (wasabi-changes-test--in-chat
        (list (wasabi-changes-test--row "ORIG1" '((conversation . "see you at 5"))))
      ;; The original, then the edit under it with its time.
      (should (string-match-p "see you at 5\n *(EDITED [0-9]+:[0-9]+: see you at 6)"
                              (buffer-string)))
      (goto-char (point-min))
      (search-forward "(EDITED")
      (should (eq (get-text-property (point) 'face) 'wasabi-chat-edited)))))

(ert-deftest wasabi-changes-test-edit-on-another-day-shows-the-date ()
  (should (string-match-p "\\`[0-9]+:[0-9]+\\'"
                          (wasabi-chat--change-time "2026-09-22T12:05:00Z"
                                                    "2026-09-22T12:00:00Z")))
  (should (string-match-p "\\`Sep [0-9]+ [0-9]+:[0-9]+\\'"
                          (wasabi-chat--change-time "2026-09-10T12:05:00Z"
                                                    "2026-09-22T12:00:00Z")))
  (should-not (wasabi-chat--change-time nil "2026-09-22T12:00:00Z")))

(ert-deftest wasabi-changes-test-live-change-keeps-what-you-are-typing ()
  (wasabi-changes-test--isolated
    (wasabi-changes-test--in-chat
        (list (wasabi-changes-test--row "ORIG1" '((conversation . "first")))
              (wasabi-changes-test--row "ORIG2" '((conversation . "second"))
                                        "2026-09-22T12:01:00Z"))
      (goto-char (point-max))
      (insert "half a repl")
      ;; A deletion arrives while typing.
      (wasabi-chat--record-change '((:target . "ORIG1") (:kind . deleted)
                                    (:time . "2026-09-22T12:05:00Z")))
      (wasabi-chat--apply-change "ORIG1")
      (should (string-match-p "first (DELETED)" (buffer-string)))
      (should (string-match-p "second" (buffer-string)))
      (should (equal (wasabi-chat--get-prompt-input) "half a repl"))
      ;; And the chat's layout is untouched: one prompt, the second
      ;; message still after the first.
      (should (equal (how-many "^> " (point-min) (point-max)) 1))
      (should (< (string-match "first" (buffer-string))
                 (string-match "second" (buffer-string))))
      ;; The last message can change too.
      (wasabi-chat--record-change '((:target . "ORIG2") (:kind . edited)
                                    (:time . "2026-09-22T12:06:00Z") (:text . "2nd")))
      (wasabi-chat--apply-change "ORIG2")
      (should (string-match-p "second\n *(EDITED [0-9]+:[0-9]+: 2nd)\n\n> half a repl\\'"
                              (buffer-string))))))

(ert-deftest wasabi-changes-test-change-to-something-not-loaded ()
  (wasabi-changes-test--isolated
    (wasabi-changes-test--in-chat
        (list (wasabi-changes-test--row "ORIG1" '((conversation . "first"))))
      (let ((before (buffer-string)))
        (wasabi-chat--record-change '((:target . "ELSEWHERE") (:kind . deleted) (:time . "t")))
        (wasabi-chat--apply-change "ELSEWHERE")
        (should (equal (buffer-string) before))))))

(provide 'wasabi-changes-test)
;;; wasabi-changes-test.el ends here
