;;; wasabi-receipts-test.el --- Tests for read receipts  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-receipts-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The requests themselves are stubbed: there is no wuzapi to talk to.

;;; Code:

(require 'ert)
(require 'wasabi)

(defun wasabi-receipts-test--message (id &rest fields)
  "An internal message with ID, plus FIELDS as a plist of overrides."
  ;; `copy-tree', since the overrides below mutate it and backquote is
  ;; free to share its constant cells between calls.
  (let ((message (copy-tree `((:message-id . ,id)
                              (:sender-name . "John")
                              (:content . "hi")
                              (:from-me . nil)
                              (:sender-jid . "447123456789@s.whatsapp.net")
                              (:chat-jid . "447123456789@s.whatsapp.net")))))
    (while fields
      (setf (alist-get (pop fields) message) (pop fields)))
    message))

(defmacro wasabi-receipts-test--sending (&rest body)
  "Run BODY in a chat buffer, and return the receipt requests it made."
  (declare (indent 0))
  `(let ((wasabi-chat--read-receipts-sent (make-hash-table :test 'equal))
         (wasabi-buffer (get-buffer-create "*Wasabi*"))
         (chat-buffer (generate-new-buffer "*wasabi-receipts-test*"))
         (requests '()))
     (unwind-protect
         (cl-letf (((symbol-function 'wasabi--send-chat-markread-request)
                    (lambda (&rest args)
                      (push args requests)
                      (funcall (plist-get args :on-success) nil))))
           (with-current-buffer chat-buffer
             (wasabi-chat-mode)
             (setq wasabi-chat--chat
                   (wasabi-chat--make-chat :chat-jid "447123456789@s.whatsapp.net"))
             ,@body)
           (nreverse requests))
       (kill-buffer chat-buffer)
       (kill-buffer wasabi-buffer))))

;;; The request

(ert-deftest wasabi-receipts-test-request-shape ()
  (let ((request (wasabi--make-chat-markread-request
                  :token "tok" :chat "123@g.us" :sender "447123456789@s.whatsapp.net"
                  :ids '("A" "B"))))
    (should (equal (map-elt request :method) "chat.markread"))
    (should (equal (map-nested-elt request '(:params Id)) ["A" "B"]))
    (should (equal (map-nested-elt request '(:params ChatPhone)) "123@g.us"))
    (should (equal (map-nested-elt request '(:params SenderPhone))
                   "447123456789@s.whatsapp.net")))
  ;; One to one: no sender needed, so none sent.
  (should-not (map-elt (map-elt (wasabi--make-chat-markread-request
                                 :token "tok" :chat "1@s.whatsapp.net" :ids '("A"))
                                :params)
                       'SenderPhone))
  (should-error (wasabi--make-chat-markread-request :token "tok" :chat "1@s.whatsapp.net"))
  (should-error (wasabi--make-chat-markread-request :token "tok" :ids '("A"))))

;;; What is owed a receipt

(ert-deftest wasabi-receipts-test-only-incoming ()
  (let ((wasabi-chat--read-receipts-sent (make-hash-table :test 'equal)))
    (should (equal (wasabi-chat--pending-receipts
                    (list (wasabi-receipts-test--message "A")
                          (wasabi-receipts-test--message "B" :from-me t)
                          (wasabi-receipts-test--message "C")))
                   '(("447123456789@s.whatsapp.net" nil "A" "C"))))))

(ert-deftest wasabi-receipts-test-one-request-per-group-sender ()
  (let ((wasabi-chat--read-receipts-sent (make-hash-table :test 'equal)))
    ;; whatsmeow can only mark one sender's messages per request in a group.
    (should (equal (wasabi-chat--pending-receipts
                    (list (wasabi-receipts-test--message
                           "A" :chat-jid "123@g.us" :sender-jid "1@s.whatsapp.net")
                          (wasabi-receipts-test--message
                           "B" :chat-jid "123@g.us" :sender-jid "2@lid")
                          (wasabi-receipts-test--message
                           "C" :chat-jid "123@g.us" :sender-jid "1@s.whatsapp.net")))
                   '(("123@g.us" "1@s.whatsapp.net" "A" "C")
                     ("123@g.us" "2@lid" "B"))))))

(ert-deftest wasabi-receipts-test-split-chat-gets-a-request-per-half ()
  (let ((wasabi-chat--read-receipts-sent (make-hash-table :test 'equal)))
    ;; A conversation filed under both a phone number and a LID: each
    ;; message is acknowledged in the chat it actually arrived in.
    (should (equal (length (wasabi-chat--pending-receipts
                            (list (wasabi-receipts-test--message "A")
                                  (wasabi-receipts-test--message
                                   "B" :chat-jid "99988877@lid"))))
                   2))))

(ert-deftest wasabi-receipts-test-latest-only ()
  (let ((wasabi-chat--read-receipts-sent (make-hash-table :test 'equal))
        (messages (mapcar (lambda (n) (wasabi-receipts-test--message (format "M%03d" n)))
                          (number-sequence 1 200))))
    ;; The first open of a long chat marks its recent end, not all of it.
    (let ((ids (cddr (car (wasabi-chat--pending-receipts messages)))))
      (should (equal (length ids) wasabi-chat--read-receipts-max))
      (should (equal (car (last ids)) "M200")))))

(ert-deftest wasabi-receipts-test-skips-what-was-already-sent ()
  (let ((wasabi-chat--read-receipts-sent (make-hash-table :test 'equal)))
    (puthash "A" t wasabi-chat--read-receipts-sent)
    (should (equal (wasabi-chat--pending-receipts
                    (list (wasabi-receipts-test--message "A")
                          (wasabi-receipts-test--message "B")))
                   '(("447123456789@s.whatsapp.net" nil "B"))))))

(ert-deftest wasabi-receipts-test-skips-messages-without-ids ()
  (let ((wasabi-chat--read-receipts-sent (make-hash-table :test 'equal)))
    (should-not (wasabi-chat--pending-receipts
                 (list (wasabi-receipts-test--message nil))))))

;;; Sending them

(ert-deftest wasabi-receipts-test-sent-once ()
  (let ((wasabi-send-read-receipts t))
    (should (equal (mapcar (lambda (request) (plist-get request :ids))
                           (wasabi-receipts-test--sending
                             (wasabi-chat--update-chat
                              :messages (list (wasabi-receipts-test--message "A")))
                             (wasabi-chat--send-read-receipts)
                             ;; Opening the chat again sends nothing new.
                             (wasabi-chat--send-read-receipts)))
                   '(("A"))))))

(ert-deftest wasabi-receipts-test-nothing-when-off ()
  (let ((wasabi-send-read-receipts nil))
    (should-not (wasabi-receipts-test--sending
                  (wasabi-chat--update-chat
                   :messages (list (wasabi-receipts-test--message "A")))
                  (wasabi-chat--send-read-receipts)))))

(ert-deftest wasabi-receipts-test-failure-is-retried ()
  (let ((wasabi-send-read-receipts t)
        (wasabi-chat--read-receipts-sent (make-hash-table :test 'equal))
        (wasabi-buffer (get-buffer-create "*Wasabi*"))
        (attempts 0))
    (unwind-protect
        ;; A stub that never reports success, as when a request fails.
        (cl-letf (((symbol-function 'wasabi--send-chat-markread-request)
                   (lambda (&rest _args) (setq attempts (1+ attempts)))))
          (with-temp-buffer
            (wasabi-chat-mode)
            (setq wasabi-chat--chat
                  (wasabi-chat--make-chat
                   :chat-jid "447123456789@s.whatsapp.net"
                   :messages (list (wasabi-receipts-test--message "A"))))
            ;; Not marked sent, so the next open asks again.
            (wasabi-chat--send-read-receipts)
            (wasabi-chat--send-read-receipts)
            (should (equal attempts 2))))
      (kill-buffer wasabi-buffer))))

;;; What history and live messages carry

(ert-deftest wasabi-receipts-test-fields-from-history ()
  (let ((fields (wasabi-chat--receipt-fields
                 `((message_id . "A")
                   (chat_jid . "447123456789@s.whatsapp.net")
                   (sender_jid . "447123456789@s.whatsapp.net")
                   (data_json . ,(json-encode
                                  '((Info . ((Chat . "99988877@lid")
                                             (Sender . "99988877@lid")
                                             (IsFromMe . :json-false)))))))
                 "fallback@s.whatsapp.net")))
    ;; The message's own Info wins over the row it was filed in.
    (should (equal (map-elt fields :chat-jid) "99988877@lid"))
    (should (equal (map-elt fields :sender-jid) "99988877@lid"))
    (should-not (map-elt fields :from-me))))

(ert-deftest wasabi-receipts-test-fields-without-data-json ()
  ;; Messages sent from here are stored with no data_json and "me".
  (let ((fields (wasabi-chat--receipt-fields
                 '((message_id . "A")
                   (chat_jid . "447123456789@s.whatsapp.net")
                   (sender_jid . "me"))
                 "fallback@s.whatsapp.net")))
    (should (map-elt fields :from-me))
    (should (equal (map-elt fields :chat-jid) "447123456789@s.whatsapp.net"))))

(ert-deftest wasabi-receipts-test-fields-on-live-messages ()
  (let ((parsed (wasabi-chat--parse-notification
                 :p-message '((conversation . "hi"))
                 :p-info '((ID . "LIVE1")
                           (Chat . "447123456789@s.whatsapp.net")
                           (Sender . "447123456789@s.whatsapp.net")
                           (IsFromMe . nil)
                           (Timestamp . "2026-09-22T12:00:00Z"))
                 :chat-jid "447123456789@s.whatsapp.net")))
    (should (equal (map-elt parsed :message-id) "LIVE1"))
    (should-not (map-elt parsed :from-me))
    (should (equal (map-elt parsed :chat-jid) "447123456789@s.whatsapp.net"))))

;;; The toggle

(ert-deftest wasabi-receipts-test-toggle ()
  (let ((wasabi-send-read-receipts t))
    (wasabi-toggle-read-receipts)
    (should-not wasabi-send-read-receipts)
    (wasabi-toggle-read-receipts)
    (should wasabi-send-read-receipts)))

(ert-deftest wasabi-receipts-test-bound-and-shown ()
  (should (eq (lookup-key wasabi-mode-map (kbd "r"))
              #'wasabi-toggle-read-receipts))
  (let ((buffer (generate-new-buffer "*wasabi-receipts-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (wasabi-mode)
          (let ((wasabi-send-read-receipts t))
            (wasabi--update-header-line)
            (should (string-match-p "r receipts off"
                                    (substring-no-properties header-line-format))))
          (let ((wasabi-send-read-receipts nil))
            (wasabi--update-header-line)
            (should (string-match-p "r receipts on"
                                    (substring-no-properties header-line-format)))))
      (kill-buffer buffer))))

(provide 'wasabi-receipts-test)
;;; wasabi-receipts-test.el ends here
