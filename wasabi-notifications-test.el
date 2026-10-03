;;; wasabi-notifications-test.el --- Tests for wasabi's notification toggle  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-notifications-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wasabi)

(defmacro wasabi-notifications-test--recording (&rest body)
  "Run BODY with notifications going to a recorder, and return what it saw."
  (declare (indent 0))
  `(let* ((seen '())
          (wasabi-message-notification-function
           (lambda (message) (push message seen))))
     ,@body
     (nreverse seen)))

(ert-deftest wasabi-notifications-test-notifies-when-enabled ()
  (let ((wasabi-notifications-enabled t))
    (should (equal (wasabi-notifications-test--recording
                     (wasabi--notify '((:content . "hello"))))
                   '(((:content . "hello")))))))

(ert-deftest wasabi-notifications-test-silent-when-disabled ()
  (let ((wasabi-notifications-enabled nil))
    (should-not (wasabi-notifications-test--recording
                  (wasabi--notify '((:content . "hello")))))))

(ert-deftest wasabi-notifications-test-toggle-round-trips ()
  (let ((wasabi-notifications-enabled t))
    (wasabi-notifications-test--recording
      (wasabi-toggle-notifications)
      (should-not wasabi-notifications-enabled)
      (wasabi--notify '((:content . "muted")))
      (wasabi-toggle-notifications)
      (should wasabi-notifications-enabled)
      ;; The chosen backend survives being muted and unmuted.
      (should (functionp wasabi-message-notification-function)))))

(ert-deftest wasabi-notifications-test-toggle-reaches-only-the-unmuted ()
  (let ((wasabi-notifications-enabled t))
    (should (equal (mapcar (lambda (m) (map-elt m :content))
                           (wasabi-notifications-test--recording
                             (wasabi--notify '((:content . "before")))
                             (wasabi-toggle-notifications)
                             (wasabi--notify '((:content . "muted")))
                             (wasabi-toggle-notifications)
                             (wasabi--notify '((:content . "after")))))
                   '("before" "after")))))

(ert-deftest wasabi-notifications-test-reacted-to-message-found ()
  (let ((buffer (generate-new-buffer "*wasabi-notifications-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (wasabi-chat-mode)
            (setq wasabi-chat--chat
                  (wasabi-chat--make-chat
                   :chat-jid "447123456789@s.whatsapp.net"
                   :messages (list (list (cons :message-id "A")
                                         (cons :content "see you there"))))))
          ;; Given the chat, as wasabi's own notifiers are.
          (should (equal (wasabi--get-msg-content "A" buffer) "see you there"))
          ;; Given only the ID, as a user's notification function is.
          (should (equal (wasabi--get-msg-content "A") "see you there"))
          (should-not (wasabi--get-msg-content "nowhere")))
      (kill-buffer buffer))))

(ert-deftest wasabi-notifications-test-bound-on-the-chat-list ()
  (should (eq (lookup-key wasabi-mode-map (kbd "m"))
              #'wasabi-toggle-notifications)))

(ert-deftest wasabi-notifications-test-header-offers-the-opposite ()
  (let ((buffer (generate-new-buffer "*wasabi-notifications-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (wasabi-mode)
          (let ((wasabi-notifications-enabled t))
            (wasabi--update-header-line)
            (should (string-match-p "m mute"
                                    (substring-no-properties header-line-format))))
          (let ((wasabi-notifications-enabled nil))
            (wasabi--update-header-line)
            (should (string-match-p "m unmute"
                                    (substring-no-properties header-line-format)))))
      (kill-buffer buffer))))

(provide 'wasabi-notifications-test)
;;; wasabi-notifications-test.el ends here
