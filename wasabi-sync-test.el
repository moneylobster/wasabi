;;; wasabi-sync-test.el --- Tests for wasabi's sync indicator  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-sync-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wasabi)

(defmacro wasabi-sync-test--with-buffer (&rest body)
  "Run BODY in a throwaway `wasabi-mode' buffer with state."
  (declare (indent 0))
  `(let ((buffer (generate-new-buffer "*wasabi-sync-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (wasabi-mode)
           (setq wasabi--state (wasabi--make-state :wasabi-buffer buffer))
           ,@body)
       (with-current-buffer buffer
         (when-let ((timer (and wasabi--state
                                (map-elt wasabi--state :sync-timer))))
           (cancel-timer timer)))
       (kill-buffer buffer))))

(ert-deftest wasabi-sync-test-nothing-syncing-by-default ()
  (wasabi-sync-test--with-buffer
    (should-not (wasabi--syncing))))

(ert-deftest wasabi-sync-test-syncing-label ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-syncing "syncing messages")
    (should (equal (wasabi--syncing) "syncing messages"))
    (wasabi--set-syncing nil)
    (should-not (wasabi--syncing))))

(ert-deftest wasabi-sync-test-falls-back-to-background-refresh ()
  (wasabi-sync-test--with-buffer
    ;; Consed on rather than `map-put!', which cannot add a key to an
    ;; alist in place.
    (setq wasabi--state (cons (cons :silent-refresh t) wasabi--state))
    ;; Nothing announced, but a re-fetch is under way.
    (should (equal (wasabi--syncing) "refreshing"))
    ;; An announced sync says something more specific.
    (wasabi--set-syncing "syncing messages")
    (should (equal (wasabi--syncing) "syncing messages"))))

(ert-deftest wasabi-sync-test-batches-push-back-the-timer ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-syncing "syncing messages")
    (let ((first-timer (map-elt wasabi--state :sync-timer)))
      (should first-timer)
      ;; Another batch replaces the timer rather than stacking one up.
      (wasabi--set-syncing "syncing messages")
      (should (map-elt wasabi--state :sync-timer))
      (should-not (eq first-timer (map-elt wasabi--state :sync-timer)))
      (should-not (memq first-timer timer-list)))))

(ert-deftest wasabi-sync-test-clearing-cancels-the-timer ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-syncing "syncing messages")
    (let ((timer (map-elt wasabi--state :sync-timer)))
      (wasabi--set-syncing nil)
      (should-not (map-elt wasabi--state :sync-timer))
      (should-not (memq timer timer-list)))))

(ert-deftest wasabi-sync-test-header-line-shows-it ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-syncing "syncing messages")
    (wasabi--update-header-line)
    (should (string-match-p "syncing messages"
                            (substring-no-properties header-line-format)))
    (should (string-match-p (regexp-quote wasabi-sync-indicator)
                            (substring-no-properties header-line-format)))
    (wasabi--set-syncing nil)
    (wasabi--update-header-line)
    (should-not (string-match-p "syncing messages"
                                (substring-no-properties header-line-format)))
    ;; The rest of the header line survives either way.
    (should (string-match-p "Recent Chats"
                            (substring-no-properties header-line-format)))))

(ert-deftest wasabi-sync-test-empty-list-says-syncing ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-status :type 'ready :message nil)
    ;; No chats and no sync: an empty account, so offer to start one.
    (wasabi--refresh)
    (should (string-match-p "No recent chats" (buffer-string)))
    ;; No chats yet because they are still arriving: say so instead.
    (wasabi--set-syncing "syncing messages")
    (wasabi--refresh)
    (should (string-match-p "Syncing with WhatsApp" (buffer-string)))
    (should-not (string-match-p "No recent chats" (buffer-string)))))

(provide 'wasabi-sync-test)
;;; wasabi-sync-test.el ends here
