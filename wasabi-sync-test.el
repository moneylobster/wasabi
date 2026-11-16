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

(ert-deftest wasabi-sync-test-background-refresh-keeps-the-list ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-status :type 'ready :message nil)
    (should (string-match-p "No recent chats" (buffer-string)))
    ;; A sync starts a silent re-fetch: the status becomes a loading one,
    ;; but the list that is already up must stay.
    (map-put! wasabi--state :silent-refresh t)
    (wasabi--update-header-line)
    (wasabi--set-status :type 'fetch-contacts
                        :message "Loading..." :silent t)
    ;; Timers, window changes and AppStateSyncComplete can still ask for
    ;; a redraw while the re-fetch runs.
    (wasabi--refresh)
    ;; With no chats yet, the empty state says what is happening rather
    ;; than flashing the loading message.
    (should (string-match-p "Syncing with WhatsApp" (buffer-string)))
    (should-not (string-match-p "Loading" (buffer-string)))
    ;; The header line stays too, and says what is going on.
    (should (string-match-p "Recent Chats"
                            (substring-no-properties header-line-format)))
    (should (string-match-p "refreshing"
                            (substring-no-properties header-line-format)))
    ;; Reaching ready draws the list again.
    (wasabi--set-status :type 'ready :message nil)
    (should (string-match-p "No recent chats" (buffer-string)))
    (should-not (string-match-p "refreshing"
                                (substring-no-properties header-line-format)))))

(ert-deftest wasabi-sync-test-background-refresh-keeps-the-list-updated ()
  (wasabi-sync-test--with-buffer
    (map-put! wasabi--state :chats-index
              (list (list (cons :chat-jid "1@s.whatsapp.net")
                          (cons :display-name "Old chat")
                          (cons :last-updated "2025-11-10T12:00:00Z"))))
    (wasabi--set-status :type 'ready :message nil)
    (should (string-match-p "Old chat" (buffer-string)))
    ;; A silent re-fetch starts.
    (map-put! wasabi--state :silent-refresh t)
    (wasabi--set-status :type 'fetch-contacts
                        :message "Loading..." :silent t)
    ;; New data arrives while it runs, and a redraw is asked for.
    (map-put! wasabi--state :chats-index
              (list (list (cons :chat-jid "2@s.whatsapp.net")
                          (cons :display-name "New chat")
                          (cons :last-updated "2025-11-10T13:00:00Z"))))
    (wasabi--refresh)
    (should (string-match-p "New chat" (buffer-string)))
    (should-not (string-match-p "Old chat" (buffer-string)))
    (should-not (string-match-p "Loading" (buffer-string)))))

(ert-deftest wasabi-sync-test-background-refresh-does-not-hide-errors ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-status :type 'ready :message nil)
    (map-put! wasabi--state :silent-refresh t)
    (wasabi--set-status :type 'fetch-contacts
                        :message "Loading..." :silent t)
    (should-not (string-match-p "Loading" (buffer-string)))
    ;; A status meant to be seen ends the silent refresh and is drawn.
    (wasabi--set-status :type 'error :message "Something broke")
    (should (string-match-p "Something broke" (buffer-string)))))

(ert-deftest wasabi-sync-test-index-failure-during-startup-shows-error ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-status :type 'fetch-chats :message "Loading...")
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (funcall (plist-get args :on-failure)
                          '((message . "boom"))))))
      (wasabi--send-chat-index-request
       :on-finished (lambda () (wasabi--set-status :type 'ready :message nil))))
    (should (eq (map-nested-elt (wasabi--state) '(:status :type)) 'error))
    (should (string-match-p "Failed to fetch chats" (buffer-string)))))

(ert-deftest wasabi-sync-test-index-failure-during-background-refresh-finishes ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-status :type 'ready :message nil)
    (map-put! wasabi--state :silent-refresh t)
    (wasabi--set-status :type 'fetch-chats :message "Loading..." :silent t)
    (let (finished)
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-failure)
                            '((message . "boom"))))))
        (wasabi--send-chat-index-request
         :on-finished (lambda ()
                        (setq finished t)
                        (wasabi--set-status :type 'ready :message nil))))
      (should finished)
      (should (eq (map-nested-elt (wasabi--state) '(:status :type)) 'ready))
      (should-not (map-elt (wasabi--state) :silent-refresh))
      (should (string-match-p "No recent chats" (buffer-string))))))

(ert-deftest wasabi-sync-test-index-failure-in-background-refresh-leaves-status ()
  (wasabi-sync-test--with-buffer
    (wasabi--set-status :type 'ready :message nil)
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (funcall (plist-get args :on-failure)
                          '((message . "boom"))))))
      (wasabi--send-chat-index-request))
    (should (eq (map-nested-elt (wasabi--state) '(:status :type)) 'ready))))

(ert-deftest wasabi-sync-test-loading-screen-without-a-background-refresh ()
  ;; Startup and pairing still show their status over an empty buffer.
  (wasabi-sync-test--with-buffer
    (wasabi--set-status :type 'fetch-contacts :message "Loading...")
    (should (string-match-p "Loading" (buffer-string)))))

(provide 'wasabi-sync-test)
;;; wasabi-sync-test.el ends here
