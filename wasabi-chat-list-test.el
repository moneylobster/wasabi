;;; wasabi-chat-list-test.el --- Tests for the chats list's order and dates  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-chat-list-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wasabi)

(defun wasabi-chat-list-test--labels (&rest times)
  "The date labels chats dated TIMES are grouped under, in order."
  (mapcar #'car
          (wasabi--group-chats-by-date
           (mapcar (lambda (time)
                     (list (cons :chat-jid (format "%s@s.whatsapp.net" (random)))
                           (cons :last-updated
                                 (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))))
                   times))))

;;; Date labels

(ert-deftest wasabi-chat-list-test-a-year-ago-is-not-this-year ()
  (let* ((now (decode-time))
         (this-year (encode-time (decoded-time-set-defaults
                                  (make-decoded-time :year (decoded-time-year now)
                                                     :month 1 :day 3 :hour 12))))
         (last-year (encode-time (decoded-time-set-defaults
                                  (make-decoded-time :year (1- (decoded-time-year now))
                                                     :month 1 :day 3 :hour 12))))
         (labels (wasabi-chat-list-test--labels this-year last-year)))
    ;; Unless today is 3 or 4 January, when the first is Today or Yesterday.
    (unless (member (car labels) '("Today" "Yesterday"))
      (should (equal (car labels) (format-time-string "%B %e" this-year))))
    ;; The same day a year earlier gets a group of its own, with its year.
    (should (equal (length labels) 2))
    (should (equal (cadr labels)
                   (format-time-string "%B %e, %Y" last-year)))))

(ert-deftest wasabi-chat-list-test-today-and-yesterday ()
  (should (equal (wasabi-chat-list-test--labels
                  (current-time)
                  (time-subtract nil (* 24 60 60)))
                 '("Today" "Yesterday")))
  ;; A year ago today is not today.
  (should-not (member "Today" (wasabi-chat-list-test--labels
                               (time-subtract nil (* 365 24 60 60))))))

;;; Sending moves a chat up

(ert-deftest wasabi-chat-list-test-sending-dates-the-chat ()
  (let ((wasabi--jid-canonical-table (make-hash-table :test 'equal))
        (wasabi--jid-variants-table (make-hash-table :test 'equal))
        (wasabi--push-names-table (make-hash-table :test 'equal))
        (wasabi--chat-times-table (make-hash-table :test 'equal))
        (wasabi-data-dir (make-temp-file "wasabi-test" t))
        (buffer (get-buffer-create "*Wasabi*"))
        (reparsed 0)
        (refetched 0))
    (unwind-protect
        (cl-letf (((symbol-function 'wasabi--reparse-chat-index)
                   (lambda () (setq reparsed (1+ reparsed))))
                  ((symbol-function 'wasabi--send-chat-index-request)
                   (lambda (&rest _) (setq refetched (1+ refetched)))))
          (with-current-buffer buffer
            (wasabi-mode)
            (setq wasabi--state (list (cons :status nil))))
          (puthash "447123456789@s.whatsapp.net" "2025-09-29T10:00:00Z"
                   wasabi--chat-times-table)
          (wasabi-chat--note-sent "447123456789@s.whatsapp.net"
                                  "2026-09-29T10:00:00+0000")
          (should (equal (gethash "447123456789@s.whatsapp.net"
                                  wasabi--chat-times-table)
                         "2026-09-29T10:00:00+0000"))
          (should (equal reparsed 1))
          (should (equal refetched 1)))
      (kill-buffer buffer))))

(ert-deftest wasabi-chat-list-test-sending-without-a-chats-list ()
  (let ((wasabi--chat-times-table (make-hash-table :test 'equal))
        (wasabi--jid-canonical-table (make-hash-table :test 'equal)))
    (when (get-buffer "*Wasabi*")
      (kill-buffer "*Wasabi*"))
    ;; Nothing to reorder, and no error for it.
    (wasabi-chat--note-sent "1@s.whatsapp.net" "2026-09-29T10:00:00Z")
    (should (gethash "1@s.whatsapp.net" wasabi--chat-times-table))))

(provide 'wasabi-chat-list-test)
;;; wasabi-chat-list-test.el ends here
