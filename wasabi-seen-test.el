;;; wasabi-seen-test.el --- Tests for showing whether our messages were read  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-seen-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The receipts shown here are the ones others send us, not the ones
;; we send; those are in wasabi-receipts-test.el.

;;; Code:

(require 'ert)
(require 'wasabi)

(defmacro wasabi-seen-test--fresh (&rest body)
  "Run BODY with no receipts recorded, and none on disk."
  (declare (indent 0))
  `(let ((wasabi-chat--receipts (make-hash-table :test 'equal))
         (wasabi-chat--receipts-dirty nil)
         (wasabi-chat--receipts-save-timer nil)
         (wasabi-show-read-receipts t)
         (wasabi-data-dir (make-temp-file "wasabi-test" t)))
     (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer)))
       ,@body)))

(defun wasabi-seen-test--event (state &rest fields)
  "A receipt notification's STATE and event, with FIELDS overriding it."
  (let ((event (list (cons 'Chat "447123456789@s.whatsapp.net")
                     (cons 'Sender "447123456789:3@s.whatsapp.net")
                     (cons 'IsFromMe nil)
                     (cons 'MessageIDs ["A"])
                     (cons 'Timestamp "2026-09-29T14:07:00+03:00"))))
    (while fields
      (setf (alist-get (pop fields) event) (pop fields)))
    (list event state)))

(defun wasabi-seen-test--record (state &rest fields)
  "Record the receipt STATE and FIELDS describe."
  (let ((receipt (apply #'wasabi-chat--receipt
                        (append (apply #'wasabi-seen-test--event state fields)
                                (list nil)))))
    (wasabi-chat--record-receipt receipt)
    receipt))

(defun wasabi-seen-test--message (id from-me)
  "A message with ID, sent by us when FROM-ME."
  (list (cons :message-id id)
        (cons :sender-name (if from-me "Me" "John"))
        (cons :timestamp "2026-09-29T14:00:00+03:00")
        (cons :content (concat "text " id))
        (cons :from-me from-me)))

(defmacro wasabi-seen-test--in-chat (chat-jid messages &rest body)
  "Run BODY in a buffer for CHAT-JID showing MESSAGES."
  (declare (indent 2))
  `(let ((buffer (generate-new-buffer "*wasabi-seen-test*"))
         (wasabi-send-read-receipts nil))
     (unwind-protect
         (with-current-buffer buffer
           (wasabi-chat-mode)
           (setq wasabi-chat--chat (wasabi-chat--make-chat :chat-jid ,chat-jid))
           (cl-letf (((symbol-function 'recenter) #'ignore))
             (wasabi-chat--refresh ,messages)
             ,@body))
       (kill-buffer buffer))))

(defun wasabi-seen-test--lines ()
  "The receipt lines in this buffer, in order."
  (let ((lines '()))
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward 'face 'wasabi-chat-receipt t)))
        (push (buffer-substring-no-properties (prop-match-beginning match)
                                              (prop-match-end match))
              lines)))
    (nreverse lines)))

(defun wasabi-seen-test--hover ()
  "The hover text of this buffer's receipt line."
  (save-excursion
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward 'face 'wasabi-chat-receipt t)))
      (get-text-property (prop-match-beginning match) 'help-echo))))

;;; Which receipts count

(ert-deftest wasabi-seen-test-reads-a-receipt ()
  (let ((receipt (apply #'wasabi-chat--receipt
                        (append (wasabi-seen-test--event "Read") (list nil)))))
    (should (eq (map-elt receipt :kind) 'read))
    (should (equal (map-elt receipt :ids) '("A")))
    ;; The device it was read on is no matter.
    (should (equal (map-elt receipt :reader) "447123456789@s.whatsapp.net"))
    (should (equal (map-elt receipt :name) "447123456789")))
  (should (eq (map-elt (apply #'wasabi-chat--receipt
                              (append (wasabi-seen-test--event "Delivered") (list nil)))
                       :kind)
              'delivered)))

(ert-deftest wasabi-seen-test-ignores-our-own ()
  ;; What we read elsewhere, and our own devices' receipts, are not
  ;; anyone reading what we sent.
  (should-not (apply #'wasabi-chat--receipt
                     (append (wasabi-seen-test--event "ReadSelf") (list nil))))
  (should-not (apply #'wasabi-chat--receipt
                     (append (wasabi-seen-test--event "Read" 'IsFromMe t) (list nil))))
  (should (apply #'wasabi-chat--receipt
                 (append (wasabi-seen-test--event "Read" 'IsFromMe :false) (list nil))))
  (should-not (apply #'wasabi-chat--receipt
                     (append (wasabi-seen-test--event "Read" 'MessageIDs []) (list nil)))))

(ert-deftest wasabi-seen-test-keeps-the-first-read ()
  (wasabi-seen-test--fresh
    (wasabi-seen-test--record "Delivered" 'Timestamp "2026-09-29T14:01:00+03:00")
    (wasabi-seen-test--record "Read" 'Timestamp "2026-09-29T14:07:00+03:00")
    (wasabi-seen-test--record "Read" 'Timestamp "2026-09-29T15:00:00+03:00")
    (let ((seen (map-elt (map-elt (gethash "A" (wasabi-chat--receipts)) :readers)
                         "447123456789@s.whatsapp.net")))
      (should (string-match-p "T14:01" (map-elt seen :delivered)))
      (should (string-match-p "T14:07" (map-elt seen :read))))))

;;; Showing them

(ert-deftest wasabi-seen-test-only-the-latest-of-ours ()
  (wasabi-seen-test--fresh
    (wasabi-seen-test--record "Read" 'MessageIDs ["A" "C"])
    (wasabi-seen-test--in-chat "447123456789@s.whatsapp.net"
        (list (wasabi-seen-test--message "A" t)
              (wasabi-seen-test--message "B" nil)
              (wasabi-seen-test--message "C" t)
              ;; Their reply after it does not take the line.
              (wasabi-seen-test--message "D" nil))
      (should (equal (wasabi-seen-test--lines)
                     (list (format-time-string
                            "Read %H:%M"
                            (parse-iso8601-time-string "2026-09-29T14:07:00+03:00")))))
      ;; Under C: after its text, before D's.
      (should (string-match-p "text C\n *Read" (buffer-string))))))

(ert-deftest wasabi-seen-test-delivered-until-read ()
  (wasabi-seen-test--fresh
    (wasabi-seen-test--record "Delivered")
    (wasabi-seen-test--in-chat "447123456789@s.whatsapp.net"
        (list (wasabi-seen-test--message "A" t))
      (should (string-prefix-p "Delivered" (car (wasabi-seen-test--lines))))
      ;; Read while the chat is open: the line catches up.
      (wasabi-chat--apply-receipt (wasabi-seen-test--record "Read"))
      (should (string-prefix-p "Read" (car (wasabi-seen-test--lines))))
      (should (string-match-p "Delivered.*\nRead" (wasabi-seen-test--hover))))))

(ert-deftest wasabi-seen-test-nothing-known-nothing-shown ()
  (wasabi-seen-test--fresh
    (wasabi-seen-test--in-chat "447123456789@s.whatsapp.net"
        (list (wasabi-seen-test--message "A" t))
      (should-not (wasabi-seen-test--lines)))))

(ert-deftest wasabi-seen-test-groups-count-and-name ()
  (wasabi-seen-test--fresh
    ;; Recorded out of order: the hover lists them as they read it.
    (wasabi-seen-test--record "Read" 'Chat "123@g.us" 'Sender "2@lid"
                              'Timestamp "2026-09-29T14:09:00+03:00")
    (wasabi-seen-test--record "Read" 'Chat "123@g.us" 'Sender "1@s.whatsapp.net"
                              'Timestamp "2026-09-29T14:07:00+03:00")
    (wasabi-seen-test--record "Delivered" 'Chat "123@g.us" 'Sender "3@s.whatsapp.net")
    (wasabi-seen-test--in-chat "123@g.us"
        (list (wasabi-seen-test--message "A" t))
      (should (equal (wasabi-seen-test--lines) '("Read by 2")))
      (let ((hover (wasabi-seen-test--hover)))
        (should (string-match-p "Read by:\n  1 .*\n  2 " hover))
        (should (string-match-p "Delivered to:\n  3 " hover))))))

(ert-deftest wasabi-seen-test-sending-moves-the-line ()
  (wasabi-seen-test--fresh
    (wasabi-seen-test--record "Read")
    (wasabi-seen-test--in-chat "447123456789@s.whatsapp.net"
        (list (wasabi-seen-test--message "A" t))
      (should (wasabi-seen-test--lines))
      ;; A newer message of ours, not yet delivered: A no longer says.
      (wasabi-chat--append-message (wasabi-seen-test--message "B" t))
      (should-not (wasabi-seen-test--lines))
      (should (string-match-p "text A" (buffer-string)))
      (should (string-match-p "text B" (buffer-string)))
      ;; Their message after ours leaves it be.
      (wasabi-chat--record-receipt
       (wasabi-seen-test--record "Read" 'MessageIDs ["B"]))
      (wasabi-chat--redraw-latest-own)
      (wasabi-chat--append-message (wasabi-seen-test--message "C" nil))
      (should (equal (length (wasabi-seen-test--lines)) 1)))))

(ert-deftest wasabi-seen-test-toggle ()
  (wasabi-seen-test--fresh
    (wasabi-seen-test--record "Read")
    (wasabi-seen-test--in-chat "447123456789@s.whatsapp.net"
        (list (wasabi-seen-test--message "A" t))
      (should (wasabi-seen-test--lines))
      (wasabi-toggle-read-receipt-display)
      (should-not wasabi-show-read-receipts)
      (should-not (wasabi-seen-test--lines))
      (should (string-match-p "text A" (buffer-string)))
      (wasabi-toggle-read-receipt-display)
      (should (wasabi-seen-test--lines)))))

;;; Keeping them

(ert-deftest wasabi-seen-test-saved-most-recent-first ()
  (wasabi-seen-test--fresh
    (let ((wasabi-chat--receipts-kept 2))
      (dolist (id '("A" "B" "C"))
        (wasabi-seen-test--record "Read" 'MessageIDs (vector id))
        ;; Recorded in turn, so each is more recent than the last.
        (puthash id (cons (cons :seen (length (map-keys (wasabi-chat--receipts))))
                          (assq-delete-all :seen (gethash id (wasabi-chat--receipts))))
                 (wasabi-chat--receipts)))
      (wasabi-chat--save-receipts)
      (let ((wasabi-chat--receipts nil))
        (should (equal (sort (map-keys (wasabi-chat--receipts)) #'string<)
                       '("B" "C")))))))

(ert-deftest wasabi-seen-test-subscribed ()
  ;; wuzapi forwards receipts under this name, and only when asked to.
  (should (member "ReadReceipt" wasabi--event-subscriptions)))

(provide 'wasabi-seen-test)
;;; wasabi-seen-test.el ends here
