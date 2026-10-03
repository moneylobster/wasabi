;;; wasabi-links-test.el --- Tests for links in messages  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-links-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wasabi)

(defun wasabi-links-test--links (text)
  "The (TEXT-OF-LINK . TARGET) of each link in TEXT, in order."
  (let ((links '())
        (position 0))
    (while (setq position (text-property-not-all position (length text)
                                                 'wasabi-url nil text))
      (let ((end (or (next-single-property-change position 'wasabi-url text)
                     (length text))))
        (push (cons (substring-no-properties text position end)
                    (get-text-property position 'wasabi-url text))
              links)
        (setq position end)))
    (nreverse links)))

(ert-deftest wasabi-links-test-finds-addresses ()
  (should (equal (wasabi-links-test--links
                  (wasabi-chat--linkify
                   "see https://example.com/a?b=1. and HTTP://Example.org/x, too"))
                 '(("https://example.com/a?b=1" . "https://example.com/a?b=1")
                   ("HTTP://Example.org/x" . "HTTP://Example.org/x")))))

(ert-deftest wasabi-links-test-keeps-brackets-straight ()
  ;; A Wikipedia link keeps its own parentheses, and not the ones round it.
  (should (equal (wasabi-links-test--links
                  (wasabi-chat--linkify "(https://en.wikipedia.org/wiki/Foo_(bar))"))
                 '(("https://en.wikipedia.org/wiki/Foo_(bar)"
                    . "https://en.wikipedia.org/wiki/Foo_(bar)")))))

(ert-deftest wasabi-links-test-bare-www ()
  (should (equal (wasabi-links-test--links (wasabi-chat--linkify "go to www.example.com, now"))
                 '(("www.example.com" . "https://www.example.com")))))

(ert-deftest wasabi-links-test-plain-text-untouched ()
  (let ((text "nothing to see here: example dot com"))
    (should (equal-including-properties (wasabi-chat--linkify text) text))))

(ert-deftest wasabi-links-test-does-not-change-the-message ()
  (let ((text (copy-sequence "https://example.com")))
    (wasabi-chat--linkify text)
    (should-not (text-properties-at 0 text))))

(ert-deftest wasabi-links-test-leaves-actions-alone ()
  ;; An image's text already does something; it is not made a link too.
  (let ((text (concat (propertize "https://mmg.whatsapp.net/x" 'keymap (make-sparse-keymap))
                      " and https://example.com")))
    (should (equal (mapcar #'car (wasabi-links-test--links (wasabi-chat--linkify text)))
                   '("https://example.com")))))

(ert-deftest wasabi-links-test-opens-from-a-chat ()
  (let ((buffer (generate-new-buffer "*wasabi-links-test*"))
        (opened nil)
        (wasabi-send-read-receipts nil))
    (unwind-protect
        (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url)))
                  ((symbol-function 'recenter) #'ignore))
          (with-current-buffer buffer
            (wasabi-chat-mode)
            (setq wasabi-chat--chat
                  (wasabi-chat--make-chat :chat-jid "447123456789@s.whatsapp.net"))
            (wasabi-chat--refresh
             (list (list (cons :message-id "A")
                         (cons :sender-name "John")
                         (cons :timestamp "2026-10-03T12:00:00Z")
                         (cons :content "look: www.example.com/page"))))
            (goto-char (point-min))
            (search-forward "www.")
            (should (eq (get-text-property (point) 'face) 'link))
            (call-interactively (lookup-key (get-text-property (point) 'keymap) (kbd "RET")))
            (should (equal opened "https://www.example.com/page"))))
      (kill-buffer buffer))))

(ert-deftest wasabi-links-test-in-edits-too ()
  (let ((rendered (wasabi-chat--render-message
                   :sender-name "John" :timestamp "2026-10-03T12:00:00Z"
                   :content "old" :max-sender-width 4
                   :changes '((:edits ("2026-10-03T12:05:00Z" . "https://new.example"))))))
    (should (member "https://new.example"
                    (mapcar #'cdr (wasabi-links-test--links rendered))))))

(provide 'wasabi-links-test)
;;; wasabi-links-test.el ends here
