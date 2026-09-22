;;; wasabi-send-image-test.el --- Tests for sending images  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-send-image-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The send itself is stubbed: there is no wuzapi to talk to.

;;; Code:

(require 'ert)
(require 'wasabi)

(defconst wasabi-send-image-test--png
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
  "A 1x1 transparent PNG, base64 encoded.")

(defun wasabi-send-image-test--write (name)
  "Write the test PNG to a temporary file called NAME, returning its path."
  (let ((file (expand-file-name name (make-temp-file "wasabi-test" t))))
    (let ((coding-system-for-write 'binary))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert (base64-decode-string wasabi-send-image-test--png))))
    file))

(ert-deftest wasabi-send-image-test-request-shape ()
  (let ((request (wasabi--make-chat-send-image-request
                  :token "tok" :phone "447123456789@s.whatsapp.net"
                  :image "data:image/png;base64,AAAA" :caption "look")))
    (should (equal (map-elt request :method) "chat.send.image"))
    (should (equal (map-nested-elt request '(:params Phone))
                   "447123456789@s.whatsapp.net"))
    (should (equal (map-nested-elt request '(:params Image))
                   "data:image/png;base64,AAAA"))
    (should (equal (map-nested-elt request '(:params Caption)) "look")))
  ;; No caption, or an empty one, sends none.
  (dolist (caption '(nil ""))
    (should-not (map-elt (map-elt (wasabi--make-chat-send-image-request
                                   :token "tok" :phone "1@s.whatsapp.net"
                                   :image "data:image/png;base64,AAAA"
                                   :caption caption)
                                  :params)
                         'Caption)))
  (should-error (wasabi--make-chat-send-image-request
                 :token "tok" :phone "1@s.whatsapp.net")))

(ert-deftest wasabi-send-image-test-data-url ()
  (let ((url (wasabi-chat--image-data-url
              (wasabi-send-image-test--write "photo.png"))))
    (should (string-prefix-p "data:image/png;base64," url))
    ;; What goes out decodes back to the file, byte for byte.
    (should (equal (base64-decode-string
                    (string-remove-prefix "data:image/png;base64," url))
                   (base64-decode-string wasabi-send-image-test--png)))
    ;; One line: a wrapped payload would not survive as a data URL.
    (should-not (string-match-p "\n" url))))

(ert-deftest wasabi-send-image-test-mimetype-follows-extension ()
  (should (string-prefix-p "data:image/jpeg;"
                           (wasabi-chat--image-data-url
                            (wasabi-send-image-test--write "photo.JPG"))))
  (should (string-prefix-p "data:image/gif;"
                           (wasabi-chat--image-data-url
                            (wasabi-send-image-test--write "anim.gif")))))

(ert-deftest wasabi-send-image-test-refuses-what-wuzapi-cannot-send ()
  ;; wuzapi has no decoder for these, and would fail building a thumbnail.
  (should-error (wasabi-chat--image-data-url
                 (wasabi-send-image-test--write "photo.webp"))
                :type 'user-error)
  (should-error (wasabi-chat--image-data-url
                 (wasabi-send-image-test--write "photo.heic"))
                :type 'user-error)
  (should-error (wasabi-chat--image-data-url "/no/such/photo.png")
                :type 'user-error))

(ert-deftest wasabi-send-image-test-picker-filter ()
  (let ((dir (make-temp-file "wasabi-test" t)))
    ;; Directories stay browsable.
    (should (wasabi-chat--sendable-image-p dir))
    (should (wasabi-chat--sendable-image-p "photo.png"))
    (should (wasabi-chat--sendable-image-p "photo.JPEG"))
    (should-not (wasabi-chat--sendable-image-p "photo.webp"))
    (should-not (wasabi-chat--sendable-image-p "notes.txt"))
    (should-not (wasabi-chat--sendable-image-p "no-extension"))))

(ert-deftest wasabi-send-image-test-sends-to-this-chat ()
  (let ((wasabi-buffer (get-buffer-create "*Wasabi*"))
        (chat-buffer (generate-new-buffer "*wasabi-send-image-test*"))
        (file (wasabi-send-image-test--write "photo.png"))
        (sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wasabi--send-chat-send-image-request)
                   (lambda (&rest args) (setq sent args))))
          (with-current-buffer chat-buffer
            (wasabi-chat-mode)
            (setq wasabi-chat--chat
                  (wasabi-chat--make-chat :chat-jid "447123456789@s.whatsapp.net"
                                          :contact-name "John"))
            (wasabi-chat-send-image file "look at this"))
          (should (equal (plist-get sent :phone) "447123456789@s.whatsapp.net"))
          (should (equal (plist-get sent :caption) "look at this"))
          (should (string-prefix-p "data:image/png;base64,"
                                   (plist-get sent :image))))
      (kill-buffer chat-buffer)
      (kill-buffer wasabi-buffer))))

(ert-deftest wasabi-send-image-test-needs-a-chat ()
  (with-temp-buffer
    (should-error (wasabi-chat-send-image
                   (wasabi-send-image-test--write "photo.png"))
                  :type 'user-error)))

(ert-deftest wasabi-send-image-test-sent-image-shows-caption ()
  (let ((content (wasabi-chat--sent-image-content
                  (wasabi-send-image-test--write "photo.png") "look")))
    (should (equal (substring-no-properties content) "[image]\nlook")))
  (should (equal (substring-no-properties
                  (wasabi-chat--sent-image-content
                   (wasabi-send-image-test--write "photo.png") ""))
                 "[image]")))

(ert-deftest wasabi-send-image-test-bound-in-chats ()
  (should (eq (lookup-key wasabi-chat-mode-map (kbd "C-c C-a"))
              #'wasabi-chat-send-image)))

(ert-deftest wasabi-send-image-test-keeps-a-copy ()
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (file (wasabi-send-image-test--write "Holiday Photo.PNG"))
         (copy (wasabi-chat--keep-sent-image file "3EB0ABC/123+")))
    (should (file-exists-p copy))
    ;; Named by message ID, keeping the original's kind of image.
    (should (equal (file-name-nondirectory copy) "sent-3EB0ABC123.png"))
    (should (equal (wasabi-chat--sent-image-file "3EB0ABC/123+") copy))
    ;; Nothing to name it by, nothing kept.
    (should-not (wasabi-chat--keep-sent-image file nil))
    (should-not (wasabi-chat--sent-image-file "never-sent"))))

(ert-deftest wasabi-send-image-test-sent-image-is-drawn ()
  (let ((content (wasabi-chat--sent-image-content
                  (wasabi-send-image-test--write "photo.png") nil)))
    ;; The file itself, not just the word.
    (should (eq (car (get-text-property 0 'display content)) 'image))
    ;; And RET opens it.
    (should (get-text-property 0 'keymap content))))

(ert-deftest wasabi-send-image-test-no-copy-still-says-image ()
  (let ((content (wasabi-chat--sent-image-content nil "look")))
    (should (equal (substring-no-properties content) "[image]\nlook"))
    (should-not (get-text-property 0 'display content))))

(ert-deftest wasabi-send-image-test-reloaded-from-history ()
  ;; What wuzapi stores for an image sent from here: the caption, if
  ;; any, and no data_json at all.
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (row '((message_id . "SENT1")
                (message_type . "image")
                (sender_jid . "me")
                (text_content . "")
                (timestamp . "2026-09-22T12:00:00Z")
                (data_json . ""))))
    ;; Without our copy it still says what it was, not a blank line.
    (should (equal (substring-no-properties
                    (map-elt (wasabi-chat--parse-message row :chat-jid "1@s.whatsapp.net")
                             :content))
                   "[image]"))
    ;; With it, the image comes back.
    (wasabi-chat--keep-sent-image (wasabi-send-image-test--write "photo.png") "SENT1")
    (let ((content (map-elt (wasabi-chat--parse-message row :chat-jid "1@s.whatsapp.net")
                            :content)))
      (should (eq (car (get-text-property 0 'display content)) 'image)))))

(ert-deftest wasabi-send-image-test-other-stored-messages ()
  ;; Text sent from here reads as itself.
  (should (equal (wasabi-chat--stored-content
                  '((message_type . "text") (text_content . "hello")))
                 "hello"))
  ;; Something with no text says what it was, rather than nothing.
  (should (equal (wasabi-chat--stored-content
                  '((message_type . "video") (text_content . "")))
                 "[video]"))
  (should (equal (wasabi-chat--stored-content
                  '((message_type . "text") (text_content . "")))
                 "[message]")))

(ert-deftest wasabi-send-image-test-send-keeps-the-copy ()
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (wasabi-buffer (get-buffer-create "*Wasabi*"))
         (chat-buffer (generate-new-buffer "*wasabi-send-image-test*"))
         (file (wasabi-send-image-test--write "photo.png"))
         (appended nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wasabi--send-chat-send-image-request)
                   (lambda (&rest args)
                     ;; What wuzapi answers with.
                     (funcall (plist-get args :on-success)
                              '((Details . "Sent") (Timestamp . 1790000000)
                                (Id . "3EB0SENT")))))
                  ((symbol-function 'wasabi-chat--append-message)
                   (lambda (message) (setq appended message))))
          (with-current-buffer chat-buffer
            (wasabi-chat-mode)
            (setq wasabi-chat--chat
                  (wasabi-chat--make-chat :chat-jid "1@s.whatsapp.net"))
            (wasabi-chat-send-image file nil))
          ;; Kept under the ID wuzapi gave it, for the next reload.
          (should (wasabi-chat--sent-image-file "3EB0SENT"))
          (should (eq (car (get-text-property 0 'display (map-elt appended :content)))
                      'image)))
      (kill-buffer chat-buffer)
      (kill-buffer wasabi-buffer))))

(provide 'wasabi-send-image-test)
;;; wasabi-send-image-test.el ends here
