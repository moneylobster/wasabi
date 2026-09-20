;;; wasabi-chat-test.el --- Tests for wasabi-chat  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-chat-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Note that thumbnail rendering is not covered: `create-image' and
;; `image-size' need a display, which batch mode has not got.

;;; Code:

(require 'ert)
(require 'wasabi)

;;; Stickers

(defun wasabi-chat-test--sticker-message (&rest overrides)
  "Build a protocol sticker message, merging OVERRIDES into stickerMessage."
  `((stickerMessage . ,(append
                        overrides
                        '((URL . "https://mmg.whatsapp.net/enc-sticker")
                          (directPath . "/v/t62.1/sticker.enc")
                          (mediaKey . "c3RpY2tlcm1lZGlha2V5")
                          (mimetype . "image/webp")
                          (fileEncSHA256 . "ZW5jc2hhMjU2")
                          (fileSHA256 . "c2hhMjU2Lw==")
                          (fileLength . 12345)
                          (width . 512)
                          (height . 512))))))

(ert-deftest wasabi-chat-test-sticker-content-carries-media-metadata ()
  (let ((content (wasabi-chat--parse-content
                  (wasabi-chat-test--sticker-message))))
    (should (equal (substring-no-properties content) "[sticker]"))
    ;; Everything the download needs travels with the text.
    (should (equal (get-text-property 0 'sticker-url content)
                   "https://mmg.whatsapp.net/enc-sticker"))
    (should (equal (get-text-property 0 'sticker-direct-path content)
                   "/v/t62.1/sticker.enc"))
    (should (equal (get-text-property 0 'sticker-media-key content)
                   "c3RpY2tlcm1lZGlha2V5"))
    (should (equal (get-text-property 0 'sticker-mimetype content) "image/webp"))
    (should (equal (get-text-property 0 'sticker-file-enc-sha256 content)
                   "ZW5jc2hhMjU2"))
    (should (equal (get-text-property 0 'sticker-file-sha256 content)
                   "c2hhMjU2Lw=="))
    (should (equal (get-text-property 0 'sticker-file-length content) 12345))
    (should (equal (get-text-property 0 'sticker-width content) 512))
    (should (equal (get-text-property 0 'sticker-height content) 512))))

(ert-deftest wasabi-chat-test-sticker-is-actionable ()
  (let ((content (wasabi-chat--parse-content
                  (wasabi-chat-test--sticker-message))))
    ;; A keymap is what makes TAB land on it and RET open it.
    (should (get-text-property 0 'keymap content))
    (should (eq (get-text-property 0 'mouse-face content) 'highlight))))

(ert-deftest wasabi-chat-test-sticker-renders-as-text-until-fetched ()
  ;; Sticker messages carry no thumbnail, so one we have not fetched has
  ;; nothing to show yet.  It still has to render, and stay actionable.
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (content (wasabi-chat--parse-content
                   (wasabi-chat-test--sticker-message))))
    (should (equal (substring-no-properties content) "[sticker]"))
    (should-not (get-text-property 0 'display content))
    (should (get-text-property 0 'keymap content))))

(ert-deftest wasabi-chat-test-sticker-renders-inline-once-cached ()
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (cache-file (wasabi-chat--sticker-cache-file "c2hhMjU2Lw==" "image/png")))
    (make-directory (file-name-directory cache-file) t)
    (wasabi-chat-test--write-png cache-file)
    (let ((content (wasabi-chat--parse-content
                    (wasabi-chat-test--sticker-message
                     (cons 'mimetype "image/png")))))
      ;; Fetched once, shown inline from then on.
      (should (eq (car (get-text-property 0 'display content)) 'image))
      (should (get-text-property 0 'keymap content)))))

;;; Fetching stickers into a rendered chat

(defconst wasabi-chat-test--png
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
  "A 1x1 transparent PNG, base64 encoded.")

(defun wasabi-chat-test--write-png (file-path)
  "Write a tiny valid PNG to FILE-PATH."
  (make-directory (file-name-directory file-path) t)
  (let ((coding-system-for-write 'binary))
    (with-temp-file file-path
      (set-buffer-multibyte nil)
      (insert (base64-decode-string wasabi-chat-test--png)))))

(ert-deftest wasabi-chat-test-sticker-cache-file-naming ()
  (let ((wasabi-data-dir (make-temp-file "wasabi-test" t)))
    ;; Named by content hash, so the same sticker is fetched once no
    ;; matter how many chats it turns up in.
    (should (equal (file-name-nondirectory
                    (wasabi-chat--sticker-cache-file "ab/cd+12==" nil))
                   "abcd12.webp"))
    (should (equal (file-name-nondirectory
                    (wasabi-chat--sticker-cache-file "abcd" "image/png"))
                   "abcd.png"))
    ;; Nothing to name it by.
    (should-not (wasabi-chat--sticker-cache-file nil "image/webp"))
    (should-not (wasabi-chat--sticker-cache-file "" "image/webp"))))

(ert-deftest wasabi-chat-test-sticker-regions ()
  (with-temp-buffer
    (insert "before ")
    (insert (propertize "[sticker]" 'sticker-url "u1" 'sticker-file-sha256 "s1"))
    (insert " between ")
    (insert (propertize "[sticker]" 'sticker-url "u2" 'sticker-file-sha256 "s2"))
    (insert " after")
    (let ((regions (wasabi-chat--sticker-regions)))
      (should (equal (length regions) 2))
      (should (equal (buffer-substring-no-properties (car (nth 0 regions))
                                                     (cdr (nth 0 regions)))
                     "[sticker]"))
      (should (equal (get-text-property (car (nth 1 regions))
                                        'sticker-file-sha256)
                     "s2")))))

(ert-deftest wasabi-chat-test-show-sticker-draws-every-copy ()
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (cache-file (expand-file-name "s1.png" wasabi-data-dir)))
    (wasabi-chat-test--write-png cache-file)
    (with-temp-buffer
      (insert (propertize "[sticker]" 'sticker-url "u" 'sticker-file-sha256 "s1"))
      (insert " and again ")
      (insert (propertize "[sticker]" 'sticker-url "u" 'sticker-file-sha256 "s1"))
      (insert " but not ")
      (insert (propertize "[sticker]" 'sticker-url "u" 'sticker-file-sha256 "s2"))
      ;; The same sticker is drawn everywhere it appears, from one fetch.
      (wasabi-chat--show-sticker "s1" cache-file)
      (let ((regions (wasabi-chat--sticker-regions)))
        (should (eq (car (get-text-property (car (nth 0 regions)) 'display)) 'image))
        (should (eq (car (get-text-property (car (nth 1 regions)) 'display)) 'image))
        (should-not (get-text-property (car (nth 2 regions)) 'display))))))

(ert-deftest wasabi-chat-test-show-sticker-keeps-text-actionable ()
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (cache-file (expand-file-name "s1.png" wasabi-data-dir)))
    (wasabi-chat-test--write-png cache-file)
    (with-temp-buffer
      (insert (propertize "[sticker]"
                          'sticker-url "u"
                          'sticker-file-sha256 "s1"
                          'keymap (make-sparse-keymap)))
      (wasabi-chat--show-sticker "s1" cache-file)
      ;; Drawing over the placeholder must not cost it its bindings.
      (should (get-text-property (point-min) 'keymap))
      (should (get-text-property (point-min) 'sticker-url)))))

(ert-deftest wasabi-chat-test-save-media ()
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (file-path (expand-file-name "media/out.png" wasabi-data-dir)))
    (should (wasabi-chat--save-media
             (concat "data:image/png;base64," wasabi-chat-test--png)
             file-path))
    (should (file-exists-p file-path))
    (should (eq (wasabi-chat--image-type file-path
                                         (with-temp-buffer
                                           (set-buffer-multibyte nil)
                                           (insert-file-contents-literally file-path)
                                           (buffer-string)))
                'png))
    ;; Not a data URL.
    (should-not (wasabi-chat--save-media "https://example.com/x.png" file-path))
    (should-not (wasabi-chat--save-media nil file-path))))

(ert-deftest wasabi-chat-test-view-sticker-needs-a-sticker ()
  (with-temp-buffer
    (insert "no sticker here")
    (goto-char (point-min))
    (should-error (wasabi-chat-view-sticker-at-point) :type 'user-error)))

(ert-deftest wasabi-chat-test-other-media-still-parses ()
  ;; The sticker branch must not have shadowed its neighbours.
  (should (equal (wasabi-chat--parse-content '((conversation . "Hi"))) "Hi"))
  (should (equal (wasabi-chat--parse-content '((audioMessage . ((seconds . 3)))))
                 "[audio]"))
  (should (equal (wasabi-chat--parse-content '((documentMessage . ((title . "x")))))
                 "[document]"))
  (should (equal (wasabi-chat--parse-content '((reactionMessage . ((text . "x")))))
                 "[reaction]"))
  (should (equal (wasabi-chat--parse-content '((somethingElse . t))) "[unknown]")))

;;; Image type detection

(ert-deftest wasabi-chat-test-image-type-from-data ()
  ;; Magic bytes win over the extension, which only records what
  ;; WhatsApp claimed the MIME type was.
  (should (eq (wasabi-chat--image-type "sticker.webp" "\x89PNG\r\n\x1a\n rest")
              'png))
  (should (eq (wasabi-chat--image-type "sticker.webp" "GIF89a rest") 'gif)))

(ert-deftest wasabi-chat-test-image-type-falls-back-to-extension ()
  (should (eq (wasabi-chat--image-type "sticker.webp" "") 'webp))
  (should (eq (wasabi-chat--image-type "photo.jpg" "") 'jpeg))
  (should (eq (wasabi-chat--image-type "photo.png" "") 'png))
  (should (eq (wasabi-chat--image-type "anim.gif" "") 'gif))
  (should (eq (wasabi-chat--image-type "mystery.dat" "") 'jpeg)))

(provide 'wasabi-chat-test)
;;; wasabi-chat-test.el ends here
