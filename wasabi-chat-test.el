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

(ert-deftest wasabi-chat-test-sticker-survives-an-undrawable-thumbnail ()
  ;; Not valid PNG data.  The sticker must still render as text rather
  ;; than taking the whole chat buffer down with it.
  (let ((content (wasabi-chat--parse-content
                  (wasabi-chat-test--sticker-message
                   (cons 'pngThumbnail (base64-encode-string "not a png"))))))
    (should (equal (substring-no-properties content) "[sticker]"))
    (should (get-text-property 0 'sticker-url content))))

(ert-deftest wasabi-chat-test-sticker-without-a-thumbnail ()
  (let ((content (wasabi-chat--parse-content
                  (wasabi-chat-test--sticker-message))))
    (should-not (get-text-property 0 'sticker-thumbnail content))
    ;; No thumbnail to show, but it can still be downloaded on RET.
    (should (get-text-property 0 'keymap content))))

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
