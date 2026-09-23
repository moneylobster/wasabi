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
  ;; Attach: the clipboard's image if there is one, else pick a file.
  (should (eq (lookup-key wasabi-chat-mode-map (kbd "C-c C-a"))
              #'wasabi-chat-attach)))

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

;;; From the clipboard

(defmacro wasabi-send-image-test--in-chat (&rest body)
  "Run BODY in a chat buffer, with a *Wasabi* buffer alongside."
  (declare (indent 0))
  `(let ((wasabi-buffer (get-buffer-create "*Wasabi*"))
         (chat-buffer (generate-new-buffer "*wasabi-send-image-test*")))
     (unwind-protect
         (with-current-buffer chat-buffer
           (wasabi-chat-mode)
           (setq wasabi-chat--chat
                 (wasabi-chat--make-chat :chat-jid "1@s.whatsapp.net"
                                         :contact-name "John"))
           ,@body)
       (kill-buffer chat-buffer)
       (kill-buffer wasabi-buffer))))

(ert-deftest wasabi-send-image-test-png-check ()
  (should (wasabi-chat--png-file-p (wasabi-send-image-test--write "a.png")))
  (let ((not-png (make-temp-file "wasabi-test" nil ".png" "just text")))
    (should-not (wasabi-chat--png-file-p not-png)))
  (should-not (wasabi-chat--png-file-p "/no/such/file.png")))

(ert-deftest wasabi-send-image-test-clipboard-through-emacs ()
  ;; Where Emacs can read images from the clipboard itself.
  (let ((system-type 'gnu/linux)
        (png-data (base64-decode-string wasabi-send-image-test--png)))
    (cl-letf (((symbol-function 'gui-get-selection)
               (lambda (selection type)
                 (and (eq selection 'CLIPBOARD) (eq type 'image/png) png-data))))
      (let ((found (wasabi-chat--clipboard-image)))
        (unwind-protect
            (progn
              (should (wasabi-chat--png-file-p (car found)))
              ;; Made to be sent, so it goes afterwards.
              (should (cdr found)))
          (when found (delete-file (car found))))))))

(ert-deftest wasabi-send-image-test-clipboard-through-a-tool ()
  ;; Where Emacs cannot, a tool writing the PNG to stdout.
  (let ((system-type 'gnu/linux)
        (png-data (base64-decode-string wasabi-send-image-test--png)))
    (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) nil))
              ((symbol-function 'executable-find)
               (lambda (name) (equal name "wl-paste")))
              ((symbol-function 'call-process)
               (lambda (program _infile destination _display &rest _args)
                 (should (equal program "wl-paste"))
                 (let ((coding-system-for-write 'binary))
                   (with-temp-file (cadr destination)
                     (set-buffer-multibyte nil)
                     (insert png-data)))
                 0)))
      (let ((found (wasabi-chat--clipboard-image)))
        (unwind-protect
            (should (wasabi-chat--png-file-p (car found)))
          (when found (delete-file (car found))))))))

(ert-deftest wasabi-send-image-test-no-clipboard-image-leaves-nothing ()
  (let* ((system-type 'gnu/linux)
         (made nil))
    (cl-letf* ((real-make-temp-file (symbol-function 'make-temp-file))
               ((symbol-function 'make-temp-file)
                (lambda (&rest args) (setq made (apply real-make-temp-file args))))
               ((symbol-function 'gui-get-selection) (lambda (&rest _) nil))
               ((symbol-function 'executable-find) (lambda (_) nil)))
      (should-not (wasabi-chat--clipboard-image))
      ;; The file it would have saved to is gone again.
      (should made)
      (should-not (file-exists-p made)))))

(ert-deftest wasabi-send-image-test-clipboard-on-windows ()
  (let ((system-type 'windows-nt)
        (png-data (base64-decode-string wasabi-send-image-test--png))
        (answer nil)
        (script nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "powershell"))
              ((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest args)
                 (setq script (decode-coding-string
                               (base64-decode-string (car (last args))) 'utf-16le))
                 (pcase answer
                   ('picture
                    (let ((out (progn (string-match "\\$out = '\\(.*\\)'" script)
                                      (match-string 1 script))))
                      (let ((coding-system-for-write 'binary))
                        (with-temp-file (string-replace "''" "'" out)
                          (set-buffer-multibyte nil)
                          (insert png-data)))
                      (insert "IMAGE " out "\n")
                      0))
                   ('file (insert "FILE C:/Users/Anil/Pictures/cat.jpg\n") 0)
                   (_ 1))))
              ((symbol-function 'file-readable-p)
               (lambda (file)
                 (or (equal file "C:/Users/Anil/Pictures/cat.jpg")
                     (file-exists-p file)))))
      ;; A copied picture: saved, and made to be sent.
      (setq answer 'picture)
      (let ((found (wasabi-chat--clipboard-image)))
        (unwind-protect
            (progn (should (wasabi-chat--png-file-p (car found)))
                   (should (cdr found)))
          (when found (delete-file (car found)))))
      ;; The script arrives whole, encoded, and asks the clipboard.
      (should (string-match-p "Clipboard\\]::GetImage()" script))
      ;; A file copied in Explorer: sent as it is, and never deleted.
      (setq answer 'file)
      (should (equal (wasabi-chat--clipboard-image)
                     '("C:/Users/Anil/Pictures/cat.jpg")))
      ;; Neither.
      (setq answer nil)
      (should-not (wasabi-chat--clipboard-image)))))

(ert-deftest wasabi-send-image-test-attach-prefers-the-clipboard ()
  (let* ((file (wasabi-send-image-test--write "clip.png"))
         (sent nil))
    (wasabi-send-image-test--in-chat
      (cl-letf (((symbol-function 'wasabi-chat--clipboard-image)
                 (lambda () (cons file t)))
                ((symbol-function 'wasabi-chat--read-caption-for)
                 (lambda (_file) "look"))
                ((symbol-function 'wasabi-chat-send-image)
                 (lambda (&rest args) (setq sent args))))
        (wasabi-chat-attach)
        (should (equal sent (list file "look" t)))))))

(ert-deftest wasabi-send-image-test-attach-falls-back-to-picking ()
  (let ((picked 0))
    (wasabi-send-image-test--in-chat
      (cl-letf (((symbol-function 'wasabi-chat--clipboard-image) (lambda () nil))
                ((symbol-function 'call-interactively)
                 (lambda (command &rest _)
                   (should (eq command #'wasabi-chat-send-image))
                   (setq picked (1+ picked)))))
        ;; Nothing on the clipboard.
        (wasabi-chat-attach)
        (should (equal picked 1))))))

(ert-deftest wasabi-send-image-test-prefix-always-picks ()
  (let ((looked nil) (picked nil))
    (wasabi-send-image-test--in-chat
      (cl-letf (((symbol-function 'wasabi-chat--clipboard-image)
                 (lambda () (setq looked t) (cons "x.png" t)))
                ((symbol-function 'call-interactively)
                 (lambda (&rest _) (setq picked t))))
        (wasabi-chat-attach '(4))
        (should picked)
        ;; Not even a look at the clipboard.
        (should-not looked)))))

(ert-deftest wasabi-send-image-test-cancelled-clipboard-image-is-cleaned-up ()
  (let ((file (wasabi-send-image-test--write "clip.png"))
        (sent nil))
    (wasabi-send-image-test--in-chat
      (cl-letf (((symbol-function 'wasabi-chat--clipboard-image)
                 (lambda () (cons file t)))
                ((symbol-function 'wasabi-chat--read-caption-for)
                 (lambda (_file) (signal 'quit nil)))
                ((symbol-function 'wasabi-chat-send-image)
                 (lambda (&rest _) (setq sent t))))
        (wasabi-chat-attach)
        (should-not sent)
        (should-not (file-exists-p file))))))

(ert-deftest wasabi-send-image-test-cancelled-copied-file-is-kept ()
  ;; A file copied in Explorer is the user's own: never deleted.
  (let ((file (wasabi-send-image-test--write "mine.png")))
    (wasabi-send-image-test--in-chat
      (cl-letf (((symbol-function 'wasabi-chat--clipboard-image)
                 (lambda () (cons file nil)))
                ((symbol-function 'wasabi-chat--read-caption-for)
                 (lambda (_file) (signal 'quit nil))))
        (wasabi-chat-attach)
        (should (file-exists-p file))))))

(ert-deftest wasabi-send-image-test-temporary-file-goes-after-sending ()
  (let* ((wasabi-data-dir (make-temp-file "wasabi-test" t))
         (file (wasabi-send-image-test--write "clip.png"))
         (appended nil))
    (wasabi-send-image-test--in-chat
      (cl-letf (((symbol-function 'wasabi--send-chat-send-image-request)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-success)
                            '((Timestamp . 1790000000) (Id . "3EBCLIP")))))
                ((symbol-function 'wasabi-chat--append-message)
                 (lambda (message) (setq appended message))))
        (wasabi-chat-send-image file nil t)
        (should-not (file-exists-p file))
        ;; The kept copy is what the chat shows, and it stays.
        (should (wasabi-chat--sent-image-file "3EBCLIP"))
        (should (eq (car (get-text-property 0 'display (map-elt appended :content)))
                    'image))))))

(ert-deftest wasabi-send-image-test-temporary-file-goes-after-failing ()
  (let ((file (wasabi-send-image-test--write "clip.png")))
    (wasabi-send-image-test--in-chat
      (cl-letf (((symbol-function 'wasabi--send-chat-send-image-request)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-failure) '((message . "no"))))))
        (wasabi-chat-send-image file nil t)
        (should-not (file-exists-p file))))))

(ert-deftest wasabi-send-image-test-clipboard-answer-among-noise ()
  ;; What PowerShell really prints when stderr is not kept apart: a
  ;; progress record in CLIXML around the answer.
  (should (equal (wasabi-chat--clipboard-script-answer
                  "#< CLIXML\nIMAGE c:/Temp/clip.png\n<Objs Version=\"1.1.0.1\"><Obj S=\"progress\"/></Objs>")
                 '(image . "c:/Temp/clip.png")))
  (should (equal (wasabi-chat--clipboard-script-answer "FILE C:/Pictures/cat.jpg\r\n")
                 '(file . "C:/Pictures/cat.jpg")))
  (should-not (wasabi-chat--clipboard-script-answer "#< CLIXML\n<Objs/>"))
  (should-not (wasabi-chat--clipboard-script-answer nil)))

(ert-deftest wasabi-send-image-test-powershell-stderr-kept-apart ()
  (let ((system-type 'windows-nt)
        (destination nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "powershell"))
              ((symbol-function 'call-process)
               (lambda (_program _infile dest &rest _)
                 (setq destination dest)
                 1)))
      (wasabi-chat--clipboard-image)
      ;; Standard output to the buffer, standard error discarded.
      (should (equal destination '(t nil))))))

(provide 'wasabi-send-image-test)
;;; wasabi-send-image-test.el ends here
