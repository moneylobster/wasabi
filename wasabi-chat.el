;;; wasabi-chat.el --- Chat buffer for wasabi  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/wasabi

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; wasabi-chat provides the chat buffer functionality for wasabi,
;; handling the display and interaction with individual WhatsApp
;; conversations.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'parse-time)
(require 'wasabi-icon)

(declare-function wasabi--add-action-to-text "wasabi")
(declare-function wasabi--buffer "wasabi")
(declare-function wasabi--face-height-pixels "wasabi")
(declare-function wasabi--header-graphical-p "wasabi")
(declare-function wasabi--log "wasabi")
(declare-function wasabi--send-chat-history-request "wasabi")
(declare-function wasabi--send-chat-send-text-request "wasabi")
(declare-function wasabi--send-chat-send-image-request "wasabi")
(declare-function wasabi--send-download-image-request "wasabi")
(declare-function wasabi--send-download-video-request "wasabi")

(cl-defun wasabi-chat--make-chat (&key chat-jid contact-name max-sender-width messages)
  "Create a chat alist with CHAT-JID, CONTACT-NAME, MAX-SENDER-WIDTH, and MESSAGES."
  (list (cons :chat-jid chat-jid)
        (cons :contact-name contact-name)
        (cons :max-sender-width (or max-sender-width 0))
        (cons :messages (or messages nil))))

(defvar-local wasabi-chat--chat (wasabi-chat--make-chat)
  "Alist containing chat information for this buffer.
Keys:
  :chat-jid - The chat JID
  :contact-name - The contact name
  :max-sender-width - Maximum sender name width for alignment")

(defvar-local wasabi-chat--prompt-marker nil
  "Marker for the start of the prompt.")

(defvar-local wasabi-chat--input-start-marker nil
  "Marker for the start of user input.")

(defun wasabi-chat--update-chat (key value)
  "Update KEY in wasabi-chat--chat with VALUE, preserving other keys."
  (setq wasabi-chat--chat
        (cons (cons key value)
              (assq-delete-all key wasabi-chat--chat))))

(defvar-keymap wasabi-chat-mode-map
  :doc "Keymap for `wasabi-chat-mode'."
  "q" #'wasabi-chat-quit
  "n" #'wasabi-chat-next-message
  "p" #'wasabi-chat-previous-message
  "g" #'wasabi-chat-refresh
  "RET" #'wasabi-chat-send-input
  "C-c C-a" #'wasabi-chat-attach
  "C-a" #'wasabi-chat-beginning-of-line
  "TAB" #'wasabi-chat-next-actionable
  "S-TAB" #'wasabi-chat-previous-actionable
  "<tab>" #'wasabi-chat-next-actionable
  "<backtab>" #'wasabi-chat-previous-actionable)

;; Parsing functions - convert protocol structures to internal format

(defun wasabi-chat--parse-content (p-message)
  "Parse displayable content from protocol P-MESSAGE structure.
Returns string like \"Hello\" or \"[image]\"."
  (cond
   ((map-elt p-message 'conversation)
    (map-elt p-message 'conversation))
   ((map-elt p-message 'extendedTextMessage)
    (map-nested-elt p-message '(extendedTextMessage text)))
   ((map-elt p-message 'imageMessage)
    (wasabi--log "Image message arrived")
    (let* ((thumbnail (map-nested-elt p-message '(imageMessage JPEGThumbnail)))
           (image-text (if thumbnail
                           (propertize "[image]" 'display
                                       (wasabi-chat--create-rounded-image
                                        :image-data (base64-decode-string thumbnail)
                                        :image-type 'jpeg
                                        :max-width 50
                                        :max-height 50
                                        :corner-radius 6
                                        :padding-top 5
                                        :padding-bottom 5))
                         "[image]")))
      ;; Store metadata as text properties
      (add-text-properties 0 (length image-text)
                           `(image-url ,(map-nested-elt p-message '(imageMessage URL))
                                       image-direct-path ,(map-nested-elt p-message '(imageMessage directPath))
                                       image-media-key ,(map-nested-elt p-message '(imageMessage mediaKey))
                                       image-mimetype ,(map-nested-elt p-message '(imageMessage mimetype))
                                       image-file-enc-sha256 ,(map-nested-elt p-message '(imageMessage fileEncSHA256))
                                       image-file-sha256 ,(map-nested-elt p-message '(imageMessage fileSHA256))
                                       image-file-length ,(map-nested-elt p-message '(imageMessage fileLength))
                                       image-width ,(map-nested-elt p-message '(imageMessage width))
                                       image-height ,(map-nested-elt p-message '(imageMessage height))
                                       image-thumbnail ,thumbnail)
                           image-text)
      ;; Add action to view image on RET
      (setq image-text (wasabi--add-action-to-text
                        image-text
                        (lambda ()
                          (interactive)
                          (wasabi-chat-view-image-at-point))))
      (concat image-text
              (when-let ((caption (map-nested-elt p-message '(imageMessage caption))))
                (concat "\n" caption)))))
   ((map-elt p-message 'videoMessage)
    (wasabi--log "Video message arrived")
    (let* ((thumbnail (map-nested-elt p-message '(videoMessage JPEGThumbnail)))
           (video-text (if thumbnail
                           (propertize "[video]" 'display
                                       (wasabi-chat--create-rounded-image
                                        :image-data (base64-decode-string thumbnail)
                                        :image-type 'jpeg
                                        :max-width 50
                                        :max-height 50
                                        :corner-radius 6
                                        :padding-top 5
                                        :padding-bottom 5
                                        :is-video t))
                         "[video]")))
      ;; Store metadata as text properties
      (add-text-properties 0 (length video-text)
                           `(video-url ,(map-nested-elt p-message '(videoMessage URL))
                                       video-direct-path ,(map-nested-elt p-message '(videoMessage directPath))
                                       video-media-key ,(map-nested-elt p-message '(videoMessage mediaKey))
                                       video-mimetype ,(map-nested-elt p-message '(videoMessage mimetype))
                                       video-file-enc-sha256 ,(map-nested-elt p-message '(videoMessage fileEncSHA256))
                                       video-file-sha256 ,(map-nested-elt p-message '(videoMessage fileSHA256))
                                       video-file-length ,(map-nested-elt p-message '(videoMessage fileLength))
                                       video-seconds ,(map-nested-elt p-message '(videoMessage seconds))
                                       video-width ,(map-nested-elt p-message '(videoMessage width))
                                       video-height ,(map-nested-elt p-message '(videoMessage height)))
                           video-text)
      ;; Add action to play video on RET
      (setq video-text (wasabi--add-action-to-text
                        video-text
                        (lambda ()
                          (interactive)
                          (wasabi-chat-play-video-at-point))))
      (concat video-text
              (when-let ((caption (map-nested-elt p-message '(videoMessage caption))))
                (concat "\n" caption)))))
   ((map-elt p-message 'documentMessage) "[document]")
   ((map-elt p-message 'audioMessage) "[audio]")
   ((map-elt p-message 'stickerMessage)
    ;; (message "[sticker]\n\n%s" p-message)
    "[sticker]")
   ((map-elt p-message 'reactionMessage)
    ;; (message "[reaction]\n\n%s" p-message)
    "[reaction]")
   (t
    ;; (message "[unknown]\n\n%s" p-message)
    "[unknown]")))

(cl-defun wasabi-chat--parse-sender-name (p-data p-sender-jid &key contacts contact-name)
  "Parse sender name from P-DATA and P-SENDER-JID.
CONTACTS is the internal contacts alist for name resolution.
CONTACT-NAME is an optional fallback name."
  (cond
   ((map-nested-elt p-data '(Info IsFromMe))
    "Me")
   ((and p-sender-jid contacts
         (map-elt contacts (intern p-sender-jid)))
    (let* ((contact (map-elt contacts (intern p-sender-jid)))
           (full-name (map-elt contact :full-name))
           (push-name (map-elt contact :push-name)))
      (or (and full-name (not (string-empty-p full-name)) full-name)
          (and push-name (not (string-empty-p push-name)) push-name))))
   (t
    (let ((push-name (map-nested-elt p-data '(Info PushName))))
      (or (and push-name (not (string-empty-p push-name)) push-name)
          (and contact-name (not (string-empty-p contact-name)) contact-name)
          (and p-sender-jid
               (if (string-match "\\([^@]+\\)@" p-sender-jid)
                   (match-string 1 p-sender-jid)
                 p-sender-jid))
          "Unknown")))))

(cl-defun wasabi-chat--parse-message (p-message &key chat-jid contact-name contacts reactions)
  "Parse a protocol message (from database) into internal display format.
Returns alist with :sender-name, :timestamp, :content, :message-id, and :reactions.
Returns nil for reaction messages (they're handled separately).
CONTACTS should be internal contacts alist for sender name resolution.
REACTIONS is a hash table of message-id -> list of reactions."
  (let* ((data-json (map-elt p-message 'data_json))
         (timestamp (map-elt p-message 'timestamp))
         (msg-id (map-elt p-message 'message_id)))
    (if (and data-json (not (string-empty-p data-json)))
        ;; Parse from data_json (preferred - has full info)
        (let ((p-data (json-parse-string data-json :object-type 'alist
                                         :null-object nil
                                         :false-object nil)))
          ;; Skip reaction messages - they're already in reactions.
          (unless (map-nested-elt p-data '(Message reactionMessage))
            (let* ((p-sender-jid (map-nested-elt p-data '(Info Sender)))
                   (p-sender-name (wasabi-chat--parse-sender-name p-data p-sender-jid
                                                                  :contacts contacts
                                                                  :contact-name contact-name)))
              (if (and msg-id reactions (map-elt reactions msg-id))
                  `((:message-id . ,msg-id)
                    (:sender-name . ,p-sender-name)
                    (:timestamp . ,(map-nested-elt p-data '(Info Timestamp)))
                    (:content . ,(wasabi-chat--parse-content (map-elt p-data 'Message)))
                    (:reactions . ,(reverse (map-elt reactions msg-id))))
                `((:message-id . ,msg-id)
                  (:sender-name . ,p-sender-name)
                  (:timestamp . ,(map-nested-elt p-data '(Info Timestamp)))
                  (:content . ,(wasabi-chat--parse-content (map-elt p-data 'Message))))))))
      ;; Fallback: parse from basic fields (outgoing messages without data_json)
      (let* ((is-from-me (string= (map-elt p-message 'sender_jid) "me"))
             (sender-name (if is-from-me "Me" (or contact-name chat-jid)))
             (content (wasabi-chat--stored-content p-message))
             (reactions (when (and msg-id reactions)
                          (map-elt reactions msg-id))))
        (if reactions
            `((:message-id . ,msg-id)
              (:sender-name . ,sender-name)
              (:timestamp . ,timestamp)
              (:content . ,content)
              (:reactions . ,(reverse reactions)))
          `((:message-id . ,msg-id)
            (:sender-name . ,sender-name)
            (:timestamp . ,timestamp)
            (:content . ,content)))))))

(cl-defun wasabi-chat--parse-notification (&key p-message p-info contact-name chat-jid contacts)
  "Parse protocol notification MESSAGE and INFO into internal message format.
Returns alist with :sender-name, :timestamp, :content.
For reaction messages, also includes :is-reaction, :target-id, and :emoji."
  (if-let ((reaction-msg (map-elt p-message 'reactionMessage)))
      ;; This is a reaction
      (let* ((target-id (map-nested-elt reaction-msg '(key ID)))
             (emoji (map-elt reaction-msg 'text))
             (sender-jid (map-elt p-info 'Sender))
             (sender-name (if (map-elt p-info 'IsFromMe)
                              "Me"
                            (or contact-name
                                (map-elt p-info 'PushName)
                                (when sender-jid
                                  (if (string-match "\\([^@]+\\)@" sender-jid)
                                      (match-string 1 sender-jid)
                                    sender-jid))
                                chat-jid))))
        `((:is-reaction . t)
          (:target-id . ,target-id)
          (:emoji . ,emoji)
          (:sender-name . ,sender-name)))
    ;; Regular message
    (let* ((is-from-me (map-elt p-info 'IsFromMe))
           (sender-name (if is-from-me
                            "Me"
                          (or contact-name
                              (map-elt p-info 'PushName)
                              (when-let ((sender (map-elt p-info 'Sender)))
                                (if (string-match "\\([^@]+\\)@" sender)
                                    (match-string 1 sender)
                                  sender))
                              chat-jid)))
           (content (wasabi-chat--parse-content p-message))
           (timestamp (map-elt p-info 'Timestamp)))
      `((:sender-name . ,sender-name)
        (:timestamp . ,timestamp)
        (:content . ,content)))))

(cl-defun wasabi-chat--parse-reactions (p-messages &key contacts)
  "Parse reactions from P-MESSAGES and return a hash map of message-id -> reactions.
Each reaction is an alist with :emoji and :sender keys.
CONTACTS is used to resolve sender names."
  (let ((reactions (make-hash-table :test 'equal)))
    (dolist (p-msg (append p-messages nil))
      (when-let* ((data-json (map-elt p-msg 'data_json))
                  ((not (string-empty-p data-json)))
                  (p-data (json-parse-string data-json :object-type 'alist
                                             :null-object nil
                                             :false-object nil))
                  ((map-nested-elt p-data '(Message reactionMessage)))
                  (p-target-id (map-nested-elt p-data '(Message reactionMessage key ID)))
                  (p-sender-jid (map-nested-elt p-data '(Info Sender)))
                  (p-sender-name (wasabi-chat--parse-sender-name p-data p-sender-jid
                                                                 :contacts contacts)))
        (map-put! reactions p-target-id
                  (cons `((:emoji . ,(map-nested-elt p-data '(Message reactionMessage text)))
                          (:sender . ,p-sender-name))
                        (map-elt reactions p-target-id)))))
    reactions))

(cl-defun wasabi-chat--parse-messages (p-messages &key chat-jid contact-name contacts)
  "Parse array of protocol messages into list of internal display messages.
Returns list of message alists, sorted by timestamp (oldest first).
Messages with reactions will have a :reactions field."
  (let* ((reactions (wasabi-chat--parse-reactions p-messages :contacts contacts))
         (parsed (delq nil
                       (mapcar (lambda (p-msg)
                                 (wasabi-chat--parse-message p-msg
                                                             :chat-jid chat-jid
                                                             :contact-name contact-name
                                                             :contacts contacts
                                                             :reactions reactions))
                               (append p-messages nil)))))
    (sort parsed
          (lambda (a b)
            (string< (map-elt a :timestamp)
                     (map-elt b :timestamp))))))

(defun wasabi-chat--calculate-max-sender-width (messages)
  "Calculate maximum sender name width from internal MESSAGES for alignment."
  (if (null messages)
      0
    (apply #'max
           (mapcar (lambda (msg)
                     (string-width (map-elt msg :sender-name)))
                   messages))))

;; UI functions

(defun wasabi-chat--has-actionable-items-p ()
  "Return non-nil if buffer contains at least one actionable item."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (next-single-property-change (point) 'keymap)))
      (and pos (get-text-property pos 'keymap)))))

(defun wasabi-chat--get-binding-string (command)
  "Get the key binding string for COMMAND, or nil if not bound."
  (when-let ((keys (where-is-internal command wasabi-chat-mode-map)))
    (propertize (key-description (car keys)) 'face 'help-key-binding)))

(defun wasabi-chat--update-header-line ()
  "Update the header line with chat name and key bindings.
Shows different bindings depending on whether point is in input area."
  (let* ((in-input-area (wasabi-chat--in-input-area-p))
         (has-actionables (wasabi-chat--has-actionable-items-p))
         (title (or (map-elt wasabi-chat--chat :contact-name)
                    (map-elt wasabi-chat--chat :chat-jid))))
    (setq header-line-format
          (concat
           (when (wasabi--header-graphical-p)
             (concat
              " "
              (wasabi-icon (wasabi--face-height-pixels 'font-lock-doc-face))))
           (when title
             (concat
              " "
              (propertize title 'face 'font-lock-doc-face) " "))
           (if in-input-area
               ;; In input area
               (if has-actionables
                   (concat
                    (wasabi-chat--get-binding-string #'wasabi-chat-previous-actionable)
                    "/"
                    (wasabi-chat--get-binding-string #'wasabi-chat-next-actionable)
                    " media "
                    (wasabi-chat--get-binding-string #'wasabi-chat-send-input)
                    " to send message")
                 ;; No actionables
                 (concat
                  (wasabi-chat--get-binding-string #'wasabi-chat-send-input)
                  " to send message"))
             ;; Outside input area
             (concat
              (when has-actionables
                (concat
                 (wasabi-chat--get-binding-string #'wasabi-chat-previous-actionable)
                 "/"
                 (wasabi-chat--get-binding-string #'wasabi-chat-next-actionable)))
              " media "
              (wasabi-chat--get-binding-string #'wasabi-chat-next-message)
              "/"
              (wasabi-chat--get-binding-string #'wasabi-chat-previous-message)
              " message "
              (wasabi-chat--get-binding-string #'wasabi-chat-refresh)
              " refresh"))))))

(defun wasabi-chat--setup-prompt ()
  "Set up the read-only prompt at the end of the buffer."
  (goto-char (point-max))
  (let ((inhibit-read-only t)
        (prompt-start (point)))
    ;; Ensure we're on a new line
    (unless (bolp)
      (insert "\n"))
    (insert "> ")
    (setq wasabi-chat--prompt-marker (copy-marker prompt-start))
    (setq wasabi-chat--input-start-marker (point-marker))
    (set-marker-insertion-type wasabi-chat--input-start-marker nil)
    (set-marker-insertion-type wasabi-chat--prompt-marker t)
    (put-text-property prompt-start (point) 'read-only t)
    (put-text-property prompt-start (point) 'rear-nonsticky '(read-only))
    (put-text-property prompt-start (point) 'front-sticky '(read-only))))

(defun wasabi-chat--get-prompt-input ()
  "Get the current input text after the prompt."
  (when wasabi-chat--input-start-marker
    (buffer-substring-no-properties wasabi-chat--input-start-marker (point-max))))

(defun wasabi-chat--clear-prompt-input ()
  "Clear the input area after the prompt."
  (when wasabi-chat--input-start-marker
    (delete-region wasabi-chat--input-start-marker (point-max))))

(define-derived-mode wasabi-chat-mode fundamental-mode "Wasabi"
  "Major mode for displaying individual chat conversations.

\\{wasabi-chat-mode-map}"
  (setq-local inhibit-read-only nil)
  (add-hook 'post-command-hook #'wasabi-chat--update-header-line nil t)
  (wasabi-chat--update-header-line))

(defun wasabi-chat--in-input-area-p ()
  "Return non-nil if point is in the input area."
  (and wasabi-chat--input-start-marker
       (>= (point) wasabi-chat--input-start-marker)))

(defun wasabi-chat-beginning-of-line ()
  "Like `move-beginning-of-line' but prompt-aware."
  (interactive)
  (if (and (wasabi-chat--in-input-area-p)
           wasabi-chat--input-start-marker)
      (if (= (point) wasabi-chat--input-start-marker)
          ;; Already at input start, go to real beginning
          (move-beginning-of-line 1)
        ;; Go to input start
        (goto-char wasabi-chat--input-start-marker))
    ;; Not in input area, use default behavior
    (move-beginning-of-line 1)))

(defun wasabi-chat-quit ()
  "Quit the chat buffer."
  (interactive)
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (if (wasabi-chat--in-input-area-p)
      (self-insert-command 1)
    (quit-restore-window (get-buffer-window (current-buffer)) 'kill)))

(defun wasabi-chat-send-input ()
  "Send the current input as a message."
  (interactive)
  (unless (wasabi-chat--in-input-area-p)
    (user-error "Press RET after > prompt to send a message"))
  (when (string-empty-p (string-trim (wasabi-chat--get-prompt-input)))
    (user-error "Nothing to send"))
  (unless wasabi-chat--chat
    (error "No chat information available"))
  (unless (map-elt wasabi-chat--chat :chat-jid)
    (error "No chat JID available"))
  (let ((text (string-trim (wasabi-chat--get-prompt-input)))
        (chat-jid (map-elt wasabi-chat--chat :chat-jid))
        (chat-buffer (current-buffer)))
    (wasabi-chat--clear-prompt-input)
    (message "Sending...")
    (with-current-buffer (wasabi--buffer)
      (wasabi--send-chat-send-text-request
       :phone chat-jid
       :body text
       :on-failure (lambda (error)
                     (message "Failed to send")
                     (wasabi--log "Failed to send message: %s"
                                  (or (map-elt error 'message)
                                      "unknown error"))
                     ;; Restore cleared input
                     (with-current-buffer chat-buffer
                       (goto-char (point-max))
                       (insert text)))
       :on-success (lambda (response)
                     (message "Sent")
                     ;; Response Timestamp is Unix timestamp (integer),
                     ;; convert to ISO 8601 string.
                     (let* ((timestamp-str (format-time-string "%Y-%m-%dT%H:%M:%S%z" (map-elt response 'Timestamp)))
                            (message `((:sender-name . "Me")
                                       (:timestamp . ,timestamp-str)
                                       (:content . ,text))))
                       (with-current-buffer chat-buffer
                         (wasabi-chat--append-message message))
                       (with-current-buffer chat-buffer
                         (goto-char (point-max)))
                       (with-current-buffer chat-buffer
                         (when (get-buffer-window chat-buffer)
                           ;; Recenter to bottom (based on `recenter-top-bottom')
                           (recenter (- -1 (min (max 0 scroll-margin)
		                                (truncate (/ (window-body-height) 4.0)))) t)))))))))

(defconst wasabi-chat--sendable-image-types
  '(("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("png" . "image/png")
    ("gif" . "image/gif"))
  "Image extensions that can be sent, and their MIME types.

wuzapi decodes an image to build its thumbnail, and has decoders for
these alone: anything else, WebP and HEIC included, fails on its side.")

(defconst wasabi-chat--max-image-bytes (* 16 1024 1024)
  "The largest image WhatsApp will take.")

(defun wasabi-chat--sendable-image-p (file)
  "Return non-nil when FILE is an image wuzapi can send, or a directory.
Directories pass so that `read-file-name' can still browse."
  (or (file-directory-p file)
      (and (assoc (downcase (or (file-name-extension file) ""))
                  wasabi-chat--sendable-image-types)
           t)))

(defun wasabi-chat--image-data-url (file)
  "Return FILE as a base64 data URL, checking it can be sent first."
  (let ((mimetype (cdr (assoc (downcase (or (file-name-extension file) ""))
                              wasabi-chat--sendable-image-types))))
    (unless (file-readable-p file)
      (user-error "Can't read %s" file))
    (unless mimetype
      (user-error "Can't send %s: only JPEG, PNG and GIF images can be sent"
                  (file-name-nondirectory file)))
    (when (> (file-attribute-size (file-attributes file))
             wasabi-chat--max-image-bytes)
      (user-error "%s is too large to send: WhatsApp takes images up to 16 MB"
                  (file-name-nondirectory file)))
    (concat "data:" mimetype ";base64,"
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file)
              (base64-encode-region (point-min) (point-max) t)
              (buffer-string)))))

(defun wasabi-chat--sent-image-copy-name (message-id)
  "Return the file name our copy of the image sent as MESSAGE-ID has.
Without its extension, which is the original's."
  (when (and (stringp message-id) (not (string-empty-p message-id)))
    (expand-file-name (concat "sent-"
                              (replace-regexp-in-string "[^a-zA-Z0-9]" ""
                                                        message-id))
                      (expand-file-name "media" (wasabi-data-dir)))))

(defun wasabi-chat--keep-sent-image (file message-id)
  "Keep a copy of the image FILE, sent as MESSAGE-ID.  Return its path.

wuzapi records a sent image as a caption and nothing else, and WhatsApp
does not echo our own messages back, so this copy is the only way to
show it again when the chat is reloaded."
  (when-let ((name (wasabi-chat--sent-image-copy-name message-id)))
    (ignore-errors
      (let ((copy (concat name "." (downcase (or (file-name-extension file)
                                                 "jpg")))))
        (make-directory (file-name-directory copy) t)
        (copy-file file copy t)
        copy))))

(defun wasabi-chat--sent-image-file (message-id)
  "Return our copy of the image sent as MESSAGE-ID, or nil."
  (when-let ((name (wasabi-chat--sent-image-copy-name message-id)))
    (car (file-expand-wildcards (concat name ".*")))))

(defun wasabi-chat--sent-image-content (file caption)
  "Return how the image FILE, sent with CAPTION, shows in the chat.

Drawn from FILE directly, scaled down like a received thumbnail, and
opened in full with RET.  Embedding a whole photo in the SVG that rounds
received thumbnails' corners could fail on a large one and leave
nothing but the word.  FILE may be nil, when there is no copy to draw."
  (let* ((drawable (and file (file-readable-p file)))
         (preview (when drawable
                    (condition-case nil
                        (create-image file nil nil
                                      :max-width 50
                                      :max-height 50
                                      :ascent 'center)
                      (error nil))))
         (image-text (if preview
                         (propertize "[image]" 'display preview)
                       (copy-sequence "[image]"))))
    (when drawable
      (setq image-text
            (wasabi--add-action-to-text
             image-text
             (lambda ()
               (interactive)
               (wasabi-chat--display-cached-image file nil nil)))))
    (concat image-text
            (when (and (stringp caption) (not (string-empty-p caption)))
              (concat "\n" caption)))))

(defun wasabi-chat--stored-content (p-message)
  "Return the content of stored P-MESSAGE, one with no data_json.

Those are the messages sent from here, of which wuzapi keeps only the
text: a caption at most, for an image.  So an image is drawn from the
copy kept when it was sent, and anything else with no text at all says
what it was rather than showing as a blank line."
  (let ((text (map-elt p-message 'text_content))
        (type (map-elt p-message 'message_type)))
    (cond
     ((equal type "image")
      (wasabi-chat--sent-image-content
       (wasabi-chat--sent-image-file (map-elt p-message 'message_id))
       text))
     ((and (stringp text) (not (string-empty-p text)))
      text)
     ((and (stringp type) (not (member type '("" "text"))))
      (format "[%s]" type))
     (t "[message]"))))

(defun wasabi-chat-send-image (file &optional caption delete-after)
  "Send the image FILE to this chat, with an optional CAPTION.

Offers only images that can be sent: JPEG, PNG and GIF, up to 16 MB.
DELETE-AFTER, when non-nil, deletes FILE once the send is done with it,
for a file made only to be sent."
  (interactive
   (progn
     (unless (derived-mode-p 'wasabi-chat-mode)
       (user-error "Open a chat to send an image to"))
     (list (read-file-name "Send image: " nil nil t nil
                           #'wasabi-chat--sendable-image-p)
           (read-string "Caption (optional): "))))
  (unless (derived-mode-p 'wasabi-chat-mode)
    (user-error "Open a chat to send an image to"))
  (unless (map-elt wasabi-chat--chat :chat-jid)
    (error "No chat JID available"))
  (let ((file (expand-file-name file))
        (chat-jid (map-elt wasabi-chat--chat :chat-jid))
        (chat-buffer (current-buffer)))
    (when (file-directory-p file)
      (user-error "Pick an image, not a directory"))
    (let ((image (wasabi-chat--image-data-url file)))
      (message "Sending %s..." (file-name-nondirectory file))
      (with-current-buffer (wasabi--buffer)
        (wasabi--send-chat-send-image-request
         :phone chat-jid
         :image image
         :caption caption
         :on-failure (lambda (error)
                       (message "Failed to send image: %s"
                                (or (map-elt error 'message) "unknown error"))
                       (when delete-after
                         (ignore-errors (delete-file file))))
         :on-success
         (lambda (response)
           (message "Sent %s" (file-name-nondirectory file))
           (let ((shown (or (wasabi-chat--keep-sent-image file (map-elt response 'Id))
                            file)))
           (when (buffer-live-p chat-buffer)
             (with-current-buffer chat-buffer
               (wasabi-chat--append-message
                `((:sender-name . "Me")
                  (:timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                     (or (map-elt response 'Timestamp)
                                                         (current-time))))
                  (:content . ,(wasabi-chat--sent-image-content shown caption)))))))
           ;; The kept copy is what the chat draws from now.
           (when delete-after
             (ignore-errors (delete-file file)))))))))

(defconst wasabi-chat--w32-clipboard-script
  "[Console]::OutputEncoding = [Text.Encoding]::UTF8
Add-Type -AssemblyName System.Windows.Forms, System.Drawing
$out = '%s'
$image = [Windows.Forms.Clipboard]::GetImage()
if ($image -ne $null) {
  $image.Save($out, [Drawing.Imaging.ImageFormat]::Png)
  Write-Output ('IMAGE ' + $out)
  exit 0
}
foreach ($file in [Windows.Forms.Clipboard]::GetFileDropList()) {
  if ($file -match '\\.(png|jpe?g|gif)$') {
    Write-Output ('FILE ' + $file)
    exit 0
  }
}
exit 1"
  "PowerShell that finds an image on the Windows clipboard.
Either a picture, saved as a PNG to the path put in place of %s, or an
image file copied in Explorer.  Prints which, and exits 1 for neither.
Emacs itself can only read text from the clipboard on Windows.")

(defun wasabi-chat--png-file-p (file)
  "Return non-nil when FILE is a non-empty PNG."
  (and (file-readable-p file)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file nil 0 8)
         (equal (buffer-string) "\x89PNG\r\n\x1a\n"))))

(defun wasabi-chat--clipboard-image-w32 (png)
  "Find an image on the Windows clipboard, saving a picture to PNG.
Returns (FILE . TEMPORARY), or nil if there is no image to be had."
  (when (executable-find "powershell")
    (let* ((script (format wasabi-chat--w32-clipboard-script
                           (string-replace "'" "''" (convert-standard-filename png))))
           ;; Encoded, so that nothing in the script or path needs quoting.
           (encoded (base64-encode-string (encode-coding-string script 'utf-16le) t))
           (coding-system-for-read 'utf-8)
           (output (with-temp-buffer
                     (and (zerop (call-process "powershell" nil t nil
                                               "-NoProfile" "-NonInteractive" "-STA"
                                               "-EncodedCommand" encoded))
                          (string-trim (buffer-string))))))
      (cond
       ((and output (string-prefix-p "IMAGE " output) (wasabi-chat--png-file-p png))
        (cons png t))
       ((and output (string-prefix-p "FILE " output))
        (let ((file (string-remove-prefix "FILE " output)))
          (when (file-readable-p file)
            (cons file nil))))))))

(defun wasabi-chat--clipboard-image-gui (png)
  "Save the clipboard's image to PNG through Emacs, where it can.
Returns (PNG . t), or nil."
  (when-let ((data (ignore-errors (gui-get-selection 'CLIPBOARD 'image/png))))
    (when (and (stringp data) (> (length data) 0))
      (let ((coding-system-for-write 'binary))
        (with-temp-file png
          (set-buffer-multibyte nil)
          (insert (if (multibyte-string-p data)
                      (encode-coding-string data 'binary)
                    data))))
      (when (wasabi-chat--png-file-p png)
        (cons png t)))))

(defun wasabi-chat--clipboard-image-command (png)
  "Save the clipboard's image to PNG with whichever tool is installed.
Returns (PNG . t), or nil."
  (when (seq-some
         (lambda (command)
           (when (executable-find (car command))
             (ignore-errors
               (if (equal (car command) "pngpaste")
                   ;; pngpaste writes the file itself.
                   (zerop (call-process "pngpaste" nil nil nil png))
                 (zerop (apply #'call-process (car command) nil (list :file png) nil
                               (cdr command)))))))
         '(("wl-paste" "--no-newline" "--type" "image/png")
           ("xclip" "-selection" "clipboard" "-target" "image/png" "-out")
           ("pngpaste")))
    (when (wasabi-chat--png-file-p png)
      (cons png t))))

(defun wasabi-chat--clipboard-image ()
  "Return (FILE . TEMPORARY) for the image on the clipboard, or nil.
FILE is a picture saved for sending, and TEMPORARY, or an image file
copied in a file manager, which is sent as it is."
  (let ((png (make-temp-file "wasabi-clipboard-" nil ".png")))
    (or (if (memq system-type '(windows-nt cygwin))
            (wasabi-chat--clipboard-image-w32 png)
          (or (wasabi-chat--clipboard-image-gui png)
              (wasabi-chat--clipboard-image-command png)))
        (progn
          (ignore-errors (delete-file png))
          nil))))

(defun wasabi-chat--read-caption-for (file)
  "Ask for a caption for FILE, showing it so it is not sent unseen."
  (read-string
   (concat (when (display-images-p)
             (when-let ((preview (ignore-errors
                                   (create-image file nil nil
                                                 :max-width 240 :max-height 160))))
               (concat (propertize " " 'display preview) "\n")))
           (format "Send this to %s?  Caption (optional, C-g to cancel): "
                   (or (map-elt wasabi-chat--chat :contact-name) "this chat")))))

(defun wasabi-chat--send-found-image (found)
  "Send FOUND, a (FILE . TEMPORARY) from the clipboard, once seen.
A TEMPORARY file is deleted afterwards, or at once if not sent."
  (let ((file (car found))
        (temporary (cdr found)))
    (condition-case nil
        (wasabi-chat-send-image file (wasabi-chat--read-caption-for file) temporary)
      (quit
       (when temporary (ignore-errors (delete-file file)))
       (message "Not sent")))))

(defun wasabi-chat-send-clipboard-image ()
  "Send the image on the clipboard to this chat.
A copied picture, a screenshot say, or an image file copied in a file
manager.  Shows it before sending, with a prompt for a caption."
  (interactive)
  (unless (derived-mode-p 'wasabi-chat-mode)
    (user-error "Open a chat to send an image to"))
  (wasabi-chat--send-found-image
   (or (wasabi-chat--clipboard-image)
       (user-error "No image on the clipboard"))))

(defun wasabi-chat-attach (&optional pick)
  "Send the clipboard's image if there is one, or pick an image file.
With a prefix argument PICK, always pick a file."
  (interactive "P")
  (unless (derived-mode-p 'wasabi-chat-mode)
    (user-error "Open a chat to send an image to"))
  (if-let ((found (unless pick (wasabi-chat--clipboard-image))))
      (wasabi-chat--send-found-image found)
    (call-interactively #'wasabi-chat-send-image)))

(defun wasabi-chat-refresh ()
  "Refresh the current chat buffer by fetching new messages."
  (interactive)
  (if (wasabi-chat--in-input-area-p)
      (self-insert-command 1)
    (unless wasabi-chat--chat
      (error "No chat information available"))
    (wasabi-chat--update-header-line)
    (let ((chat-jid (or (map-elt wasabi-chat--chat :chat-jid)
                        (error "No chat JID available")))
          (contact-name (map-elt wasabi-chat--chat :contact-name)))
      (with-current-buffer (wasabi--buffer)
        (wasabi--send-chat-history-request
         :chat-jid chat-jid
         :contact-name contact-name
         :on-finished (lambda ()
                        (message "Refreshed")))))))

(defun wasabi-chat-next-message ()
  "Jump to the next message (sender line)."
  (interactive)
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (if (wasabi-chat--in-input-area-p)
      (self-insert-command 1)
    ;; First, skip past the current sender if we're on one
    (let ((start-pos (save-excursion
                       (end-of-line)
                       (if (get-text-property (point) 'wasabi-sender)
                           (or (next-single-property-change (point) 'wasabi-sender)
                               (point-max))
                         (point)))))
      ;; Then find the next sender
      (let ((pos (next-single-property-change start-pos 'wasabi-sender)))
        (if (and pos (get-text-property pos 'wasabi-sender))
            (progn
              (goto-char pos)
              (beginning-of-line))
          ;; If at last message, bump to prompt.
          (goto-char (point-max)))))))

(defun wasabi-chat-previous-message ()
  "Jump to the previous message (sender line)."
  (interactive)
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (if (wasabi-chat--in-input-area-p)
      (self-insert-command 1)
    ;; First, skip to the start of current sender if we're in the middle of one
    (let ((start-pos (if (get-text-property (point) 'wasabi-sender)
                         (or (previous-single-property-change (point) 'wasabi-sender)
                             (point-min))
                       (point))))
      ;; Then find the previous sender
      (let ((pos (previous-single-property-change start-pos 'wasabi-sender)))
        (if pos
            ;; Move to the start of that sender region
            (let ((sender-start (or (previous-single-property-change pos 'wasabi-sender)
                                    (point-min))))
              (progn
                (goto-char (if (get-text-property sender-start 'wasabi-sender)
                               sender-start
                             pos))
                (beginning-of-line)))
          (message "No previous message"))))))

(defun wasabi-chat-next-actionable ()
  "Move point to the next actionable item (image/video)."
  (interactive)
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (if (wasabi-chat--in-input-area-p)
      (user-error "No more left")
    (let ((start-pos (if (get-text-property (point) 'keymap)
                         ;; If on an actionable, move past it first
                         (or (next-single-property-change (point) 'keymap)
                             (point-max))
                       (point))))
      (if-let ((pos (next-single-property-change start-pos 'keymap))
               (actionable (get-text-property pos 'keymap)))
          (goto-char pos)
        (goto-char (point-max))))))

(defun wasabi-chat-previous-actionable ()
  "Move point to the previous actionable item (image/video).
If in input area, move to just before the prompt."
  (interactive)
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (when (and (wasabi-chat--in-input-area-p)
             (wasabi-chat--has-actionable-items-p)
             wasabi-chat--prompt-marker)
    (goto-char wasabi-chat--prompt-marker))
  (let ((start-pos (if (get-text-property (point) 'keymap)
                       ;; If on an actionable, move before it first
                       (or (previous-single-property-change (point) 'keymap)
                           (point-min))
                     (point))))
    (if-let* ((pos (previous-single-property-change start-pos 'keymap))
              (found (and pos (> pos (point-min)))))
        ;; Move back to find a position that actually has the keymap property
        (progn
          (goto-char pos)
          (unless (get-text-property (point) 'keymap)
            (let ((prev (previous-single-property-change (point) 'keymap)))
              (when (and prev (get-text-property prev 'keymap))
                (goto-char prev)))))
      (user-error "No more left"))))

(defun wasabi-chat--refresh (messages)
  "Refresh the current chat buffer with internal MESSAGES.
MESSAGES is a list of already-parsed internal message alists."
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (map-put! wasabi-chat--chat :messages messages)
  (map-put! wasabi-chat--chat
            :max-sender-width (wasabi-chat--calculate-max-sender-width messages))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (null messages)
        (insert "\n")
      (wasabi-chat--render-messages messages))
    (wasabi-chat--setup-prompt)
    (wasabi-chat--update-header-line)
    (goto-char (point-max))))

(cl-defun wasabi-chat--render-message (&key sender-name timestamp content max-sender-width reactions message-id)
  "Render a single internal message.
SENDER-NAME is the display name of the sender.
TIMESTAMP is the ISO8601 timestamp string.
CONTENT is the display content (already parsed, may include text properties).
MAX-SENDER-WIDTH is used for padding alignment.
REACTIONS is a list of reaction alists with :emoji and :sender keys.
MESSAGE-ID is used to tag the rendered message for later updates."
  (let* ((col1-width max-sender-width)
         (is-from-me (string= sender-name "Me"))
         (sender (propertize sender-name
                             'face `(:inherit ,(if is-from-me
                                                   'font-lock-variable-name-face
                                                 'font-lock-function-name-face) :box nil)
                             'wasabi-sender t
                             'wasabi-message-id message-id))
         (sender-padding (make-string (max 0 (- (or max-sender-width 0)
                                                (string-width sender))) ?\s))
         (time (when timestamp
                 (propertize (format-time-string "%H:%M" (parse-iso8601-time-string timestamp))
                             'face 'font-lock-comment-face))))
    ;;
    ;; Intended layout per message:
    ;;
    ;; Mateo 15:32
    ;;       Off to granny's
    ;;
    ;; With reactions:
    ;;
    ;; Mateo 15:32
    ;;       Off to granny's
    ;;       ❤️ George
    ;;       ❤️ Paul
    ;;
    (concat sender-padding sender " " (or time "")
            "\n" (make-string col1-width ?\s) " "
            (string-replace "\n" (concat "\n " (make-string col1-width ?\s)) content)
            ;; Add reactions below the message
            (when reactions
              (concat "\n"
                      (mapconcat
                       (lambda (reaction)
                         (let ((emoji (map-elt reaction :emoji))
                               (reaction-sender (map-elt reaction :sender)))
                           (concat (make-string col1-width ?\s) " "
                                   emoji
                                   " "
                                   (propertize reaction-sender
                                               'face 'font-lock-comment-face))))
                       reactions
                       "\n")))
            "\n\n")))

(defun wasabi-chat--render-messages (messages)
  "Render internal format MESSAGES to current buffer.
MESSAGES is a list of alists with :sender-name, :timestamp, :content."
  (let* ((max-sender-width (map-elt wasabi-chat--chat :max-sender-width))
         (message-lines
          (mapcar
           (lambda (msg)
             (wasabi-chat--render-message
              :sender-name (map-elt msg :sender-name)
              :timestamp (map-elt msg :timestamp)
              :content (map-elt msg :content)
              :max-sender-width max-sender-width
              :reactions (map-elt msg :reactions)
              :message-id (map-elt msg :message-id)))
           messages)))
    (let ((start (point)))
      (insert "\n" (mapconcat #'identity message-lines))
      (put-text-property start (point) 'read-only t))))

(defun wasabi-chat--append-message (message)
  "Append a single internal MESSAGE to current chat buffer.
MESSAGE is an alist with :sender-name, :timestamp, :content.
Updates :messages list and :max-sender-width in chat state."
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (unless message
    (error "message is required"))
  (let ((inhibit-read-only t)
        (saved-input nil))
    (when wasabi-chat--prompt-marker
      ;; Save any user input before deleting the prompt
      (setq saved-input (wasabi-chat--get-prompt-input))
      ;; Delete the existing prompt
      (delete-region wasabi-chat--prompt-marker (point-max)))
    (goto-char (point-max))
    (let* ((start (point))
           (sender-width (string-width (map-elt message :sender-name)))
           (old-max-width (or (map-elt wasabi-chat--chat :max-sender-width) 0))
           (new-max-width (max old-max-width sender-width))
           (updated-messages (append (map-elt wasabi-chat--chat :messages)
                                     (list message))))
      ;; Update chat state with new messages and max-width
      (wasabi-chat--update-chat :max-sender-width new-max-width)
      (wasabi-chat--update-chat :messages updated-messages)
      ;; Render the message
      (insert (wasabi-chat--render-message
               :sender-name (map-elt message :sender-name)
               :timestamp (map-elt message :timestamp)
               :content (map-elt message :content)
               :max-sender-width (map-elt wasabi-chat--chat :max-sender-width)
               :reactions (map-elt message :reactions)
               :message-id (map-elt message :message-id)))
      (put-text-property start (point) 'read-only t))
    (wasabi-chat--setup-prompt)
    ;; Restore saved input
    (when (and saved-input (not (string-empty-p saved-input)))
      (goto-char (point-max))
      (insert saved-input))
    ;; Recenter to bottom (based on `recenter-top-bottom')
    (recenter (- -1 (min (max 0 scroll-margin)
		         (truncate (/ (window-body-height) 4.0)))) t)
    (wasabi-chat--update-header-line)))

(cl-defun wasabi-chat--add-reaction (&key target-id emoji sender)
  "Add a reaction to an existing message with TARGET-ID.
EMOJI is the reaction emoji, SENDER is the name of who reacted.
Finds the message in :messages, updates it, and re-renders just that message."
  (unless (derived-mode-p 'wasabi-chat-mode)
    (error "Not in a chat buffer"))
  (wasabi--log "add-reaction called with target-id: %s, emoji: %s, sender: %s" target-id emoji sender)
  (if-let* ((target-idx (seq-position (map-elt wasabi-chat--chat :messages)
                                      target-id
                                      (lambda (msg id) (string= (map-elt msg :message-id) id))))
            (target-msg (nth target-idx (map-elt wasabi-chat--chat :messages)))
            (updated-msg (cons `(:reactions . ,(append (map-elt target-msg :reactions)
                                                       (list `((:emoji . ,emoji) (:sender . ,sender)))))
                               (assq-delete-all :reactions (copy-alist target-msg))))
            (updated-messages (append (seq-take (map-elt wasabi-chat--chat :messages) target-idx)
                                      (list updated-msg)
                                      (seq-drop (map-elt wasabi-chat--chat :messages) (1+ target-idx)))))
      (progn
        (wasabi--log "Found message at index %d, message-id: %s" target-idx (map-elt target-msg :message-id))
        (wasabi-chat--update-chat :messages updated-messages)
        (let ((inhibit-read-only t))
          (save-excursion
            ;; Find message by its message-id text property using text-property-search-forward
            (wasabi--log "Looking for message-id in buffer: %s" target-id)
            (goto-char (point-min))
            (when-let* ((match (text-property-search-forward 'wasabi-message-id target-id #'equal))
                        (prop-pos (prop-match-beginning match)))
              ;; prop-pos is somewhere in the sender text, find the start of the line
              (goto-char prop-pos)
              (beginning-of-line)
              (let* ((msg-start (point))
                     ;; Find the next message by looking for the next wasabi-sender property
                     ;; First, move past the current sender property
                     (after-sender (next-single-property-change prop-pos 'wasabi-sender))
                     ;; Then find the next sender (start of next message)
                     (next-sender (when after-sender
                                    (next-single-property-change after-sender 'wasabi-sender)))
                     ;; If there's a next message, find its line start; otherwise use prompt marker
                     (msg-end (if next-sender
                                  (save-excursion
                                    (goto-char next-sender)
                                    (beginning-of-line)
                                    ;; Skip back over the \n\n separator
                                    (skip-chars-backward "\n")
                                    (point))
                                ;; Last message: stop at prompt marker (or point-max if no prompt)
                                (or wasabi-chat--prompt-marker (point-max)))))
                (delete-region msg-start msg-end)
                (goto-char msg-start)
                (insert (wasabi-chat--render-message
                         :sender-name (map-elt updated-msg :sender-name)
                         :timestamp (map-elt updated-msg :timestamp)
                         :content (map-elt updated-msg :content)
                         :max-sender-width (map-elt wasabi-chat--chat :max-sender-width)
                         :reactions (map-elt updated-msg :reactions)
                         :message-id (map-elt updated-msg :message-id)))
                ;; Ensure newline before prompt.
                (unless next-sender
                  (insert "\n\n")))))))
    (wasabi--log "Could not find message with ID %s to add reaction" target-id)))

(cl-defun wasabi-chat--start (&key chat-jid messages contact-name)
  "Create and display a chat buffer for CHAT-JID.
MESSAGES is a list of already-parsed internal message alists.
CONTACT-NAME is the display name of the contact (or nil if not available).
Displays messages in a two-column format: sender | message."
  (unless chat-jid
    (error ":chat-jid is required"))
  ;; TODO: Consolidate buffer creation logic.
  (let ((chat-buffer (get-buffer-create (format "*Wasabi: %s*" (or contact-name chat-jid)))))
    (with-current-buffer chat-buffer
      (unless (derived-mode-p 'wasabi-chat-mode)
        (wasabi-chat-mode))
      (setq wasabi-chat--chat (wasabi-chat--make-chat :chat-jid chat-jid
                                                      :contact-name contact-name))
      (wasabi-chat--refresh messages)
      (goto-char (point-max)))

    (switch-to-buffer chat-buffer)))

(defun wasabi-chat-play-video-at-point ()
  "Download and play the video at point using external player."
  (interactive)
  (unless (get-text-property (point) 'video-url)
    (user-error "No video at point"))
  (let* ((url (get-text-property (point) 'video-url))
         (direct-path (get-text-property (point) 'video-direct-path))
         (media-key (get-text-property (point) 'video-media-key))
         (mimetype (get-text-property (point) 'video-mimetype))
         (file-enc-sha256 (get-text-property (point) 'video-file-enc-sha256))
         (file-sha256 (get-text-property (point) 'video-file-sha256))
         (file-length (get-text-property (point) 'video-file-length))
         ;; Check if file already exists
         (file-id (if file-sha256
                      (replace-regexp-in-string "[^a-zA-Z0-9]" "" file-sha256)
                    (format "%d" (random 1000000))))
         (extension (cond
                     ((string-match "video/mp4" mimetype) ".mp4")
                     ((string-match "video/quicktime" mimetype) ".mov")
                     ((string-match "video/x-matroska" mimetype) ".mkv")
                     ((string-match "video/webm" mimetype) ".webm")
                     (t ".mp4")))
         (media-dir (expand-file-name "media" (wasabi-data-dir)))
         (media-file (expand-file-name (concat file-id extension) media-dir)))
    (unless (file-directory-p media-dir)
      (make-directory media-dir t))
    (if (file-exists-p media-file)
        ;; File already downloaded, just open it
        (wasabi-chat--open-video-externally media-file)
      ;; Download the video
      (message "Downloading video...")
      (with-current-buffer (wasabi--buffer)
        (wasabi--send-download-video-request
         :url url
         :direct-path direct-path
         :media-key media-key
         :mimetype mimetype
         :file-enc-sha256 file-enc-sha256
         :file-sha256 file-sha256
         :file-length file-length
         :on-success (lambda (response)
                       (message "Downloading video... done")
                       (wasabi-chat--save-and-play-video
                        :data-url (map-elt response 'Data)
                        :mimetype mimetype
                        :file-sha256 file-sha256))
         :on-failure (lambda (error)
                       (message "Failed to download video")))))))

(cl-defun wasabi-chat--save-and-play-video (&key data-url mimetype file-sha256)
  "Save video to media directory and open with external player.
DATA-URL is the base64-encoded data URL from the backend.
MIMETYPE is the video MIME type.
FILE-SHA256 is used to create a unique filename."
  (unless data-url
    (error ":data-url is required"))
  ;; Extract base64 data from data URL
  (unless (string-match "data:[^;]+;base64,\\(.*\\)" data-url)
    (error "Invalid data URL format"))
  (let* ((base64-data (match-string 1 data-url))
         (video-data (base64-decode-string base64-data))
         ;; Use fileSHA256 (base64) as unique identifier, sanitize for filename
         (file-id (if file-sha256
                      (replace-regexp-in-string "[^a-zA-Z0-9]" "" file-sha256)
                    (format "%d" (random 1000000))))
         ;; Determine extension from mimetype
         (extension (cond
                     ((string-match "video/mp4" mimetype) ".mp4")
                     ((string-match "video/quicktime" mimetype) ".mov")
                     ((string-match "video/x-matroska" mimetype) ".mkv")
                     ((string-match "video/webm" mimetype) ".webm")
                     (t ".mp4")))
         (media-dir (expand-file-name "media" (wasabi-data-dir)))
         (temp-file (expand-file-name (concat file-id extension) media-dir)))
    ;; Ensure media directory exists
    (unless (file-directory-p media-dir)
      (make-directory media-dir t))
    ;; Write video data to file
    (let ((coding-system-for-write 'binary))
      (with-temp-file temp-file
        (set-buffer-multibyte nil)
        (insert video-data)))
    (wasabi-chat--open-video-externally temp-file)))

(defun wasabi-chat--open-video-externally (file-path)
  "Open video FILE-PATH with configured or system default player."
  (if wasabi-video-player-function
      (funcall wasabi-video-player-function file-path)
    (cond
     ;; macOS
     ((eq system-type 'darwin)
      (start-process "open-video" nil "open" file-path))
     ;; Linux
     ((eq system-type 'gnu/linux)
      (start-process "open-video" nil "xdg-open" file-path))
     ;; Windows
     ((memq system-type '(windows-nt ms-dos))
      (start-process "open-video" nil "cmd" "/c" "start" "" file-path))
     ;; Fallback
     (t
      (browse-url-of-file file-path)))))

(defun wasabi-chat-view-image-at-point ()
  "View the full image at point in a *Wasabi photo* buffer."
  (interactive)
  (unless (get-text-property (point) 'image-url)
    (user-error "No image at point"))
  (let* ((url (get-text-property (point) 'image-url))
         (direct-path (get-text-property (point) 'image-direct-path))
         (media-key (get-text-property (point) 'image-media-key))
         (mimetype (get-text-property (point) 'image-mimetype))
         (file-enc-sha256 (get-text-property (point) 'image-file-enc-sha256))
         (file-sha256 (get-text-property (point) 'image-file-sha256))
         (file-length (get-text-property (point) 'image-file-length))
         (width (get-text-property (point) 'image-width))
         (height (get-text-property (point) 'image-height))
         ;; Check if file already exists in cache
         (file-id (if file-sha256
                      (replace-regexp-in-string "[^a-zA-Z0-9]" "" file-sha256)
                    (format "%d" (random 1000000))))
         (extension (cond
                     ((string-match "image/jpeg" mimetype) ".jpg")
                     ((string-match "image/png" mimetype) ".png")
                     ((string-match "image/gif" mimetype) ".gif")
                     ((string-match "image/webp" mimetype) ".webp")
                     (t ".jpg")))
         (media-dir (expand-file-name "media" (wasabi-data-dir)))
         (cached-file (expand-file-name (concat file-id extension) media-dir)))
    ;; Ensure media directory exists
    (unless (file-directory-p media-dir)
      (make-directory media-dir t))
    (if (file-exists-p cached-file)
        ;; File already cached, display it directly
        (wasabi-chat--display-cached-image cached-file width height)
      ;; Download the image
      (message "Downloading image...")
      (with-current-buffer (wasabi--buffer)
        (wasabi--send-download-image-request
         :url url
         :direct-path direct-path
         :media-key media-key
         :mimetype mimetype
         :file-enc-sha256 file-enc-sha256
         :file-sha256 file-sha256
         :file-length file-length
         :on-success (lambda (response)
                       (message "Downloading image... done")
                       (wasabi-chat--save-and-display-image
                        :data-url (map-elt response 'Data)
                        :mimetype mimetype
                        :file-path cached-file
                        :width width
                        :height height))
         :on-failure (lambda (error)
                       (message "Failed to download image")))))))

(cl-defun wasabi-chat--display-cached-image (file-path width height)
  "Display cached image from FILE-PATH."
  (let* ((image-data (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally file-path)
                       (buffer-string)))
         (image-type (cond
                      ((string-suffix-p ".jpg" file-path) 'jpeg)
                      ((string-suffix-p ".png" file-path) 'png)
                      ((string-suffix-p ".gif" file-path) 'gif)
                      ((string-suffix-p ".webp" file-path) 'imagemagick)
                      (t 'jpeg)))
         (photo-buffer (get-buffer-create "*Wasabi photo*")))
    (with-current-buffer photo-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (setq buffer-read-only t)
        (local-set-key (kbd "q") #'quit-window)
        (insert (propertize "q" 'face 'help-key-binding) " to close")
        (insert "\n\n")))
    (switch-to-buffer photo-buffer)
    ;; Calculate max dimensions based on window size
    (let* ((win-width (window-pixel-width))
           (win-height (window-pixel-height))
           (max-height (- win-height 60))
           (max-width win-width)
           (image (create-image image-data image-type t
                                :max-width max-width
                                :max-height max-height)))
      (with-current-buffer photo-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize "🌄" 'display image))
          (insert "\n")
          (goto-char (point-min)))))))

(cl-defun wasabi-chat--save-and-display-image (&key data-url mimetype file-path width height)
  "Save image to FILE-PATH and display it.
DATA-URL is the base64-encoded data URL from the backend.
MIMETYPE is the image MIME type.
FILE-PATH is where to save the cached image."
  (unless data-url
    (error ":data-url is required"))
  ;; Extract base64 data from data URL
  (unless (string-match "data:[^;]+;base64,\\(.*\\)" data-url)
    (error "Invalid data URL format"))
  ;; Save to cache
  (let ((coding-system-for-write 'binary))
    (with-temp-file file-path
      (set-buffer-multibyte nil)
      (insert (base64-decode-string (match-string 1 data-url)))))
  ;; Display it
  (wasabi-chat--display-cached-image file-path width height))

(cl-defun wasabi-chat--display-image-in-buffer (&key data-url mimetype width height)
  "Display image in *Wasabi photo* buffer.
DATA-URL is the base64-encoded data URL from the backend.
MIMETYPE is the image MIME type."
  (unless data-url
    (error ":data-url is required"))
  ;; Extract base64 data from data URL (format: "data:image/jpeg;base64,...")
  (unless (string-match "data:[^;]+;base64,\\(.*\\)" data-url)
    (error "Invalid data URL format"))
  (let* ((base64-data (match-string 1 data-url))
         (image-data (base64-decode-string base64-data))
         (image-type (cond
                      ((string-match "image/jpeg" mimetype) 'jpeg)
                      ((string-match "image/png" mimetype) 'png)
                      ((string-match "image/gif" mimetype) 'gif)
                      ((string-match "image/webp" mimetype) 'imagemagick)
                      (t 'imagemagick)))
         (photo-buffer (get-buffer-create "*Wasabi photo*")))
    (with-current-buffer photo-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (setq buffer-read-only t)
        (local-set-key (kbd "q") #'quit-window)
        (insert (propertize "q" 'face 'help-key-binding) " to close")
        (insert "\n\n")))
    (switch-to-buffer photo-buffer)
    ;; Calculate max dimensions based on window size (after switching to buffer)
    (let* ((win-width (window-pixel-width))
           (win-height (window-pixel-height))
           ;; Reserve some space for the header text
           (max-height (- win-height 60))
           (max-width win-width)
           ;; Create initial image to get actual dimensions
           (temp-image (create-image image-data image-type t))
           (image-size (image-size temp-image t))
           (actual-width (car image-size))
           (actual-height (cdr image-size))
           ;; Calculate scale factor to fit window
           (scale-x (/ (float max-width) actual-width))
           (scale-y (/ (float max-height) actual-height))
           (scale (min scale-x scale-y 1.0))
           ;; Create final scaled image
           (image (create-image image-data image-type t :scale scale)))
      (with-current-buffer photo-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize "🌄" 'display image))
          (insert "\n")
          (goto-char (point-min)))))))

(cl-defun wasabi-chat--create-rounded-image (&key image-data image-type max-width max-height corner-radius padding-top padding-bottom padding-leading padding-trailing is-video)
  "Create an SVG image with rounded corners containing IMAGE-DATA.
IMAGE-DATA is the raw image data.
IMAGE-TYPE is the image type (jpeg, png, etc.).
MAX-WIDTH and MAX-HEIGHT are the maximum dimensions (excluding padding).
CORNER-RADIUS is the radius for rounded corners.
PADDING-TOP is the top padding (default 0).
PADDING-BOTTOM is the bottom padding (default 0).
PADDING-LEADING is the left padding (default 0).
PADDING-TRAILING is the right padding (default 0).
IS-VIDEO if non-nil, overlays a play button on the thumbnail."
  (let* ((base64-data (base64-encode-string image-data))
         (pad-top (or padding-top 0))
         (pad-bottom (or padding-bottom 0))
         (pad-leading (or padding-leading 0))
         (pad-trailing (or padding-trailing 0))
         ;; Get actual image dimensions
         (temp-image (create-image image-data image-type t))
         (image-size (image-size temp-image t))
         (actual-width (car image-size))
         (actual-height (cdr image-size))
         ;; Calculate scaled dimensions that fit within max-width/max-height while preserving aspect ratio
         (scale-x (/ (float max-width) actual-width))
         (scale-y (/ (float max-height) actual-height))
         (scale (min scale-x scale-y 1.0)) ; Don't scale up, only down
         (display-width (floor (* actual-width scale)))
         (display-height (floor (* actual-height scale)))
         ;; Adjust corner radius to match actual display size (don't use full radius on tiny images)
         (adjusted-radius (min corner-radius (/ display-width 4) (/ display-height 4)))
         (clip-id (format "rounded-%d" (random 1000000)))
         (play-button (when is-video
                        (let* ((center-x (/ display-width 2))
                               (center-y (/ display-height 2))
                               ;; Circle radius: 40% of smaller dimension
                               (circle-radius (* 0.4 (min display-width display-height)))
                               ;; Triangle size: 40% of circle radius
                               (triangle-size (* 0.4 circle-radius))
                               ;; Triangle points (equilateral-ish, pointing right)
                               (tri-left-x (- center-x (* triangle-size 0.5)))
                               (tri-right-x (+ center-x triangle-size))
                               (tri-top-y (- center-y (* triangle-size 0.866))) ; sqrt(3)/2 ≈ 0.866
                               (tri-bottom-y (+ center-y (* triangle-size 0.866))))
                          (format "  <g transform=\"translate(%d,%d)\">
    <circle cx=\"%.1f\" cy=\"%.1f\" r=\"%.1f\" fill=\"rgba(0,0,0,0.6)\"/>
    <polygon points=\"%.1f,%.1f %.1f,%.1f %.1f,%.1f\" fill=\"white\"/>
  </g>"
                                  pad-leading pad-top
                                  center-x center-y circle-radius
                                  tri-left-x tri-top-y
                                  tri-right-x center-y
                                  tri-left-x tri-bottom-y))))
         (svg-template (format
                        "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%d\" height=\"%d\">
  <defs>
    <clipPath id=\"%s\">
      <rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" rx=\"%d\" ry=\"%d\"/>
    </clipPath>
  </defs>
  <g transform=\"translate(%d,%d)\" clip-path=\"url(#%s)\">
    <image xlink:href=\"data:image/%s;base64,%s\" x=\"0\" y=\"0\" width=\"%d\" height=\"%d\"/>
  </g>
%s</svg>"
                        (+ display-width pad-leading pad-trailing) (+ display-height pad-top pad-bottom)
                        clip-id
                        display-width display-height adjusted-radius adjusted-radius
                        pad-leading pad-top clip-id
                        (symbol-name image-type) base64-data
                        display-width display-height
                        (or play-button ""))))
    (create-image svg-template 'svg t)))

(provide 'wasabi-chat)
;;; wasabi-chat.el ends here
