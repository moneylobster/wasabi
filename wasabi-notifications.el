;;; wasabi-notifications.el --- A WhatsApp Emacs client  -*- lexical-binding: t; -*-

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

;;; wasabi-notifications provide the notification functionality for
;;; wasabi. Notifications can: be disabled, use the built-in notifications, use
;;; knockknock package (https://github.com/konrad1977/knockknock) or a custom
;;; function.


;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'wasabi-chat)
(require 'wasabi-icon)

(defcustom wasabi-message-notification-function 'notifications
  "Function or symbol to handle message notifications.

This can be:
- nil to disable notifications.
- The symbol `notifications` for using the built-in notifications system.
- The symbol `knockknock` for using the Knockknock notification system.
- A custom function, which will be called with `funcall`."
  :type
  '(choice
    (const :tag "Disabled" nil)
    (const :tag "Use notifications" notifications)
    (const :tag "Use knockknock" knockknock)
    (function :tag "Custom function"))
  :group 'wasabi)

(cl-defun wasabi--notify (message &key chat-buffer)
  "Display a notification with MESSAGE if needed.

CHAT-BUFFER is the chat buffer MESSAGE belongs to, when one is open.
It is optional: a message is worth announcing whether or not its chat
happens to be on screen."
  (when wasabi-message-notification-function
    (cond
     ((eq wasabi-message-notification-function 'notifications)
      (wasabi--notify-with-notifications message chat-buffer))
     ((eq wasabi-message-notification-function 'knockknock)
      (wasabi--notify-with-knockknock message chat-buffer))
     ((functionp wasabi-message-notification-function)
      (funcall wasabi-message-notification-function message)))))

(defun wasabi--get-msg-content (target-id chat-buffer)
  "Get the content of the message with TARGET-ID in CHAT-BUFFER.

Returns nil when CHAT-BUFFER is not open: the reacted-to message is
only known to a rendered chat."
  (when (and target-id chat-buffer (buffer-live-p chat-buffer))
    (when-let ((msg (seq-find
                     (lambda (msg)
                       (equal (map-elt msg :message-id) target-id))
                     (map-elt (buffer-local-value 'wasabi-chat--chat chat-buffer)
                              :messages))))
      (map-elt msg :content))))

(defun wasabi--reaction-body (message chat-buffer)
  "Describe the reaction in MESSAGE, looking up its target in CHAT-BUFFER."
  (if-let ((content (wasabi--get-msg-content (map-elt message :target-id)
                                             chat-buffer)))
      (concat "Reacted to " content " with: " (map-elt message :emoji))
    (concat "Reacted with: " (map-elt message :emoji))))

(defun wasabi--notify-with-notifications (message chat-buffer)
  "Display a notification with MESSAGE using the `notifications' package.

CHAT-BUFFER is the chat MESSAGE belongs to, or nil."
  (when (require 'notifications nil t)
    (notifications-notify
     :title (map-elt message :sender-name)
     :body (if (map-elt message :is-reaction)
               (wasabi--reaction-body message chat-buffer)
             (map-elt message :content))
     :app-name "Wasabi"
     :app-icon (wasabi-icon--svg-file)
     :urgency 'normal)))

(defun wasabi--notify-with-knockknock (message chat-buffer)
  "Display a notification with MESSAGE using the `knockknock' package.

CHAT-BUFFER is the chat MESSAGE belongs to, or nil."
  (when (require 'knockknock nil t)
    (knockknock-notify
     :title (map-elt message :sender-name)
     :message (if (map-elt message :is-reaction)
                  (wasabi--reaction-body message chat-buffer)
                (map-elt message :content))
     :app-name "Wasabi"
     :icon-file (wasabi-icon--svg-file))))


(provide 'wasabi-notifications)
;;; wasabi-notifications.el ends here
