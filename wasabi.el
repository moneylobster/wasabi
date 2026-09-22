;;; wasabi.el --- A WhatsApp Emacs client  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/wasabi
;; Version: 0.3.1
;; Package-Requires: ((emacs "29.1") (acp "0.7.1"))

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

;; wasabi provides a native Emacs interface for WhatsApp messaging.
;; It communicates with a wuzapi process over standard I/O using json-rpc.

;;; Code:

(require 'acp) ;; for json-rpc.
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'parse-time)
(require 'seq)

(defgroup wasabi nil
  "A native Emacs interface for WhatsApp messaging."
  :group 'comm
  :prefix "wasabi-")

(require 'wasabi-chat)
(require 'wasabi-icon)
(require 'wasabi-notifications)

(defcustom wasabi-user-token (concat
                              (or (user-login-name)
                                  (error "wasabi: No login name available"))
                              "-token")
  "User token for identifying wuzapi user.

Defaults to (user-login-name) + \"-token\".

This token identifies the single user added to the wuzapi database and
must remain constant across Emacs sessions. Changing this will cause
you to lose your WhatsApp session and require re-scanning the QR code.

`wasabi' communicates with a wuzapi process over standard I/O, thus
this token is not used for network security, but for rather user
identification in the wuzapi database.

Each wuzapi user has their own WhatsApp session associated with their
token.

If you need to start fresh delete the wuzapi database files."
  :type 'string
  :group 'wasabi)

(defcustom wasabi-data-dir (expand-file-name "wasabi" user-emacs-directory)
  "Directory for wasabi data storage.

This directory will be used to store the wuzapi database and session files.
If the directory does not exist, it will be created automatically."
  :type 'directory
  :group 'wasabi)

(defcustom wasabi-header-style (if (display-graphic-p) 'graphical 'text)
  "Style for wasabi buffer headers.

Can be one of:

 \='graphical: Display header with icon and styled text.
 \='text: Display simple text-only header."
  :type '(choice (const :tag "Graphical" graphical)
                 (const :tag "Text only" text))
  :group 'wasabi)

(defcustom wasabi-video-player-function nil
  "Function to open video files for viewing.

If nil, uses the default system player via `wasabi-chat--open-video-externally'.
If set to a function, it will be called with one argument: the absolute path
to the video file.

Example:
  (setq wasabi-video-player-function
        (lambda (file)
          (start-process \"mpv\" nil \"mpv\" file)))"
  :type '(choice (const :tag "Use system default" nil)
                 (function :tag "Custom function"))
  :group 'wasabi)

(defcustom wasabi-sync-quiet-seconds 30
  "Seconds of quiet before a WhatsApp sync is taken to have finished.

WhatsApp sends history in batches and does not reliably announce the
last one, so the syncing indicator clears itself after this long
without another batch arriving."
  :type 'natnum
  :group 'wasabi)

(defcustom wasabi-send-read-receipts t
  "Non-nil to tell senders when you have read their messages.

When on, opening a chat or sending to it marks what they sent you as
read, and they see blue ticks, as they would from the phone.  When off,
nothing is sent and your messages still go through.

Toggle it for the session with `wasabi-toggle-read-receipts', bound to
\\<wasabi-mode-map>\\[wasabi-toggle-read-receipts] in the chat list."
  :type 'boolean
  :group 'wasabi)

(defun wasabi-data-dir ()
  "Return the data directory, ensuring it exists.
Creates the directory if it doesn't exist.
Signals an error if the directory cannot be created."
  (let ((dir (expand-file-name wasabi-data-dir)))
    (unless (file-directory-p dir)
      (condition-case err
          (make-directory dir t)
        (error
         (error "Cannot create wasabi data directory %s: %s" dir (error-message-string err)))))
    dir))

(defcustom wasabi-wuzapi-command
  `("wuzapi" "-mode=stdio" ,(format "-datadir=%s" (wasabi-data-dir)))
  "Command and parameters for the wuzapi binary.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'wasabi)

(defvar wasabi--admin-token "emacs-admin-token"
  "Wuzapi admin token for administrative operations.

`wasabi' communicates with a wuzapi process over standard I/O.
While this token is not technically required for network authentication,
it is required internally by the process.")

(defconst wasabi--event-subscriptions
  '("Message"
    "Receipt"
    "Connected"
    "Disconnected"
    "ConnectFailure"
    "LoggedOut"
    "QR"
    "PairSuccess"
    "PairError"
    "StreamError"
    "StreamReplaced"
    "ClientOutdated"
    "TemporaryBan"
    "KeepAliveRestored"
    "KeepAliveTimeout"
    "UndecryptableMessage"
    "GroupInfo"
    "JoinedGroup"
    "IdentityChange"
    "HistorySync"
    "OfflineSyncCompleted"
    "AppStateSyncComplete")
  "WhatsApp event types to subscribe to during connection.")


;;; Chat identity (JIDs)
;;
;; WhatsApp addresses the same person two ways: by phone number
;; ("447123456789@s.whatsapp.net", a "PN" JID) and by linked identity
;; ("123456789@lid", a "LID" JID).  Which one shows up depends on the
;; chat's addressing mode, so the chat index, the contact list and
;; incoming events routinely disagree about a contact's JID.  Left
;; unreconciled, one person shows up as two chats: messages sent to one
;; JID come back addressed to the other.
;;
;; whatsmeow reports the counterpart JID on message events (Info's
;; SenderAlt and RecipientAlt), so we learn the pairing as messages flow
;; through and canonicalise every JID we display, route or send to.
;; Pairings are cached on disk, so this survives restarts.

(defvar wasabi--jid-canonical-table (make-hash-table :test 'equal)
  "Map of known JID to the canonical JID addressing the same peer.")

(defvar wasabi--jid-variants-table (make-hash-table :test 'equal)
  "Map of canonical JID to all JIDs known to address the same peer.")

(defvar wasabi--push-names-table (make-hash-table :test 'equal)
  "Map of JID to the name its owner goes by, as seen on their messages.")

(defvar wasabi--chat-times-table (make-hash-table :test 'equal)
  "Map of canonical JID to when that chat last saw a message.

Read from the messages themselves, since neither the chat index nor
wuzapi's message rows record anything but when they were written.")

(defvar wasabi--jid-aliases-dirty nil
  "Non-nil when learned JID pairings have yet to be written to disk.")

(defun wasabi--jid-string (jid)
  "Return JID as a non-empty string, or nil.

Protocol JIDs are usually strings, but arrive as symbols when they
were read as JSON object keys."
  (cond ((null jid) nil)
        ((stringp jid) (unless (string-empty-p jid) jid))
        ((symbolp jid) (symbol-name jid))
        (t (format "%s" jid))))

(defun wasabi--normalize-jid (jid)
  "Return JID without its device and agent suffixes.

\"447123456789:12@s.whatsapp.net\" => \"447123456789@s.whatsapp.net\"."
  (when-let ((jid (wasabi--jid-string jid)))
    (if (string-match "\\`\\([^@:.]+\\)[^@]*@\\(.+\\)\\'" jid)
        (concat (match-string 1 jid) "@" (downcase (match-string 2 jid)))
      jid)))

(defun wasabi--jid-identifier (jid)
  "Return the user part of JID (everything before the \"@\")."
  (when-let ((jid (wasabi--jid-string jid)))
    (if (string-match "\\`\\([^@]+\\)@" jid)
        (match-string 1 jid)
      jid)))

(defun wasabi--group-jid-p (jid)
  "Return non-nil if JID addresses a group."
  (when-let ((jid (wasabi--jid-string jid)))
    (string-suffix-p "@g.us" jid)))

(defun wasabi--jid-rank (jid)
  "Rank JID as a canonical candidate.  Lower sorts first.

Phone number JIDs win: that is what the send API expects and what
users recognise."
  (cond ((string-suffix-p "@s.whatsapp.net" jid) 0)
        ((string-suffix-p "@lid" jid) 1)
        (t 2)))

(defun wasabi--canonical-jid (jid)
  "Return the canonical JID for JID.

Falls back to JID itself when no counterpart is known."
  (when-let ((jid (wasabi--normalize-jid jid)))
    (or (gethash jid wasabi--jid-canonical-table) jid)))

(defun wasabi--jid-variants (jid)
  "Return every JID known to address the same peer as JID."
  (when-let ((canonical (wasabi--canonical-jid jid)))
    (or (gethash canonical wasabi--jid-variants-table)
        (list canonical))))

(defun wasabi--same-chat-p (jid-a jid-b)
  "Return non-nil when JID-A and JID-B address the same chat."
  (let ((a (wasabi--canonical-jid jid-a))
        (b (wasabi--canonical-jid jid-b)))
    (and a b (equal a b))))

(defun wasabi--learn-jid-alias (jid-a jid-b)
  "Record that JID-A and JID-B address the same peer.

Return non-nil when this taught us something new."
  (let ((a (wasabi--normalize-jid jid-a))
        (b (wasabi--normalize-jid jid-b)))
    (when (and a b
               (not (equal a b))
               ;; Group JIDs have no counterpart: only participants do.
               (not (wasabi--group-jid-p a))
               (not (wasabi--group-jid-p b))
               (not (wasabi--same-chat-p a b)))
      (let* ((members (seq-uniq (append (wasabi--jid-variants a)
                                        (wasabi--jid-variants b))))
             (canonical (car (sort (copy-sequence members)
                                   (lambda (x y)
                                     (< (wasabi--jid-rank x)
                                        (wasabi--jid-rank y)))))))
        (dolist (member members)
          ;; Superseded canonicals must not keep a variant list of their own.
          (remhash member wasabi--jid-variants-table)
          (puthash member canonical wasabi--jid-canonical-table))
        (puthash canonical members wasabi--jid-variants-table)
        (wasabi--log "Learned JID alias: %s" (string-join members " = "))
        (setq wasabi--jid-aliases-dirty t)))))

(defun wasabi--learn-push-name (jid push-name)
  "Record that JID goes by PUSH-NAME.

Return non-nil when this taught us something new.

The contact list only knows the people WhatsApp has got round to
sending, and knows no name at all for most of them.  Every message
carries its sender's chosen name, though, so anyone who has written to
us can be named from their own messages."
  (let ((jid (wasabi--normalize-jid jid)))
    (when (and jid
               (stringp push-name)
               (not (string-empty-p push-name))
               (not (wasabi--group-jid-p jid))
               (not (equal (gethash jid wasabi--push-names-table) push-name)))
      (puthash jid push-name wasabi--push-names-table)
      (setq wasabi--jid-aliases-dirty t))))

(defun wasabi--known-push-name (jid)
  "Return the name JID goes by, as seen on their messages, or nil."
  (when jid
    (seq-some (lambda (variant)
                (gethash variant wasabi--push-names-table))
              (wasabi--jid-variants jid))))

(defun wasabi--learn-from-message-info (p-info)
  "Learn what a protocol message P-INFO says about who sent it.

whatsmeow reports the sender's other addressing in SenderAlt and, for
messages we sent, the peer's other addressing in RecipientAlt, plus the
name the sender goes by in PushName."
  (when p-info
    ;; Our own name on our own messages says nothing about anyone else.
    (unless (map-elt p-info 'IsFromMe)
      (wasabi--learn-push-name (map-elt p-info 'Sender)
                               (map-elt p-info 'PushName))
      (wasabi--learn-push-name (map-elt p-info 'SenderAlt)
                               (map-elt p-info 'PushName)))
    (let* ((chat (map-elt p-info 'Chat))
           (sender (map-elt p-info 'Sender))
           (sender-alt (map-elt p-info 'SenderAlt))
           (recipient-alt (map-elt p-info 'RecipientAlt))
           (is-group (or (map-elt p-info 'IsGroup)
                         (wasabi--group-jid-p chat)))
           (learned (wasabi--learn-jid-alias sender sender-alt)))
      ;; In a one-to-one chat the chat JID is the peer, so the peer's
      ;; alternative addressing applies to the chat itself.
      (unless is-group
        (setq learned (or (wasabi--learn-jid-alias
                           chat
                           (if (map-elt p-info 'IsFromMe) recipient-alt sender-alt))
                          learned)))
      learned)))

(defun wasabi--jid-aliases-file ()
  "Return the file caching what we have learned about who is who."
  (expand-file-name "jid-aliases.eld" (wasabi-data-dir)))

(defun wasabi--save-jid-aliases ()
  "Persist learned JID pairings to disk, if any are outstanding.

Callers save once per batch of learning rather than per pairing."
  (when wasabi--jid-aliases-dirty
    (setq wasabi--jid-aliases-dirty nil)
    (ignore-errors
      (let ((groups '()))
        (maphash (lambda (canonical members)
                   (push (cons canonical members) groups))
                 wasabi--jid-variants-table)
        (let ((push-names '())
              (chat-times '()))
          (maphash (lambda (jid push-name)
                     (push (cons jid push-name) push-names))
                   wasabi--push-names-table)
          (maphash (lambda (jid timestamp)
                     (push (cons jid timestamp) chat-times))
                   wasabi--chat-times-table)
          (with-temp-file (wasabi--jid-aliases-file)
            (let ((print-length nil)
                  (print-level nil))
              ;; Tagged, to tell it from the bare list of pairings
              ;; written before anything else was cached alongside.
              (prin1 (list :aliases groups
                           :push-names push-names
                           :chat-times chat-times)
                     (current-buffer)))))))))

(defun wasabi--load-jid-aliases ()
  "Load what we had learned about who is who from disk."
  (ignore-errors
    (when (file-exists-p (wasabi--jid-aliases-file))
      (let* ((cached (with-temp-buffer
                       (insert-file-contents (wasabi--jid-aliases-file))
                       (read (current-buffer))))
             ;; Files written before push names were cached hold the
             ;; bare list of pairings.
             (tagged (and (listp cached) (eq (car cached) :aliases)))
             (groups (if tagged (plist-get cached :aliases) cached))
             (push-names (and tagged (plist-get cached :push-names)))
             (chat-times (and tagged (plist-get cached :chat-times))))
        (dolist (group groups)
          (let ((canonical (car group))
                (members (cdr group)))
            (dolist (member members)
              (puthash member canonical wasabi--jid-canonical-table))
            (puthash canonical members wasabi--jid-variants-table)))
        (dolist (entry push-names)
          (puthash (car entry) (cdr entry) wasabi--push-names-table))
        (dolist (entry chat-times)
          (puthash (car entry) (cdr entry) wasabi--chat-times-table))))))

(defcustom wasabi-message-history-limit 5000
  "How many messages wuzapi keeps per chat.

wuzapi trims a chat to this many rows every time a message is sent or
received, and it trims by the order rows were written rather than by
when the messages were sent.  So a small number does not keep the most
recent messages: it keeps whichever happened to be written last, and
deletes the rest for good.  Wasabi asked for 100, which cost most of a
conversation the first time anyone wrote to it.

Wasabi keeps this in step with wuzapi at startup, so raising it takes
effect on the next run.  Lowering it deletes messages, as wuzapi trims
to the new figure."
  :type 'natnum
  :group 'wasabi)

(defcustom wasabi-history-sync-days 365
  "How many days of history to ask WhatsApp for, from 0 to 365.

Applied when a device is paired, and only then: WhatsApp decides what
to send at that moment and will not be asked again.  0 leaves it to
WhatsApp, which sends rather little.  A year costs a longer first sync
in exchange for having your conversations."
  :type 'natnum
  :group 'wasabi)

(defcustom wasabi-chat-history-limit 1000
  "How many stored rows to ask for when opening a chat.

wuzapi returns rows ordered by when it wrote them rather than by when
the messages were sent, so asking for few returns an arbitrary slice of
a conversation rather than its recent end.  Wasabi asks for plenty and
sorts by the real timestamps, which are inside the messages themselves.
Lower this if opening a long chat feels slow; raise it if old messages
still show as the latest."
  :type 'natnum
  :group 'wasabi)

(defcustom wasabi-jid-resolve-batch-size 20
  "How many phone numbers `wasabi-resolve-jids' asks about at once.

Kept modest on purpose: asking WhatsApp about a great many numbers in
one go is what bulk contact scrapers do, and is worth not looking like."
  :type 'natnum
  :group 'wasabi)

(defun wasabi--learn-from-check-response (response)
  "Pair the phone numbers in a \"user.check\" RESPONSE with their JIDs.

Returns how many pairings were new."
  (let ((learned 0))
    (dolist (user (append (map-elt response 'Users) nil))
      (let ((query (map-elt user 'Query))
            (jid (map-elt user 'JID)))
        (when (and (wasabi--jid-string query)
                   (wasabi--jid-string jid)
                   (wasabi--learn-jid-alias
                    (concat (string-remove-prefix "+" query) "@s.whatsapp.net")
                    jid))
          (setq learned (1+ learned)))))
    learned))

(defun wasabi--unpaired-chat-numbers ()
  "Return the phone numbers of chats with no known linked identity."
  (delq nil
        (mapcar (lambda (chat)
                  (let ((jid (map-elt chat :chat-jid)))
                    (when (and jid
                               (string-suffix-p "@s.whatsapp.net" jid)
                               ;; Only one JID known for it, so nothing
                               ;; has paired it up yet.
                               (null (cdr (wasabi--jid-variants jid))))
                      (wasabi--jid-identifier jid))))
                (map-elt (wasabi--state) :chats-index))))

(cl-defun wasabi--resolve-jids-batch (&key numbers resolved on-complete)
  "Ask WhatsApp about NUMBERS, a batch at a time.

RESOLVED counts the pairings learned so far.  Calls ON-COMPLETE with
the total.  A batch that fails is logged and skipped rather than losing
the rest."
  (if (null numbers)
      (funcall on-complete resolved)
    (let ((batch (seq-take numbers wasabi-jid-resolve-batch-size))
          (rest (seq-drop numbers wasabi-jid-resolve-batch-size)))
      (acp-send-request
       :client (map-elt (wasabi--state) :client)
       :request (wasabi--make-user-check-request
                 :token wasabi-user-token
                 :phones batch)
       :on-success (lambda (response)
                     (let ((learned (wasabi--learn-from-check-response response)))
                       (wasabi--resolve-jids-batch
                        :numbers rest
                        :resolved (+ resolved learned)
                        :on-complete on-complete)))
       :on-failure (lambda (error)
                     (wasabi--log "Couldn't resolve a batch of %d: %s"
                                  (length batch)
                                  (or (map-elt error 'message) "unknown"))
                     (wasabi--resolve-jids-batch
                      :numbers rest
                      :resolved resolved
                      :on-complete on-complete))))))

;;;###autoload
(defun wasabi-resolve-jids ()
  "Pair each phone number chat with the linked identity behind it.

WhatsApp has moved to addressing people by a linked identity rather
than their phone number, and wuzapi files a conversation under whichever
of the two each message arrived with.  One conversation then sits in two
halves: what you sent under one JID, what they replied under the other,
each with its own dates.

Messages report the pairing only as they arrive live, and never on
history, so a conversation that moved before you paired this client
cannot be put back together from what is stored.  Asking WhatsApp which
JID a phone number belongs to is the one way to recover it.

Pairings are cached, so this is worth running once rather than often."
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (user-error "Not in a chats buffer"))
  (let ((numbers (wasabi--unpaired-chat-numbers)))
    (if (null numbers)
        (message "Every chat already knows its linked identity")
      (message "Asking WhatsApp about %d chats..." (length numbers))
      (wasabi--resolve-jids-batch
       :numbers numbers
       :resolved 0
       :on-complete
       (lambda (learned)
         (wasabi--save-jid-aliases)
         (wasabi--reparse-chat-index)
         (message "Paired %d of %d chats" learned (length numbers)))))))

(defun wasabi--find-contacts (jid contacts)
  "Return every CONTACTS entry addressing the same peer as JID.

Contacts are keyed by whichever JID WhatsApp happened to store, which
is often not the one a chat or an event uses; and the same person can
hold an entry under each of their JIDs, only one of which carries the
name we saved for them."
  (when-let ((jid (wasabi--jid-string jid)))
    (when contacts
      (delq nil
            (seq-uniq
             (cons (map-elt contacts (intern jid))
                   (mapcar (lambda (variant)
                             (map-elt contacts (intern variant)))
                           (wasabi--jid-variants jid))))))))

(defun wasabi--push-name (push-name)
  "Return PUSH-NAME marked as a push name, or nil if there is none.

WhatsApp prefixes a name its owner chose with \"~\", distinguishing it
from one we saved ourselves."
  (when (and push-name
             (stringp push-name)
             (not (string-empty-p push-name)))
    (if (string-prefix-p "~" push-name)
        push-name
      (concat "~" push-name))))

(defun wasabi--contact-display-name (jid contacts)
  "Return the best known name for JID in CONTACTS, or nil.

A name we saved wins over a push name even when the two are filed
under different JIDs for the same person, which is the usual case: the
phone number entry carries the saved name and the LID entry carries
only what they call themselves."
  (let ((entries (wasabi--find-contacts jid contacts)))
    (or (seq-some (lambda (contact) (map-elt contact :full-name)) entries)
        (wasabi--push-name
         (or (seq-some (lambda (contact) (map-elt contact :push-name)) entries)
             ;; WhatsApp knows no name for most contacts, so fall back to
             ;; what this person has called themselves on their messages.
             (wasabi--known-push-name jid))))))

;;; Timestamps

(defun wasabi--parse-timestamp (timestamp)
  "Parse protocol TIMESTAMP into an Emacs time value, or nil.

Handles ISO 8601 (\"2025-11-11T12:00:00Z\", used by message events),
Go's default time format (\"2025-11-11 12:00:00.000000 +0000 GMT\", used
by the chat index) and a Unix epoch, as a number or a string of
digits, in seconds or milliseconds."
  (cond
   ((null timestamp) nil)
   ;; A Unix epoch, in seconds or in milliseconds.
   ((numberp timestamp)
    (ignore-errors
      (seconds-to-time (if (> timestamp 100000000000) (/ timestamp 1000.0)
                         timestamp))))
   ((not (stringp timestamp)) nil)
   ((string-empty-p timestamp) nil)
   ;; An epoch that arrived as a string.  Checked before ISO 8601, which
   ;; would otherwise make something of the digits.
   ((string-match-p "\\`[0-9]+\\'" timestamp)
    (wasabi--parse-timestamp (string-to-number timestamp)))
   (t
    (or (ignore-errors (parse-iso8601-time-string timestamp))
        (ignore-errors
          (let ((parsed (parse-time-string timestamp)))
            (when (and (decoded-time-year parsed)
                       (decoded-time-month parsed)
                       (decoded-time-day parsed))
              (encode-time (decoded-time-set-defaults parsed)))))))))

(defun wasabi--timestamp-newer-p (a b)
  "Return non-nil when timestamp A is more recent than timestamp B.

Missing or unparseable timestamps sort last."
  (let ((ta (wasabi--parse-timestamp a))
        (tb (wasabi--parse-timestamp b)))
    (cond ((and ta tb) (time-less-p tb ta))
          (ta t)
          (t nil))))

(defun wasabi--timestamp-older-p (a b)
  "Return non-nil when timestamp A precedes timestamp B.

A timestamp we cannot read sorts first.  In a conversation that puts it
out of the way at the top, where it reads as old news, rather than at
the bottom pretending to be the latest thing said."
  (let ((ta (wasabi--parse-timestamp a))
        (tb (wasabi--parse-timestamp b)))
    (cond ((and ta tb) (time-less-p ta tb))
          (tb t)
          (t nil))))

(defun wasabi--chat-display-name (chat-jid)
  "Return the best known display name for CHAT-JID, or nil.

Looks in the chat index first, so a chat keeps the name it is listed
under, then falls back to the group and contact lists."
  (when-let ((chat-jid (wasabi--jid-string chat-jid)))
    (or (seq-some (lambda (chat)
                    (when (wasabi--same-chat-p (map-elt chat :chat-jid) chat-jid)
                      (map-elt chat :display-name)))
                  (map-elt (wasabi--state) :chats-index))
        (if (wasabi--group-jid-p chat-jid)
            (map-nested-elt (map-elt (wasabi--state) :groups)
                            (list (intern (wasabi--normalize-jid chat-jid)) :name))
          (wasabi--contact-display-name chat-jid (map-elt (wasabi--state) :contacts))))))

(defvar-local wasabi--state nil)

(cl-defun wasabi--initialize (&key wasabi-buffer status-type status-message)
  "Initialize wasabi client and progress through startup sequence.

Requires the WASABI-BUFFER.

Optional STATUS-TYPE and STATUS-MESSAGE are used for progressing through init.

For silent progression, set :silent-refresh in state before calling."
  (unless wasabi-buffer
    (error ":wasabi-buffer is required"))

  (unless wasabi--state
    (setq wasabi--state (wasabi--make-state :wasabi-buffer wasabi-buffer)))

  (wasabi--log "wasabi--initialize (wasabi-buffer: %s)(status-type: %s)"
               wasabi-buffer
               (map-nested-elt (wasabi--state) '(:status :type)))

  ;; Set status if provided
  (when status-type
    (wasabi--set-status :type status-type
                        :message status-message
                        :silent (map-elt (wasabi--state) :silent-refresh)))

  (cond
   ;; Step 1: Create client
   ((not (map-elt (wasabi--state) :client))
    (map-put! (wasabi--state)
              :client (acp-make-client :command (car wasabi-wuzapi-command)
                                       :command-params (cdr wasabi-wuzapi-command)
                                       :environment-variables (list (concat "WUZAPI_ADMIN_TOKEN=" wasabi--admin-token)
                                                                    (concat "TZ=" (wasabi--timezone)))
                                       :context-buffer wasabi-buffer))
    (wasabi--load-jid-aliases)
    (wasabi--initialize-subscriptions)
    (wasabi--initialize :wasabi-buffer wasabi-buffer
                        :status-type 'check-user
                        :status-message (wasabi--make-loading-message)))
   ;; Step 2: Check if user exists
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'check-user)
    (wasabi--set-status :type 'checking-user
                        :message (wasabi--make-loading-message))
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-admin-users-list-request
                                :admin-token wasabi--admin-token)
                      :on-success (lambda (users)
                                    (wasabi--log "Found %d users" (length users))
                                    (if (seq-empty-p users)
                                        ;; Need to add user
                                        (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                            :status-type 'add-user
                                                            :status-message (wasabi--make-loading-message))
                                      ;; User exists.  Put its history
                                      ;; settings right before carrying on:
                                      ;; accounts made by earlier versions
                                      ;; keep only 100 messages a chat.
                                      (wasabi--send-history-config-request
                                       :on-finished
                                       (lambda ()
                                         (wasabi--initialize
                                          :wasabi-buffer wasabi-buffer
                                          :status-type 'check-session-status
                                          :status-message (wasabi--make-loading-message))))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Couldn't load user: %s"
                                                 (or (map-elt error 'message) "unknown"))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Couldn't load user")))))
   ;; Step 3: Add user if needed
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'add-user)
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-admin-add-user-request
                                :admin-token wasabi--admin-token
                                :name (user-login-name)
                                :token wasabi-user-token
                                :events wasabi--event-subscriptions
                                :history wasabi-message-history-limit
                                :days-to-sync-history wasabi-history-sync-days)
                      :on-success (lambda (_response)
                                    (wasabi--log "User added successfully")
                                    (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                        :status-type 'check-session-status
                                                        :status-message (wasabi--make-loading-message)))
                      :on-failure (lambda (error)
                                    (wasabi--log "Couldn't add local user: %s" (map-elt error 'message))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Couldn't initialize user")))))
   ;; Step 4: Check session status
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'check-session-status)
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-session-status-request
                                :token wasabi-user-token)
                      :on-success (lambda (response)
                                    (wasabi--log "Status check response: connected=%s logged-in=%s"
                                                 (map-elt response 'connected)
                                                 (map-elt response 'loggedIn))
                                    (cond
                                     ;; Already connected and logged in - fetch data
                                     ((and (map-elt response 'connected)
                                           (map-elt response 'loggedIn))
                                      (map-put! (wasabi--state) :connected t)
                                      (wasabi--log "Already connected and logged in")
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'fetch-contacts
                                                          :status-message (wasabi--make-loading-message)))
                                     ;; Connected but not logged in - wait for notification
                                     ((map-elt response 'connected)
                                      (map-put! (wasabi--state) :connected t)
                                      (wasabi--log "Connected but not logged in, waiting for Connected notification")
                                      (wasabi--set-status :type 'already-connected
                                                          :message (wasabi--make-loading-message)))
                                     ;; Not connected - initiate connection
                                     (t
                                      (wasabi--log "Not connected, initiating connection")
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'connect-session
                                                          :status-message (wasabi--make-loading-message)))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Status check failed: %s" (or (map-elt error 'message) "unknown"))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Status check failed")))))
   ;; Step 5: Connect to WhatsApp
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'connect-session)
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-session-connect-request
                                :token wasabi-user-token
                                :immediate t
                                :subscribe wasabi--event-subscriptions)
                      :on-success (lambda (_response)
                                    (wasabi--log "Connection request successful, awaiting notification")
                                    (wasabi--set-status :type 'awaiting-connection
                                                        :message (wasabi--make-loading-message)))
                      :on-failure (lambda (error)
                                    (if (and (map-elt error 'message)
                                             (string-match-p "already connected" (map-elt error 'message)))
                                        (wasabi--log "Already connected (ignored)")
                                      (wasabi--log "Connect failed: %s" (map-elt error 'message))
                                      (wasabi--set-status :type 'error
                                                          :message (wasabi--refresh-error :message "Failed to connect"))))))
   ;; Step 6: Fetch contacts
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'fetch-contacts)
    (wasabi--log "Fetching contacts...")
    (wasabi--send-contacts-request
     ;; Continue to fetch groups regardless of contact count.
     ;; WhatsApp Web may not provide contacts on fresh pairing.
     :on-finished (lambda (_contacts)
                    (wasabi--initialize :wasabi-buffer wasabi-buffer
                                        :status-type 'fetch-groups
                                        :status-message
                                        (wasabi--make-loading-message)))
     :on-failure (lambda (_error)
                   (wasabi--set-status
                    :type 'error
                    :message (wasabi--refresh-error
                              :message "Failed to fetch contacts")))))
   ;; Step 7: Fetch groups
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'fetch-groups)
    (wasabi--log "Fetching groups...")
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-group-list-request
                                :token wasabi-user-token)
                      :on-success (lambda (response)
                                    ;; Response format: {"Groups": [...]} from backend
                                    ;; Extract the Groups array:
                                    ;;
                                    ;; '((Groups . [((JID . "123@g.us") (Name . "Family"))
                                    ;;              ((JID . "456@g.us") (Name . "Work"))]))
                                    ;;
                                    ;; => [((JID . "123@g.us") (Name . "Family"))
                                    ;;     ((JID . "456@g.us") (Name . "Work"))]
                                    (let* ((p-groups (map-elt response 'Groups))
                                           (groups (wasabi--parse-groups p-groups)))
                                      (map-put! (wasabi--state) :groups groups)
                                      (wasabi--log "Fetched %d groups" (length groups))
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'fetch-chats
                                                          :status-message (wasabi--make-loading-message))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Failed to fetch groups: %s" (map-elt error 'message))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Failed to fetch groups")))))

   ;; Step 8: Fetch chat index
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'fetch-chats)
    (wasabi--log "Fetching chat index...")
    (wasabi--send-chat-history-request
     :chat-jid "index"
     :on-finished (lambda ()
                    (wasabi--log "Chat index loaded")
                    (wasabi--set-status :type 'ready :message nil))))
   ;; Expected statuse changes (nothing to do)
   ((memq status-type '(awaiting-connection qr paired already-connected ready error disconnected))
    nil)))

(defun wasabi--make-icon-message (message)
  "Make MESSAGE with icon."
  (concat
   (wasabi-icon (* 2 (wasabi--face-height-pixels 'font-lock-doc-face)))
   "\n\n"
   message))

(defun wasabi--header-graphical-p ()
  "Return non-nil if header should show graphics."
  (and (display-graphic-p)
       (eq wasabi-header-style 'graphical)))

(defun wasabi--make-loading-message ()
  "Make \"Loading...\" message."
  (if (wasabi--header-graphical-p)
      ;; Compensate with prefixed whitespace
      ;; for visual centering as the ...
      ;; makes it look off center compared
      ;; to the icon.
      (wasabi--make-icon-message "    Loading...")
    "Loading..."))

(cl-defun wasabi--send-disconnect-request (&key on-disconnected)
  "Send disconnect request.

Invokes ON-DISCONNECTED (lambda ()) on success."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (wasabi--log "Requesting to disconnected")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-session-disconnect-request
                              :token wasabi-user-token)
                    :on-success (lambda (_response)
                                  (wasabi--log "Disconnect: success")
                                  (if on-disconnected
                                      ;; Let callback handle status
                                      (funcall on-disconnected)
                                    ;; No callback, set disconnected status
                                    (wasabi--set-status :type 'disconnected :message "Disconnected")))
                    :on-failure (lambda (error)
                                  (wasabi--log "Disconnect: failure %s" (map-elt error 'message))
                                  (wasabi--set-status
                                   :type 'error
                                   :message (wasabi--refresh-error :message "Something is not right")))))



;; TODO: Reconsider naming and splitting into two separate requests "index" vs "jid".
(cl-defun wasabi--send-chat-history-request (&key chat-jid contact-name on-finished)
  "Fetch chat history for CHAT-JID and store in state.
CONTACT-NAME is the display name to use for the chat buffer.

When CHAT-JID is \"index\", stores normalized chat index
as alist in :chats-index:
  ((\"chat-jid-1\" . chat-metadata-1)
   (\"chat-jid-2\" . chat-metadata-2) ...)

For a specific chat JID, stores message array in :chats and opens chat buffer.

Invoke ON-FINISHED on success."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (unless chat-jid
    (error ":chat-jid is required"))
  (if (equal chat-jid "index")
      (wasabi--send-chat-index-request :on-finished on-finished)
    (wasabi--fetch-chat-messages
     ;; One conversation can be recorded under both a LID and a phone
     ;; number JID, and wuzapi keeps a separate history under each, so
     ;; gather the messages from every JID that addresses this chat.
     :jids (seq-uniq (cons chat-jid (wasabi--jid-variants chat-jid)))
     :on-complete
     (lambda (p-messages)
       (let* ((p-messages (wasabi--dedupe-messages p-messages))
              (messages (wasabi-chat--parse-messages
                         p-messages
                         :chat-jid chat-jid
                         :contact-name contact-name
                         :contacts (map-elt (wasabi--state) :contacts))))
         (wasabi--log "Chat history for %s: %d messages"
                      chat-jid (length p-messages))
         (map-put! (wasabi--state) :chats
                   (map-insert (or (map-elt (wasabi--state) :chats) '())
                               chat-jid p-messages))
         ;; Sorted oldest first, so the last of them dates the chat.  Its
         ;; timestamp comes from the message's own Info where there is
         ;; one, the only place the real time survives.
         (wasabi--remember-chat-time chat-jid
                                     (map-elt (car (last messages)) :timestamp))
         (wasabi--reparse-chat-index)
         (wasabi-chat--start
          :chat-jid chat-jid
          :messages messages
          :contact-name contact-name))
       (when on-finished
         (funcall on-finished))))))

(cl-defun wasabi--fetch-chat-messages (&key jids acc on-complete)
  "Fetch the messages stored under each of JIDS.

Calls ON-COMPLETE with every message gathered, accumulated in ACC.  A
JID whose history cannot be fetched is logged and skipped rather than
losing the rest."
  (if (null jids)
      (funcall on-complete acc)
    (let ((jid (car jids)))
      (acp-send-request
       :client (map-elt (wasabi--state) :client)
       :request (wasabi--make-chat-history-request
                 :token wasabi-user-token
                 :chat-jid jid
                 :limit wasabi-chat-history-limit)
       :on-success (lambda (response)
                     (wasabi--fetch-chat-messages
                      :jids (cdr jids)
                      :acc (append acc (append response nil))
                      :on-complete on-complete))
       :on-failure (lambda (error)
                     (wasabi--log "Failed to fetch chat history for %s: %s"
                                  jid (or (map-elt error 'message) "unknown"))
                     (wasabi--fetch-chat-messages
                      :jids (cdr jids)
                      :acc acc
                      :on-complete on-complete))))))

(defun wasabi--dedupe-messages (p-messages)
  "Drop P-MESSAGES that repeat a message_id, keeping the first seen.

Gathering a chat from several JIDs can turn up the same message twice."
  (let ((seen (make-hash-table :test 'equal))
        (kept '()))
    (dolist (p-message (append p-messages nil))
      (let ((id (map-elt p-message 'message_id)))
        (unless (and id (gethash id seen))
          (when id (puthash id t seen))
          (push p-message kept))))
    (nreverse kept)))

(cl-defun wasabi--send-contacts-request (&key on-finished on-failure)
  "Fetch the contact list and store it in state as :contacts.

Invoke ON-FINISHED on success, or ON-FAILURE with the error."
  (acp-send-request
   :client (map-elt (wasabi--state) :client)
   :request (wasabi--make-user-contacts-request :token wasabi-user-token)
   ;; Response is an array of contacts
   :on-success (lambda (p-contacts)
                 (let ((contacts (wasabi--parse-contacts p-contacts)))
                   (map-put! (wasabi--state) :contacts contacts)
                   (wasabi--log "Fetched %d contacts" (length contacts))
                   (when (= (length contacts) 0)
                     (wasabi--log "No contacts from backend (expected on fresh pairing)"))
                   (when on-finished
                     (funcall on-finished contacts))))
   :on-failure (lambda (error)
                 (wasabi--log "Failed to fetch contacts: %s"
                              (or (map-elt error 'message) "unknown"))
                 (when on-failure
                   (funcall on-failure error)))))

(cl-defun wasabi--send-chat-index-request (&key on-finished)
  "Fetch the chat index and store it in state as :chats-index.

Invoke ON-FINISHED when done."
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-chat-history-request
                              :token wasabi-user-token
                              :chat-jid "index")
                    :on-success (lambda (response)
                                  (cond
                                   ;; Handle "index" response: {"user-id": [...]} from backend
                                   ;; Backend returns map of user-id to chat arrays
                                   ;; Merge all users' chats (typically only one user)
                                   (response
                                    (let* (;; Join all the chats into a single list.
                                           ;;
                                           ;;'((user-123 . [((chat_jid . "123") (last_updated . "2025-11-19"))
                                           ;;               ((chat_jid . "456") (last_updated . "2025-11-18"))])
                                           ;;  (user-456 . [((chat_jid . "789") (last_updated . "2025-11-19"))
                                           ;;               ((chat_jid . "101") (last_updated . "2025-11-18"))]))
                                           ;;
                                           ;; =>
                                           ;;
                                           ;; '(((chat_jid . "123") (last_updated . "2025-11-19"))
                                           ;;   ((chat_jid . "456") (last_updated . "2025-11-18"))
                                           ;;   ((chat_jid . "789") (last_updated . "2025-11-19"))
                                           ;;   ((chat_jid . "101") (last_updated . "2025-11-18")))
                                           (p-chats (apply #'append (mapcar (lambda (v) (append v nil)) (map-values response))))
                                           (p-chat-index (mapcar (lambda (p-chat)
                                                                   (cons (map-elt p-chat 'chat_jid) p-chat))
                                                                 p-chats)))
                                      (wasabi--log "Raw chats from response: %d" (length p-chats))
                                      ;; Kept so the index can be read again
                                      ;; as contacts arrive and histories
                                      ;; load, without refetching it.
                                      (map-put! (wasabi--state) :p-chat-index p-chat-index)
                                      (wasabi--reparse-chat-index))))
                                  (when on-finished
                                    (funcall on-finished)))
                    :on-failure (lambda (error)
                                  (wasabi--log "Failed to fetch chat index: %s"
                                               (or (map-elt error 'message) "unknown"))
                                  (message "Failed to fetch chat history"))))

(cl-defun wasabi--send-chat-send-text-request (&key phone body on-success on-failure)
  "Send a text message to PHONE with BODY.
Calls ON-SUCCESS when message is sent successfully.
Calls ON-FAILURE with error if sending fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless phone
    (error ":phone is required"))
  (unless body
    (error ":body is required"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-chat-send-text-request
                              :token wasabi-user-token
                              :phone phone
                              :body body)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (message "Message sent")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (message "Failed to send message: %s" (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--send-chat-send-image-request (&key phone image caption on-success on-failure)
  "Send IMAGE, a data URL, to PHONE with an optional CAPTION.
Calls ON-SUCCESS with the response when the image is sent.
Calls ON-FAILURE with the error if sending fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless phone
    (error ":phone is required"))
  (unless image
    (error ":image is required"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-chat-send-image-request
                              :token wasabi-user-token
                              :phone phone
                              :image image
                              :caption caption)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (message "Image sent")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (message "Failed to send image: %s"
                                               (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--send-chat-markread-request (&key chat sender ids on-success on-failure)
  "Mark the messages IDS in CHAT as read.
SENDER is who sent them, and is needed in a group, where one request
can only cover one sender's messages.
Calls ON-SUCCESS with the response, or ON-FAILURE with the error."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-chat-markread-request
                              :token wasabi-user-token
                              :chat chat
                              :sender sender
                              :ids ids)
                    :on-success (or on-success #'ignore)
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (wasabi--log "Couldn't send read receipts: %s"
                                                   (or (map-elt error 'message)
                                                       "unknown"))))))

(cl-defun wasabi--send-download-image-request (&key url direct-path media-key mimetype
                                                    file-enc-sha256 file-sha256 file-length
                                                    on-success on-failure)
  "Download URL with DIRECT-PATH and decrypt an image from WhatsApp servers.

DIRECT-PATH - WhatsApp direct path to encrypted file
MEDIA-KEY - Base64 encryption key for decrypting the image
MIMETYPE - Image MIME type (e.g., \"image/jpeg\")
FILE-ENC-SHA256 - SHA256 hash of encrypted file
FILE-SHA256 - SHA256 hash of decrypted file
FILE-LENGTH - File size in bytes

Calls ON-SUCCESS with response containing decrypted image data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (wasabi--log "Downloading image...")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-download-image-request
                              :token wasabi-user-token
                              :url url
                              :direct-path direct-path
                              :media-key media-key
                              :mimetype mimetype
                              :file-enc-sha256 file-enc-sha256
                              :file-sha256 file-sha256
                              :file-length file-length)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (wasabi--log "Image downloaded")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (wasabi--log "Failed to download image: %s"
                                                   (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--send-download-video-request (&key url direct-path media-key mimetype
                                                    file-enc-sha256 file-sha256 file-length
                                                    on-success on-failure)
  "Download URL and decrypt a video from WhatsApp servers.

DIRECT-PATH - WhatsApp direct path to encrypted file
MEDIA-KEY - Base64 encryption key for decrypting the image
MIMETYPE - Image MIME type (e.g., \"image/jpeg\")
FILE-ENC-SHA256 - SHA256 hash of encrypted file
FILE-SHA256 - SHA256 hash of decrypted file
FILE-LENGTH - File size in bytes

Calls ON-SUCCESS with response containing decrypted video data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (wasabi--log "Downloading video")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-download-video-request
                              :token wasabi-user-token
                              :url url
                              :direct-path direct-path
                              :media-key media-key
                              :mimetype mimetype
                              :file-enc-sha256 file-enc-sha256
                              :file-sha256 file-sha256
                              :file-length file-length)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (wasabi--log "Video downloaded")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (wasabi--log "Failed to download video: %s"
                                                   (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--make-session-disconnect-request (&key token)
  "Instantiate a \"session.disconnect\" request.

  Required parameters:
    TOKEN - User authentication token

  Disconnects from WhatsApp without logging out. Session can be
  reconnected later without re-pairing. Clears event subscriptions
  in the database.

  See: stdio.go:189, handlers.go (disconnectHandler)"
  (unless token
    (error ":token is required"))
  `((:method . "session.disconnect")
    (:params . ((token . ,token)))))

(cl-defun wasabi--initialize-subscriptions ()
  "Initialize json-rpc subscriptions with SHELL.."
  (unless (map-elt wasabi--state :client)
    (error "Missing client"))
  (acp-subscribe-to-errors
   :client (map-elt wasabi--state :client)
   :on-error (lambda (error)
               (wasabi--log "Something is wrong %s" error)
               ;; wuzapi writes normal INFO/DEBUG/WARN logs to stderr, which newer acp.el
               ;; versions treat as errors. Only treat it as a real error if the message
               ;; doesn't look like a plain wuzapi log line (which start with a timestamp
               ;; or JSON level field, not an actual error condition).
               (let* ((msg (or (map-elt error 'message) ""))
                      (looks-like-log (or (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" msg)
                                          (string-match-p "\"level\":" msg))))
                 (unless looks-like-log
                   (wasabi--set-status
                    :type 'error
                    :message (wasabi--refresh-error :message "Something is not right"))))))
  (acp-subscribe-to-notifications
   :client (map-elt wasabi--state :client)
   :on-notification (lambda (notification)
                      (wasabi--log "Notification: %s" (map-elt notification 'method))
                      (cond ((equal (map-elt notification 'method) "QR")
                             (when (map-nested-elt notification '(params qrCodeBase64))
                               (wasabi--display-qr-code (map-nested-elt notification '(params qrCodeBase64)))))
                            ((equal (map-elt notification 'method) "PairSuccess")
                             (wasabi--log "Pairing successful")
                             (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                 :status-type 'paired
                                                 :status-message (wasabi--make-loading-message)))
                            ((equal (map-elt notification 'method) "Connected")
                             (map-put! (wasabi--state) :connected t)
                             (wasabi--log "Connected to WhatsApp")
                             ;; Resume initialization after connection
                             (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
                               (when (memq status-type '(awaiting-connection already-connected qr paired))
                                 (wasabi--log "Resuming initialization after connection")
                                 (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                     :status-type 'fetch-contacts
                                                     :status-message (wasabi--make-loading-message)))))
                            ((equal (map-elt notification 'method) "LoggedOut")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Logged out, disconnecting and reconnecting for QR code")
                             ;; Disconnect first, then reconnect to get QR code
                             (wasabi--send-disconnect-request
                              :on-disconnected (lambda ()
                                                 (wasabi--log "Disconnected, now reconnecting")
                                                 (let ((wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)))
                                                   (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                                       :status-type 'connect-session
                                                                       :status-message (wasabi--make-loading-message))))))
                            ((equal (map-elt notification 'method) "OfflineSyncCompleted")
                             (wasabi--log "Offline sync completed")
                             (wasabi--set-syncing nil)
                             (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
                               ;; If we're ready, re-fetch all data since WhatsApp just synced
                               (when (eq status-type 'ready)
                                 (wasabi--log "Refreshing all data after OfflineSyncCompleted")
                                 ;; Re-run the fetch sequence to pick up synced data
                                 ;; Silent mode: no visual status updates, only refresh at the end
                                 (map-put! (wasabi--state) :silent-refresh t)
                                 (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                     :status-type 'fetch-contacts
                                                     :status-message (wasabi--make-loading-message)))))
                            ((equal (map-elt notification 'method) "AppStateSyncComplete")
                             (wasabi--log "App state sync complete")
                             ;; Contacts travel with app state, which lands
                             ;; well after the startup fetch has run, so the
                             ;; names we could not resolve then arrive here.
                             (if (eq (map-nested-elt (wasabi--state) '(:status :type))
                                     'ready)
                                 (wasabi--send-contacts-request
                                  :on-finished (lambda (_contacts)
                                                 (wasabi--reparse-chat-index)))
                               (wasabi--refresh)))
                            ((equal (map-elt notification 'method) "ConnectFailure")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Couldn't connect: %s"
                                          (format "%s" (or (map-nested-elt notification '(params reason))
                                                           (map-nested-elt notification '(params error))
                                                           "???")))
                             (wasabi--set-status
                              :type 'error
                              :message (wasabi--refresh-error :message "Couldn't connect")))
                            ((equal (map-elt notification 'method) "Disconnected")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--set-status
                              :type 'disconnected
                              :message (wasabi--refresh-error :message "Disconnected")))
                            ((equal (map-elt notification 'method) "Message")
                             (let* ((p-message (map-nested-elt notification '(params event Message)))
                                    (p-info (map-nested-elt notification '(params event Info)))
                                    (chat-jid (wasabi--jid-string (map-elt p-info 'Chat)))
                                    ;; Must capture contacts before with-current-buffer.
                                    (contacts (map-elt (wasabi--state) :contacts)))
                               ;; Learn this chat's LID/phone-number pairing before
                               ;; anything routes on the JID, or the message lands
                               ;; in a duplicate chat instead of the open one.
                               (wasabi--learn-from-message-info p-info)
                               (wasabi--save-jid-aliases)
                               (wasabi--remember-chat-time
                                chat-jid (map-elt p-info 'Timestamp))
                               ;; Trigger re-fetching index to show recent
                               ;; chats and groups with latest order.
                               (wasabi--send-chat-history-request :chat-jid "index")
                               (let* ((chat-buffer (wasabi-chat--find-buffer chat-jid))
                                      (contact-name
                                       (or (when chat-buffer
                                             (map-elt (buffer-local-value 'wasabi-chat--chat
                                                                          chat-buffer)
                                                      :contact-name))
                                           (wasabi--chat-display-name chat-jid)))
                                      (parsed (wasabi-chat--parse-notification
                                               :p-message p-message
                                               :p-info p-info
                                               :contact-name contact-name
                                               :chat-jid chat-jid
                                               :contacts contacts)))
                                 (when parsed
                                   ;; Notify whether or not the chat is open, but
                                   ;; never for our own messages, which echo back
                                   ;; from other devices.
                                   (unless (map-elt p-info 'IsFromMe)
                                     (wasabi--notify parsed :chat-buffer chat-buffer))
                                   (when chat-buffer
                                     (with-current-buffer chat-buffer
                                       (if (map-elt parsed :is-reaction)
                                           (wasabi-chat--add-reaction
                                            :target-id (map-elt parsed :target-id)
                                            :emoji (map-elt parsed :emoji)
                                            :sender (map-elt parsed :sender-name))
                                         (wasabi-chat--append-message parsed))))))))
                            ((equal (map-elt notification 'method) "HistorySync")
                             (wasabi--log "HistorySync received")
                             ;; Batches keep arriving for a while; each one
                             ;; pushes back when we stop saying so.
                             (wasabi--set-syncing "syncing messages")
                             (wasabi--log "HistorySync: current-buffer=%s, major-mode=%s" (current-buffer) major-mode)
                             ;; If we're ready, re-fetch all data since WhatsApp just synced history
                             (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
                               (wasabi--log "HistorySync: status-type=%s, ready?=%s" status-type (eq status-type 'ready))
                               (when (eq status-type 'ready)
                                 (wasabi--log "Refreshing all data after HistorySync")
                                 ;; Re-run the fetch sequence to pick up synced data.
                                 ;; Starting from fetch-contacts triggers the chain:
                                 ;; fetch-contacts -> fetch-groups -> fetch-chats -> ready
                                 ;; Silent mode: no visual status updates, only refresh at the end
                                 (map-put! (wasabi--state) :silent-refresh t)
                                 (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                     :status-type 'fetch-contacts
                                                     :status-message (wasabi--make-loading-message)))))
                            ;; Unrecoverable failures: surface as an error so the
                            ;; user isn't left staring at "Loading..." forever.
                            ((equal (map-elt notification 'method) "ClientOutdated")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Client outdated")
                             (wasabi--set-status
                              :type 'error
                              :message (wasabi--refresh-error
                                        :message (format "The %s util is too old. Please update."
                                                         (or (executable-find "wuzapi")
                                                             "wuzapi")))))
                            ((equal (map-elt notification 'method) "StreamReplaced")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Stream replaced")
                             (wasabi--set-status
                              :type 'error
                              :message (wasabi--refresh-error
                                        :message "WhatsApp session was opened elsewhere. Please close it.")))
                            ((equal (map-elt notification 'method) "TemporaryBan")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Temporary ban")
                             (wasabi--set-status
                              :type 'error
                              :message (wasabi--refresh-error
                                        :message "This WhatsApp account is temporarily banned.")))
                            ((equal (map-elt notification 'method) "StreamError")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Stream error")
                             (wasabi--set-status
                              :type 'error
                              :message (wasabi--refresh-error
                                        :message "WhatsApp connection stream error.")))
                            ((equal (map-elt notification 'method) "PairError")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Pair error")
                             (wasabi--set-status
                              :type 'error
                              :message (wasabi--refresh-error
                                        :message "Pairing with WhatsApp failed.")))
                            ;; Keep-alive timeout is transient once connected
                            ;; (whatsmeow reconnects and emits KeepAliveRestored),
                            ;; but while still loading it can strand us. Only
                            ;; surface it as an error in that case.
                            ((equal (map-elt notification 'method) "KeepAliveTimeout")
                             (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
                               (wasabi--log "KeepAliveTimeout (status-type: %s)" status-type)
                               (unless (eq status-type 'ready)
                                 (map-put! (wasabi--state) :connected nil)
                                 (wasabi--set-status
                                  :type 'error
                                  :message (wasabi--refresh-error
                                            :message "Lost connection to WhatsApp (keep-alive timeout).")))))))))

(defun wasabi--syncing ()
  "Return a label for what is being synced, or nil if nothing is."
  (when wasabi--state
    (cond ((map-elt wasabi--state :syncing))
          ;; A background re-fetch, which is what a sync triggers once
          ;; the chat list is already up.
          ((map-elt wasabi--state :silent-refresh) "refreshing")
          (t nil))))

(defun wasabi--set-syncing (label)
  "Note that LABEL is being synced, or nil when nothing is.

WhatsApp announces history in batches without reliably announcing the
last one, so the note clears itself once the batches stop arriving."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (unless (equal (map-elt wasabi--state :syncing) label)
    (map-put! wasabi--state :syncing label)
    (wasabi--update-header-line)
    (force-mode-line-update))
  (when-let ((timer (map-elt wasabi--state :sync-timer)))
    (cancel-timer timer)
    (map-put! wasabi--state :sync-timer nil))
  (when label
    (let ((buffer (current-buffer)))
      (map-put! wasabi--state :sync-timer
                (run-at-time wasabi-sync-quiet-seconds nil
                             (lambda ()
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (when wasabi--state
                                     (wasabi--set-syncing nil)
                                     (wasabi--refresh))))))))))

(defun wasabi--log (format-string &rest args)
  "Log a debug message to *Wasabi-Log* buffer.

FORMAT-STRING and ARGS like `message'."
  (with-current-buffer (get-buffer-create "*Wasabi-Log*")
    (goto-char (point-max))
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
    (insert (apply #'format format-string args))
    (insert "\n")))

(cl-defun wasabi--refresh-error (&key message)
  "Return error MESSAGE with refresh instructions.
If refresh keybinding exists, appends \"\\n\\n<keybinding> to reload\".
Otherwise returns just the message.

MESSAGE should be a string like \"Failed to connect\"."
  (if-let ((key (car (where-is-internal 'wasabi-reload))))
      (concat message "\n\n"
              (propertize (key-description key) 'face 'help-key-binding)
              " to reload")
    message))

(defun wasabi--display-qr-code (base64-data)
  "Display a QR code from BASE64-DATA in the home buffer."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (let ((image (create-image (base64-decode-string
                              (replace-regexp-in-string
                               "^data:image/png;base64," "" base64-data))
                             'png t)))
    (wasabi--set-status :type 'qr
                        :message (concat
                                  (propertize
                                   ;; Pad image string with * so it can be centered in screen.
                                   (concat (make-string (round (car (image-size image))) ?*) "\n")
                                   'display image)
                                  "\nScan from WhatsApp mobile to enable client"
                                  "\n\nor invoke M-x wasabi-pair-phone-number"))))

(cl-defun wasabi--message (&key text)
  "Display centered TEXT centered in current buffer.

This function displayes TEXT only, wiping everything else."
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (let* ((lines (split-string (or text "") "\n"))
         (n-lines (length lines))
         (win-height (window-body-height))
         (win-width  (window-body-width))
         (top-pad (max 0 (/ (- win-height n-lines) 2)))
         centered-lines)
    ;; Compute centered each line horizontally
    (setq centered-lines
          (mapcar
           (lambda (line)
             (let* ((col-pad (max 0 (/ (- win-width (string-width line)) 2)))
                    (spaces (make-string col-pad ?\s)))
               (concat spaces line)))
           lines))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Pad with newlines to center vertically
      (insert (make-string top-pad ?\n))
      ;; Insert centered lines
      (dolist (cl centered-lines)
        (insert cl "\n")))))

(cl-defun wasabi--make-state (&key wasabi-buffer)
  "Construct chat client state with WASABI-BUFFER.

State uses :status to track initialization progress (see `wasabi--make-status').
The :connected flag tracks WhatsApp connection state (updated by notifications)."
  (unless wasabi-buffer
    (error ":wasabi-buffer is required"))
  (list (cons :client nil)
        (cons :wasabi-buffer wasabi-buffer)
        (cons :status nil)
        ;; :connected tracks async WhatsApp connection state (set by notifications)
        (cons :connected nil)
        ;; Sample contacts structure:
        ;;
        ;; ((555123456789@lid (BusinessName . "") (FirstName . "John") (Found . t)
        ;;                    (FullName . "John Smith") (PushName . "Johnny") (RedactedPhone . ""))
        ;;  (555987654321@lid (BusinessName . "Acme Corp") (FirstName . "Jane") (Found . t)
        ;;                    (FullName . "Jane Doe") (PushName . "Jane") (RedactedPhone . "")))
        (cons :contacts nil)
        ;; Sample chats index structure:
        ;;
        ;; (("1234567890@s.whatsapp.net" . ((chat_jid . "1234567890@s.whatsapp.net")
        ;;                                  (last_updated . "2025-11-11 12:00:00.000000 +0000 GMT")))
        ;;  ("987654321@g.us" . ((chat_jid . "987654321@g.us")
        ;;                       (last_updated . "2025-11-10 18:30:00.000000 +0000 GMT")))
        ;;  ...)
        (cons :chats-index nil)
        ;; Raw chat index as received, kept so it can be read again
        ;; when contacts or histories improve what we can say about it.
        (cons :p-chat-index nil)
        ;; Sample chats structure:
        ;;
        ;; (("1234567890@s.whatsapp.net" . [((chat_jid . "1234567890@s.whatsapp.net")
        ;;                                   (message_id . "ABC123DEF456")
        ;;                                   (message_type . "text")
        ;;                                   (sender_jid . "9876543210@s.whatsapp.net")
        ;;                                   (text_content . "Hello world")
        ;;                                   (timestamp . "2025-11-11T12:00:00Z")
        ;;                                   (data_json . "{}")
        ;;                                   (media_link . "")
        ;;                                   (id . 1)
        ;;                                   (user_id . "user123"))
        ;;                                  ...])
        ;;  ("987654321@g.us" . [...])
        ;;  ...)
        (cons :chats nil)
        (cons :groups nil)
        ;; Set while a background re-fetch runs, so status changes do
        ;; not flash over the chat list.  Declared here because
        ;; `map-put!' cannot add a key to an alist in place: it signals
        ;; map-not-inplace, which used to abort the sync handlers before
        ;; they could re-fetch anything.
        (cons :silent-refresh nil)
        ;; What WhatsApp is currently syncing, if anything, and the
        ;; timer that gives up waiting for it to say it has finished.
        (cons :syncing nil)
        (cons :sync-timer nil)))

(cl-defun wasabi--make-status (&key type message)
  "Create a status object with TYPE and optional MESSAGE.

TYPE can be one of:
  Initialization: loading, checking-user, checking-session-status,
  adding-user, connecting, awaiting-connection
  Authentication: qr, already-connected
  Data Loading: fetching-contacts, fetching-groups, fetching-chats
  Terminal: ready, error, disconnected

MESSAGE is displayed if present.  When TYPE is ready, the chat list is rendered."
  `((:type . ,type)
    (:message . ,message)))

(cl-defun wasabi--set-status (&key type message silent)
  "Set the current status TYPE and MESSAGE and refresh the display.

Optional SILENT suppresses visual messaging during status change."
  (wasabi--log "Status change: %s \"%s\"" type message)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (map-put! (wasabi--state) :status (wasabi--make-status :type type :message message))
  ;; A background refresh is over once we are ready again.  Set to nil
  ;; rather than deleted: `map-put!' can only update a key an alist
  ;; already has, so removing it would break the next refresh.
  (when (eq type 'ready)
    (map-put! (wasabi--state) :silent-refresh nil))
  (if (eq type 'ready)
      (wasabi--update-header-line)
    (setq header-line-format nil))
  (when (or (eq type 'ready) (not silent))
    (wasabi--refresh)))

(defun wasabi--timezone ()
  "Return the current time zone."
  (or
   ;; macOS: Try /etc/localtime symlink
   (let ((target (ignore-errors (file-truename "/etc/localtime"))))
     (when (and target
                (string-match "\\(\\([A-Za-z_]+\\)/\\([A-Za-z_+-]+\\)\\)$" target))
       (match-string 1 target)))
   ;; macOS: systemsetup command
   (let ((tz (string-trim
              (ignore-errors
                (shell-command-to-string
                 "systemsetup -gettimezone 2>/dev/null | awk -F': ' '{print $2}'")))))
     (when (and tz (string-match "/" tz))
       tz))
   ;; Linux: /etc/timezone
   (let ((tz (and (file-exists-p "/etc/timezone")
                  (with-temp-buffer
                    (insert-file-contents "/etc/timezone")
                    (string-trim (buffer-string))))))
     (when (and tz (string-match "/" tz))
       tz))
   ;; Fallback: TZ env variable
   (let ((tz (getenv "TZ")))
     (when (and tz (string-match "/" tz))
       tz))
   (error "Couldn't determine timezone")))

(defun wasabi--make-action-keymap (action)
  "Create keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun wasabi--add-action-to-text (text action &optional on-entered face)
  "Add ACTION lambda to propertized TEXT and return modified text.
ON-ENTERED is a function to call when the cursor enters the text.
FACE when non-nil applies the specified face to the text."
  (add-text-properties 0 (length text)
                       `(keymap ,(wasabi--make-action-keymap action)
                                mouse-face highlight
                                pointer hand)
                       text)
  (when on-entered
    (add-text-properties 0 (length text)
                         (list 'cursor-sensor-functions
                               (list (lambda (_window _old-pos sensor-action)
                                       (when (eq sensor-action 'entered)
                                         (funcall on-entered)))))
                         text))
  (when face
    (add-text-properties 0 (length text)
                         `(font-lock-face ,face)
                         text)
    (add-text-properties 0 (length text)
                         `(face ,face)
                         text))
  text)

(defvar wasabi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "C") #'wasabi-new-chat)
    (define-key map (kbd "c") #'wasabi-new-chat)
    (define-key map (kbd "+") #'wasabi-new-chat-new-number)
    (define-key map (kbd "q") #'wasabi-quit)
    (define-key map (kbd "g") #'wasabi-reload)
    (define-key map (kbd "m") #'wasabi-toggle-notifications)
    (define-key map (kbd "r") #'wasabi-toggle-read-receipts)
    map)
  "Keymap for `wasabi-mode'.")

(defcustom wasabi-sync-indicator "(*)"
  "Marker shown in the header line while WhatsApp is syncing."
  :type 'string
  :group 'wasabi)

(defun wasabi--update-header-line ()
  "Update the header line for the main chats app buffer."
  (let ((bindings `((:command wasabi-new-chat :description "new chat")
                    (:command wasabi-new-chat-new-number :description "new number")
                    (:command wasabi-toggle-notifications
                              :description ,(if wasabi-notifications-enabled
                                                "mute"
                                              "unmute"))
                    (:command wasabi-toggle-read-receipts
                              :description ,(if wasabi-send-read-receipts
                                                "receipts off"
                                              "receipts on"))
                    (:command wasabi-reload :description "reload")
                    (:command wasabi-quit :description "quit"))))
    (setq header-line-format
          (concat
           (when (wasabi--header-graphical-p)
             (concat
              " "
              (wasabi-icon (wasabi--face-height-pixels 'font-lock-doc-face))))
           " "
           (propertize "Recent Chats" 'face 'font-lock-doc-face)
           " "
           (when-let ((syncing (wasabi--syncing)))
             (concat (propertize (format "%s %s" wasabi-sync-indicator syncing)
                                 'face 'font-lock-comment-face)
                     " "))
           (mapconcat
            #'identity
            (seq-filter
             #'identity
             (mapcar
              (lambda (binding)
                (when-let* ((command (map-elt binding :command))
                            (description (map-elt binding :description))
                            (keys (where-is-internal command wasabi-mode-map))
                            (key (key-description (car keys))))
                  (concat
                   (propertize key 'face 'help-key-binding)
                   " "
                   description)))
              bindings))
            " ")))))

(define-derived-mode wasabi-mode fundamental-mode "Wasabi"
  "Major mode for chat interfaces.

\\{wasabi-mode-map}"
  (setq buffer-read-only t)
  (wasabi--update-header-line)
  (goto-char (point-max)))

(defun wasabi-quit ()
  "Quit `wasabi' and disconnect from WhatsApp."
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

;;;###autoload
(defun wasabi ()
  "Create or switch to the `*Wasabi*` buffer in `wasabi-mode`."
  (interactive)
  (let ((wasabi-buffer (get-buffer-create "*Wasabi*")))
    (with-current-buffer wasabi-buffer
      (unless (derived-mode-p 'wasabi-mode)
        (wasabi-mode))
      (add-hook 'kill-buffer-hook #'wasabi--clean-up nil t)
      (add-hook 'window-size-change-functions
                (lambda (_frame)
                  (with-current-buffer wasabi-buffer
                    (when (map-nested-elt (wasabi--state) '(:status :message))
                      (wasabi--refresh)))) nil t)
      (add-hook 'window-configuration-change-hook
                (lambda ()
                  (with-current-buffer wasabi-buffer
                    (when (map-nested-elt (wasabi--state) '(:status :message))
                      (wasabi--refresh)))) nil t)
      (wasabi--initialize :wasabi-buffer wasabi-buffer))
    (switch-to-buffer wasabi-buffer)
    (wasabi--refresh)))

(defun wasabi--buffer ()
  "Get the `wasabi'.

Error if not found."
  (or (get-buffer "*Wasabi*")
      (user-error "Wasabi buffer not found (start with M-x wasabi)")))

(defun wasabi-reload ()
  "Reload `wassabi' buffer."
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (user-error "Not in a chats buffer"))
  (wasabi--log "Refresh requested, resetting state and restarting...")
  (let ((wasabi-buffer (current-buffer)))
    (when (map-elt (wasabi--state) :client)
      (acp-shutdown :client (map-elt (wasabi--state) :client)))
    (setq wasabi--state nil)
    (wasabi--log "==== new session ====")
    (wasabi--initialize :wasabi-buffer wasabi-buffer)))

(defun wasabi-new-chat-new-number ()
  "Start a new chat with a new phone number."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'wasabi-new-chat)))

(defun wasabi--chat-index-jid-p (jid chats-index)
  "Return non-nil when JID is a JID CHATS-INDEX already uses."
  (let ((jid (wasabi--normalize-jid jid)))
    (seq-some (lambda (chat)
                (or (equal (wasabi--normalize-jid (map-elt chat :chat-jid)) jid)
                    (seq-find (lambda (alt)
                                (equal (wasabi--normalize-jid alt) jid))
                              (map-elt chat :alt-jids))))
              chats-index)))

(defun wasabi--dedupe-contact-entries (entries chats-index)
  "Drop ENTRIES addressing a peer already covered by an earlier entry.

The contact list can hold the same person under both a LID and a phone
number JID.  Offering both means picking one starts a second chat
alongside the existing one, so keep a single entry and prefer the JID
CHATS-INDEX already uses."
  (let ((seen (make-hash-table :test 'equal))
        (kept '()))
    (dolist (entry entries)
      (let* ((jid (map-elt entry :jid))
             (canonical (wasabi--canonical-jid jid))
             (existing (gethash canonical seen)))
        (cond
         ((null existing)
          (puthash canonical entry seen)
          (push entry kept))
         ((and (wasabi--chat-index-jid-p jid chats-index)
               (not (wasabi--chat-index-jid-p (map-elt existing :jid) chats-index)))
          (map-put! existing :jid jid)))))
    (nreverse kept)))

(defun wasabi--disambiguate-entries (entries)
  "Make the display names in ENTRIES unique, and return ENTRIES.

`completing-read' hands back a label, so two people sharing a name
would always resolve to whichever of them came first."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (puthash (map-elt entry :display-name)
               (1+ (gethash (map-elt entry :display-name) counts 0))
               counts))
    (dolist (entry entries entries)
      (when (> (gethash (map-elt entry :display-name) counts 0) 1)
        (map-put! entry :display-name
                  (format "%s <%s>"
                          (map-elt entry :display-name)
                          (wasabi--jid-identifier (map-elt entry :jid))))))))

(defun wasabi-new-chat (new-number)
  "Select a contact or group and open a new chat.

With prefix argument NEW-NUMBER, prompt for a phone number."
  (interactive
   (list (when current-prefix-arg
           (read-string "Phone number (with country code, e.g., 447123456789): "))))
  (with-current-buffer (wasabi--buffer)
    (if new-number
        ;; Direct phone number chat
        (wasabi--send-chat-history-request
         :chat-jid (concat (string-trim new-number) "@s.whatsapp.net")
         :contact-name (string-trim new-number))
      ;; Normal contact/group selection
      (unless (or (map-elt (wasabi--state) :contacts)
                  (map-elt (wasabi--state) :groups))
        (user-error "No contacts or groups available"))
      (let* ((contact-entries
              (mapcar (lambda (contact-entry)
                        (let* ((jid (symbol-name (car contact-entry)))
                               (full-name (map-elt (cdr contact-entry) :full-name))
                               (push-name (map-elt (cdr contact-entry) :push-name))
                               (display-name (or (and full-name (not (string-empty-p full-name)) full-name)
                                                 (and push-name (not (string-empty-p push-name)) push-name)
                                                 jid)))
                          `((:display-name . ,display-name)
                            (:jid . ,jid)
                            (:is-group . nil))))
                      (map-elt (wasabi--state) :contacts)))
             (group-entries
              (mapcar (lambda (group-entry)
                        (let* ((jid (symbol-name (car group-entry)))
                               (group-info (cdr group-entry))
                               (group-name (map-elt group-info :name))
                               (display-name (or (and group-name (not (string-empty-p group-name)) group-name)
                                                 jid)))
                          `((:display-name . ,display-name)
                            (:jid . ,jid)
                            (:is-group . t))))
                      (map-elt (wasabi--state) :groups)))
             (all-entries
              (wasabi--disambiguate-entries
               (wasabi--dedupe-contact-entries
                (sort
                 (seq-filter
                  (lambda (entry)
                    ;; Filter out groups and numbers we couldn't name: their
                    ;; display name is the raw JID, no use to pick from.
                    (not (and (equal (map-elt entry :display-name) (map-elt entry :jid))
                              (or (string-suffix-p "@lid" (map-elt entry :jid))
                                  (string-suffix-p "@g.us" (map-elt entry :jid))))))
                  (append contact-entries group-entries))
                 (lambda (a b) (string< (map-elt a :display-name) (map-elt b :display-name))))
                (map-elt (wasabi--state) :chats-index))))
             (max-width (if all-entries
                            (apply #'max (mapcar (lambda (entry)
                                                   (string-width (map-elt entry :display-name)))
                                                 all-entries))
                          0))
             (candidates
              (mapcar (lambda (entry)
                        ;; return list of (label . jid)
                        (cons (if (map-elt entry :is-group)
                                  (concat (map-elt entry :display-name)
                                          ;; Pad using longest contact name.
                                          (make-string (- max-width (string-width (map-elt entry :display-name))) ?\s)
                                          " (group)")
                                (map-elt entry :display-name))
                              (map-elt entry :jid)))
                      all-entries)))
        (unless all-entries
          (user-error "No contacts or groups available"))
        (if-let* ((selected-label (completing-read "Chat with: " candidates nil t))
                  (selected-jid (map-elt candidates selected-label))
                  (selected-entry (seq-find (lambda (entry)
                                              (string= (map-elt entry :jid) selected-jid))
                                            all-entries)))
            (wasabi--send-chat-history-request
             :chat-jid selected-jid
             ;; No " (group)" suffix here: the chat list opens the same chat
             ;; without one, and a differing name used to mean a second buffer.
             :contact-name (map-elt selected-entry :display-name))
          (user-error "No contact or group found"))))))

(defun wasabi-toggle-notifications ()
  "Turn notifications for incoming messages off, or back on.

Lasts for this session.  Customize `wasabi-notifications-enabled' to
change what wasabi starts with."
  (interactive)
  (setq wasabi-notifications-enabled (not wasabi-notifications-enabled))
  ;; The chat list header offers mute or unmute, so keep it truthful.
  (when-let ((buffer (get-buffer "*Wasabi*")))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'wasabi-mode)
                 wasabi--state
                 (eq (map-nested-elt wasabi--state '(:status :type)) 'ready))
        (wasabi--update-header-line))))
  (message "Wasabi notifications %s"
           (if wasabi-notifications-enabled "on" "off")))

(defun wasabi-toggle-read-receipts ()
  "Stop telling senders you have read their messages, or start again.

Lasts for this session.  Customize `wasabi-send-read-receipts' to
change what wasabi starts with."
  (interactive)
  (setq wasabi-send-read-receipts (not wasabi-send-read-receipts))
  ;; The chat list header offers the opposite, so keep it truthful.
  (when-let ((buffer (get-buffer "*Wasabi*")))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'wasabi-mode)
                 wasabi--state
                 (eq (map-nested-elt wasabi--state '(:status :type)) 'ready))
        (wasabi--update-header-line))))
  (message "Wasabi read receipts %s"
           (if wasabi-send-read-receipts "on" "off")))

(defun wasabi-open-data-directory ()
  "Open data directory (database, media, etc)."
  (interactive)
  (find-file (wasabi-data-dir)))

(defun wasabi-pair-phone-number (phone)
  "Pair WhatsApp using PHONE number instead of QR code.

This is an alternative to QR code scanning for users who cannot
scan QR codes (e.g., screen reader users or TTY environments).

PHONE should include the country code (e.g., \"+1234567890\").

The session must be connected but not logged in (i.e., the QR code
should be displayed). After calling this, an 8-character code will
be shown in the minibuffer and copied to the kill ring. Enter this
code in WhatsApp mobile: Settings -> Linked Devices -> Link a Device
-> Link with phone number instead."
  (interactive "sPhone number (with country code, e.g., +1234567890): ")
  (unless (wasabi--state)
    (user-error "Wasabi not running.  Start with M-x wasabi"))
  (unless (map-elt (wasabi--state) :client)
    (user-error "Wasabi client not initialized"))
  (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
    (unless (memq status-type '(qr awaiting-connection already-connected))
      (user-error "Cannot pair now.  QR code must be displayed first (status: %s)" status-type)))
  (acp-send-request
   :client (map-elt (wasabi--state) :client)
   :request (wasabi--make-session-pairphone-request
             :token wasabi-user-token
             :phone (string-trim phone))
   :on-success (lambda (response)
                 (if (map-elt response 'LinkingCode)
                     (progn
                       (kill-new (map-elt response 'LinkingCode))
                       (message "Pairing code: %s (copied to kill ring)"
                                (map-elt response 'LinkingCode)))
                   (message "Unexpected response: %s" response)))
   :on-failure (lambda (error)
                 (message "Pairing failed: %s"
                          (or (map-elt error 'message) error)))))

(defalias 'wasabi-new-message #'wasabi-new-chat)

(defun wasabi--group-chats-by-date (chats-index)
  "Group CHATS-INDEX by date labels (Today, Yesterday, or date).
Returns list of (date-label . chats-for-that-date)."
  (let ((date-groups '())
        (today (decode-time))
        (yesterday (decode-time (time-subtract nil (* 24 60 60)))))
    (dolist (chat chats-index)
      (let* ((timestamp (wasabi--parse-timestamp (map-elt chat :last-updated)))
             (date-time (when timestamp
                          (decode-time timestamp)))
             (date-label (if date-time
                             (cond
                              ;; Today
                              ((and (= (decoded-time-year date-time)
                                       (decoded-time-year today))
                                    (= (decoded-time-month date-time)
                                       (decoded-time-month today))
                                    (= (decoded-time-day date-time)
                                       (decoded-time-day today)))
                               "Today")
                              ;; Yesterday
                              ((and (= (decoded-time-year date-time)
                                       (decoded-time-year yesterday))
                                    (= (decoded-time-month date-time)
                                       (decoded-time-month yesterday))
                                    (= (decoded-time-day date-time)
                                       (decoded-time-day yesterday)))
                               "Yesterday")
                              ;; Other dates - format as "Month Day"
                              (t
                               (format-time-string "%B %e" timestamp)))
                           ;; No timestamp - use "Sometime"
                           "Sometime"))
             (date-group (map-elt date-groups date-label)))
        (if date-group
            ;; Append/modify existing group.
            (setcdr date-group (append (cdr date-group) (list chat)))
          ;; Create new group
          (push (cons date-label (list chat)) date-groups))))
    (nreverse date-groups)))

(cl-defun wasabi--format-chat-preview (&key display-name is-group last-updated)
  "Format a chat preview line for the chats list.

DISPLAY-NAME is the contact/group name.
IS-GROUP indicates if this is a group chat.
LAST-UPDATED is the protocol timestamp string."
  (let* ((timestamp (wasabi--parse-timestamp last-updated))
         (time-str (when timestamp
                     (format-time-string "%H:%M" timestamp))))
    (concat (if is-group
                (propertize "G" 'face 'success)
              " ")
            " "
            (if time-str
                (propertize time-str 'face 'font-lock-comment-face)
              "     ")
            " "
            display-name)))

(defun wasabi--refresh ()
  "Refresh the display based on current status."
  (let* ((status (map-elt (wasabi--state) :status))
         (chats-index (map-elt (wasabi--state) :chats-index))
         ;; Save position
         (saved-line (line-number-at-pos))
         (saved-col (current-column)))
    (cond
     ;; Only render chat list when status is 'ready
     ((eq (map-elt status :type) 'ready)
      (if (null chats-index)
          ;; No chats available - show empty state
          (let ((inhibit-read-only t))
            (erase-buffer)
            (wasabi--message :text
                             (if (wasabi--syncing)
                                 (concat "Syncing with WhatsApp"
                                         "\n\n"
                                         "Chats will appear as they arrive")
                               (concat
                                "No recent chats"
                                "\n\n"
                                (propertize "c" 'face 'help-key-binding)
                                " "
                                "to start a new chat"))))
        ;; Render chat list
        (let ((sections (mapcar
                         (lambda (date-group)
                           (let* ((date-label (car date-group))
                                  (chats (cdr date-group))
                                  (chat-lines
                                   (mapcar
                                    (lambda (chat)
                                      (wasabi--add-action-to-text
                                       (wasabi--format-chat-preview
                                        :display-name (map-elt chat :display-name)
                                        :is-group (map-elt chat :is-group)
                                        :last-updated (map-elt chat :last-updated))
                                       (lambda ()
                                         (interactive)
                                         (wasabi--send-chat-history-request
                                          :chat-jid (map-elt chat :chat-jid)
                                          :contact-name (map-elt chat :display-name)))))
                                    chats)))
                             (concat (propertize date-label 'face 'bold)
                                     "\n\n"
                                     (mapconcat #'identity chat-lines "\n"))))
                         (wasabi--group-chats-by-date chats-index))))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "\n")
            (insert (mapconcat #'identity sections "\n\n")))
          ;; Restore point position
          (goto-char (point-min))
          (forward-line (1- saved-line))
          (move-to-column saved-col))))
     ((map-elt status :message)
      ;; Not ready yet, display centered message.
      (wasabi--message :text (map-elt status :message))))))

(defun wasabi--state ()
  "Get shell state or fail in an incompatible buffer."
  (unless (derived-mode-p 'wasabi-mode)
    (error "No access outside wasabi-mode: %s" major-mode))
  (unless wasabi--state
    (error "No wasabi-mode state available"))
  wasabi--state)

(defun wasabi--clean-up ()
  "Clean up resources.

For example, shut down wuzapi process."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (when (map-elt (wasabi--state) :client)
    (acp-shutdown :client (map-elt (wasabi--state) :client))))

;; Protocol parsing functions

(defun wasabi--parse-contact (p-contact)
  "Parse protocol contact to internal format.
P-CONTACT is an alist from user.contacts response.
Returns alist with :full-name and :push-name.
Empty strings are converted to nil for easier fallback logic."
  (let ((full-name (map-elt p-contact 'FullName))
        (push-name (map-elt p-contact 'PushName)))
    `((:full-name . ,(and full-name (not (string-empty-p full-name)) full-name))
      (:push-name . ,(and push-name (not (string-empty-p push-name)) push-name)))))

(defun wasabi--parse-contacts (p-contacts)
  "Parse contacts response to alist of (jid . contact-info).
P-CONTACTS is the response from user.contacts.
Returns alist: ((jid1 . contact1) (jid2 . contact2) ...)"
  (mapcar (lambda (p-contact-entry)
            (let ((jid (car p-contact-entry))
                  (p-contact-data (cdr p-contact-entry)))
              (cons jid (wasabi--parse-contact p-contact-data))))
          p-contacts))

(defun wasabi--parse-group (p-group)
  "Parse protocol group to internal format.
P-GROUP is an alist from group.list response.
Returns alist with :name.
Empty strings are converted to nil for easier fallback logic."
  (let ((name (map-elt p-group 'Name)))
    `((:name . ,(and name (not (string-empty-p name)) name)))))

(defun wasabi--parse-groups (p-groups)
  "Parse groups response to alist of (jid . group-info).
P-GROUPS is the response from group.list (vector or list of groups).
Returns alist: ((jid1 . group1) (jid2 . group2) ...)"
  (mapcar (lambda (p-group)
            ;; TODO: Do we need symbols here? Why not keep as string?
            (let ((jid (intern (map-elt p-group 'JID))))
              (cons jid (wasabi--parse-group p-group))))
          (append p-groups nil)))

(defun wasabi--parse-chat-index-entry (p-chat-entry contacts groups &optional chats)
  "Parse protocol chat index entry with name enrichment.
P-CHAT-ENTRY is protocol chat entry:
 (chat-jid . ((chat_jid . ...) (last_updated . ...))).
CONTACTS is internal contacts alist (already parsed).
GROUPS is internal groups alist (already parsed).
CHATS is the :chats alist of already fetched histories.
Returns alist with :chat-jid, :canonical-jid, :alt-jids, :display-name,
:named, :is-group and :last-updated."
  (let* ((chat-jid (wasabi--jid-string (car p-chat-entry)))
         (p-metadata (cdr p-chat-entry))
         (last-updated (or (wasabi--latest-message-timestamp chat-jid chats)
                           (map-elt p-metadata 'last_updated)))
         (is-group (wasabi--group-jid-p chat-jid))
         (name (if is-group
                   ;; TODO: Do we need symbols here? Why not keep as string?
                   (map-nested-elt groups (list (intern (wasabi--normalize-jid chat-jid))
                                                :name))
                 ;; For contacts, look up in contacts list.  The lookup goes
                 ;; through every known JID variant: the contact list is
                 ;; keyed by whichever addressing WhatsApp stored, which is
                 ;; often not the one the chat index uses.
                 (wasabi--contact-display-name chat-jid contacts)))
         (identifier (wasabi--jid-identifier chat-jid))
         (display-name (or name identifier chat-jid)))
    ;; Built with `list' and `cons' rather than a backquote: the
    ;; :alt-jids cell is mutated when entries merge, and backquote is
    ;; free to share its constant sub-structure between calls.
    (list (cons :chat-jid chat-jid)
          (cons :canonical-jid (wasabi--canonical-jid chat-jid))
          (cons :alt-jids nil)
          (cons :display-name display-name)
          (cons :named (and name t))
          (cons :is-group (and is-group t))
          (cons :last-updated last-updated))))

(defun wasabi--remember-chat-time (chat-jid timestamp)
  "Record TIMESTAMP as when CHAT-JID last saw a message.

Only if it is more recent than what we had: a chat is dated by its
newest message, and messages do not arrive in order.  Cached on disk,
so the dates survive a restart without reopening every chat."
  (when (and chat-jid timestamp (wasabi--parse-timestamp timestamp))
    (let ((jid (wasabi--canonical-jid chat-jid)))
      (when (wasabi--timestamp-newer-p
             timestamp (gethash jid wasabi--chat-times-table))
        (puthash jid timestamp wasabi--chat-times-table)
        (setq wasabi--jid-aliases-dirty t)))))

(defun wasabi--latest-message-timestamp (chat-jid times)
  "Return when CHAT-JID last saw a message, per TIMES, or nil.

Neither the chat index nor wuzapi's message rows can say: both record
when wuzapi wrote them, which after a sync is the moment everything was
backfilled.  The real time is inside each message, so it is taken from
there as histories load and remembered here."
  (when (and chat-jid times)
    ;; A hash table in use, an alist under test: `map-elt' takes either.
    (map-elt times (wasabi--canonical-jid chat-jid))))

(defun wasabi--reparse-chat-index ()
  "Read the stored chat index again and refresh the display.

Names and times improve as contacts arrive and histories load, so the
index is read again rather than refetched."
  (when-let ((p-chat-index (map-elt (wasabi--state) :p-chat-index)))
    (let ((chats-index (wasabi--parse-chat-index
                        p-chat-index
                        (map-elt (wasabi--state) :contacts)
                        (map-elt (wasabi--state) :groups)
                        wasabi--chat-times-table)))
      (map-put! (wasabi--state) :chats-index chats-index)
      (wasabi--log "Loaded chat index: %d chats (%d dated by their messages)"
                   (length chats-index)
                   (seq-count (lambda (chat)
                                (wasabi--latest-message-timestamp
                                 (map-elt chat :chat-jid)
                                 wasabi--chat-times-table))
                              chats-index))
      (wasabi--refresh))))

(defun wasabi--merge-chat-index-entries (entries)
  "Merge ENTRIES that address the same peer under different JIDs.

ENTRIES must be ordered most recently updated first.  A merged entry
keeps the most recent JID, which is where new messages land, plus the
best display name either entry could resolve.  Superseded JIDs are kept
in :alt-jids.

History recorded under a superseded JID is not folded in: wuzapi stores
it under that JID and we fetch one history per chat."
  (let ((merged '()))
    (dolist (entry entries)
      (let ((existing (seq-find (lambda (candidate)
                                  (equal (map-elt candidate :canonical-jid)
                                         (map-elt entry :canonical-jid)))
                                merged)))
        (if (not existing)
            (push (copy-alist entry) merged)
          (map-put! existing :alt-jids
                    (append (map-elt existing :alt-jids)
                            (list (map-elt entry :chat-jid))
                            (map-elt entry :alt-jids)))
          ;; Keep whichever JID managed to resolve a real name.
          (when (and (not (map-elt existing :named))
                     (map-elt entry :named))
            (map-put! existing :display-name (map-elt entry :display-name))
            (map-put! existing :named t)))))
    (nreverse merged)))

(defun wasabi--parse-chat-index (p-chat-index contacts groups &optional chats)
  "Parse chat index response to list of internal chat entries.
P-CHAT-INDEX is the raw response from chat.history with chat_jid='index'.
CONTACTS is internal contacts alist (already parsed).
GROUPS is internal groups alist (already parsed).
CHATS is the :chats alist of already fetched histories, used for real
message times.
Returns list of internal chat entry alists, filtered, sorted and merged."
  (let* ((parsed (delq nil
                       (mapcar (lambda (p-entry)
                                 ;; Filter out status broadcasts
                                 (unless (equal (wasabi--jid-string (car p-entry))
                                                "status@broadcast")
                                   (wasabi--parse-chat-index-entry p-entry contacts
                                                                   groups chats)))
                               p-chat-index)))
         (sorted (sort parsed
                       (lambda (a b)
                         (wasabi--timestamp-newer-p (map-elt a :last-updated)
                                                    (map-elt b :last-updated))))))
    (wasabi--merge-chat-index-entries sorted)))

;; Protocol request builders

(cl-defun wasabi--make-session-connect-request (&key token subscribe immediate)
  "Instantiate a \"session.connect\" request.

  Required parameters:
    TOKEN - User authentication token

  Optional parameters:
    SUBSCRIBE - List of event types to subscribe to during connection
    IMMEDIATE - Whether to immediately trigger connection process

  Initiates a WhatsApp connection for the user, preparing for QR code
  scanning or device pairing."
  (unless token
    (error ":token is required"))
  (let ((params `((token . ,token))))
    (when subscribe
      (push `(subscribe . ,(vconcat subscribe)) params))
    (when immediate
      (push `(immediate . ,immediate) params))
    `((:method . "session.connect")
      (:params . ,params))))

(cl-defun wasabi--make-user-contacts-request (&key token)
  "Instantiate a \"user.contacts\" request to get all contacts.

Requires user TOKEN."
  (unless token (error ":token is required"))
  `((:method . "user.contacts")
    (:params . ((token . ,token)))))

(cl-defun wasabi--make-session-history-set-request (&key token history days)
  "Instantiate a \"session.history.set\" request.

  Required parameters:
    TOKEN - User authentication token

  Optional parameters:
    HISTORY - How many messages to keep per chat
    DAYS - How many days of history to ask WhatsApp for on pairing

  HISTORY takes effect at once, and is what wuzapi trims a chat down to
  on every message sent or received.  DAYS applies at the next pairing
  and not before.

  See: stdio.go (session.history.set), handlers.go (SetHistory)"
  (unless token
    (error ":token is required"))
  (unless (or history days)
    (error "Either :history or :days is required"))
  (let ((params `((token . ,token))))
    (when history
      (setq params (append params `((history . ,history)))))
    (when days
      (setq params (append params `((days_to_sync_history . ,days)))))
    `((:method . "session.history.set")
      (:params . ,params))))

(cl-defun wasabi--send-history-config-request (&key on-finished)
  "Keep wuzapi's history settings in step with ours.

Existing accounts were created asking to keep 100 messages a chat, and
wuzapi deletes down to that on every message.  Putting it right costs
one request at startup and saves whatever has not been trimmed yet."
  (acp-send-request
   :client (map-elt (wasabi--state) :client)
   :request (wasabi--make-session-history-set-request
             :token wasabi-user-token
             :history wasabi-message-history-limit
             :days wasabi-history-sync-days)
   :on-success (lambda (response)
                 (wasabi--log "History configured: keep %s per chat, sync %s days"
                              (map-nested-elt response '(data History))
                              (map-nested-elt response '(data days_to_sync_history)))
                 (when on-finished (funcall on-finished)))
   :on-failure (lambda (error)
                 ;; Not fatal: an older wuzapi may not know the method, and
                 ;; a smaller history still works, just with less of it.
                 (wasabi--log "Couldn't configure history: %s"
                              (or (map-elt error 'message) "unknown"))
                 (when on-finished (funcall on-finished)))))

(cl-defun wasabi--make-user-check-request (&key token phones)
  "Instantiate a \"user.check\" request.

  Required parameters:
    TOKEN - User authentication token
    PHONES - List of phone numbers, with country code and no \"+\"

  Asks WhatsApp which of PHONES are on WhatsApp.  Each answer carries
  the JID that number belongs to, which since the move to linked
  identities is that contact's LID.  That is the one place the pairing
  between a phone number and a LID can be had: messages report it only
  as they arrive live, and never on history."
  (unless token
    (error ":token is required"))
  (unless phones
    (error ":phones is required"))
  `((:method . "user.check")
    (:params . ((token . ,token)
                (Phone . ,(vconcat phones))))))

(cl-defun wasabi--make-group-list-request (&key token)
  "Instantiate a \"group.list\" request to get all groups.

Requires user TOKEN."
  (unless token (error ":token is required"))
  `((:method . "group.list")
    (:params . ((token . ,token)))))

(cl-defun wasabi--make-chat-history-request (&key token chat-jid limit)
  "Instantiate a \"chat.history\" request.

  Required parameters:
    TOKEN - User authentication token
    CHAT-JID - Chat JID identifying the conversation:
               - For contacts: \"1234567890@s.whatsapp.net\"
               - For groups: \"groupid@g.us\"
               - Special value: \"index\" returns mapping of all chats

  Optional parameters:
    LIMIT - How many rows to return.  Sent for a conversation; the
            index ignores it.

  Retrieves message history for a specific chat. History must be
  enabled when creating the user account.

  Worth knowing what LIMIT selects.  wuzapi stores time.Now() in each
  row's timestamp column, so it records when the row was written and
  never when the message was sent, and this query is
  \"ORDER BY timestamp DESC LIMIT n\".  So it returns the rows written
  most recently, which during a history sync bears no relation to the
  order the conversation happened in, and which puts anything just sent
  from here at the front.  Asking for a small number therefore returns
  an arbitrary slice of the chat, not its recent end.  The real times
  are inside each message, so wasabi asks for plenty and sorts them
  itself.

  See: stdio.go:199, handlers.go (GetHistory), db.go
  (saveMessageToHistory)"
  (unless token
    (error ":token is required"))
  (unless chat-jid
    (error ":chat-jid is required"))
  (let ((params `((token . ,token)
                  (chat_jid . ,chat-jid))))
    (when limit
      (setq params (append params `((limit . ,limit)))))
    `((:method . "chat.history")
      (:params . ,params))))

(cl-defun wasabi--make-chat-clear-request (&key token chat-jid)
  "Instantiate a \"chat.clear\" request.

  Required parameters:
    TOKEN - User authentication token
    CHAT-JID - Chat JID identifying the conversation to clear

  Clears the local message history for a specific chat in the database.
  This only affects the local wuzapi database, not the actual WhatsApp chat."
  (unless token
    (error ":token is required"))
  (unless chat-jid
    (error ":chat-jid is required"))
  `((:method . "chat.clear")
    (:params . ((token . ,token)
                (chat_jid . ,chat-jid)))))

(cl-defun wasabi--make-admin-users-list-request (&key admin-token)
  "Instantiate an \"admin.users.list\" request.

  Required parameters:
    ADMIN-TOKEN - Admin authentication token

  Returns array of all user accounts with their details including
  connection status, JID, webhook configuration, and event subscriptions.

  See: stdio.go:159, handlers.go (listUsersHandler)"
  (unless admin-token
    (error ":admin-token is required"))
  `((:method . "admin.users.list")
    (:params . ((adminToken . ,admin-token)))))

(cl-defun wasabi--make-admin-add-user-request (&key admin-token
                                                    name
                                                    token
                                                    events
                                                    hmac-key
                                                    expiration
                                                    history
                                                    days-to-sync-history
                                                    proxy-config)
  "Instantiate an \"admin.users.add\" request.

Required parameters:
  ADMIN-TOKEN - Admin authentication token
  NAME - User display name
  TOKEN - User authentication token

Optional parameters:
  EVENTS - Event subscriptions (list or comma-separated string)
  HISTORY - Number of historical messages to sync
  HMAC-KEY - HMAC key for message authentication
  EXPIRATION - Token expiration time
  PROXY-CONFIG - Proxy configuration"
  (unless admin-token (error ":admin-token is required"))
  (unless name (error ":name is required"))
  (unless token (error ":token is required"))
  `((:method . "admin.users.add")
    (:params . ,(append `((adminToken . ,admin-token)
                          (name . ,name)
                          (token . ,token)
                          (events . ,(and events
                                          (if (listp events)
                                              (mapconcat #'identity events ",")
                                            events)))
                          (history . ,history)
                          (days_to_sync_history . ,days-to-sync-history)
                          (proxyConfig . ,proxy-config))
                        (when expiration
                          `((expiration . ,expiration)))
                        (when hmac-key
                          `((hmacKey . ,hmac-key)))))))

(cl-defun wasabi--make-session-status-request (&key token)
  "Instantiate a \"session.status\" request.

  Required parameters:
    TOKEN - User authentication token

  Returns connection and login status including connected state,
  logged in state, JID, name, and configuration status.

  See: stdio.go:186, handlers.go (statusHandler)"
  (unless token
    (error ":token is required"))
  `((:method . "session.status")
    (:params . ((token . ,token)))))

(cl-defun wasabi--make-session-pairphone-request (&key token phone)
  "Instantiate a \"session.pairphone\" request.

  Required parameters:
    TOKEN - User authentication token
    PHONE - Phone number with country code (e.g., \"+1234567890\")

  Returns an 8-character linking code that the user enters in their
  WhatsApp mobile app to pair the device. This is an alternative to
  QR code scanning, useful for accessibility or TTY environments.

  The session must be connected but not logged in for this to work.

  See: stdio.go:260, handlers.go (PairPhone)"
  (unless token
    (error ":token is required"))
  (unless phone
    (error ":phone is required"))
  `((:method . "session.pairphone")
    (:params . ((token . ,token)
                (Phone . ,phone)))))

(cl-defun wasabi--make-chat-send-text-request (&key token
                                                    phone
                                                    body
                                                    link-preview
                                                    id
                                                    context-info
                                                    quoted-text)
  "Instantiate a \"chat.send.text\" request.

  Required parameters:
    TOKEN - User authentication token
    PHONE - Phone number with country code or group JID
    BODY - Message text content

  Optional parameters:
    LINK-PREVIEW - Enable link preview for URLs (boolean)
    ID - Custom message ID (string, auto-generated if not provided)
    CONTEXT-INFO - Context object with :stanza-id and :participant
    QUOTED-TEXT - Text to quote from another message

  Sends a text message to a WhatsApp contact or group. Requires
  an active WhatsApp session (connected and logged in).

  See: stdio.go:196, handlers.go (sendHandler)"
  (unless token
    (error ":token is required"))
  (unless phone
    (error ":phone is required"))
  (unless body
    (error ":body is required"))
  (let ((params `((token . ,token)
                  (Phone . ,phone)
                  (Body . ,body))))
    (when link-preview
      (push `(LinkPreview . ,link-preview) params))
    (when id
      (push `(Id . ,id) params))
    (when context-info
      (push `(ContextInfo . ,context-info) params))
    (when quoted-text
      (push `(QuotedText . ,quoted-text) params))
    `((:method . "chat.send.text")
      (:params . ,params))))

(cl-defun wasabi--make-chat-send-image-request (&key token phone image caption)
  "Instantiate a \"chat.send.image\" request.

  Required parameters:
    TOKEN - User authentication token
    PHONE - Phone number with country code or group JID
    IMAGE - The image as a data URL, \"data:image/png;base64,...\"

  Optional parameters:
    CAPTION - Text shown beneath the image

  wuzapi uploads the image to WhatsApp and builds its thumbnail
  itself, which means decoding it: only JPEG, PNG and GIF will do.

  See: stdio.go (chat.send.image), handlers.go (SendImage)"
  (unless token
    (error ":token is required"))
  (unless phone
    (error ":phone is required"))
  (unless image
    (error ":image is required"))
  (let ((params `((token . ,token)
                  (Phone . ,phone)
                  (Image . ,image))))
    (when (and caption (not (string-empty-p caption)))
      (setq params (append params `((Caption . ,caption)))))
    `((:method . "chat.send.image")
      (:params . ,params))))

(cl-defun wasabi--make-chat-markread-request (&key token chat sender ids)
  "Instantiate a \"chat.markread\" request.

  Required parameters:
    TOKEN - User authentication token
    CHAT - JID of the chat the messages are in
    IDS - List of message IDs to mark as read

  Optional parameters:
    SENDER - JID of who sent them.  Required in a group: whatsmeow
             can only mark one sender's messages per request there.

  Sends read receipts, so the sender sees the messages as read.

  See: stdio.go (chat.markread), handlers.go (MarkRead)"
  (unless token
    (error ":token is required"))
  (unless chat
    (error ":chat is required"))
  (unless ids
    (error ":ids is required"))
  (let ((params `((token . ,token)
                  (Id . ,(vconcat ids))
                  (ChatPhone . ,chat))))
    (when sender
      (setq params (append params `((SenderPhone . ,sender)))))
    `((:method . "chat.markread")
      (:params . ,params))))

(cl-defun wasabi--make-download-image-request (&key token
                                                    url
                                                    direct-path
                                                    media-key
                                                    mimetype
                                                    file-enc-sha256
                                                    file-sha256
                                                    file-length)
  "Instantiate a \"chat.download.image\" request.

  Required parameters:
    TOKEN - User authentication token
    URL - WhatsApp media URL
    DIRECT-PATH - WhatsApp direct path to encrypted file
    MEDIA-KEY - Base64 encryption key for decrypting the image
    MIMETYPE - Image MIME type (e.g., \"image/jpeg\")
    FILE-ENC-SHA256 - SHA256 hash of encrypted file
    FILE-SHA256 - SHA256 hash of decrypted file
    FILE-LENGTH - File size in bytes

  Downloads and decrypts an image from WhatsApp servers. Returns
  a data URL with the decrypted image data.

  See: stdio.go:248, handlers.go (DownloadImage)"
  (unless token
    (error ":token is required"))
  (unless url
    (error ":url is required"))
  `((:method . "chat.download.image")
    (:params . ((token . ,token)
                (Url . ,url)
                (DirectPath . ,direct-path)
                (MediaKey . ,media-key)
                (Mimetype . ,mimetype)
                (FileEncSHA256 . ,file-enc-sha256)
                (FileSHA256 . ,file-sha256)
                (FileLength . ,file-length)))))

(cl-defun wasabi--make-download-video-request (&key token
                                                    url
                                                    direct-path
                                                    media-key
                                                    mimetype
                                                    file-enc-sha256
                                                    file-sha256
                                                    file-length)
  "Instantiate a \"chat.download.video\" request.

  Required parameters:
    TOKEN - User authentication token
    URL - WhatsApp media URL
    DIRECT-PATH - WhatsApp direct path to encrypted file
    MEDIA-KEY - Base64 encryption key for decrypting the video
    MIMETYPE - Video MIME type (e.g., \"video/mp4\")
    FILE-ENC-SHA256 - SHA256 hash of encrypted file
    FILE-SHA256 - SHA256 hash of decrypted file
    FILE-LENGTH - File size in bytes

  Downloads and decrypts a video from WhatsApp servers. Returns
  a data URL with the decrypted video data.

  See: stdio.go:251, handlers.go (DownloadVideo)"
  (unless token
    (error ":token is required"))
  (unless url
    (error ":url is required"))
  `((:method . "chat.download.video")
    (:params . ((token . ,token)
                (Url . ,url)
                (DirectPath . ,direct-path)
                (MediaKey . ,media-key)
                (Mimetype . ,mimetype)
                (FileEncSHA256 . ,file-enc-sha256)
                (FileSHA256 . ,file-sha256)
                (FileLength . ,file-length)))))

(defun wasabi--face-height-pixels (face)
  "Get the approximate pixel height of FACE."
  (let* ((height-attr (face-attribute face :height nil 'default))
         (height-points (cond
                         ((integerp height-attr) (/ height-attr 10.0))
                         ((floatp height-attr) (* height-attr 12.0)) ; assume 12pt default
                         (t 12.0)))) ; fallback to 12pt
    ;; Convert points to pixels (assuming 96 DPI)
    (round (* height-points 96.0 (/ 1.0 72.0)))))

;;; Diagnostics

(defun wasabi--jid-server (jid)
  "Return the server part of JID, the bit after the \"@\"."
  (when-let ((jid (wasabi--jid-string jid)))
    (if (string-match "@\\(.+\\)\\'" jid)
        (concat "@" (match-string 1 jid))
      "(none)")))

(defun wasabi--tally (items key-function)
  "Tally ITEMS by KEY-FUNCTION, most common first.
Returns a string like \"@s.whatsapp.net 200, @lid 150\"."
  (let ((counts (make-hash-table :test 'equal))
        (tallied '()))
    (dolist (item items)
      (let ((key (or (funcall key-function item) "(none)")))
        (puthash key (1+ (gethash key counts 0)) counts)))
    (maphash (lambda (key count) (push (cons key count) tallied)) counts)
    (if (null tallied)
        "none"
      (mapconcat (lambda (entry) (format "%s %d" (car entry) (cdr entry)))
                 (sort tallied (lambda (a b) (> (cdr a) (cdr b))))
                 ", "))))

(defun wasabi--message-info (p-message)
  "Return the Info of P-MESSAGE, parsed out of its data_json, or nil."
  (when-let ((data-json (map-elt p-message 'data_json))
             ((stringp data-json))
             ((not (string-empty-p data-json))))
    (ignore-errors
      (map-elt (json-parse-string data-json :object-type 'alist
                                  :null-object nil :false-object nil)
               'Info))))

(defun wasabi--filled-p (value)
  "Return non-nil when VALUE is something rather than an empty nothing.

A protocol field can be present and still say nothing: whatsmeow writes
an empty string where it has no JID or name to give."
  (and value
       (not (equal value ""))
       (not (equal value :null))))

(defun wasabi--insert-info-report (messages)
  "Report what the Info of MESSAGES carries, into the current buffer.

Whether a conversation's LID and phone number addressing can ever be
paired up turns on SenderAlt and RecipientAlt carrying a JID, and
naming its sender turns on PushName carrying a name.  Counted by what
they hold, not by whether the field is there: it always is."
  (let ((infos (delq nil (mapcar #'wasabi--message-info messages))))
    (insert (format "    with data_json: %d of %d\n"
                    (length infos) (length messages)))
    (when infos
      (insert (format "    of %d Info: SenderAlt %d, RecipientAlt %d, PushName %d\n"
                      (length infos)
                      (seq-count (lambda (info)
                                   (wasabi--filled-p (map-elt info 'SenderAlt)))
                                 infos)
                      (seq-count (lambda (info)
                                   (wasabi--filled-p (map-elt info 'RecipientAlt)))
                                 infos)
                      (seq-count (lambda (info)
                                   (wasabi--filled-p (map-elt info 'PushName)))
                                 infos)))
      (insert (format "    incoming: %d   IsFromMe: %d\n"
                      (seq-count (lambda (info)
                                   (not (map-elt info 'IsFromMe)))
                                 infos)
                      (seq-count (lambda (info) (map-elt info 'IsFromMe)) infos)))
      (insert (format "    Info.Chat keyed by: %s\n"
                      (wasabi--tally infos
                                     (lambda (info)
                                       (wasabi--jid-server
                                        (map-elt info 'Chat))))))
      (insert (format "    Info.Sender keyed by: %s\n"
                      (wasabi--tally infos
                                     (lambda (info)
                                       (wasabi--jid-server
                                        (map-elt info 'Sender))))))
      (insert (format "    Info.SenderAlt: %s\n"
                      (wasabi--tally infos
                                     (lambda (info)
                                       (let ((alt (map-elt info 'SenderAlt)))
                                         (if (wasabi--filled-p alt)
                                             (wasabi--jid-server alt)
                                           (format "empty %S" alt)))))))
      (insert (format "    addressing modes: %s\n"
                      (wasabi--tally infos
                                     (lambda (info)
                                       (format "%S" (map-elt info
                                                             'AddressingMode)))))))))

(defun wasabi--describe-timestamp (timestamp)
  "Describe how TIMESTAMP reads, for a diagnostics report."
  (let ((parsed (wasabi--parse-timestamp timestamp)))
    (format "%S (%s) -> %s"
            timestamp
            (type-of timestamp)
            (if parsed
                (format-time-string "%Y-%m-%dT%H:%M:%S%z" parsed)
              "UNREADABLE"))))

;;;###autoload
(defun wasabi-diagnose ()
  "Report what `wasabi' knows, for working out what it is getting wrong.

Shows counts, shapes and timestamps rather than contents: no names,
phone numbers or message text, so the report is safe to paste into a
bug report."
  (interactive)
  (let* ((state (buffer-local-value 'wasabi--state (wasabi--buffer)))
         (contacts (map-elt state :contacts))
         (index (map-elt state :chats-index))
         (p-index (map-elt state :p-chat-index))
         (chats (map-elt state :chats))
         (buffer (get-buffer-create "*Wasabi Diagnostics*")))
    (unless state
      (user-error "Wasabi has no state yet.  Start it with M-x wasabi"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Wasabi diagnostics\n")
        (insert "==================\n\n")
        (insert (format "emacs %s, image types: %s
"
                        emacs-version
                        (mapconcat #'symbol-name
                                   (seq-filter #'image-type-available-p
                                               '(svg png jpeg gif webp))
                                   " ")))
        (insert (format "status: %s   connected: %s   syncing: %s\n"
                        (map-nested-elt state '(:status :type))
                        (if (map-elt state :connected) "yes" "no")
                        (or (map-elt state :syncing) "no")))
        (insert (format "JID pairings learned: %d   push names learned: %d\n\n"
                        (hash-table-count wasabi--jid-canonical-table)
                        (hash-table-count wasabi--push-names-table)))

        (insert (format "Contacts: %d\n" (length contacts)))
        (insert (format "  with a saved name: %d\n"
                        (seq-count (lambda (contact)
                                     (map-elt (cdr contact) :full-name))
                                   contacts)))
        (insert (format "  with a push name:  %d\n"
                        (seq-count (lambda (contact)
                                     (map-elt (cdr contact) :push-name))
                                   contacts)))
        (insert (format "  keyed by:          %s\n\n"
                        (wasabi--tally contacts
                                       (lambda (contact)
                                         (wasabi--jid-server (car contact))))))

        (insert (format "Chat index: %d\n" (length index)))
        (insert (format "  named:             %d\n"
                        (seq-count (lambda (chat) (map-elt chat :named)) index)))
        (insert (format "  named with a ~:    %d\n"
                        (seq-count (lambda (chat)
                                     (string-prefix-p
                                      "~" (or (map-elt chat :display-name) "")))
                                   index)))
        (insert (format "  showing a raw JID: %d\n"
                        (seq-count (lambda (chat)
                                     (not (map-elt chat :named)))
                                   index)))
        (insert (format "  merged rows:       %d\n"
                        (seq-count (lambda (chat) (map-elt chat :alt-jids))
                                   index)))
        (insert (format "  keyed by:          %s\n"
                        (wasabi--tally index
                                       (lambda (chat)
                                         (wasabi--jid-server
                                          (map-elt chat :chat-jid))))))
        (insert (format "  dated by messages: %d\n"
                        (seq-count (lambda (chat)
                                     (wasabi--latest-message-timestamp
                                      (map-elt chat :chat-jid)
                                      wasabi--chat-times-table))
                                   index)))
        ;; One distinct value means every row was written by the same
        ;; sync, which is why they all claim the same time.
        (insert (format "  distinct last_updated values: %d of %d rows\n"
                        (length (seq-uniq (mapcar (lambda (entry)
                                                    (map-elt (cdr entry)
                                                             'last_updated))
                                                  p-index)))
                        (length p-index)))
        (when p-index
          (insert (format "  sample last_updated: %s\n"
                          (wasabi--describe-timestamp
                           (map-elt (cdr (car p-index)) 'last_updated)))))
        (insert "\n")

        (insert (format "Loaded histories: %d\n" (length chats)))
        (if (null chats)
            (insert "  (open a chat first: timestamps are read from its messages)\n")
          (dolist (chat (seq-take chats 3))
            (let* ((messages (append (cdr chat) nil))
                   (message (car messages)))
              (insert (format "  %s: %d messages\n"
                              (wasabi--jid-server (car chat))
                              (length messages)))
              (insert (format "    keys: %s\n"
                              (mapconcat #'symbol-name (mapcar #'car message)
                                         " ")))
              (insert (format "    timestamp: %s\n"
                              (wasabi--describe-timestamp
                               (or (map-elt message 'timestamp)
                                   (map-elt message 'Timestamp)
                                   (map-elt message 'message_timestamp)))))
              (insert (format "    unreadable timestamps: %d of %d\n"
                              (seq-count (lambda (p-message)
                                           (not (wasabi--parse-timestamp
                                                 (or (map-elt p-message 'timestamp)
                                                     (map-elt p-message 'Timestamp)
                                                     (map-elt p-message
                                                              'message_timestamp)))))
                                         messages)
                              (length messages)))
              (wasabi--insert-info-report messages)
              (insert (format "    newest: %s\n"
                              (or (wasabi--latest-message-timestamp
                                   (car chat) wasabi--chat-times-table)
                                  "not recorded"))))))
        (goto-char (point-min))
        (special-mode)))
    (switch-to-buffer buffer)))

(provide 'wasabi)

;;; wasabi.el ends here
