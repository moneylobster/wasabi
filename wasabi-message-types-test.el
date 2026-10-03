;;; wasabi-message-types-test.el --- Tests for the less common message types  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-message-types-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The messages here have the shapes wuzapi stores, values aside.

;;; Code:

(require 'ert)
(require 'wasabi)

(defun wasabi-message-types-test--content (p-message)
  "The text P-MESSAGE parses to, without its properties."
  (substring-no-properties (wasabi-chat--parse-content p-message)))

;;; Albums

(ert-deftest wasabi-message-types-test-album-photos-are-images ()
  ;; Each photo in an album comes wrapped; inside, it is an ordinary image.
  (let ((content (wasabi-chat--parse-content
                  '((associatedChildMessage
                     . ((message . ((imageMessage . ((URL . "https://mmg/photo")
                                                     (mimetype . "image/jpeg")
                                                     (caption . "the view")))))))))))
    (should (string-prefix-p "[image]" (substring-no-properties content)))
    (should (string-match-p "the view" content))
    (should (equal (get-text-property 0 'image-url content) "https://mmg/photo")))
  (should (equal (wasabi-chat--summarize
                  '((associatedChildMessage
                     . ((message . ((videoMessage . ((caption . "jump")))))))))
                 "[video] jump")))

(ert-deftest wasabi-message-types-test-album-announced ()
  (should (equal (wasabi-message-types-test--content
                  '((albumMessage . ((expectedImageCount . 2) (expectedVideoCount . 0)))))
                 "[album: 2 photos]"))
  (should (equal (wasabi-message-types-test--content
                  '((albumMessage . ((expectedImageCount . 1) (expectedVideoCount . 3)))))
                 "[album: 1 photo, 3 videos]")))

(ert-deftest wasabi-message-types-test-meta-ai-text ()
  (should (equal (wasabi-message-types-test--content
                  '((botInvokeMessage
                     . ((message . ((extendedTextMessage . ((text . "hello there")))))))))
                 "hello there")))

;;; Labels

(ert-deftest wasabi-message-types-test-contacts ()
  (should (equal (wasabi-message-types-test--content
                  '((contactMessage . ((displayName . "Ayşe") (vcard . "BEGIN:VCARD")))))
                 "[contact: Ayşe]"))
  (should (equal (wasabi-message-types-test--content
                  '((contactsArrayMessage
                     . ((displayName . "2 contacts")
                        (contacts . [((displayName . "Ayşe")) ((displayName . "Mehmet"))])))))
                 "[contacts: Ayşe, Mehmet]")))

(ert-deftest wasabi-message-types-test-locations-link-to-a-map ()
  (should (equal (wasabi-message-types-test--content
                  '((locationMessage . ((degreesLatitude . 39.854443)
                                        (degreesLongitude . 32.6760177)))))
                 "[location] https://maps.google.com/?q=39.854443,32.6760177"))
  (should (string-prefix-p "[location: Kızılay] https://"
                           (wasabi-message-types-test--content
                            '((locationMessage . ((degreesLatitude . 39.9)
                                                  (degreesLongitude . 32.8)
                                                  (name . "Kızılay")))))))
  (should (string-prefix-p "[live location] https://"
                           (wasabi-message-types-test--content
                            '((liveLocationMessage . ((degreesLatitude . 39.9)
                                                      (degreesLongitude . 32.8)))))))
  ;; Clickable once drawn.
  (should (get-text-property
           (string-match "https" (wasabi-chat--linkify "[location] https://maps.google.com/?q=1,2"))
           'wasabi-url
           (wasabi-chat--linkify "[location] https://maps.google.com/?q=1,2"))))

(ert-deftest wasabi-message-types-test-polls ()
  (should (equal (wasabi-message-types-test--content
                  '((pollCreationMessageV3 . ((name . "Dinner?")
                                              (options . [((optionName . "Pizza"))
                                                          ((optionName . "Sushi"))])))))
                 "[poll: Dinner?]\n• Pizza\n• Sushi"))
  (should (equal (wasabi-message-types-test--content '((pollUpdateMessage . ((vote . "x")))))
                 "[poll vote]"))
  ;; A quote keeps it to one line.
  (should (equal (wasabi-chat--summarize
                  '((pollCreationMessageV3 . ((name . "Dinner?")
                                              (options . [((optionName . "Pizza"))])))))
                 "[poll: Dinner?] • Pizza")))

(ert-deftest wasabi-message-types-test-events ()
  (let ((start (format-time-string " %b %-d, %H:%M" (seconds-to-time 1760824800))))
    (should (equal (wasabi-message-types-test--content
                    '((eventMessage . ((name . "Picnic") (isCanceled . nil)
                                       (startTime . 1760824800)))))
                   (concat "[event: Picnic]" start)))
    (should (string-prefix-p "[cancelled event: Picnic]"
                             (wasabi-message-types-test--content
                              '((eventMessage . ((name . "Picnic") (isCanceled . t)
                                                 (startTime . 1760824800)))))))))

(ert-deftest wasabi-message-types-test-the-rest ()
  (should (equal (wasabi-message-types-test--content
                  '((groupInviteMessage . ((groupName . "Hiking") (caption . "join us")))))
                 "[group invite: Hiking]\njoin us"))
  (should (equal (wasabi-message-types-test--content
                  '((stickerPackMessage . ((name . "Cats") (publisher . "someone")))))
                 "[sticker pack: Cats]"))
  (should (equal (wasabi-message-types-test--content
                  '((templateMessage . ((hydratedTemplate . ((hydratedContentText . "Sale!")))))))
                 "[business message: Sale!]"))
  (should (equal (wasabi-message-types-test--content
                  '((interactiveMessage . ((body . ((text . "Pick one")))))))
                 "[business message: Pick one]"))
  ;; Never met: named, so it can be told apart and added.
  (should (equal (wasabi-message-types-test--content '((messageContextInfo . t)
                                                       (newFangledMessage . t)))
                 "[newFangledMessage]")))

;;; Bookkeeping

(ert-deftest wasabi-message-types-test-bookkeeping-is-silent ()
  ;; A disappearing-message timer change, and a group's keys going round.
  (dolist (p-message '(((protocolMessage . ((type . 3))))
                       ((senderKeyDistributionMessage . ((groupID . "g")))
                        (messageContextInfo . t))))
    (should (wasabi-chat--silent-p p-message))
    (should-not (wasabi-chat--parse-notification
                 :p-message p-message
                 :p-info '((ID . "X") (Chat . "1@s.whatsapp.net") (Sender . "1@s.whatsapp.net"))
                 :chat-jid "1@s.whatsapp.net"))
    (should-not (wasabi-chat--parse-message
                 `((message_id . "X")
                   (data_json . ,(json-encode `((Info . ((Sender . "1@s.whatsapp.net")
                                                         (Timestamp . "2026-10-03T12:00:00Z")))
                                                (Message . ,p-message)))))
                 :chat-jid "1@s.whatsapp.net")))
  (should-not (wasabi-chat--silent-p '((conversation . "hi") (messageContextInfo . t)))))

;;; Reactions

(ert-deftest wasabi-message-types-test-one-reaction-each ()
  (let ((reactions (wasabi-chat--react nil "👍" "John")))
    (setq reactions (wasabi-chat--react reactions "❤️" "Mary"))
    ;; John changes his mind: his replaces his, Mary's stays.
    (setq reactions (wasabi-chat--react reactions "😂" "John"))
    (should (equal (mapcar (lambda (r) (cons (map-elt r :sender) (map-elt r :emoji))) reactions)
                   '(("Mary" . "❤️") ("John" . "😂"))))
    ;; Mary takes hers back.
    (setq reactions (wasabi-chat--react reactions "" "Mary"))
    (should (equal (mapcar (lambda (r) (map-elt r :sender)) reactions) '("John")))))

(defun wasabi-message-types-test--stored-reaction (time sender emoji)
  "A stored reaction to message T1, by SENDER at TIME."
  `((message_id . ,(format "R%s" time))
    (data_json . ,(json-encode
                   `((Info . ((Sender . ,sender) (PushName . ,sender)
                              (Timestamp . ,time)))
                     (Message . ((reactionMessage . ((key . ((ID . "T1")))
                                                     (text . ,emoji))))))))))

(ert-deftest wasabi-message-types-test-history-reactions-settle ()
  ;; Stored newest first, as wuzapi returns them.
  (let* ((stored (list (wasabi-message-types-test--stored-reaction
                        "2026-10-03T12:03:00Z" "2@s.whatsapp.net" "")
                       (wasabi-message-types-test--stored-reaction
                        "2026-10-03T12:02:00Z" "1@s.whatsapp.net" "😂")
                       (wasabi-message-types-test--stored-reaction
                        "2026-10-03T12:01:00Z" "2@s.whatsapp.net" "❤️")
                       (wasabi-message-types-test--stored-reaction
                        "2026-10-03T12:00:00Z" "1@s.whatsapp.net" "👍")))
         (reactions (map-elt (wasabi-chat--parse-reactions stored) "T1")))
    ;; One left: the second sender took theirs back, the first changed theirs.
    (should (equal (mapcar (lambda (r) (map-elt r :emoji)) reactions) '("😂")))))

(ert-deftest wasabi-message-types-test-live-reaction-replaces ()
  (let ((buffer (generate-new-buffer "*wasabi-types-test*"))
        (wasabi-send-read-receipts nil))
    (unwind-protect
        (cl-letf (((symbol-function 'recenter) #'ignore))
          (with-current-buffer buffer
            (wasabi-chat-mode)
            (setq wasabi-chat--chat
                  (wasabi-chat--make-chat :chat-jid "1@s.whatsapp.net"))
            (wasabi-chat--refresh
             (list (list (cons :message-id "T1") (cons :sender-name "Me")
                         (cons :timestamp "2026-10-03T12:00:00Z") (cons :content "hi"))))
            (wasabi-chat--add-reaction :target-id "T1" :emoji "👍" :sender "John")
            (wasabi-chat--add-reaction :target-id "T1" :emoji "😂" :sender "John")
            (should (string-match-p "😂 John" (buffer-string)))
            (should-not (string-match-p "👍" (buffer-string)))
            (wasabi-chat--add-reaction :target-id "T1" :emoji "" :sender "John")
            (should-not (string-match-p "John" (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest wasabi-message-types-test-taking-back-is-not-news ()
  (let* ((seen '())
         (wasabi-notifications-enabled t)
         (wasabi-message-notification-function (lambda (message) (push message seen))))
    (wasabi--notify '((:is-reaction . t) (:emoji . "") (:target-id . "T1")))
    (wasabi--notify '((:is-reaction . t) (:emoji . nil) (:target-id . "T1")))
    (should-not seen)
    (wasabi--notify '((:is-reaction . t) (:emoji . "👍") (:target-id . "T1")))
    (should (equal (length seen) 1))))

(provide 'wasabi-message-types-test)
;;; wasabi-message-types-test.el ends here
