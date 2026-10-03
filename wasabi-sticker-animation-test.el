;;; wasabi-sticker-animation-test.el --- Tests for playing animated stickers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-sticker-animation-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Batch Emacs cannot decode images, so whether one has frames is
;; stubbed.  The animation timers are Emacs's own.

;;; Code:

(require 'ert)
(require 'wasabi)

(defun wasabi-sticker-animation-test--image (name)
  "A stand-in image spec called NAME."
  (list 'image :type 'webp :data name))

(defun wasabi-sticker-animation-test--insert (image)
  "Insert a sticker drawn as IMAGE."
  (insert (propertize "[sticker]"
                      'sticker-url (format "https://%s" (plist-get (cdr image) :data))
                      'display image)
          "\n"))

(defmacro wasabi-sticker-animation-test--with (images &rest body)
  "Run BODY in a chat showing stickers IMAGES.
In BODY, `animated' lists the images that have frames."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer "*wasabi-animation-test*"))
         (wasabi-chat-animate-stickers 10)
         (animated ,images))
     (unwind-protect
         (cl-letf (((symbol-function 'image-multi-frame-p)
                    (lambda (image) (and (memq image animated) '(4 . 0.1)))))
           (with-current-buffer buffer
             (wasabi-chat-mode)
             (dolist (image ,images)
               (wasabi-sticker-animation-test--insert image))
             ,@body))
       (with-current-buffer buffer
         (mapc #'cancel-timer (wasabi-chat--animation-timers)))
       (kill-buffer buffer))))

(defun wasabi-sticker-animation-test--playing ()
  "The images playing in this buffer."
  (mapcar (lambda (timer) (car (timer--args timer)))
          (wasabi-chat--animation-timers)))

(defun wasabi-sticker-animation-test--stop-all ()
  "Stop this buffer's animations, as their time running out would."
  (mapc #'cancel-timer (wasabi-chat--animation-timers)))

(ert-deftest wasabi-sticker-animation-test-plays-for-a-while ()
  (let ((a (wasabi-sticker-animation-test--image "a"))
        (b (wasabi-sticker-animation-test--image "b")))
    (wasabi-sticker-animation-test--with (list a b)
      (wasabi-chat--animate-stickers)
      (should (equal (length (wasabi-sticker-animation-test--playing)) 2))
      ;; From its first frame, for the seconds set, not for good.
      (let ((args (timer--args (image-animate-timer a))))
        (should (equal (nth 1 args) 0))
        (should (equal (nth 4 args) 10))))))

(ert-deftest wasabi-sticker-animation-test-plays-once ()
  (let ((a (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list a)
      (wasabi-chat--animate-stickers)
      (wasabi-sticker-animation-test--stop-all)
      ;; The chat drawn again, say for a new message: it has had its turn.
      (wasabi-chat--animate-stickers)
      (should-not (wasabi-sticker-animation-test--playing)))))

(ert-deftest wasabi-sticker-animation-test-new-ones-play ()
  (let ((a (wasabi-sticker-animation-test--image "a"))
        (b (wasabi-sticker-animation-test--image "b")))
    (wasabi-sticker-animation-test--with (list a)
      (wasabi-chat--animate-stickers)
      (wasabi-sticker-animation-test--stop-all)
      ;; A sticker arriving plays; the one before it does not again.
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (wasabi-sticker-animation-test--insert b))
      (setq animated (list a b))
      (wasabi-chat--animate-stickers)
      (should (equal (wasabi-sticker-animation-test--playing) (list b))))))

(ert-deftest wasabi-sticker-animation-test-same-sticker-twice ()
  ;; Sent twice, and drawn as two images that are `equal' but not `eq':
  ;; both play.
  (let ((first (wasabi-sticker-animation-test--image "a"))
        (again (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list first again)
      (wasabi-chat--animate-stickers)
      (should (equal (length (wasabi-sticker-animation-test--playing)) 2)))))

(ert-deftest wasabi-sticker-animation-test-stills-stay-still ()
  (let ((a (wasabi-sticker-animation-test--image "a"))
        (still (wasabi-sticker-animation-test--image "still")))
    (wasabi-sticker-animation-test--with (list a still)
      (setq animated (list a))
      (wasabi-chat--animate-stickers)
      (should (equal (wasabi-sticker-animation-test--playing) (list a))))))

(ert-deftest wasabi-sticker-animation-test-drawn-over-stops ()
  (let ((old (wasabi-sticker-animation-test--image "a"))
        (new (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list old)
      (wasabi-chat--animate-stickers)
      (should (equal (length (wasabi-sticker-animation-test--playing)) 1))
      ;; The same sticker drawn again, as a new image, which plays afresh.
      (let ((inhibit-read-only t))
        (put-text-property (point-min) (+ (point-min) 9) 'display new))
      (setq animated (list old new))
      (wasabi-chat--animate-stickers)
      (should (equal (wasabi-sticker-animation-test--playing) (list new))))))

(ert-deftest wasabi-sticker-animation-test-toggle ()
  (let ((a (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list a)
      (wasabi-chat--animate-stickers)
      (should (wasabi-sticker-animation-test--playing))
      (wasabi-chat-toggle-sticker-animation)
      (should-not wasabi-chat-animate-stickers)
      (should-not (wasabi-sticker-animation-test--playing))
      ;; Back on: they play again, for as long as before.
      (wasabi-chat-toggle-sticker-animation)
      (should (equal wasabi-chat-animate-stickers 10))
      (should (equal (wasabi-sticker-animation-test--playing) (list a))))))

(ert-deftest wasabi-sticker-animation-test-off-from-the-start ()
  (let ((a (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list a)
      (setq wasabi-chat-animate-stickers nil)
      (wasabi-chat--animate-stickers)
      (should-not (wasabi-sticker-animation-test--playing)))))

(ert-deftest wasabi-sticker-animation-test-not-on-window-changes ()
  ;; Resuming on a chat's return to view is what slowed them to a crawl.
  (add-hook 'window-buffer-change-functions #'wasabi-chat--sync-animations)
  (with-temp-buffer
    (wasabi-chat-mode)
    (should-not (memq #'wasabi-chat--sync-animations
                      (default-value 'window-buffer-change-functions)))))

(provide 'wasabi-sticker-animation-test)
;;; wasabi-sticker-animation-test.el ends here
