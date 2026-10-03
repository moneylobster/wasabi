;;; wasabi-sticker-animation-test.el --- Tests for playing animated stickers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L <acp-dir> -l wasabi-sticker-animation-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Batch Emacs cannot decode images, so whether one has frames, and
;; whether the chat is in a window, are stubbed.  The animation timers
;; are Emacs's own.

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
In BODY, setting `shown' decides whether the chat is in a window, and
`animated' lists the images that have frames."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer "*wasabi-animation-test*"))
         (wasabi-chat-animate-stickers t)
         (shown t)
         (animated ,images))
     (unwind-protect
         (cl-letf (((symbol-function 'get-buffer-window)
                    (lambda (&rest _) (and shown 'window)))
                   ((symbol-function 'image-multi-frame-p)
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

(ert-deftest wasabi-sticker-animation-test-plays-when-shown ()
  (let ((a (wasabi-sticker-animation-test--image "a"))
        (b (wasabi-sticker-animation-test--image "b")))
    (wasabi-sticker-animation-test--with (list a b)
      (wasabi-chat--animate-stickers)
      (should (equal (length (wasabi-sticker-animation-test--playing)) 2))
      ;; Looping for good, not played once through.
      (should (eq (nth 4 (timer--args (image-animate-timer a))) t))
      ;; Asked again, nothing doubles up.
      (wasabi-chat--animate-stickers)
      (should (equal (length (wasabi-sticker-animation-test--playing)) 2)))))

(ert-deftest wasabi-sticker-animation-test-same-sticker-twice ()
  ;; Sent twice, and drawn as two images that are `equal' but not `eq':
  ;; both play.
  (let ((first (wasabi-sticker-animation-test--image "a"))
        (again (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list first again)
      (wasabi-chat--animate-stickers)
      (should (equal (length (wasabi-sticker-animation-test--playing)) 2)))))

(ert-deftest wasabi-sticker-animation-test-pauses-out-of-sight ()
  (let ((a (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list a)
      (setq shown nil)
      (wasabi-chat--animate-stickers)
      (should-not (wasabi-sticker-animation-test--playing))
      (setq shown t)
      (wasabi-chat--sync-animations)
      (should (equal (wasabi-sticker-animation-test--playing) (list a)))
      ;; Buried: the window change pauses it.
      (setq shown nil)
      (wasabi-chat--sync-animations)
      (should-not (wasabi-sticker-animation-test--playing)))))

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
      ;; The same sticker drawn again, as a new image.
      (let ((inhibit-read-only t))
        (put-text-property (point-min) (+ (point-min) 9) 'display new))
      (setq animated (list old new))
      (wasabi-chat--animate-stickers)
      (should (equal (length (wasabi-sticker-animation-test--playing)) 1))
      (should (eq (car (wasabi-sticker-animation-test--playing)) new)))))

(ert-deftest wasabi-sticker-animation-test-toggle ()
  (let ((a (wasabi-sticker-animation-test--image "a")))
    (wasabi-sticker-animation-test--with (list a)
      (wasabi-chat--animate-stickers)
      (should (wasabi-sticker-animation-test--playing))
      (wasabi-chat-toggle-sticker-animation)
      (should-not wasabi-chat-animate-stickers)
      (should-not (wasabi-sticker-animation-test--playing))
      (wasabi-chat-toggle-sticker-animation)
      (should (wasabi-sticker-animation-test--playing)))))

(ert-deftest wasabi-sticker-animation-test-hooked-on-window-changes ()
  (with-temp-buffer
    (wasabi-chat-mode)
    (should (memq #'wasabi-chat--sync-animations
                  (default-value 'window-buffer-change-functions)))))

(provide 'wasabi-sticker-animation-test)
;;; wasabi-sticker-animation-test.el ends here
