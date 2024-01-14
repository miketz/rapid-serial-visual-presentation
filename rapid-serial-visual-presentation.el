;;; rapid-serial-visual-presentation.el --- speed reading -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Package-Requires: ((emacs "24.1"))
;;; Version: 0.1.0
;;; URL: TODO


;;; Commentary:
;;; Speed reading tool. Display text 1 word at a time. Show the next word after
;;; a delay. This is known as "rapid serial visual presentation" or RSVP
;;; for short. The main idea is your eyes can focus in the same spot, more time
;;; spent abosorbing words, less time moving your eyes left to right.

;;; In addition the words are aligned at a focal point, highlighted
;;; in red, where you might most easily identify the word. Currently this point
;;; is identified by a fake heuristic, not based on research. But the focal
;;; point may prove useful for you regardless.
;;;
;;; Orignally made for _0x4aV on #emacs IRC. He was looking for an RSVP in
;;; Emacs.


;;; Installation:
;;; Place `rapid-serial-visual-presentation.el' in `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;; (push "/your/chosen/folder" load-path)
;;; (autoload #'rsvp-start-reader "rapid-serial-visual-presentation" nil t)
;;; ;; Config vars
;;; (setq rsvp-delay-seconds 0.4)
;;; (setq rsvp-font-scale-level 4)
;;; (setq rsvp-pad-above 5
;;;       rsvp-pad-left 10)
;;; (custom-set-faces `(rsvp-focal-point-face ((t :foreground "red"))))
;;; ;; Sample key binds.  Press "C-c r" with text highlighted (or not for full buffer text).
;;; (global-set-key (kbd "C-c r") #'rsvp-start-reader)
;;; (with-eval-after-load 'rapid-serial-visual-presentation
;;;   ;; Recommended key binds for the output buffer.
;;;   (define-key rapid-serial-visual-presentation-mode-map (kbd "C-c q")
;;;               #'rsvp-stop-reader))


;;; TODO: centered view option? something like darkroom-mode? look into how to handle
;;;       width/height with regards to font scaling.
;;; TODO: pause and rewind

;;; Code:
(require 'cl-lib)

(defvar rsvp-buff-name "*serial-reader*")

(defvar rsvp-delay-seconds 0.3)

(defvar rsvp-font-scale-level 4
  "Number of steps to scale font size.
Positive numbers will increase font size.
0 will have no effect on font size.
Negative numbers will decrease font size which you probably don't want.")

;; For now just add padding to achieve a more centered look.
;; although truly centering the text can be achieved via `window-height'
;; and `window-width', these functions do not account for font scaling.
(defvar rsvp-pad-above 5
  "New line padding above the text.")
(defvar rsvp-pad-left 2
  "Space padding left of the text.
Note there will already be default minimum padding added up to the focal point
of each word. This is extra padding on top of that, so you may need to play
around with this value until it looks how you like.")

(defface rsvp-focal-point-face
  '((t (:foreground "#FF0000")))
  "Face for the word focal point of the word."
  :group 'mode-on-region)



;; TODO: use real eye training data to find the ORP.
(defun rsvp-optimal-recognition-point (word)
  "Return a point (array index) of a word string.
Attempts to find the point where a user can most optimally recognize the word.
This is just a fake heuristic for now. Not based on eye training data.
Near the middle for short words, a bit left of center (3rd of length) for
larger words."
  ;; Focus rougly a 3rd of the way through the word.
  ;; Relies on integer truncation to get the index. Produces a center index
  ;; for short words.
  (/ (length word) 3))

;; We must ensure the focal point display has enough padding for the part of
;; the word left of focal point to fit on the screen. Use the longest possible
;; word to calculate minimum padding. Also it looks good to some default padding
;; anyway.
(defconst rsvp--longest-word "pneumonoultramicroscopicsilicovolcanoconiosis"
  "Longest word in English.")

(defconst rsvp--min-focal-point-padding
  (rsvp-optimal-recognition-point rsvp--longest-word))

(define-minor-mode rapid-serial-visual-presentation-mode
  "Minor mode to support key binds and kill-buffer-hook."
  :lighter " serial-reader"
  ;; Ideally users should choose their own key binds. But it is important they
  ;; be able to STOP the serial reader easily. So I'm taking the liberty of
  ;; binding a key for them. This binding will be shown to the user in the
  ;; header of the output buffer.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c q") #'rsvp-stop-reader)
            map))


;; The only timer. Only 1 serial reader may be running at any time.
(defvar rsvp--timer nil)

(defvar rsvp--horizontal-line "---------------------------------------------\n")

;;;###autoload
(cl-defun rsvp-start-reader (&optional start end)
  "Entry point function.
Display current buffer text 1 word at a time in new buffer `rsvp-buff-name'.
Uses selected region if available, otherwise the entire buffer text."

  ;; NOTE: avoiding (interactive "r"). It breaks in the case where Emacs has
  ;; just started up with no mark set yet.
  (interactive (if (use-region-p)
                   ;; use selected region for `start' and `end'
                   (list (region-beginning) (region-end))
                 ;; else use entire buffer
                 (list (point-min) (point-max))))

  ;; stop any running serial reader from a previous invocation.
  ;; for this style of display users can only read 1 buffer at a time, so
  ;; there is little reason to allow multiple serial readers to run at the same time.
  (unless (null rsvp--timer)
    (rsvp-stop-reader))

  (let* ((txt (buffer-substring-no-properties start end))
         (words (split-string txt))
         (buff (get-buffer-create rsvp-buff-name)))

    ;; GUARD: there must be at least 1 word to display.
    (when (= (length words) 0)
      (message "No words to display.")
      (cl-return-from rsvp-start-reader))

    (switch-to-buffer-other-window buff)

    (with-current-buffer buff
      ;; scale font size to configured value
      (text-scale-set rsvp-font-scale-level)

      ;; turn on mode. supports key binds, and the kill-buffer-hook
      (rapid-serial-visual-presentation-mode)

      ;; add a fancy header to the buffer. With info on how to abort.
      (set (make-local-variable 'header-line-format)
           (substitute-command-keys
            "serial reader     [Abort]: \\[rsvp-stop-reader]")))


    ;; show a word every `rsvp-delay-seconds' via a timer.
    (setq rsvp--timer (run-with-timer
                    0 rsvp-delay-seconds
                    (let ((i 0) ; word list index
                          ;; where to apply the focal point face
                          (overlay-point (+ rsvp-pad-above
                                            ;; includes newline
                                            (length rsvp--horizontal-line)
                                            (+ rsvp-pad-left
                                               rsvp--min-focal-point-padding)
                                            ;; 2 for the pipe | and newline
                                            2
                                            (+ rsvp-pad-left
                                               rsvp--min-focal-point-padding)
                                            ;; add extra 1 to fix it?
                                            1)))
                      (lambda ()
                        (with-current-buffer buff
                          (erase-buffer)
                          ;; add padding
                          (cl-loop repeat rsvp-pad-above do (insert "\n"))
                          (insert rsvp--horizontal-line)
                          (cl-loop repeat (+ rsvp-pad-left
                                             rsvp--min-focal-point-padding)
                                   do (insert " "))
                          (insert "|\n")
                          (let* ((orp (rsvp-optimal-recognition-point (nth i words)))
                                 (padding (- rsvp--min-focal-point-padding orp)))
                            ;; insert spaces to line up orp with the |
                            (cl-loop repeat (+ rsvp-pad-left
                                               padding)
                                     do (insert " "))
                            ;; insert word
                            (insert (nth i words)))
                          (insert "\n")
                          (cl-loop repeat (+ rsvp-pad-left
                                             rsvp--min-focal-point-padding)
                                   do (insert " "))
                          (insert "|\n")
                          (insert rsvp--horizontal-line)
                          ;; apply face to word focal point
                          (let ((ov (make-overlay overlay-point (1+ overlay-point))))
                            (overlay-put ov 'face 'rsvp-focal-point-face))
                          ;; book keeping on index
                          (cl-incf i)
                          (when (>= i (length words))
                            (rsvp-stop-reader)))))))))


(defun rsvp-stop-reader ()
  "Stop the display of text into buffer `rsvp-buff-name'.
Cancels `rsvp--timer'.
Call this if the serial display is taking too long."
  (interactive)
  (if (timerp rsvp--timer)
      (progn
        (cancel-timer rsvp--timer)
        ;; Set to nil to make the timer object "extra canceled" and eligible for garbage collection.
        (setq rsvp--timer nil)
        (message "stopped serial reader!"))
    ;; else
    (message "serial reader was already stopped.")))


;; use a hook to cancel the timer if output buffer is killed
(add-hook 'rapid-serial-visual-presentation-mode-hook
          (lambda ()
            (message (format "adding clean up hook to buffer %s"
                             (buffer-name (current-buffer))))
            (add-hook 'kill-buffer-hook
                      #'rsvp-stop-reader
                      nil
                      ;; only add hook to the output buffer, not all buffers!
                      'make-it-local)))


(provide 'rapid-serial-visual-presentation)
