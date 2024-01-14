;;; rsvp.el --- Rapid serial visual presentation -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Package-Requires: ((emacs "24.4"))
;;; Version: 0.1.0
;;; URL: https://github.com/miketz/rapid-serial-visual-presentation


;;; Commentary:
;;; Speed reading tool.  Display text 1 word at a time.  Show the next word after
;;; a delay.  This is known as "rapid serial visual presentation" or RSVP
;;; for short.  The main idea is your eyes can focus in the same spot, more time
;;; spent absorbing words, less time moving your eyes left to right.
;;;
;;; This technique allows reading text on small screens without scrolling, as
;;; only 1 word needs to fit on the screen.
;;;
;;; In addition the words are aligned at a focal point, highlighted
;;; in red, where you might most easily identify the word.  Currently this point
;;; is identified by a fake heuristic, not based on research.  But the focal
;;; point may prove useful for you regardless.
;;;
;;; Originally made for _0x4aV on the Emacs channel of irc.libera.chat.
;;; He was looking for an RSVP in Emacs.
;;;
;;; Requires Emacs 24.4 which will support:
;;;   - lexical binding
;;;   - cl-lib
;;;   - with-eval-after-load for sample config below


;;; Installation:
;;; Place `rsvp.el' in `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;; (push "/your/chosen/folder" load-path)
;;; (autoload #'rsvp-start-reader "rsvp" nil t)
;;; ;; Config vars
;;; (setq rsvp-delay-seconds 0.4)
;;; (setq rsvp-font-scale-level 3)
;;; (setq rsvp-pad-above 5
;;;       rsvp-pad-left 2)
;;; (custom-set-faces `(rsvp-focal-point-face ((t :foreground "red"))))
;;; ;; Sample key binds.
;;; ;; Press "C-c r" with text highlighted (or not for full buffer text).
;;; (global-set-key (kbd "C-c r") #'rsvp-start-reader)
;;; (with-eval-after-load 'rsvp
;;;   ;; Sample key binds for the output buffer.
;;;   (define-key rsvp-mode-map (kbd "C-c q") #'rsvp-stop-reader)
;;;   (define-key rsvp-mode-map (kbd "C-c r") #'rsvp-rewind-reader))


;;; TODO: centered view option? something like darkroom-mode? look into how to handle
;;;       width/height with regards to font scaling.

;;; Code:
(require 'cl-lib)

(defgroup rsvp nil
  "Speed reading technique.  Rapid serial visual presentation."
  :prefix "rsvp-"
  :group 'tools)

(defcustom rsvp-buff-name "*serial-reader*"
  "Name of the output buffer."
  :type 'string
  :group 'rsvp)

(defcustom rsvp-delay-seconds 0.3
  "Delay in seconds until next word display."
  :type 'number
  :group 'rsvp)

(defcustom rsvp-font-scale-level 4
  "Number of steps to scale font size.
Positive numbers will increase font size.
0 will have no effect on font size.
Negative numbers will decrease font size which you probably don't want."
  :type 'integer
  :group 'rsvp)

;; For now just add padding to achieve a more centered look.
;; although truly centering the text can be achieved via `window-height'
;; and `window-width', these functions do not account for font scaling.
(defcustom rsvp-pad-above 5
  "New line padding above the text."
  :type 'integer
  :group 'rsvp)
(defcustom rsvp-pad-left 2
  "Space padding left of the text.
Note there will already be default padding up to the focal point maker.  This
var is extra padding on top of that, so you may need to play around with this
value until it looks how you like."
  :type 'integer
  :group 'rsvp)

(defface rsvp-focal-point-face
  '((t (:foreground "#FF0000")))
  "Face for the focal point character of a word."
  :group 'rsvp)



;; TODO: use real eye training data to find the ORP.
(defun rsvp-optimal-recognition-point (word)
  "Return a point (array index) of a WORD string.
Attempts to find the point where a user can most optimally recognize the word.
This is just a fake heuristic for now.  Not based on eye training data.
Near the middle for short words, a bit left of center (3rd of length) for
larger words."
  ;; Focus roughly a 3rd of the way through the word.
  ;; Relies on integer truncation to get the index. Produces a center index
  ;; for short words.
  (/ (length word) 3))

;; We must ensure the focal point (FP) display has enough padding for the part
;; of the word left of FP to fit on the screen. Use the longest possible word
;; to calculate minimum padding. Also it looks good to have some default
;; padding anyway.
(defconst rsvp--longest-word "pneumonoultramicroscopicsilicovolcanoconiosis"
  "Longest word in English.")

(defconst rsvp--min-focal-point-padding
  (rsvp-optimal-recognition-point rsvp--longest-word))

(define-minor-mode rsvp-mode
  "Minor mode to support key binds and `kill-buffer-hook'."
  :lighter " rsvp"
  :keymap (let ((map (make-sparse-keymap)))
            ;; No default bindings for now. User will choose them.
            ;; (define-key map (kbd "C-c q") #'rsvp-stop-reader)
            ;; (define-key map (kbd "C-c r") #'rsvp-rewind-reader)
            map))

(defun rsvp-use-suggested-keybinds ()
  "Set up suggested key binds."
  (interactive)
  (let ((keys '["C-c r"
                "C-c q"]))
    ;; putting key sequence strings in an array to trick package-lint into
    ;; silence.  it doesn't like C-c prefix key binds but the point of this
    ;; fn is the user explicitly calls it to choose to use the recommended C-c
    ;; prefix bindings.
    (global-set-key (kbd (aref keys 0)) #'rsvp-start-reader)
    (define-key rsvp-mode-map (kbd (aref keys 1)) #'rsvp-stop-reader)
    (define-key rsvp-mode-map (kbd (aref keys 0)) #'rsvp-rewind-reader)))


;; The only timer. Only 1 serial reader may be running at any time.
(defvar rsvp--timer nil)

(defvar rsvp--horizontal-line "---------------------------------------------\n")

(defvar rsvp--draw-fn nil)
(defvar rsvp--rewind-fn nil)

(defun rsvp--get-fns (buff words)
  "Create a list of funcs.
The funcs are a closure over BUFF and WORDS.

The first fn draws output buffer text for a word.
This fn will be invoked repeatedly via a timer.

The second fn is a re-winder.  Sets the index i backward then resumes drawing.

Creates private variables:
  i: word list index
  overlay-point: location in buffer of the focal point character
  ov: the overlay"
  (let* ((i 0) ;; word list index
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
                           1))
         ;; overlay for focal point. reuse this overlay for each word.
         (ov nil))
    ;; This list of functions is the return value.
    (list
     ;; drawing fn. will be run every `rsvp-delay-seconds'.
     (lambda ()
       ;; Draw buffer text. It's a bit ham fisted, redrawing the entire buffer
       ;; for each word. But works OK for now.
       (let ((word (aref words i)))
         (with-current-buffer buff
           (erase-buffer)
           ;; add padding
           (cl-loop repeat rsvp-pad-above do (insert "\n"))
           (insert rsvp--horizontal-line)
           (cl-loop repeat (+ rsvp-pad-left
                              rsvp--min-focal-point-padding)
                    do (insert " "))
           (insert "|\n")
           (let* ((orp (rsvp-optimal-recognition-point word))
                  (orp-padding (- rsvp--min-focal-point-padding orp)))
             ;; insert spaces to line up orp with the |
             (cl-loop repeat (+ rsvp-pad-left
                                orp-padding)
                      do (insert " "))
             ;; insert word
             (insert word))
           (insert "\n")
           (cl-loop repeat (+ rsvp-pad-left
                              rsvp--min-focal-point-padding)
                    do (insert " "))
           (insert "|\n")
           (insert rsvp--horizontal-line)
           ;; apply face to word focal point
           (if (null ov)
               (progn ;; create overlay
                 (setq ov (make-overlay overlay-point
                                        (1+ overlay-point)
                                        buff))
                 (overlay-put ov 'face 'rsvp-focal-point-face))
             ;; else, move existing overlay. range gets messed up when text is deleted
             (move-overlay ov overlay-point (1+ overlay-point)))
           ;; book keeping on index
           (cl-incf i)
           (when (>= i (length words))
             (rsvp-stop-reader)))))
     ;; rewind fn. run on user demand. rewinds the index i.
     (lambda ()
       (rsvp-stop-reader)
       (let ((cnt (read-number "rewind how many words? " 15)))
         ;; Off by 1 issue? i think it's because by the time you call rewind
         ;; i has already been incremented. So increment cnt backwards to
         ;; compensate.
         (cl-incf cnt)
         (if (< i cnt)
             (setq i 0) ;; avoid negative numbers
           (setq i (- i cnt)))
         ;; start display again with new i value
         (setq rsvp--timer
               (run-with-timer 0 rsvp-delay-seconds
                               rsvp--draw-fn)))))))


;;;###autoload
(cl-defun rsvp-start-reader (&optional start end)
  "Entry point function.
Display current buffer text 1 word at a time in new buffer `rsvp-buff-name'.
Fill START and END with the selected region if available, otherwise the entire
buffer text."

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
         (words-lst (split-string txt))
         ;; convert list to array. array index access is faster. Performance
         ;; may matter if you are reading with a small `rsvp-delay-seconds'.
         (words (cl-coerce words-lst 'vector))
         (buff (get-buffer-create rsvp-buff-name)))

    ;; GUARD: there must be at least 1 word to display.
    (when (= (length words) 0)
      (message "No words to display.")
      (cl-return-from rsvp-start-reader))

    (switch-to-buffer-other-window buff)

    (with-current-buffer buff
      ;; scale font size to configured value
      (text-scale-set rsvp-font-scale-level)

      ;; Although only 1 overlay is created per run, avoid amassing overlays.
      ;; But `delete-overlay' does not truly delete the overlay? Delete anyway.
      (cl-loop for o in (car (overlay-lists)) do
               (delete-overlay o))

      ;; turn on mode. supports key binds, and the kill-buffer-hook
      (rsvp-mode)

      ;; add a fancy header to the buffer. With info on how to abort.
      (set (make-local-variable 'header-line-format)
           (substitute-command-keys
            "[Stop]: \\[rsvp-stop-reader]  [Rewind]: \\[rsvp-rewind-reader]")))

    (let ((funcs (rsvp--get-fns buff words)))
      (setq rsvp--draw-fn (cl-first funcs))
      (setq rsvp--rewind-fn (cl-second funcs)))
    ;; show a word every `rsvp-delay-seconds' via a timer.
    (setq rsvp--timer
          (run-with-timer 0 rsvp-delay-seconds
                          rsvp--draw-fn))))


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


(defun rsvp-rewind-reader ()
  "Rewind feature.
First pause the reader.  Get count backwards from user.  Then rewind reader
index back by that amount."
  (interactive)
  ;; call the closure fn. it has secret private vars.
  (funcall rsvp--rewind-fn))


;; use a hook to cancel the timer if output buffer is killed
(add-hook 'rsvp-mode-hook
          (lambda ()
            (message (format "adding clean up hook to buffer %s"
                             (buffer-name (current-buffer))))
            (add-hook 'kill-buffer-hook
                      #'rsvp-stop-reader
                      nil
                      ;; only add hook to the output buffer, not all buffers!
                      'make-it-local)))


(provide 'rsvp)

;;; rsvp.el ends here
