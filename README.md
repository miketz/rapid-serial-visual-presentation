# Rapid serial visual presentation (RSVP) speed reading tool

Speed reading tool. Display text 1 word at a time. Show the next word after
a delay. This is known as "rapid serial visual presentation" or RSVP
for short. The main idea is your eyes can focus in the same spot, with more
time spent absorbing words, less time moving your eyes left to right.

This technique allows reading text on small screens without scrolling, as
only 1 word needs to fit on the screen.

In addition the words are aligned at a focal point, highlighted
in red, where you might most easily identify the word. Currently this point
is identified by a fake heuristic, not based on research. But the focal
point may prove useful for you regardless.

Originally made for _0x4aV on #emacs IRC. He was looking for an RSVP in
Emacs.

# Installation

Requires Emacs 24.4 which will support:
  - lexical binding
  - cl-lib
  - with-eval-after-load for sample config below

Place **rsvp.el** in **/your/chosen/folder**.

Add the following text to your .emacs or init.el file:

```lisp
;;;----------------------------------------------------------------------------
;;; rsvp.  rapid serial visual presentation.  speed reader.
;;;----------------------------------------------------------------------------
(push "/your/chosen/folder" load-path)
(autoload #'rsvp-start-reader "rsvp" nil t)
;; Config vars
(setq rsvp-delay-seconds 0.4)
(setq rsvp-initial-delay-seconds 0.5)
(setq rsvp-font-scale-level 3)
(setq rsvp-pad-above 5
      rsvp-pad-left  2)
(setq rsvp-skip-words-p nil)
(custom-set-faces `(rsvp-focal-point-face ((t :foreground "red"))))
;; Sample key binds.
;; Press "C-c r" with text highlighted (or not for full buffer text).
(global-set-key (kbd "C-c r") #'rsvp-start-reader)
(with-eval-after-load 'rsvp
  ;; Sample key binds for the output buffer.
  (define-key rsvp-mode-map (kbd "<SPC>") #'rsvp-toggle-start-stop)
  (define-key rsvp-mode-map (kbd "r") #'rsvp-rewind-reader)
  (with-eval-after-load 'evil
    ;; weird key bind issues with evil mode, so avoid it for now.
    (push `(,rsvp-buff-name . emacs) evil-buffer-regexps)))
```
