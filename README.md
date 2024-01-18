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

Requires Emacs 24.4.

Place **rsvp.el** in **/your/chosen/folder**.

Byte compile the file for a bit of extra performance.
Open **rsvp.el** in an Emacs buffer then type:

    M-x byte-compile-file

Add the following text to your .emacs or init.el file:

```elisp
;;;----------------------------------------------------------------------------
;;; rsvp.  rapid serial visual presentation.  speed reader.
;;;----------------------------------------------------------------------------
(push "/your/chosen/folder" load-path)
(autoload #'rsvp-start-reader "rsvp" nil t)
;; Config vars
(setq rsvp-delay-seconds 0.3)
(setq rsvp-pause-comma-p t
      rsvp-pause-comma-% 0.3)
(setq rsvp-pause-end-of-sentence-p t
      rsvp-pause-end-of-sentence-% 2.0)
(setq rsvp-scale-delay-to-word-length-p t)
(setq rsvp-initial-delay-seconds 0.5)
(setq rsvp-font-scale-level 3)
(setq rsvp-pad-above 5
      rsvp-pad-left  2)
(setq rsvp-skip-words-p nil)
(setq rsvp-use-focal-point-face-p t)
(custom-set-faces
 `(rsvp-focal-point-face ((((background dark))
                           (:foreground "#FFA500")) ;; orange
                          (t ;; light background
                           (:foreground "#FF0000")))))
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


# Tips

- RSVP works best on prose. And better on prose conveying familiar ideas. If you are learning something new and need to mull over the text, RSVP may not be ideal.

- Try experimenting with faster speeds. By default a modest speed of 0.3 seconds is used so you don't freak out. But you should move on to something faster soon. You may be surprised how fast you can read and comprehend the text.

    ```elisp
    ;; go fast
    (setq rsvp-delay-seconds 0.08)
    ```


- Sometimes going faster makes it easier to comprehend a set of words. Consider the extreme slow speed of 1 word per minute. At any given time you would have no understanding or context of what you are reading. Words are dropped from your brain before you can comprehend the full sentence. The cure is speed. Go as fast as you can handle.

- As you approach ludicrous speed you may need a longer break at the end of each sentence. It makes a nice cadence of fast/rest. 

    ```elisp
    ;; ludicrous speed!
    (setq rsvp-delay-seconds 0.06)
    ;; but take a 7x break after each sentence to avoid burnout.
    (setq rsvp-pause-end-of-sentence-% 7.0)
    ```