(require 'term)

(setq ansi-term-color-vector
      [unspecified
       "black"
       "tomato"
       "#6ac214" ;; executable files
       "#edd400"
       "dodger blue" ;; folder color
       "#ad7fa8"
       "#729fcf"
       "#eeeeec"])

;; Based on:
;; http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(defun ansi-term-visit-dwim ()
  "Visit ansi-term Doing What I Mean
If the current buffer is:
  1) a running ansi-term named *ansi-term*, rename it.
  2) a stopped ansi-term, kill it and create a new one.
  3) a non ansi-term, go to an already running ansi-term
     or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (let ((newname
                       (format "*ansi-term: %s*"
                               (read-buffer "Rename buffer (to new name): "))))
                  (rename-buffer newname))
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(defun term-my-hook ()
   (make-local-variable 'mouse-yank-at-point)
   (make-local-variable 'transient-mark-mode)
   (auto-fill-mode -1)
   (setq mouse-yank-at-point t
         term-scroll-to-bottom-on-output nil
         term-scroll-show-maximum-output nil
         term-buffer-maximum-size 1024
         transient-mark-mode nil
         tab-width 8))

(add-hook 'term-mode-hook 'term-my-hook)
