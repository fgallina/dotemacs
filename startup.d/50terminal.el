(require 'term)

(when (facep 'term-color-black)
  (set-face-attribute
   'term-color-black nil
   :foreground "#282A2E" :background "#373B41") ; 0 8

  (set-face-attribute
   'term-color-red nil
   :foreground "#A54242" :background "#CC6666") ; 1 9

  (set-face-attribute
   'term-color-green nil
   :foreground "#B5BD68" :background "#8C9440") ; 2 10

  (set-face-attribute
   'term-color-yellow nil
   :foreground "#DE935F" :background "#f0c674") ; 3 11

  (set-face-attribute
   'term-color-blue nil
   :foreground "#81A2BE" :background "#5F819D") ; 4 12

  (set-face-attribute
   'term-color-magenta nil
   :foreground "#85678F" :background "#B294BB") ; 5 13

  (set-face-attribute
   'term-color-cyan nil
   :foreground "#5E8D87" :background "#8ABEB7") ; 6 14

  (set-face-attribute
   'term-color-white nil
   :foreground "#C5C8C6" :background "#E8E8D3")) ; 7 15

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
        (anon-term (get-buffer "*ansi-term*"))
        (program (or explicit-shell-file-name
                     (getenv "ESHELL")
                     (getenv "SHELL")
                     "/bin/sh")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (let ((newname
                       (format "*ansi-term: %s*"
                               (read-buffer "Rename buffer (to new name): "))))
                  (rename-buffer newname))
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term program)))
          (kill-buffer (buffer-name))
          (ansi-term program))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term program))
        (ansi-term program)))))

(defun term-my-hook ()
   (make-local-variable 'mouse-yank-at-point)
   (make-local-variable 'transient-mark-mode)
   (auto-fill-mode -1)
   (setq mouse-yank-at-point t
         term-scroll-to-bottom-on-output nil
         term-scroll-show-maximum-output nil
         term-buffer-maximum-size 2048))

(add-hook 'term-mode-hook 'term-my-hook)
