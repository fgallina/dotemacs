(require 'term)

(defun my:term-set-faces ()
  "Force terminal colors using term-color-* faces.
This is particularly useful as a workaround for themes which
define term faces the old way."
  (let ((color00 "#282A2E")
        (color01 "#A54242")
        (color02 "#B5BD68")
        (color03 "#DE935F")
        (color04 "#81A2BE")
        (color05 "#85678F")
        (color06 "#5E8D87")
        (color07 "#C5C8C6")
        (color08 "#373B41")
        (color09 "#CC6666")
        (color10 "#8C9440")
        (color11 "#f0c674")
        (color12 "#5F819D")
        (color13 "#B294BB")
        (color14 "#8ABEB7")
        (color15 "#E8E8D3"))
   (set-face-attribute
    'term-color-black nil
    :foreground color00 :background color08)
   (set-face-attribute
    'term-color-red nil
    :foreground color01 :background color09)
   (set-face-attribute
    'term-color-green nil
    :foreground color02 :background color10)
   (set-face-attribute
    'term-color-yellow nil
    :foreground color03 :background color11)
   (set-face-attribute
    'term-color-blue nil
    :foreground color04 :background color12)
   (set-face-attribute
    'term-color-magenta nil
    :foreground color05 :background color13)
  (set-face-attribute
    'term-color-cyan nil
    :foreground color06 :background color14)
   (set-face-attribute
    'term-color-white nil
    :foreground color07 :background color15)
   (setq ansi-term-color-vector
         [term
          term-color-black
          term-color-red
          term-color-green
          term-color-yellow
          term-color-blue
          term-color-magenta
          term-color-cyan
          term-color-white])
   (setq ansi-color-names-vector
         [color00
          color01
          color02
          color03
          color04
          color05
          color06
          color07])))

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

(defun my:term-mode-hook ()
  (my:term-set-faces)
  (make-local-variable 'mouse-yank-at-point)
  (make-local-variable 'transient-mark-mode)
  (auto-fill-mode -1)
  (setq mouse-yank-at-point t
        term-scroll-to-bottom-on-output nil
        term-scroll-show-maximum-output nil
        term-buffer-maximum-size 2048))

(when (facep 'term)
  ;; Use the new faces mechanism for term
  (add-hook 'term-mode-hook 'my:term-set-faces))
(add-hook 'term-mode-hook 'my:term-mode-hook)
