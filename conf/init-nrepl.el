(setq
 nrepl-lein-command (executable-find "lein")
 nrepl-hide-special-buffers nil
 nrepl-popup-stacktraces t
 nrepl-popup-stacktraces-in-repl t
 nrepl-mode-hook 'subword-mode)

(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
