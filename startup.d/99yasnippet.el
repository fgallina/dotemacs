(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets" "~/.emacs.d/snippets"))

(defun yas-minor-mode-on ()
  "Turn on YASnippet minor mode.

Do this unless `yas--dont-activate' is truish "
  (interactive)
  (unless (or (minibufferp)
              (member major-mode '(term-mode)))
    (yas-minor-mode 1)))

(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt))
