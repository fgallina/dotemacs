(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets" "~/.emacs.d/snippets"))
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt))
