(dolist (hook '(clojure-mode-hook emacs-lisp-mode-hook lisp-mode-hook
                nrepl-mode-hook eval-expression-minibuffer-setup-hook
                ielm-mode-hook lisp-interaction-mode-hook scheme-mode-hook))
  (add-hook hook 'enable-paredit-mode))
