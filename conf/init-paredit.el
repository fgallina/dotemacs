(dolist (hook '(clojure-mode-hook emacs-lisp-mode-hook lisp-mode-hook
                nrepl-mode-hook))
  (add-hook hook 'enable-paredit-mode))
