(require 'python)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)


;;; Hooks
(remove-hook 'python-mode-hook 'wisent-python-default-setup)
(add-hook 'python-mode-hook 'jedi:setup)
