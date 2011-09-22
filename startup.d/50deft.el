(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/Org/deft/"
   deft-text-mode 'org-mode)
  (deft-setup)
  (global-set-key (kbd "C-x t") 'deft))
