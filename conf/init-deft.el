(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/Org/deft/"
   deft-text-mode 'org-mode)
  (global-set-key (kbd "C-x t") 'deft))
