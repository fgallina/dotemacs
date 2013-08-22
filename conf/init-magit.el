(add-hook
 'magit-mode-hook
 (lambda ()
   (require 'magit-svn)
   (require 'rebase-mode)))
