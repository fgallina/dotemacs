(add-to-list 'load-path "~/.emacs.d/vendor")
(let ((default-directory "~/.emacs.d/vendor"))
  (normal-top-level-add-subdirs-to-load-path))

(ignore-errors (load-file "~/.emacs.d/secrets.el"))

(require 'startupd)
(startupd-load-files)

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)
