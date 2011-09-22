(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))

(require 'startupd)
(startupd-load-files)

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)
