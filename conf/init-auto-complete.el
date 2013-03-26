(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")

(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-variables ac-source-symbols) ac-sources)))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags) ac-sources)))

(ac-config-default)
