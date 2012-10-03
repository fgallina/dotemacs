(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")

(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename 'ac-source-yasnippet))

(ac-config-default)
