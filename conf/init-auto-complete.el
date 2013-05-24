(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")

(ac-config-default)

(setq-default ac-sources '(ac-source-yasnippet
                           ac-source-filename
                           ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers))
