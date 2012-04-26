(require 'coffee-mode)

(setq coffee-js-mode 'js-mode)
(setq coffee-args-compile '("-c"))
(setq coffee-cleanup-whitespace t)
(setq coffee-tab-width 2)

(add-hook 'coffee-mode-hook
          (lambda ()
            (and (file-exists-p (buffer-file-name))
                 (file-exists-p (coffee-compiled-file-name))
                 (coffee-cos-mode t))))
