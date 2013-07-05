(require 'uniquify)

(winner-mode 1)
(savehist-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(ido-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(windmove-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)

(setq auto-save-default nil)
(setq warning-suppress-types nil)
(setq max-specpdl-size 999999999)
(setq enable-recursive-minibuffers t)
(setq redisplay-dont-pause t)
(setq echo-keystrokes 0.01)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq uniquify-buffer-name-style 'forward)
(setq visible-bell t)
(setq ido-enable-flex-matching t)
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq vc-make-backup-files t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
