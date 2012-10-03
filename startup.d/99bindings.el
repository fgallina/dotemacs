(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-f") 'insert-file-name)
(global-set-key (kbd "M-Y") 'yank-pop-backwards)
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-M-}") 'forward-page)
(global-set-key (kbd "C-M-{") 'backward-page)
(global-set-key (kbd "C-x t") 'deft)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "S-SPC") 'er/expand-region)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
