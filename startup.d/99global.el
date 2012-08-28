(windmove-default-keybindings)
(winner-mode 1)
(savehist-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(ido-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(setq visible-bell t)
(setq ido-enable-flex-matching t)
(column-number-mode 1)
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (or (buffer-file-name)
               (and (member (current-buffer) (emms-playlist-buffer-list))
                    (emms-playlist-current-selected-track)
                    (emms-track-description
                     (emms-playlist-current-selected-track)))
               (buffer-name)))))
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-default nil)

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(setq warning-suppress-types nil)

(defadvice flymake-start-syntax-check-process
  (after my-flymake-start-syntax-check-process
         (cmd args dir) activate compile)
  ;; set flag to allow exit without query on any active flymake
  ;; processes
  (set-process-query-on-exit-flag ad-return-value nil))

;; (add-hook 'find-file-hook (lambda ()
;;                             (when (and buffer-file-name
;;                                        (not (file-writable-p buffer-file-name))
;;                                        (y-or-n-p (format "File %s  is read-only. Open it as root?"
;;                                                          buffer-file-name)))
;;                               (sudo))))

(set-face-attribute 'default nil :height 105)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq max-specpdl-size 999999999)

(setq enable-recursive-minibuffers t)
(follow-mode t)
(setq redisplay-dont-pause t)
