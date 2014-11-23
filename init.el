(defvar package-archives)
(defvar package-archive-contents)
(defvar my:disabled-packages nil)


;;; Bootstrap
(setq package-archives '(("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(load "~/.emacs.d/pre-startup.el" 'noerror)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;;; User package
(require 'use-package)

(defmacro user-package (name &rest args)
  "Wrapper over `use-package'.
Disables all packages that are member of the
`my:disabled-packages' list by injecting membership into
`use-package' :if keyword ."
  (declare (indent 1))
  (when (not (memq name my:disabled-packages))
    `(use-package ,name ,@args)))

(defconst user-package-font-lock-keywords
  '(("(\\(user-package\\)\\_>[ \t']*\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode user-package-font-lock-keywords)


;;; el-get integration
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

(mapc (lambda (filename)
        (let ((filename (expand-file-name filename el-get-dir)))
          (when (file-directory-p filename)
            (setq load-path (cons filename load-path)))))
      (cddr (directory-files el-get-dir)))

(setq el-get-sources '(python))

(defalias 'el-get-init 'ignore
  "Don't use el-get for making packages available for use.")

(dolist (pkg el-get-sources)
  (unless (or (memq pkg my:disabled-packages)
              (el-get-package-is-installed pkg))
    (el-get-install pkg)))


;;; Environment
(let ((bindir (expand-file-name "~/bin")))
  (setenv "PATH" (concat bindir  ":" (getenv "PATH")))
  (add-to-list 'exec-path bindir))


;;; Packages and config

(user-package ace-jump-mode
  :if (not noninteractive)
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode-pop-mark))
  :ensure ace-jump-mode
  :config
  (progn
    (setq ace-jump-mode-case-fold t)
    (ace-jump-mode-enable-mark-sync)
    (setq ace-jump-mode-submode-list
          '(ace-jump-word-mode ace-jump-char-mode ace-jump-line-mode))))

(user-package ag
  :if (not noninteractive)
  :ensure ag)

(user-package alert
  :ensure alert
  :config (setq alert-default-style 'libnotify))

(user-package auto-complete
  :if (not noninteractive)
  :ensure auto-complete
  :diminish auto-complete-mode
  :config (progn
            (require 'auto-complete-config)
            (ac-config-default)
            (setq-default ac-sources '(ac-source-yasnippet
                                       ac-source-filename
                                       ac-source-abbrev
                                       ac-source-dictionary
                                       ac-source-words-in-same-mode-buffers))
            (global-auto-complete-mode 1)))

(user-package browse-url
  :config (setq browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "firefox"))

(user-package clojure-mode
  :if (not noninteractive)
  :ensure clojure-mode
  :config
  (progn
    (user-package cider
      :ensure cider
      :diminish cider-mode)
    (user-package ac-nrepl
      :ensure ac-nrepl)
    (user-package align-cljlet
      :ensure align-cljlet)
    (user-package clojure-cheatsheet
      :ensure clojure-cheatsheet)
    (user-package clj-refactor
      :ensure clj-refactor)
    (user-package slamhound
      :ensure slamhound)
    (setq
     cider-lein-command (executable-find "lein")
     cider-popup-stacktraces t
     cider-repl-mode-hook #'subword-mode
     nrepl-hide-special-buffers t)
    (add-to-list 'same-window-buffer-names "*nrepl*")
    (add-hook 'clojure-mode-hook #'cider-mode)
    (add-hook 'clojure-mode-hook #'(lambda ()
                                     (cljr-add-keybindings-with-prefix "C-c")
                                     (clj-refactor-mode 1)))
    (add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'ac-nrepl-setup)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes #'cider-mode))))

(user-package css-mode
  :if (not noninteractive)
  :ensure css-mode
  :config (setq css-indent-offset 2))

(user-package cus-theme
  :config
  (progn
    (defun my:unload-themes (&optional msg)
      "Unload all custom enabled themes."
      (interactive "p")
      (mapc #'disable-theme custom-enabled-themes)
      (when msg
        (message "Unloaded all themes.")))
    (defun my:load-single-theme (theme &optional no-confirm no-enable)
      "Load a single custom theme."
      (interactive
       (list
        (intern (completing-read "Load custom theme: "
                                 (mapcar 'symbol-name
                                         (custom-available-themes))))
        nil nil))
      (my:unload-themes)
      (load-theme theme no-confirm no-enable))
    (defun my:load-random-theme (&optional msg)
      "Load a random theme."
      (interactive "p")
      (let ((success))
        (while (not success)
          (let* ((themes (custom-available-themes))
                 (random-theme
                  (progn
                    (random t)
                    (nth (random (length themes)) themes))))
            (condition-case err
                (progn
                  (my:load-single-theme random-theme t)
                  (setq success t))
              (error
               (message "Failed to load %s. Retrying..." random-theme)))
            (when (and success msg)
              (message "Loaded theme %s" random-theme))))))
    (user-package ample-theme
      :ensure ample-theme
      :defer t)
    (user-package color-theme-sanityinc-solarized
      :ensure color-theme-sanityinc-solarized
      :defer t)
    (user-package color-theme-sanityinc-tomorrow
      :ensure color-theme-sanityinc-tomorrow
      :defer t)
    (user-package cyberpunk-theme
      :ensure cyberpunk-theme
      :defer t)
    (user-package leuven-theme
      :ensure leuven-theme
      :defer t)
    (user-package monokai-theme
      :ensure monokai-theme
      :defer t)
    (user-package zenburn-theme
      :ensure zenburn-theme
      :defer t)))

(user-package deft
  :if (not noninteractive)
  :ensure deft
  :config (progn
            (setq
             deft-extension "org"
             deft-directory "~/Org/deft/"
             deft-text-mode 'org-mode)))

(user-package dired-details
  :if (not noninteractive)
  :ensure dired-details
  :config (progn
            (dired-details-install)))

(user-package eldoc
  :config
  (progn
    (add-hook 'prog-mode-hook #'turn-on-eldoc-mode)))

(user-package emerge
  :config (setq emerge-diff-ok-lines-regexp
                (concat "^\\(" "[0-9,]+[acd][0-9,]+\C-m?$" "\\|[<>] "
                        "\\|---" "\\|.*Warning *:" "\\|.*No +newline"
                        "\\|.*missing +newline" "\\|^\C-m?$" "\\)")
                emerge-diff3-ok-lines-regexp
                (concat "^\\([1-3]:\\|====\\|  \\|.*Warning *:\\|"
                        ".*No newline\\|.*missing newline\\|^\C-m$\\)")))

(user-package emms
  :if (not noninteractive)
  :ensure emms
  :bind ("C-x C-m" . my:emms)
  :defer t
  :config
  (progn
    (user-package emms-info-mediainfo
      :ensure emms-info-mediainfo)
    (emms-devel)
    (emms-default-players)

    (defun my:emms ()
      "Same as `emms' but allow creating an empty playlist."
      (interactive)
      (and (or (null emms-playlist-buffer)
               (not (buffer-live-p emms-playlist-buffer)))
           (setq emms-playlist-buffer (emms-playlist-new)))
      (emms-playlist-mode-go))

    ;; add what i mean
    (defun my:emms-awim (filename &optional arg)
      "Adds the FILENAME specified, whatever it is.
It will expand wildcards.  With ARG clear the playlist before
adding files."
      (interactive
       (let* ((fname (car (find-file-read-args "Add what? " nil)))
              (dirpart (file-name-directory fname))
              (fnames
               (or
                (and dirpart (string-match "[*]$" fname)
                     (file-expand-wildcards fname))
                (and (file-exists-p fname) (list fname)))))
         (list fnames
               current-prefix-arg)))
      (when arg
        (emms-stop)
        (emms-playlist-clear))
      (dolist (fname filename)
        (cond
         ((file-directory-p fname)
          (emms-add-directory-tree fname))
         ((file-exists-p fname)
          (emms-add-file fname))))
      (when arg
        (goto-char (point-min))
        (emms-playlist-mode-play-current-track)))

    ;; Stolen and adapted from TWB
    (defun my:emms-info-track-description (track)
      "Return a description of the current track."
      (if (not (eq (emms-track-type track) 'file))
          (emms-track-simple-description track)
        (let* ((ptot (emms-track-get track 'info-playing-time))
               (pmin (or
                      (emms-track-get track 'info-playing-time-min)
                      (and ptot (/ ptot 60)) 0))
               (psec (or
                      (emms-track-get track 'info-playing-time-sec)
                      (and ptot (% ptot 60)) 0))
               (tracknumber (string-to-number
                             (or (emms-track-get track 'info-tracknumber) "0")))
               (artist (or (emms-track-get track 'info-artist) "unknown"))
               (title (or (emms-track-get track 'info-title)
                          (file-name-nondirectory (emms-track-get track 'name)))))
          (format "%02d. %s - %s [%02d:%02d]" tracknumber artist title pmin psec))))

    (setq
     emms-info-asynchronously t
     emms-info-functions '(emms-info-mediainfo)
     emms-mode-line-format " %s "
     emms-player-mplayer-parameters '("-slave" "-quiet" "-really-quiet" "-af" "scaletempo")
     emms-repeat-playlist t
     emms-show-format "NP: %s"
     emms-track-description-function 'my:emms-info-track-description
     later-do-interval 0.0001)

    (emms-mode-line-disable)
    (emms-playing-time-enable-display)

    (bind-key "A" 'my:emms-awim emms-playlist-mode-map)
    (bind-key "S" 'emms-streams emms-playlist-mode-map)
    (bind-key "h" 'describe-mode emms-playlist-mode-map)
    (bind-key "H" 'describe-mode emms-playlist-mode-map)
    (bind-key "?" 'emms-shuffle emms-playlist-mode-map)
    (bind-key "g" 'emms-cache-sync emms-playlist-mode-map)
    (bind-key "SPC" 'emms-pause emms-playlist-mode-map)
    (bind-key "q" 'emms emms-stream-mode-map)
    (bind-key "Q" 'emms emms-stream-mode-map)))

(user-package expand-region
  :if (not noninteractive)
  :bind (("C-M-SPC" . er/expand-region)
         ("C-M-@" . er/expand-region))
  :ensure expand-region)

(user-package files
  :config (progn
            (setq auto-save-default nil)
            (setq backup-directory-alist
                  `(("." . ,(expand-file-name
                             (concat user-emacs-directory "backups")))))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(user-package god-mode
  :if (not noninteractive)
  :ensure god-mode
  :config
  (progn
    (defun my:god-toggle-on-overwrite ()
      "Toggle god-mode on overwrite-mode."
      (if (bound-and-true-p overwrite-mode)
          (god-local-mode-pause)
        (god-local-mode-resume)))

    (defun god-gnus-mode-p ()
      "Return non-nil if major-mode is a gnus mode."
      (string-match-p "^gnus-.*-mode$" (symbol-name major-mode)))

    (defun vc-mode-p ()
      "Return non-nil if major-mode is a vc mode."
      (string-match-p "^vc-.*-mode$" (symbol-name major-mode)))

    (defun my:god-update-cursor-enabled ()
      (setq cursor-type 'hollow))

    (defun my:god-update-cursor-disabled ()
      (setq cursor-type 'box))

    (defun my:god-update-cursor ()
      (if (and god-global-mode god-local-mode)
          (my:god-update-cursor-enabled)
        (my:god-update-cursor-disabled)))

    (mapc (lambda (mode)
            (add-to-list 'god-exempt-major-modes mode))
          '(git-commit-mode log-edit-mode rcirc-mode))
    (mapc (lambda (mode)
            (add-to-list 'god-exempt-predicates mode))
          '(god-gnus-mode-p vc-mode-p))

    (add-hook 'god-mode-enabled-hook #'my:god-update-cursor-enabled)
    (add-hook 'god-mode-disabled-hook #'my:god-update-cursor-disabled)
    (add-hook 'overwrite-mode-hook #'my:god-toggle-on-overwrite)

    (defadvice select-window (after my:god-update activate)
      "Trigger cursor changes for god mode after selecting a window."
      (my:god-update-cursor))

    (bind-key "<escape>" 'god-mode-all)
    (bind-key "i" 'god-local-mode god-local-mode-map)))

(user-package helm
  :if (not noninteractive)
  :ensure helm
  :config
  (progn
    (bind-key "<RET>" #'helm-maybe-exit-minibuffer helm-map)
    (bind-key "C-i" #'helm-execute-persistent-action helm-map)
    (bind-key "C-j" #'helm-maybe-exit-minibuffer helm-map)
    (bind-key "C-x b" #'helm-mini)
    (bind-key "C-x C-f" #'helm-find-files)
    (bind-key "C-z" #'helm-select-action helm-map)
    (bind-key "M-y" #'helm-show-kill-ring)
    (setq helm-buffers-fuzzy-matching t
          helm-ff-auto-update-initial-value t
          helm-ff-file-name-history-use-recentf t
          helm-ff-search-library-in-sexp t
          helm-ff-skip-boring-files t
          helm-move-to-line-cycle-in-source t
          helm-scroll-amount 8
          helm-split-window-in-side-p t)
    (helm-mode 1)
    (user-package helm-swoop
      :ensure helm-swoop)
    (bind-key "C-x M-i" #'helm-multi-swoop)
    (bind-key "M-I" #'helm-swoop-back-to-last-point)
    (bind-key "M-i" #'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
    (bind-key "M-i" #'helm-swoop)
    (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)
    (setq helm-multi-swoop-edit-save t
          helm-swoop-speed-or-color t
          helm-swoop-split-direction #'split-window-horizontally
          helm-swoop-split-with-multiple-windows nil
          helm-swoop-use-line-number-face t)))

(user-package ido
  :if (not noninteractive)
  :config
  (progn
    (user-package ido-vertical-mode
      :ensure ido-vertical-mode)
    (user-package flx
      :ensure flx)
    (user-package flx-ido
      :ensure flx-ido)
    (setq ido-enable-flex-matching t
          ido-use-faces nil
          flx-ido-use-faces t)
    (ido-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)
    (flx-ido-mode 1)))

(user-package flycheck
  :ensure flycheck
  :config (progn
            ;; Add virtualenv support for checkers
            (defadvice flycheck-check-executable
              (around python-flycheck-check-executable (checker)
                      activate compile)
              "`flycheck-check-executable' with virtualenv support."
              (if (eq major-mode 'python-mode)
                  (let* ((process-environment (python-shell-calculate-process-environment))
                         (exec-path (python-shell-calculate-exec-path)))
                    ad-do-it)
                ad-do-it))

            (defadvice flycheck-start-checker
              (around python-flycheck-start-checker (checker)
                      activate compile)
              "`flycheck-start-checker' with virtualenv support."
              (if (eq major-mode 'python-mode)
                  (let* ((process-environment (python-shell-calculate-process-environment))
                         (exec-path (python-shell-calculate-exec-path)))
                    ad-do-it)
                ad-do-it))

            (setq flycheck-mode-line-lighter " ")

            (global-flycheck-mode 1)))

(user-package gist
  :if (not noninteractive)
  :ensure gist)

(user-package git-commit-mode
  :if (not noninteractive)
  :ensure git-commit-mode)

(user-package git-rebase-mode
  :if (not noninteractive)
  :ensure git-rebase-mode)

(user-package gitconfig-mode
  :if (not noninteractive)
  :ensure gitconfig-mode)

(user-package gitignore-mode
  :if (not noninteractive)
  :ensure gitignore-mode)

(user-package hl-line
  :if (not noninteractive)
  :config (global-hl-line-mode))

(user-package ibuffer
  :if (not noninteractive)
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil
          ibuffer-saved-filter-groups
          (list (append
                 (cons "default"
                       ;; Generate filters by major modes from the
                       ;; auto-mode-alist
                       (let ((mode-filters))
                         (dolist (element auto-mode-alist)
                           (when (ignore-errors (fboundp (cdr element)))
                             (let* ((mode (cdr element))
                                    (name (if (string-match "\\(-mode\\)?\\'"
                                                            (symbol-name mode))
                                              (capitalize
                                               (substring (symbol-name mode)
                                                          0 (match-beginning 0)))
                                            (symbol-name mode))))
                               (when (not (assoc-string name mode-filters))
                                 (setq mode-filters
                                       (cons (list name (cons 'mode mode))
                                             mode-filters))))))
                         mode-filters))
                 ;; Custom added filters.
                 '(("Magit" (name . "^\\*magit"))
                   ("Irc" (mode . rcirc-mode))
                   ("Css" (mode . scss-mode))
                   ("W3m" (name . "^\\*w3m"))))))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))))

(user-package js
  :if (not noninteractive)
  :config
  (progn
    (user-package jquery-doc
      :ensure jquery-doc)
    (add-hook 'js-mode-hook 'jquery-doc-setup)))

(user-package lisp-mode
  :if (not noninteractive)
  :config
  (progn
    (bind-key "C-c e b" 'eval-buffer lisp-mode-shared-map)
    (bind-key "C-c e c" 'cancel-debug-on-entry lisp-mode-shared-map)
    (bind-key "C-c e d" 'debug-on-entry lisp-mode-shared-map)
    (bind-key "C-c e e" 'toggle-debug-on-error lisp-mode-shared-map)
    (bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load lisp-mode-shared-map)
    (bind-key "C-c e l" 'find-library lisp-mode-shared-map)
    (bind-key "C-c e r" 'eval-region lisp-mode-shared-map)))

(user-package magit
  :if (not noninteractive)
  :bind ("C-x g" . magit-status)
  :ensure magit
  :config
  (progn
    (defun magit-diff-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-diff-dont-ignore-whitespace)
        (magit-diff-ignore-whitespace)))
    (defun magit-diff-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))
    (defun magit-diff-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))
    (bind-key "W" 'magit-diff-toggle-whitespace magit-status-mode-map)))

(user-package markdown-mode
  :if (not noninteractive)
  :ensure markdown-mode)

(user-package menu-bar
  :bind ("M-k" . kill-this-buffer)
  :config (menu-bar-mode -1))

(user-package gnus
  :if (not noninteractive)
  :bind ("<f2>" . gnus)
  :pre-load (setq gnus-home-directory "~/.emacs.d/gnus"
                  gnus-inhibit-startup-message t
                  gnus-init-file "~/.emacs.d/gnus-init")
  :config
  (progn
    (user-package bbdb
      :ensure bbdb
      :config
      (progn
        (setq bbdb-complete-name-full-completion t
              bbdb-file "~/.emacs.d/gnus/bbdb.db"
              bbdb-mail-user-agent 'gnus-user-agent
              bbdb-mua-pop-up 'horiz
              bbdb-pop-up-layout 'pop-up-multi-line
              bbdb-pop-up-window-size 0.5
              bbdb-complete-mail-allow-cycling t)
        (bbdb-initialize 'gnus)))
    (user-package message
      :config (setq
               message-kill-buffer-on-exit t
               message-send-mail-function 'message-send-mail-with-sendmail
               message-send-mail-partially-limit nil
               message-sendmail-envelope-from 'header))
    (user-package sendmail
      :config (setq sendmail-program (executable-find "msmtp")))
    (user-package gnus-art)
    (user-package nnmairix
      :config
      (progn
        (defvar my:nnmairix-call-mairix-binary-folder "~/Maildir/mairix")
        (defadvice nnmairix-call-mairix-binary
          (around my:nnmairix-call-mairix-binary
                  (command folder searchquery threads)
                  activate compile)
          "Fix folder path for local imap."
          (setq folder (expand-file-name folder my:nnmairix-call-mairix-binary-folder))
          ad-do-it)))

    (setq gnus-novice-user t
          gnus-interactive-exit nil
          gnus-large-newsgroup 2000
          gnus-permanently-visible-groups ".*INBOX"
          gnus-prompt-before-saving t
          gnus-thread-hide-subtree t
          gnus-thread-sort-functions '(gnus-thread-sort-by-number)
          gnus-treat-display-smileys nil
          gnus-treat-strip-cr t
          mm-attachment-override-types (cons "image/.*"
                                             mm-attachment-override-types)
          ;; I don't use news servers, speed up a bit.
          gnus-read-active-file nil
          gnus-save-newsrc-file nil
          gnus-read-newsrc-file nil
          gnus-check-new-newsgroups nil)

    (defvar my:gnus-archive-folder nil
      "The archive folder for this group.
Intended to be set via `gnus-parameters'.")
    (defvar my:gnus-followup-folder nil
      "The follow up folder for this group.
Intended to be set via `gnus-parameters'.")
    (defvar my:gnus-hold-folder nil
      "The hold folder for this group.
Intended to be set via `gnus-parameters'.")
    (defvar my:gnus-trash-folder nil
      "The archive trash for this group.
Intended to be set via `gnus-parameters'.")

    (defun my:gnus-summary-archive (&optional n)
      "Move the current article to the archive folder.
Uses the `my:gnus-archive-folder' value to detect the folder.
Optional argument N works the same as in
`gnus-summary-move-article'."
      (interactive "P")
      (gnus-summary-move-article n my:gnus-archive-folder))

    (defun my:gnus-summary-followup (&optional n)
      "Move the current article to the follow up folder.
Uses the `my:gnus-followup-folder' value to detect the folder.
Optional argument N works the same as in
`gnus-summary-move-article'."
      (interactive "P")
      (gnus-summary-move-article n my:gnus-followup-folder))

    (defun my:gnus-summary-hold (&optional n)
      "Move the current article to the hold folder.
Uses the `my:gnus-hold-folder' value to detect the folder.
Optional argument N works the same as in
`gnus-summary-move-article'."
      (interactive "P")
      (gnus-summary-move-article n my:gnus-hold-folder))

    (defun my:gnus-summary-trash (&optional n)
      "Move the current article to the trash folder.
Uses the `my:gnus-trash-folder' value to detect the folder.
Optional argument N works the same as in
`gnus-summary-move-article'."
      (interactive "P")
      (gnus-summary-move-article n my:gnus-trash-folder))

    (defvar my:gnus-group-sync-programs nil
      "List of programs (with switches) executed for syncing email.")
    (defvar my:gnus-group-sync-running-processes 0
      "Internal counter for executed sync processes.")
    (defvar my:gnus-group-sync-buffer-name "*mailsync*"
      "Buffer name for sync processes to use for output.")
    (defvar my:gnus-group-sync-window nil
      "Window used to show the sync progress.")

    (defun my:gnus-group-sync-sentinel (process signal)
      "Sentinel for sync processes."
      (when (memq (process-status process) '(exit signal))
        (message "%S: %s."
                 (mapconcat 'identity (process-command process) " ")
                 (substring signal 0 -1))
        (setq my:gnus-group-sync-running-processes
              (1- my:gnus-group-sync-running-processes))
        (when (and (zerop my:gnus-group-sync-running-processes)
                   (window-live-p my:gnus-group-sync-window))
          (delete-window my:gnus-group-sync-window)
          (when (buffer-live-p (get-buffer gnus-group-buffer))
            (gnus-group-get-new-news))
          (message "Sync finished."))))

    ;; Shamelessly stolen from mu4e.
    (defun my:gnus-group-sync-make-window (buf height)
      "Create a temporary window for BUF with HEIGHT.
The window is set at the bottom of the screen."
      (let ((win
             (split-window
              (frame-root-window)
              (- (window-height (frame-root-window)) height))))
        (set-window-buffer win buf)
        (set-window-dedicated-p win t)
        win))

    (defun my:gnus-group-sync (&optional nosync)
      "Sync all group emails using external programs.
External programs are defined in `my:gnus-group-sync-programs'.
With optional argument NOSYNC, call `gnus-group-get-new-news'
instead and do not execute any external program."
      (interactive "P")
      (if nosync
          (when (buffer-live-p (get-buffer gnus-group-buffer))
            (gnus-group-get-new-news))
        (gnus-group-get-new-news)
        (message "Syncing groups...")
        (when (not (zerop my:gnus-group-sync-running-processes))
          (user-error "Syncing already in progress..."))
        (let ((buf (current-buffer)))
          (dolist (command my:gnus-group-sync-programs)
            (let* ((_ (split-string-and-unquote command))
                   (program (car _))
                   (args (cdr _))
                   (process (apply #'start-process
                                   program
                                   my:gnus-group-sync-buffer-name
                                   program
                                   args)))
              (setq my:gnus-group-sync-running-processes
                    (1+ my:gnus-group-sync-running-processes))
              (set-process-sentinel process #'my:gnus-group-sync-sentinel)))
          (when (not (window-live-p my:gnus-group-sync-window))
            (setq my:gnus-group-sync-window
                  (my:gnus-group-sync-make-window my:gnus-group-sync-buffer-name 8))))))

    (defun gnus-group-set-keys-hook ()
      (local-set-key "g" #'my:gnus-group-sync))

    (defun gnus-summary-set-keys-hook ()
      (local-set-key (kbd "S-<tab>") 'gnus-summary-prev-unread-article)
      (local-set-key (kbd "<tab>") 'gnus-summary-next-unread-article)
      (local-set-key "n" 'gnus-summary-next-article)
      (local-set-key "p" 'gnus-summary-prev-article)

      (local-set-key "!" 'gnus-summary-put-mark-as-ticked-next)
      (local-set-key "d" 'gnus-summary-put-mark-as-expirable-next)
      (local-set-key "u" 'gnus-summary-clear-mark-forward)
      (local-set-key "U" 'gnus-summary-unmark-all-processable)

      (local-set-key "c" 'gnus-summary-mail-other-window)
      (local-set-key "F" 'gnus-summary-mail-forward)
      (local-set-key "r" 'gnus-summary-reply-with-original)
      (local-set-key "R" 'gnus-summary-reply-to-list-with-original)

      (local-set-key "a" 'my:gnus-summary-archive)
      (local-set-key "d" 'my:gnus-summary-trash)
      (local-set-key "f" 'my:gnus-summary-followup)
      (local-set-key "h" 'my:gnus-summary-hold)
      (local-set-key "D" 'gnus-summary-delete-article)
      (local-set-key "m" 'gnus-summary-mark-as-processable)

      (local-set-key "g" 'gnus-summary-rescan-group)
      (local-set-key "?" 'gnus-info-find-node)

      (local-set-key "va" 'gnus-summary-save-parts)
      (local-set-key "vv" 'gnus-article-view-part)

      (local-set-key "$f" 'gnus-summary-sort-by-author)
      (local-set-key "$a" 'gnus-summary-sort-by-original)
      (local-set-key "$d" 'gnus-summary-sort-by-date)
      (local-set-key "$s" 'gnus-summary-sort-by-subject)
      (local-set-key "$z" 'gnus-summary-sort-by-chars)
      (local-set-key "$e" 'gnus-summary-sort-by-score))

    (add-hook 'gnus-group-mode-hook #'gnus-group-set-keys-hook)
    (add-hook 'gnus-summary-mode-hook #'gnus-summary-set-keys-hook)))

(user-package multiple-cursors
  :bind (("M-m" . mc/mark-more-like-this-extended)
         ("M-p" . mc/mark-all-in-region)
         ("M-n" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines))
  :defines (multiple-cursors-mode
            mc--read-char
            multiple-cursors-mode
            mc--read-quoted-char)
  :ensure multiple-cursors
  :config
  (progn
    (bind-key "f" 'mc/mmlte--right mc/mark-more-like-this-extended-keymap)
    (bind-key "b" 'mc/mmlte--left mc/mark-more-like-this-extended-keymap)
    (bind-key "n" 'mc/mmlte--down mc/mark-more-like-this-extended-keymap)
    (bind-key "p" 'mc/mmlte--up mc/mark-more-like-this-extended-keymap)))

(user-package multi-web-mode
  :if (not noninteractive)
  :ensure multi-web-mode
  :config (progn
            (setq mweb-default-major-mode 'html-mode)
            (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                              (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                              (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
            (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
            (multi-web-global-mode 1)))

(user-package nginx-mode
  :ensure nginx-mode)

(user-package org
  :ensure org
  :config
  (progn
    (user-package org-present
      :ensure org-present)
    (setq org-src-fontify-natively t)
    (setq org-export-html-coding-system 'utf-8)))

(user-package page
  :bind (("C-M-}" . forward-page)
         ("C-M-{" . backward-page)))

(user-package paren
  :config (show-paren-mode 1))

(user-package php-mode
  :if (not noninteractive)
  :ensure php-mode)

(user-package powerline
  :if (not noninteractive)
  :ensure powerline
  :config (powerline-default-theme))

(user-package projectile
  :if (not noninteractive)
  :diminish projectile-mode
  :ensure projectile
  :config (projectile-global-mode 1))

(user-package python
  :config
  (progn
    (user-package jedi
      :ensure jedi)
    (setq jedi:complete-on-dot t)
    (remove-hook 'python-mode-hook 'wisent-python-default-setup)
    (add-hook 'python-mode-hook 'jedi:setup)))

(user-package python-django
  :if (not noninteractive)
  :bind ("C-x j" . python-django-open-project)
  :ensure python-django)

(user-package repeat
  :if (not noninteractive)
  :bind ("C-z" . repeat))

(user-package rainbow-mode
  :if (not noninteractive)
  :ensure rainbow-mode
  :config (progn
            (mapc (lambda (mode)
                    (add-to-list 'rainbow-r-colors-major-mode-list mode))
                  '(css-mode emacs-lisp-mode lisp-interaction-mode))
            (add-hook 'prog-mode-hook #'rainbow-turn-on)))

(user-package rainbow-delimiters
  :if (not noninteractive)
  :ensure rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(user-package region-bindings-mode
  :if (not noninteractive)
  :ensure region-bindings-mode
  :config (progn
            (bind-key "m" 'mc/mark-more-like-this-extended region-bindings-mode-map)
            (bind-key "a" 'mc/mark-all-like-this region-bindings-mode-map)
            (bind-key "p" 'mc/mark-previous-like-this region-bindings-mode-map)
            (bind-key "n" 'mc/mark-next-like-this region-bindings-mode-map)
            (bind-key "b" 'mc/skip-to-previous-like-this region-bindings-mode-map)
            (bind-key "f" 'mc/skip-to-next-like-this region-bindings-mode-map)
            (bind-key "P" 'mc/unmark-previous-like-this region-bindings-mode-map)
            (bind-key "N" 'mc/unmark-next-like-this region-bindings-mode-map)
            (bind-key "u" 'er/contract-region region-bindings-mode-map)
            (setq region-bindings-mode-disabled-modes '(term-mode))
            (setq region-bindings-mode-disable-predicates
                  (list (lambda () buffer-read-only)))
            (region-bindings-mode-enable)))

(user-package rcirc
  :config
  (progn
    (user-package rcirc-groups
      :ensure rcirc-groups)
    (user-package rcirc-alertify
      :ensure rcirc-alertify)
    (rcirc-alertify-enable)
    (setq rcirc-log-flag t)
    (setq rcirc-time-format "%Y-%m-%d %H:%M ")
    (rcirc-track-minor-mode -1)
    ;; Based on http://www.emacswiki.org/emacs/rcircAll
    (defun-rcirc-command all (input)
      "Run the arguments as a command for all connections."
      (interactive "s")
      (mapc (lambda (process)
              (with-current-buffer (process-buffer process)
                (goto-char (point-max))
                (insert "/" input)
                (rcirc-send-input)))
            (rcirc-process-list)))))

(user-package savehist
  :config (progn
            (savehist-mode 1)))

(user-package scroll-bar
  :config (scroll-bar-mode -1))

(user-package simple
  :config
  (progn
    (let ((xsel-program (executable-find "xsel")))
      (when xsel-program
        (defun xsel-cut-function (text &optional push)
          (when (getenv "DISPLAY")
            (with-temp-buffer
              (insert text)
              (call-process-region
               (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input"))))
        (defun xsel-paste-function()
          (when (getenv "DISPLAY")
            (let ((xsel-output
                   (shell-command-to-string "xsel --clipboard --output")))
              (unless (string= (car kill-ring) xsel-output)
                xsel-output))))
        (setq interprogram-cut-function 'xsel-cut-function
              interprogram-paste-function 'xsel-paste-function)))))

(user-package smartparens
  :if (not noninteractive)
  :ensure smartparens
  :diminish (smartparens-mode . " π")
  :config (progn
            (--each sp--html-modes
              (eval-after-load (symbol-name it) '(require 'smartparens-html)))
            (eval-after-load "latex" '(require 'smartparens-latex))
            (eval-after-load "tex-mode" '(require 'smartparens-latex))

            (sp-pair "'" nil :unless '(sp-point-after-word-p))

            (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

            (sp-with-modes '(markdown-mode rst-mode)
              (sp-local-pair "*" "*" :bind "C-*")
              (sp-local-tag "2" "**" "**")
              (sp-local-tag "s" "```scheme" "```")
              (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

            (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
              (sp-local-tag "i" "\"<" "\">"))

            (sp-with-modes '(html-mode sgml-mode)
              (sp-local-pair "<" ">"))

            (bind-key (kbd "C-M-f") 'sp-forward-sexp sp-keymap)
            (bind-key (kbd "C-M-b") 'sp-backward-sexp sp-keymap)

            (bind-key (kbd "C-M-d") 'sp-down-sexp sp-keymap)
            (bind-key (kbd "C-M-a") 'sp-backward-down-sexp sp-keymap)
            (bind-key (kbd "C-S-a") 'sp-beginning-of-sexp sp-keymap)
            (bind-key (kbd "C-S-d") 'sp-end-of-sexp sp-keymap)

            (bind-key (kbd "C-M-e") 'sp-up-sexp sp-keymap)
            (bind-key (kbd ")") 'sp-up-sexp sp-keymap)
            (bind-key (kbd "C-M-u") 'sp-backward-up-sexp sp-keymap)
            (bind-key (kbd "C-M-t") 'sp-transpose-sexp sp-keymap)

            (bind-key (kbd "C-M-n") 'sp-next-sexp sp-keymap)
            (bind-key (kbd "C-M-p") 'sp-previous-sexp sp-keymap)

            (bind-key (kbd "C-M-k") 'sp-kill-sexp sp-keymap)
            (bind-key (kbd "C-M-w") 'sp-copy-sexp sp-keymap)

            (bind-key (kbd "M-<delete>") 'sp-unwrap-sexp sp-keymap)

            (bind-key (kbd "C-<right>") 'sp-forward-slurp-sexp sp-keymap)
            (bind-key (kbd "C-<left>") 'sp-forward-barf-sexp sp-keymap)
            (bind-key (kbd "C-M-<left>") 'sp-backward-slurp-sexp sp-keymap)
            (bind-key (kbd "C-M-<right>") 'sp-backward-barf-sexp sp-keymap)

            (bind-key (kbd "M-D") 'sp-splice-sexp sp-keymap)
            (bind-key (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward sp-keymap)
            (bind-key (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward sp-keymap)
            (bind-key (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around sp-keymap)

            (bind-key (kbd "C-]") 'sp-select-next-thing-exchange sp-keymap)
            (bind-key (kbd "M-]") 'sp-select-previous-thing sp-keymap)
            (bind-key (kbd "C-M-]") 'sp-select-next-thing sp-keymap)

            (bind-key (kbd "M-F") 'sp-forward-symbol sp-keymap)
            (bind-key (kbd "M-B") 'sp-backward-symbol sp-keymap)

            (bind-key (kbd "M-<backspace>") 'sp-backward-kill-word sp-keymap)
            (bind-key (kbd "M-<up>") 'sp-splice-sexp-killing-backward sp-keymap)
            (bind-key (kbd "M-<down>") 'sp-splice-sexp-killing-forward sp-keymap)
            (bind-key (kbd "C-<right>") 'sp-forward-slurp-sexp sp-keymap)
            (bind-key (kbd "C-<left>") 'sp-forward-barf-sexp sp-keymap)

            (sp-with-modes sp--lisp-modes
              ;; disable ', it's the quote character!
              (sp-local-pair "'" nil :actions nil)
              ;; also only use the pseudo-quote inside strings where it serve as
              ;; hyperlink.
              (sp-local-pair "`" "'" :when '(sp-in-string-p))
              (sp-local-pair "(" nil :bind "M-("))

            (add-hook 'smartparens-enabled-hook
                      (lambda ()
                        (when (memq major-mode sp--lisp-modes)
                          (smartparens-strict-mode 1))))

            (smartparens-global-mode 1)
            (show-smartparens-global-mode 1)))

(user-package smex
  :if (not noninteractive)
  :ensure smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(user-package sunrise-commander
  :ensure sunrise-commander
  :config (progn
            (user-package sunrise-x-buttons
              :ensure sunrise-x-buttons)
            (user-package sunrise-x-loop
              :ensure sunrise-x-loop)))

(user-package tex-site
  :ensure auctex
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn
    (user-package latex-mode
      :defer t
      :config
      (progn
        (user-package preview)
        (user-package ac-math
          :ensure ac-math)
        (defun ac-latex-mode-setup ()
          (nconc ac-sources
                 '(ac-source-math-unicode ac-source-math-latex
                                          ac-source-latex-commands)))
        (add-to-list 'ac-modes 'latex-mode)
        (add-hook 'latex-mode-hook 'ac-latex-mode-setup)))))

(user-package tool-bar
  :config (tool-bar-mode -1))

(user-package undo-tree
  :if (not noninteractive)
  :diminish undo-tree-mode
  :bind ("C-x _" . undo-tree-visualize)
  :ensure undo-tree
  :config (global-undo-tree-mode 1))

(user-package uniquify
  :if (not noninteractive)
  :config (setq uniquify-buffer-name-style 'forward))

(user-package vc
  :config (progn
            (setq vc-bzr-diff-switches "-F git"
                  vc-make-backup-files t)))

(user-package warnings
  :config (setq warning-suppress-types nil))

(user-package which-func
  :if (not noninteractive)
  :config (which-function-mode 1))

(user-package whitespace
  :if (not noninteractive)
  :diminish (global-whitespace-mode . " ω")
  :config (progn
            (setq whitespace-style '(trailing tabs indentation::space face))
            (setq whitespace-global-modes
                  '(c-mode c++-mode clojure-mode emacs-lisp-mode js-mode php-mode
                           python-mode lisp-mode))
            (global-whitespace-mode 1)))

(user-package windmove
  :if (not noninteractive)
  :config (windmove-default-keybindings))

(user-package winner
  :if (not noninteractive)
  :diminish winner-mode
  :init
  (progn
    (winner-mode 1)
    (bind-key "M-N" 'winner-redo winner-mode-map)
    (bind-key "M-P" 'winner-undo winner-mode-map)))

(user-package yaml-mode
  :if (not noninteractive)
  :ensure yaml-mode)

(user-package yasnippet
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-expand)
  :ensure yasnippet
  :defines (yas-dont-activate yas-keymap)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (progn
    (defun yas-not-activate ()
      (memq major-mode '(term-mode)))
    (set-default 'yas-dont-activate (cons #'yas-not-activate yas-dont-activate))
    (yas-global-mode 1)
    (yas-load-directory (expand-file-name "snippets/" user-emacs-directory))
    (bind-key "<tab>" 'yas-next-field-or-maybe-expand yas-keymap)
    (bind-key "C-c y TAB" 'yas-expand yas-keymap)
    (bind-key "C-c y n" 'yas-new-snippet yas-keymap)
    (bind-key "C-c y f" 'yas-find-snippets yas-keymap)
    (bind-key "C-c y r" 'yas-reload-all yas-keymap)
    (bind-key "C-c y v" 'yas-visit-snippet-file yas-keymap)))

(user-package zencoding-mode
  :if (not noninteractive)
  :ensure zencoding-mode
  :config
  (progn
    (bind-key "C-j" nil zencoding-mode-keymap)
    (add-hook 'sgml-mode-hook 'zencoding-mode)))


;;; Random utilities

(defun su ()
  "Reopen current file as root."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/su::" buffer-file-name))))

(defun sudo ()
  "Reopen current file as sudoer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

;; Modified to work with positives prefix args.
;; http://www.emacswiki.org/emacs/InsertFileName.
(defun insert-file-name (filename &optional arg)
  "Insert name of file FILENAME into buffer after point.

  With \\[universal-argument] ARG <= 1, insert filename's
  relative path.  See `file-relative-name' for details.

  With 1 < \\[universal-argument] ARG <= 4, insert filename's
  fully canocalized path.  See `expand-file-name'.

  With \\[universal-argument] ARG > 4, insert the file name
  exactly as it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \np")
  (cond ((<= arg 1)
         (insert (file-relative-name filename)))
        ((<= arg 4)
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(defun kill-ring-save-file-name (&optional arg)
  "Save file name for current buffer to `kill-ring'.

  With \\[universal-argument] ARG <= 1, save filename's relative
  path.  See `file-relative-name' for details.

  With 1 < \\[universal-argument] ARG <= 4, save filename's fully
  canocalized path.  See `expand-file-name'.

  With \\[universal-argument] ARG > 4, save the current value of
  `buffer-file-name' unmodified."
  (interactive "p")
  (when buffer-file-name
    (let ((file-name buffer-file-name))
      (with-temp-buffer
        (insert-file-name file-name (or arg 1))
        (kill-region (point-min) (point-max))))))

(defun other-window-backward (count &optional all-frames)
  (interactive "p")
  (other-window (- count) all-frames))

(defun scratch ()
  "Switch to *scratch* buffer with using current `major-mode'."
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window
     (get-buffer-create "*scratch*"))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;;; Global

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.01
      enable-recursive-minibuffers t
      max-specpdl-size 999999999
      redisplay-dont-pause t
      visible-bell t)

(setq-default
 frame-title-format
 '(:eval
   (format "Emacs - %s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (or (buffer-file-name)
               (and (fboundp 'emms-playlist-buffer-list)
                    (member (current-buffer) (emms-playlist-buffer-list))
                    (emms-playlist-current-selected-track)
                    (emms-track-description
                     (emms-playlist-current-selected-track)))
               (buffer-name))))
 indent-tabs-mode nil)

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(setq inhibit-startup-screen t
      initial-scratch-message ""
      x-select-enable-clipboard t
      x-select-enable-primary t)

(load "~/.emacs.d/secrets.el" 'noerror)
(load "~/.emacs.d/post-startup.el" 'noerror)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; Global bindings
;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(bind-key "<f1>" #'help-command)

;;; Fixes for xterm keys
(bind-key "\e[7~" [home] function-key-map)
(bind-key "\e[8~" [end] function-key-map)
(bind-key "\e[11~" [f1] function-key-map)
(bind-key "\e[12~" [f2] function-key-map)
(bind-key "\e[13~" [f3] function-key-map)
(bind-key "\e[14~" [f4] function-key-map)

(bind-key "\033[1;5A" [(control up)] function-key-map)
(bind-key "\033[1;5B" [(control down)] function-key-map)
(bind-key "\033[1;5D" [(control left)] function-key-map)
(bind-key "\033[1;5C" [(control right)] function-key-map)
(bind-key "\033[5;5~" [(control home)] function-key-map)
(bind-key "\033[6;5~" [(control end)] function-key-map)
(bind-key "\033[3;5~" [(control delete)] function-key-map)
(bind-key "\033[1;5F" [(control prior)] function-key-map)
(bind-key "\033[1;5H" [(control next)] function-key-map)
(bind-key "\033[1;5H" [(control next)] function-key-map)

(bind-key "\033[1;3A" [(meta up)] function-key-map)
(bind-key "\033[1;3B" [(meta down)] function-key-map)
(bind-key "\033[1;3D" [(meta left)] function-key-map)
(bind-key "\033[1;3C" [(meta right)] function-key-map)
(bind-key "\033[1;3H" [(meta home)] function-key-map)
(bind-key "\033[1;3F" [(meta end)] function-key-map)
(bind-key "\033[3;3~" [(meta delete)] function-key-map)
(bind-key "\033[5;3~" [(meta prior)] function-key-map)
(bind-key "\033[6;3~" [(meta next)] function-key-map)

(bind-key "\033[1;2A" [(shift up)] function-key-map)
(bind-key "\033[1;2B" [(shift down)] function-key-map)
(bind-key "\033[1;2D" [(shift left)] function-key-map)
(bind-key "\033[1;2C" [(shift right)] function-key-map)

;; Local Variables:
;; mode: emacs-lisp
;; mode: hs-minor
;; hs-block-start-regexp: "^(user-package[[:space:]]+[^
;; ]+"
;; End:

;;; init ends here.
