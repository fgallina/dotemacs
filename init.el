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

(setq el-get-sources
      '(emms-info-mediainfo jquery-doc mu4e mu4e-multi python rcirc-groups))

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
          '(ace-jump-char-mode ace-jump-word-mode ace-jump-line-mode))))

(user-package ack-and-a-half
  :if (not noninteractive)
  :ensure ack-and-a-half
  :config (progn
            (defalias 'ack 'ack-and-a-half)
            (defalias 'ack-same 'ack-and-a-half-same)
            (defalias 'ack-find-file 'ack-and-a-half-find-file)
            (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
            (setq ack-and-a-half-prompt-for-directory t)
            (setq ack-and-a-half-use-ido t)))

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
    (user-package cider-tracing
      :ensure cider-tracing)
    (user-package ac-nrepl
      :ensure ac-nrepl)
    (user-package clojure-cheatsheet
      :ensure clojure-cheatsheet)
    (user-package clj-refactor
      :ensure clj-refactor)
    (user-package clojure-test-mode
      :ensure clojure-test-mode)
    (setq
     cider-lein-command (executable-find "lein")
     cider-popup-stacktraces t
     cider-repl-mode-hook #'subword-mode
     nrepl-hide-special-buffers t)
    (add-to-list 'same-window-buffer-names "*nrepl*")
    (add-hook 'clojure-mode-hook #'cider-mode)
    (add-hook 'clojure-mode-hook #'clojure-test-mode)
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
    (user-package cyberpunk-theme
      :ensure cyberpunk-theme)
    (load-theme 'cyberpunk t)))

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
    (user-package emms-info-mediainfo)
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
  :bind ("S-SPC" . er/expand-region)
  :ensure expand-region)

(user-package files
  :config (progn
            (setq auto-save-default nil)
            (setq backup-directory-alist
                  `(("." . ,(expand-file-name
                             (concat user-emacs-directory "backups")))))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(user-package ido
  :if (not noninteractive)
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces nil)
            (user-package flx
              :ensure flx
              :config (user-package flx-ido
                        :ensure flx-ido
                        :config (progn
                                  (flx-ido-mode 1)
                                  (setq flx-ido-use-faces t))))))

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
    (user-package jquery-doc)
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
    (define-key magit-status-mode-map (kbd "W") 'magit-diff-toggle-whitespace)))

(user-package markdown-mode
  :if (not noninteractive)
  :ensure markdown-mode)

(user-package menu-bar
  :bind ("M-k" . kill-this-buffer)
  :config (menu-bar-mode 1))

(user-package mu4e
  :if (not noninteractive)
  :bind ("<f1>" . mu4e)
  :init (let ((mu4e-dir (expand-file-name "mu4e" el-get-dir)))
          (add-to-list 'load-path (expand-file-name "mu4e" mu4e-dir))
          (setq mu4e-mu-binary (expand-file-name "mu" (expand-file-name "mu" mu4e-dir))))
  :config
  (progn
    (user-package message
      :config (setq
               message-kill-buffer-on-exit t
               message-send-mail-function 'message-send-mail-with-sendmail
               message-sendmail-envelope-from 'header))
    (user-package sendmail
      :config (setq sendmail-program (executable-find "msmtp")))
    (setq
     mail-user-agent 'mu4e
     mu4e-attachment-dir (expand-file-name "~/Downloads")
     mu4e-compose-complete-addresses t
     mu4e-confirm-quit nil
     mu4e-get-mail-command "offlineimap -o"
     mu4e-headers-date-format "%b, %d %Y. %H:%M"
     mu4e-headers-fields '((:maildir . 12)
                           (:human-date . 20)
                           (:flags . 6)
                           (:mailing-list . 10)
                           (:from . 20)
                           (:subject . nil))
     mu4e-headers-include-related nil
     mu4e-headers-leave-behavior 'ask
     mu4e-headers-results-limit 500
     mu4e-headers-skip-duplicates t
     mu4e-html2text-command "w3m -o display_link_number=1 -dump -T text/html"
     mu4e-maildir (expand-file-name "~/Maildir")
     mu4e-sent-messages-behavior 'delete
     mu4e-use-fancy-chars t
     mu4e-view-image-max-width 800
     mu4e-view-prefer-html t
     mu4e-view-show-addresses t
     mu4e-view-show-images nil)

    (defun my:mu4e-toggle-headers-include-related ()
      "Toggle `mu4e-headers-include-related' and refresh."
      (interactive)
      (setq mu4e-headers-include-related
            (not mu4e-headers-include-related))
      (mu4e-headers-rerun-search))

    (define-key 'mu4e-headers-mode-map "o"
      'my:mu4e-toggle-headers-include-related)

    (user-package mu4e-multi)))

(user-package multiple-cursors
  :bind (("M-m" . mc/mark-more-like-this-extended)
         ("M-p" . mc/mark-all-in-region)
         ("M-n" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines))
  :defines (multiple-cursors-mode
            mc--read-char
            mc--read-char
            multiple-cursors-mode
            mc--read-quoted-char
            mc--read-quoted-char
            rectangular-region-mode
            rectangular-region-mode)
  :ensure multiple-cursors)

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
            (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
            (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
            (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
            (define-key region-bindings-mode-map "P" 'mc/unmark-previous-like-this)
            (define-key region-bindings-mode-map "N" 'mc/unmark-next-like-this)
            (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
            (define-key region-bindings-mode-map "f" 'er/expand-region)
            (define-key region-bindings-mode-map "b" 'er/contract-region)
            (setq region-bindings-mode-disabled-modes '(term-mode))
            (setq region-bindings-mode-disable-predicates
                  (list
                   (lambda ()
                     buffer-read-only)))
            (region-bindings-mode-enable)))

(user-package rcirc
  :config (progn
            (user-package rcirc-groups
              :ensure rcirc-notify)
            (setq rcirc-log-flag t)
            (setq rcirc-time-format "%Y-%m-%d %H:%M ")
            (rcirc-track-minor-mode -1)))

(user-package savehist
  :config (progn
            (savehist-mode 1)))

(user-package scroll-bar
  :config (scroll-bar-mode -1))

(user-package simple
  :config (progn
            ;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking
            (defun yank-pop-backwards ()
              "Yank backwards."
              (interactive)
              (yank-pop -1))
            (bind-key "M-Y" 'yank-pop-backwards)))

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

            (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
            (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

            (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
            (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
            (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
            (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

            (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
            (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
            (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
            (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

            (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
            (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

            (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
            (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

            (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
            (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

            (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
            (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
            (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
            (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

            (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
            (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
            (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
            (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

            (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
            (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
            (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

            (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
            (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

            (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-kill-word)
            (define-key sp-keymap (kbd "M-<up>") 'sp-splice-sexp-killing-backward)
            (define-key sp-keymap (kbd "M-<down>") 'sp-splice-sexp-killing-forward)
            (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
            (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)

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

            (smartparens-global-mode t)
            (show-smartparens-global-mode t)))

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

(user-package term
  :config (progn
            ;; Based on:
            ;; http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
            (defun ansi-term-visit-dwim ()
              "Visit ansi-term Doing What I Mean
If the current buffer is:
  1) a running ansi-term named *ansi-term*, rename it.
  2) a stopped ansi-term, kill it and create a new one.
  3) a non ansi-term, go to an already running ansi-term
     or start a new one while killing a defunt one"
              (interactive)
              (let ((is-term (string= "term-mode" major-mode))
                    (is-running (term-check-proc (buffer-name)))
                    (anon-term (get-buffer "*ansi-term*"))
                    (program (or explicit-shell-file-name
                                 (getenv "ESHELL")
                                 (getenv "SHELL")
                                 "/bin/sh")))
                (if is-term
                    (if is-running
                        (if (string= "*ansi-term*" (buffer-name))
                            (let ((newname
                                   (format "*ansi-term: %s*"
                                           (read-buffer "Rename buffer (to new name): "))))
                              (rename-buffer newname))
                          (if anon-term
                              (switch-to-buffer "*ansi-term*")
                            (ansi-term program)))
                      (kill-buffer (buffer-name))
                      (ansi-term program))
                  (if anon-term
                      (if (term-check-proc "*ansi-term*")
                          (switch-to-buffer "*ansi-term*")
                        (kill-buffer "*ansi-term*")
                        (ansi-term program))
                    (ansi-term program)))))

            (defun my:term-mode-hook ()
              (make-local-variable 'mouse-yank-at-point)
              (make-local-variable 'transient-mark-mode)
              (auto-fill-mode -1)
              (setq mouse-yank-at-point t
                    term-scroll-to-bottom-on-output nil
                    term-scroll-show-maximum-output nil
                    term-buffer-maximum-size 2048))

            (add-hook 'term-mode-hook 'my:term-mode-hook)

            (bind-key "<f2>" #'ansi-term-visit-dwim)))

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
  :config (add-hook 'sgml-mode-hook 'zencoding-mode))


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
  (cond ((<= 1 arg)
         (insert (file-relative-name filename)))
        ((<= 4 arg)
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(defun other-window-backward (count &optional all-frames)
  (interactive "p")
  (other-window (- count) all-frames))

(defun scratch ()
  "Switch to *scratch* buffer with using current `major-mode'."
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window
     (get-buffer-create "*scratch*"))))


;;; Global

(bind-key "M-3" 'split-window-horizontally)
(bind-key "M-2" 'split-window-vertically)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-0" 'delete-window)
(bind-key "M-o" 'other-window)
(bind-key "M-O" 'other-window-backward)

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
      x-select-enable-clipboard t)

(load "~/.emacs.d/secrets.el" 'noerror)
(load "~/.emacs.d/post-startup.el" 'noerror)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Local Variables:
;; mode: emacs-lisp
;; mode: hs-minor
;; hs-block-start-regexp: "^(user-package[[:space:]]+[^
;; ]+"
;; End:

;;; init ends here.
