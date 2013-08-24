(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defvar my:el-get-packages nil
  "List of packages intended to be installed/loaded.")

(defvar my:el-get-disabled-packages nil
  "List of packages not intended to be installed/loaded.")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-recipe-path (cons "~/.emacs.d/recipes" el-get-recipe-path)
      el-get-user-package-directory "~/.emacs.d/conf"
      my:el-get-packages
      '(ace-jump-mode
        ack-and-a-half
        auctex
        auto-complete
        clojure-mode
        coffee-mode
        deft
        dired-details
        emms
        expand-region
        flx
        gh
        gist
        jedi
        jquery-doc
        lua-mode
        magit
        markdown-mode
        nrepl
        ac-nrepl
        mu4e
        multiple-cursors
        multi-web-mode
        org-s5
        paredit
        php-mode-improved
        powerline
        projectile
        python
        python-django
        rainbow-mode
        region-bindings-mode
        rcirc-groups
        rcirc-notify
        scss-mode
        slime
        sunrise-commander
        sunrise-x-buttons
        sunrise-x-loop
        undo-tree
        smex
        startupd
        yaml-mode
        yasnippet
        zencoding-mode))

(load "~/.emacs.d/pre-startup.el" 'noerror)

;; Set common PATHs to search for executables.
(let ((bindirs (list
                (expand-file-name "~/.emacs.d/bin")
                (expand-file-name "~/bin"))))
  (dolist (dir bindirs)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))
    (add-to-list 'exec-path dir)))

;; Manage ignored packages
(when my:el-get-disabled-packages
  (let ((package-list (copy-list my:el-get-packages)))
    (setq my:el-get-packages nil)
    (dolist (package package-list)
      (when (not (memq package my:el-get-disabled-packages))
        (setq my:el-get-packages
              (cons package my:el-get-packages)))))
  (setq my:el-get-packages (reverse my:el-get-packages)))

;; Load all packages and configuration files
(package-initialize)
(el-get 'sync my:el-get-packages)
(startupd-load-files)

(load "~/.emacs.d/secrets.el" 'noerror)
(load "~/.emacs.d/post-startup.el" 'noerror)

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)
