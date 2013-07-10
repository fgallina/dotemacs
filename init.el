(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defvar my:el-get-disabled-packages nil
  "List of packages not intended to be installed/loaded.")

(if (not (require 'el-get nil t))
    (url-retrieve
     "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
     (lambda (s)
       (let (el-get-master-branch)
         (end-of-buffer)
         (eval-print-last-sexp))
       (load-file "~/.emacs.d/init.el")))

  (setq el-get-sources
        '((:name async
                 :description "Simple library for asynchronous processing in Emacs"
                 :type github
                 :pkgname "jwiegley/emacs-async"
                 :features async)
          (:name base16-themes
                 :description "Base16 provides carefully chosen syntax highlighting and a default set of sixteen colors suitable for a wide range of applications."
                 :website "http://chriskempson.github.io/base16/"
                 :type github
                 :pkgname "fgallina/base16-emacs"
                 :post-init (add-to-list 'custom-theme-load-path
                                         default-directory))
          (:name emms
                 :description "The Emacs Multimedia System"
                 :type git
                 :url "git://git.sv.gnu.org/emms.git"
                 :info "doc"
                 :load-path ("./lisp")
                 :features emms-setup
                 :build `(("mkdir" "-p" ,(expand-file-name (format "%s/emms" user-emacs-directory)))
                          ("make" ,(format "EMACS=%s" el-get-emacs)
                           ,(format "SITEFLAG=\\\"--no-site-file -L %s/emacs-w3m/ \\\""
                                    el-get-dir) "lisp")
                          ("make" "emms-print-metadata")
                          ("mv" "src/emms-print-metadata" ,(expand-file-name "bin/" user-emacs-directory)))
                 :depends emacs-w3m)
          (:name jedi
                 :description "An awesome Python auto-completion for Emacs"
                 :type github
                 :pkgname "tkf/emacs-jedi"
                 :build (("PYTHON=python2" "make" "requirements"))
                 :submodule nil
                 :depends (epc auto-complete))
          (:name magit
                 :website "https://github.com/magit/magit#readme"
                 :description "It's Magit! An Emacs mode for Git."
                 :type github
                 :pkgname "magit/magit"
                 :info "."
                 ;; let el-get care about autoloads so that it works with all OSes
                 :build (if (version-list-<= (version-to-list "24.3") (version-to-list emacs-version))
                            `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
                          `(("make" ,(format "EMACS=%s" el-get-emacs) "docs")))
                 :build/berkeley-unix (("touch" "`find . -name Makefile`") ("gmake")))
          (:name multi-web-mode
                 :description "Multi Web Mode is a minor mode which makes web editing in Emacs much easier"
                 :type github
                 :pkgname "fgallina/multi-web-mode"
                 :features multi-web-mode)
          (:name org-s5
                 :description "Org-mode html export of S5 slideshow presentations"
                 :type github
                 :pkgname "fgallina/org-S5"
                 :features org-export-as-s5)
          (:name startupd
                 :description "Modular loading of Emacs configuration"
                 :type github
                 :pkgname "fgallina/startupd.el"
                 :features startupd)
          (:name powerline
                 :website "https://github.com/milkypostman/powerline"
                 :description "Powerline for Emacs"
                 :type github
                 :pkgname "milkypostman/powerline"
                 :features powerline)
          (:name python24
                 :description "Python's flying circus support for Emacs (24.x)"
                 :builtin "Emacs 24.3"
                 :type http
                 :url "http://repo.or.cz/w/emacs.git/blob_plain/refs/heads/emacs-24:/lisp/progmodes/python.el")
          (:name python
                 :description "Python's flying circus support for Emacs (trunk version, hopefully Emacs 24.x compatible)"
                 :type http
                 :url "http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el")
          (:name python-django
                 :description "An Emacs package for managing Django projects"
                 :type github
                 :pkgname "fgallina/python-django.el"
                 :features python-django)
          (:name region-bindings-mode
                 :description "A minor mode that enables custom bindings when mark is active."
                 :type github
                 :pkgname "fgallina/region-bindings-mode"
                 :features region-bindings-mode)
          (:name undo-tree
                 :description "Treat undo history as a tree"
                 :type github
                 :pkgname "emacsmirror/undo-tree"
                 :prepare (progn
                            (autoload 'undo-tree-mode "undo-tree.el"
                              "Undo tree mode; see undo-tree.el for details" t)
                            (autoload 'global-undo-tree-mode "undo-tree.el"
                              "Global undo tree mode" t))))
        el-get-user-package-directory "~/.emacs.d/conf")

  (setq my:el-get-packages
        '(ace-jump-mode
          async
          base16-themes
          auto-complete
          clojure-mode
          coffee-mode
          deft
          dired-details
          emms
          expand-region
          full-ack
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
          o-blog
          paredit
          php-mode-improved
          powerline
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

  ;; Lie to package.el, all autoloads are generated by el-get.
  (setq package--initialized t)
  (load "~/.emacs.d/pre-startup.el" 'noerror)

  ;; Set common PATHs to search for executables.
  (let ((bindirs (list
                  (expand-file-name "~/.emacs.d/bin")
                  (expand-file-name "~/bin"))))
    (dolist (dir bindirs)
      (setenv "PATH" (concat dir path-separator (getenv "PATH")))
      (add-to-list 'exec-path dir)))

  (when my:el-get-disabled-packages
    (let ((package-list (copy-list my:el-get-packages)))
      (setq my:el-get-packages nil)
      (dolist (package package-list)
        (when (not (memq package my:el-get-disabled-packages))
          (setq my:el-get-packages
                (cons package my:el-get-packages)))))
    (setq my:el-get-packages (reverse my:el-get-packages)))

  (el-get 'sync my:el-get-packages)
  (startupd-load-files)
  (load "~/.emacs.d/secrets.el" 'noerror)
  (load "~/.emacs.d/post-startup.el" 'noerror)

  (setq custom-file "~/.emacs.d/customizations.el")
  (load custom-file 'noerror))
