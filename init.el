(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq el-get-sources
      '((:name async
               :description "Simple library for asynchronous processing in Emacs"
               :type github
               :pkgname "jwiegley/emacs-async"
               :features async)
        (:name multi-web-mode
               :description "Multi Web Mode is a minor mode which makes web editing in Emacs much easier"
               :type github
               :pkgname "fgallina/multi-web-mode"
               :features multi-web-mode)
        (:name org-s5
               :description "Org-mode html export of S5 slideshow presentations"
               :type github
               :pkgname "eschulte/org-S5"
               :features nil)
        (:name startupd
               :description "Modular loading of Emacs configuration"
               :type github
               :pkgname "fgallina/startupd.el"
               :features startupd)
        (:name pymacs2
               :description "Interface between Emacs Lisp and Python"
               :type github
               :pkgname "pinard/Pymacs"
               :features pymacs
               :prepare
               (progn
                 (autoload 'pymacs-load "pymacs" nil t)
                 (autoload 'pymacs-eval "pymacs" nil t)
                 (autoload 'pymacs-exec "pymacs" nil t)
                 (autoload 'pymacs-call "pymacs")
                 (autoload 'pymacs-apply "pymacs"))
               :build ("make PYTHON=python2"))
        (:name pymacs3
               :description "Interface between Emacs Lisp and Python"
               :type github
               :pkgname "pinard/Pymacs"
               :features pymacs
               :prepare
               (progn
                 (autoload 'pymacs-load "pymacs" nil t)
                 (autoload 'pymacs-eval "pymacs" nil t)
                 (autoload 'pymacs-exec "pymacs" nil t)
                 (autoload 'pymacs-call "pymacs")
                 (autoload 'pymacs-apply "pymacs"))
               :build ("make PYTHON=python"))
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
        auto-complete
        clojure-mode
        coffee-mode
        deft
        emms
        expand-region
        full-ack
        lua-mode
        magit
        multiple-cursors
        multi-web-mode
        nyan-mode
        org-mode
        org-s5
        o-blog
        php-mode-improved
        pymacs2
        pymacs3
        python
        rainbow-mode
        region-bindings-mode
        ropemacs
        rcirc-groups
        scss-mode
        slime
        undo-tree
        smex
        smart-tab
        startupd
        yaml-mode
        yasnippet
        zencoding-mode))

(el-get 'sync my:el-get-packages)

(ignore-errors (load-file "~/.emacs.d/secrets.el"))
(ignore-errors (load-file "~/.emacs.d/pre-startup.el"))
(startupd-load-files)
(ignore-errors (load-file "~/.emacs.d/post-startup.el"))

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)
