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
               :pkgname "jwiegley/emacs-async")
        (:name multi-web-mode
               :description "Multi Web Mode is a minor mode which makes web editing in Emacs much easier"
               :type github
               :pkgname "fgallina/multi-web-mode")
        (:name org-s5
               :description "Org-mode html export of S5 slideshow presentations"
               :type github
               :pkgname "eschulte/org-S5")
        (:name startupd
               :description "Modular loading of Emacs configuration"
               :type github
               :pkgname "fgallina/startupd.el")))

(setq my:el-get-packages
      '(async
        auto-complete
        clojure-mode
        coffee-mode
        deft
        emms
        expand-region
        full-ack
        lua-mode
        magit
        multi-web-mode
        nyan-mode
        org-mode
        org-s5
        o-blog
        php-mode-improved
        python
        rainbow-mode
        scss-mode
        sass-mode
        slime
        smart-tab
        startupd
        yaml-mode
        yasnippet
        zencoding-mode
        ))

(el-get 'sync my:el-get-packages)

(ignore-errors (load-file "~/.emacs.d/secrets.el"))
(ignore-errors (load-file "~/.emacs.d/pre-startup.el"))

(require 'startupd)
(startupd-load-files)

(ignore-errors (load-file "~/.emacs.d/post-startup.el"))

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)
