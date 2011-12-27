(setq inferior-lisp-program (executable-find "clisp"))
(add-to-list 'load-path "~/.emacs.d/vendor/slime")
(setq slime-lisp-implementations
      `((common-lisp (,(executable-find "clisp")))))
(require 'slime-autoloads)
(slime-setup)
