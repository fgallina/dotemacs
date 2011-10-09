(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("python" (mode . python-mode))
	       ("elisp" (mode . emacs-lisp-mode))
	       ("lua" (mode . lua-mode))
	       ("javascript" (mode . js-mode))
	       ("html" (mode . html-mode))
	       ("css" (mode . css-mode))
	       ("irc" (mode . rcirc-mode))
	       ("term" (mode . term-mode))
	       ("magit" (name . "^\\*magit"))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Compile-Log\\*$")
			 (name . "^\\*Occur\\*$")
			 (name . "^\\*Pymacs\\*$")
			 (name . "^\\*Completions\\*$")
			 (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
