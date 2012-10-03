(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-completion)))

(setq smart-tab-disabled-major-modes
      '(org-mode term-mode inferior-python-mode rcirc-mode))

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(setq smart-tab-using-hippie-expand t)

(global-smart-tab-mode 1)
