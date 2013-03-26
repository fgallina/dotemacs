(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-completion)))

(setq smart-tab-disabled-major-modes
      (list 'org-mode 'term-mode
            'inferior-python-mode 'rcirc-mode 'term-mode 'mu4e-view-mode
            'mu4e-main-mode 'mu4e-headers-mode 'mu4e-compose-mode
            'mu4e-about-mode))

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(setq smart-tab-using-hippie-expand t)

(global-smart-tab-mode 1)
