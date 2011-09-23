(require 'smart-tab)

(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-completion)))

(setq smart-tab-disabled-major-modes
      '(org-mode term-mode inferior-python-mode rcirc-mode))

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(setq smart-tab-using-hippie-expand t)

(defun smart-tab-call-completion-function ()
  "Get a completion function according to current major mode."
  (let ((completion-function
         (cdr (assq major-mode smart-tab-completion-functions-alist))))
    (if (null completion-function)
        (if (and (not (minibufferp))
                 (memq 'auto-complete-mode minor-mode-list)
                 auto-complete-mode)
            (auto-complete)
          (if smart-tab-using-hippie-expand
              (hippie-expand nil)
            (dabbrev-expand nil)))
      (funcall completion-function))))

;;;###autoload
(defun smart-tab (&optional prefix)
  "Try to 'do the smart thing' when tab is pressed.
`smart-tab' attempts to expand the text before the point or
indent the current line or selection.

In a regular buffer, `smart-tab' will attempt to expand with
either `hippie-expand' or `dabbrev-expand', depending on the
value of `smart-tab-using-hippie-expand'. Alternatively, if
`auto-complete-mode' is enabled in the current buffer,
`auto-complete' will be used to attempt expansion. If the mark is
active, or PREFIX is \\[universal-argument], then `smart-tab'
will indent the region or the current line (if the mark is not
active)."
  (interactive "P")
  (if (smart-tab-must-expand prefix)
      (smart-tab-call-completion-function)
    (smart-tab-default)))

(global-smart-tab-mode 1)
