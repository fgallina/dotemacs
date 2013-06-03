;; http://bit.ly/PXMshy
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (paredit-mode . " π")
    (undo-tree-mode . "")
    (global-whitespace-mode . " _")
    (yas-minor-mode . "")
    ;; Major modes
    (clojure-mode . "Clj")
    (emacs-lisp-mode . "El")
    (js-mode . "Js")
    (lisp-interaction-mode . "λ")
    (python-mode . "Py"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " "))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)
(defalias 'flymake-report-status 'flymake-report-status-slim)
